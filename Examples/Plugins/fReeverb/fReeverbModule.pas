unit fReeverbModule;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule,
     fReeverbFilter;

type
  TCombArray = array [0..1] of TComb;
  TAllpassArray = array [0..1] of TAllpass;

  TfReeverbVST = class(TVSTModule)
    procedure VST_EditOpen(Sender: TObject; var GUI: TForm);
    procedure VST2ModuleProcess(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure VST2ModuleProcessReplacing(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure fReeverbVSTParameterProperties0ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure fReeverbVSTParameterProperties1ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure fReeverbVSTParameterProperties2ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure fReeverbVSTParameterProperties3ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VST2ModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure fReeverbVSTParameterProperties4ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure fReeverbVSTParameterProperties5ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure fReeverbVSTParameterProperties6ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure fReeverbVSTParameterProperties8ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure fReeverbVSTParameterProperties7ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VST2ModuleCreate(Sender: TObject);
    procedure VST2ModuleDestroy(Sender: TObject);
  private
    fGain         : Single;
    fRoomSize     : Single;
    fRoomSizeI    : Single;
    fDamp         : Single;
    fDampA        : Single;
    fWet          : Single;
    fWet1         : Single;
    fWet2         : Single;
    fDry          : Single;
    fWidth        : Single;
    fMode         : Single;
    fStretch      : Single;

    fComb         : array of TCombArray; // Comb filters
    fAllpass      : array of TAllpassArray; // Allpass filters
  protected
    procedure UpdateMix;
    procedure Update;
    procedure SetRoomSize(Value: Single);
    function GetRoomSize: Single;
    procedure SetDamp(Value: Single);
    function GetDamp: Single;
    procedure SetWet(Value: Single);
    procedure SetWidth(Value: Single);
    procedure SetMode(Value: Single);
    function GetMode: Single;
    procedure ShuffleAllPassFeedBack;
    procedure BufferRezize;
  public
    procedure Mute;
    property Mode: Single read GetMode write SetMode;
    property Width: Single read fWidth write SetWidth;
    property Dry: Single read fDry write fDry;
    property Wet: Single read fWet write SetWet;
    property Damp: Single read GetDamp write SetDamp;
    property RoomSize: Single read GetRoomSize write SetRoomSize;
  end;

implementation

{$R *.DFM}

uses fReeverbGUI;

procedure TfReeverbVST.VST_EditOpen(Sender: TObject; var GUI: TForm);
begin
 GUI := TFmReverb.Create(Self);
end;

function TfReeverbVST.GetDamp: Single;
begin
 Result := fDamp / scaledamp;
end;

function TfReeverbVST.GetMode: Single;
begin
 if fMode >= freezeMode
  then Result := 1
  else Result := 0;
end;

function TfReeverbVST.GetRoomSize: Single;
begin
 Result := (fRoomSize - offsetroom) / scaleroom;
end;

procedure TfReeverbVST.Mute;
var i: integer;
begin
 if fMode >= freezeMode then Exit;
 for i:=0 to Length(fComb)-1 do
  begin
   fComb[i,0].Mute;
   fComb[i,1].Mute;
  end;
 for i:=0 to Length(fAllpass)-1 do
  begin
   fAllpass[i,0].Mute;
   fAllpass[i,1].Mute;
  end;
end;

procedure TfReeverbVST.SetDamp(Value: Single);
begin
 fDamp := Value * scaledamp;
 Update;
end;

procedure TfReeverbVST.SetMode(Value: Single);
begin
 fMode := Value;
 Update;
end;

procedure TfReeverbVST.SetRoomSize(Value: Single);
begin
 fRoomSize := (Value * scaleroom) + offsetroom;
 Update;
end;

procedure TfReeverbVST.SetWet(Value: Single);
begin
 fWet := Value;
 UpdateMix;
end;

procedure TfReeverbVST.SetWidth(Value: Single);
begin
 fWidth := Value;
 UpdateMix;
end;

procedure TfReeverbVST.UpdateMix;
begin
 // Recalculate internal values after parameter change
 fWet1 := fWet * (fWidth * 0.5 + 0.5);
 fWet2 := fWet * ((1 - fWidth) * 0.5);
end;

procedure TfReeverbVST.Update;
var i: integer;
begin
 // Recalculate internal values after parameter change
 if fMode >= freezeMode then
  begin
   fRoomSizeI := 1;
   fDampA := 0;
   fGain := Muted;
  end
 else
  begin
   fRoomSizeI := fRoomSize;
   fDampA := fDamp;
   fGain := fixedGain;
  end;
 for i:=0 to Length(fComb)-1 do
  begin
   fComb[i,0].Feedback:=fRoomSizeI;
   fComb[i,1].Feedback:=fRoomSizeI;
   fComb[i,0].Damp:=fDampA;
   fComb[i,1].Damp:=fDampA;
  end;
end;

procedure TfReeverbVST.ShuffleAllPassFeedBack;
var i : Integer;
begin
 for i:=0 to Length(fAllpass)-1 do
  begin
   fAllpass[i,0].Feedback:=0.5+0.4*Random;
   fAllpass[i,1].Feedback:=0.5+0.4*Random;
  end;
end;

procedure TfReeverbVST.VST2ModuleProcess(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var OutL, OutR, Inp: Single;
    i, j: integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
   OutL:=Inputs[0,i];
   OutR:=Inputs[1,i];
   Inp := (Inputs[0,i] + Inputs[1,i]) * fGain;
   // Accumulate comb filters in parallel
   for j := 0 to Length(fComb) - 1 do
    begin
     OutL := OutL + fComb[j,0].Process(inp);
     OutR := OutR + fComb[j,1].Process(inp);
    end;
   // Feed through allpasses in series
   for j := 0 to Length(fAllpass) - 1 do
    begin
     outL := fAllpass[j,0].Process(OutL);
     outR := fAllpass[j,1].Process(OutR);
    end;
   // Calculate output MIXING with anything already there
   Outputs[0,i] :=Outputs[0,i] + OutL * fWet1 + OutR * fWet2 + Inputs[0,i] * fDry;
   Outputs[1,i] :=Outputs[1,i] + OutR * fWet1 + OutL * fWet2 + Inputs[1,i] * fDry;
  end;
end;

procedure TfReeverbVST.VST2ModuleProcessReplacing(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var OutL, OutR, inp: Single;
    i, j: integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
   OutL := 0;
   OutR := 0;
   inp := (Inputs[0,i] + Inputs[1,i]) * fGain;
   // Accumulate comb filters in parallel
   for j := 0 to Length(fComb) - 1 do
    begin
     OutL := OutL + fComb[j,0].Process(inp);
     OutR := OutR + fComb[j,1].Process(inp);
    end;
   // Feed through allpasses in series
   for j := 0 to Length(fAllpass) - 1 do
    begin
     outL := fAllpass[j,0].Process(OutL);
     outR := fAllpass[j,1].Process(OutR);
    end;
   // Calculate output REPLACING anything already there
   Outputs[0,i] := OutL * fWet1 + OutR * fWet2 + Inputs[0,i] * fDry;
   Outputs[1,i] := OutR * fWet1 + OutL * fWet2 + Inputs[1,i] * fDry;
  end;
end;

procedure TfReeverbVST.fReeverbVSTParameterProperties0ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Dry:=Value*0.01;
end;

procedure TfReeverbVST.fReeverbVSTParameterProperties1ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Wet:=Value*0.01;
end;

procedure TfReeverbVST.fReeverbVSTParameterProperties2ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Width:=Value*0.01;
end;

procedure TfReeverbVST.fReeverbVSTParameterProperties3ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 RoomSize:=Value;
 ShuffleAllPassFeedBack;
end;

procedure TfReeverbVST.VST2ModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 BufferRezize;
end;

procedure TfReeverbVST.BufferRezize;
var s : Single;
const
  stereoSpread = 23;
  // These values assume 44.1KHz sample rate
  // they will probably be OK for 48KHz sample rate
  // but would need scaling for 96KHz (or other) sample rates.
  // The values were obtained by listening tests.
  CombTuningL1 = 1116;
  CombTuningL2 = 1188;
  CombTuningL3 = 1277;
  CombTuningL4 = 1356;
  CombTuningL5 = 1422;
  CombTuningL6 = 1491;
  CombTuningL7 = 1557;
  CombTuningL8 = 1617;
  AllpassTuningL1 = 556;
  AllpassTuningL2 = 441;
  AllpassTuningL3 = 341;
  AllpassTuningL4 = 225;
begin
 s:=SampleRate/44100*fStretch;

 fComb[0,0].BufferSize:=round(CombTuningL1*s);
 fComb[0,1].BufferSize:=round((CombTuningL1+stereoSpread)*s);
 fComb[1,0].BufferSize:=round(CombTuningL2*s);
 fComb[1,1].BufferSize:=round((CombTuningL2+stereoSpread)*s);
 fComb[2,0].BufferSize:=round(CombTuningL3*s);
 fComb[2,1].BufferSize:=round((CombTuningL3+stereoSpread)*s);
 fComb[3,0].BufferSize:=round(CombTuningL4*s);
 fComb[3,1].BufferSize:=round((CombTuningL4+stereoSpread)*s);
 fComb[4,0].BufferSize:=round(CombTuningL5*s);
 fComb[4,1].BufferSize:=round((CombTuningL5+stereoSpread)*s);
 fComb[5,0].BufferSize:=round(CombTuningL6*s);
 fComb[5,1].BufferSize:=round((CombTuningL6+stereoSpread)*s);
 fComb[6,0].BufferSize:=round(CombTuningL7*s);
 fComb[6,1].BufferSize:=round((CombTuningL7+stereoSpread)*s);
 fComb[7,0].BufferSize:=round(CombTuningL8*s);
 fComb[7,1].BufferSize:=round((CombTuningL8+stereoSpread)*s);
 fAllpass[0,0].BufferSize:=round(AllpassTuningL1*s);
 fAllpass[0,1].BufferSize:=round((AllpassTuningL1+stereoSpread)*s);
 fAllpass[1,0].BufferSize:=round(AllpassTuningL2*s);
 fAllpass[1,1].BufferSize:=round((AllpassTuningL2+stereoSpread)*s);
 fAllpass[2,0].BufferSize:=round(AllpassTuningL3*s);
 fAllpass[2,1].BufferSize:=round((AllpassTuningL3+stereoSpread)*s);
 fAllpass[3,0].BufferSize:=round(AllpassTuningL4*s);
 fAllpass[3,1].BufferSize:=round((AllpassTuningL4+stereoSpread)*s);
end;

procedure TfReeverbVST.fReeverbVSTParameterProperties4ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Mode:=Value;
end;

procedure TfReeverbVST.fReeverbVSTParameterProperties5ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fStretch:=Value;
 BufferRezize;
end;

procedure TfReeverbVST.fReeverbVSTParameterProperties6ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Damp:=Value/100;
end;

procedure TfReeverbVST.fReeverbVSTParameterProperties7ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
//var oldLength,i : Integer;
begin
(*
 oldLength:=Length(fAllpass);
 if (Value<1) or (oldLength=Round(Value)) then exit;

 if (oldLength<Round(Value)) then
  begin
   SetLength(fAllpass,Round(Value));
   for i:=oldLength to Length(fAllpass)-1 do
    begin
     fAllpass[i,0]:=TAllpass.Create(1000);
     fAllpass[i,1]:=TAllpass.Create(1000+23);
    end;
  end
 else
  begin
   SetLength(fAllpass,Round(Value));
  end;
*)
end;

procedure TfReeverbVST.fReeverbVSTParameterProperties8ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
//var oldLength,i : Integer;
begin
(*
 oldLength:=Length(fComb);
 if (Value<1) or (oldLength=Round(Value)) then exit;

 if (oldLength<Round(Value)) then
  begin
   SetLength(fComb,Round(Value));
   for i:=oldLength to Length(fComb)-1 do
    begin
     fComb[i,0]:=TComb.Create(1000);
     fComb[i,1]:=TComb.Create(1000+23);
    end;
  end
 else
  begin
   for i:=oldLength to Round(Value)-1 do
    begin
     FreeAndNil(fComb[i,0]);
     FreeAndNil(fComb[i,1]);
    end;
   SetLength(fComb,Round(Value));
  end;
*)
end;

procedure TfReeverbVST.VST2ModuleCreate(Sender: TObject);
var i : Integer;
const
  stereoSpread = 23;
  // These values assume 44.1KHz sample rate
  // they will probably be OK for 48KHz sample rate
  // but would need scaling for 96KHz (or other) sample rates.
  // The values were obtained by listening tests.
  CombTuningL1 = 1116;
  CombTuningL2 = 1188;
  CombTuningL3 = 1277;
  CombTuningL4 = 1356;
  CombTuningL5 = 1422;
  CombTuningL6 = 1491;
  CombTuningL7 = 1557;
  CombTuningL8 = 1617;
  AllpassTuningL1 = 556;
  AllpassTuningL2 = 441;
  AllpassTuningL3 = 341;
  AllpassTuningL4 = 225;
begin
 fStretch:=1;
 SetLength(fComb,8);
 SetLength(fAllpass,4);

 fComb[0,0] := TComb.Create(CombTuningL1);
 fComb[0,1] := TComb.Create(CombTuningL1 + stereoSpread);
 fComb[1,0] := TComb.Create(CombTuningL2);
 fComb[1,1] := TComb.Create(CombTuningL2 + stereoSpread);
 fComb[2,0] := TComb.Create(CombTuningL3);
 fComb[2,1] := TComb.Create(CombTuningL3 + stereoSpread);
 fComb[3,0] := TComb.Create(CombTuningL4);
 fComb[3,1] := TComb.Create(CombTuningL4 + stereoSpread);
 fComb[4,0] := TComb.Create(CombTuningL5);
 fComb[4,1] := TComb.Create(CombTuningL5 + stereoSpread);
 fComb[5,0] := TComb.Create(CombTuningL6);
 fComb[5,1] := TComb.Create(CombTuningL6 + stereoSpread);
 fComb[6,0] := TComb.Create(CombTuningL7);
 fComb[6,1] := TComb.Create(CombTuningL7 + stereoSpread);
 fComb[7,0] := TComb.Create(CombTuningL8);
 fComb[7,1] := TComb.Create(CombTuningL8 + stereoSpread);
 fAllpass[0,0] := TAllpass.Create(AllpassTuningL1);
 fAllpass[0,1] := TAllpass.Create(AllpassTuningL1 + stereoSpread);
 fAllpass[1,0] := TAllpass.Create(AllpassTuningL2);
 fAllpass[1,1] := TAllpass.Create(AllpassTuningL2 + stereoSpread);
 fAllpass[2,0] := TAllpass.Create(AllpassTuningL3);
 fAllpass[2,1] := TAllpass.Create(AllpassTuningL3 + stereoSpread);
 fAllpass[3,0] := TAllpass.Create(AllpassTuningL4);
 fAllpass[3,1] := TAllpass.Create(AllpassTuningL4 + stereoSpread);
  // Set default values
 for i:=0 to Length(fAllpass)-1 do
  begin
   fAllpass[i,0].Feedback:=0.5;
   fAllpass[i,1].Feedback:=0.5;
  end;
 Wet:=1;
 RoomSize:=initialRoom;
 Dry:=1;
 Damp:=initialDamp;
 Width:=initialWidth;
 Mode:=initialMode;
 Mute;
end;

procedure TfReeverbVST.VST2ModuleDestroy(Sender: TObject);
var i: integer;
begin
 for i:=0 to 3 do
  begin
   if Assigned(fAllpass[i,0]) then FreeAndNil(fAllpass[i,0]);
   if Assigned(fAllpass[i,1]) then FreeAndNil(fAllpass[i,1]);
  end;
 for i := 0 to 7 do
  begin
   if Assigned(fComb[i,0]) then FreeAndNil(fComb[i,0]);
   if Assigned(fComb[i,1]) then FreeAndNil(fComb[i,1]);
  end;
end;

end.
