unit fReeverbModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
  fReeverbFilter;

type
  TCombArray    = array [0..1] of TComb;
  TAllpassArray = array [0..1] of TAllpass;

  TfReeverbVST = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const inputs, outputs: TAVDArrayOfSingleDynArray; const sampleframes: Integer);
    procedure VSTModuleProcessReplacing(const inputs, outputs: TAVDArrayOfSingleDynArray; const sampleframes: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWetChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRoomSizeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFreezeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterStretchChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDampChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterNumCombsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterNumAllpassesChange(Sender: TObject; const Index: Integer; var Value: Single);
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

uses
  fReeverbGUI;

function TfReeverbVST.GetDamp: Single;
begin
 Result := fDamp / cScaleDamp;
end;

function TfReeverbVST.GetMode: Single;
begin
 if fMode >= cFreezeMode
  then Result := 1
  else Result := 0;
end;

function TfReeverbVST.GetRoomSize: Single;
begin
 Result := (fRoomSize - cOffsetRoom) / cScaleRoom;
end;

procedure TfReeverbVST.Mute;
var
  i: Integer;
begin
 if fMode >= cFreezeMode then Exit;
 for i := 0 to Length(fComb) - 1 do
  begin
   fComb[i,0].Mute;
   fComb[i,1].Mute;
  end;
 for i := 0 to Length(fAllpass) - 1 do
  begin
   fAllpass[i,0].Mute;
   fAllpass[i,1].Mute;
  end;
end;

procedure TfReeverbVST.SetDamp(Value: Single);
begin
 fDamp := Value * cScaleDamp;
 Update;
end;

procedure TfReeverbVST.SetMode(Value: Single);
begin
 fMode := Value;
 Update;
end;

procedure TfReeverbVST.SetRoomSize(Value: Single);
begin
 fRoomSize := (Value * cScaleroom) + cOffsetRoom;
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
 if fMode >= cFreezeMode then
  begin
   fRoomSizeI := 1;
   fDampA := 0;
   fGain := cMuted;
  end
 else
  begin
   fRoomSizeI := fRoomSize;
   fDampA := fDamp;
   fGain := cFixedGain;
  end;
 for i := 0 to Length(fComb)-1 do
  begin
   fComb[i,0].Feedback := fRoomSizeI;
   fComb[i,1].Feedback := fRoomSizeI;
   fComb[i,0].Damp := fDampA;
   fComb[i,1].Damp := fDampA;
  end;
end;

procedure TfReeverbVST.ShuffleAllPassFeedBack;
var
  i : Integer;
begin
 for i := 0 to Length(fAllpass) - 1 do
  begin
   fAllpass[i,0].Feedback := 0.5 + 0.4 * Random;
   fAllpass[i,1].Feedback := 0.5 + 0.4 * Random;
  end;
end;

procedure TfReeverbVST.VSTModuleProcess(const inputs, outputs: TAVDArrayOfSingleDynArray; const sampleframes: Integer);
var
  OutL, OutR, Inp: Single;
  i, j: integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   OutL := Inputs[0, i];
   OutR := Inputs[1, i];
   Inp := (Inputs[0, i] + Inputs[1, i]) * fGain;
   // Accumulate comb filters in parallel
   for j := 0 to Length(fComb) - 1 do
    begin
     OutL := OutL + fComb[j, 0].Process(inp);
     OutR := OutR + fComb[j, 1].Process(inp);
    end;
   // Feed through allpasses in series
   for j := 0 to Length(fAllpass) - 1 do
    begin
     outL := fAllpass[j, 0].Process(OutL);
     outR := fAllpass[j, 1].Process(OutR);
    end;
   // Calculate output MIXING with anything already there
   Outputs[0,i]  := Outputs[0, i] + OutL * fWet1 + OutR * fWet2 + Inputs[0, i] * fDry;
   Outputs[1,i]  := Outputs[1, i] + OutR * fWet1 + OutL * fWet2 + Inputs[1, i] * fDry;
  end;
end;

procedure TfReeverbVST.VSTModuleProcessReplacing(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  OutL, OutR, inp: Single;
  i, j: integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   OutL := 0;
   OutR := 0;
   inp := (Inputs[0, i] + Inputs[1, i]) * fGain;
   // Accumulate comb filters in parallel
   for j := 0 to Length(fComb) - 1 do
    begin
     OutL := OutL + fComb[j, 0].Process(inp);
     OutR := OutR + fComb[j, 1].Process(inp);
    end;
   // Feed through allpasses in series
   for j := 0 to Length(fAllpass) - 1 do
    begin
     outL := fAllpass[j,0].Process(OutL);
     outR := fAllpass[j,1].Process(OutR);
    end;
   // Calculate output REPLACING anything already there
   Outputs[0,i] := OutL * fWet1 + OutR * fWet2 + Inputs[0, i] * fDry;
   Outputs[1,i] := OutR * fWet1 + OutL * fWet2 + Inputs[1, i] * fDry;
  end;
end;

procedure TfReeverbVST.ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Dry := Value * 0.01;
end;

procedure TfReeverbVST.ParameterWetChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Wet := Value * 0.01;
end;

procedure TfReeverbVST.ParameterWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Width := Value * 0.01;
end;

procedure TfReeverbVST.ParameterRoomSizeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 RoomSize := Value;
 ShuffleAllPassFeedBack;
end;

procedure TfReeverbVST.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
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
 s := SampleRate / 44100 * fStretch;

 fComb[0, 0].BufferSize := round(CombTuningL1 * s);
 fComb[0, 1].BufferSize := round((CombTuningL1 + stereoSpread) * s);
 fComb[1, 0].BufferSize := round(CombTuningL2 * s);
 fComb[1, 1].BufferSize := round((CombTuningL2 + stereoSpread) * s);
 fComb[2, 0].BufferSize := round(CombTuningL3 * s);
 fComb[2, 1].BufferSize := round((CombTuningL3 + stereoSpread) * s);
 fComb[3, 0].BufferSize := round(CombTuningL4 * s);
 fComb[3, 1].BufferSize := round((CombTuningL4 + stereoSpread) * s);
 fComb[4, 0].BufferSize := round(CombTuningL5 * s);
 fComb[4, 1].BufferSize := round((CombTuningL5 + stereoSpread) * s);
 fComb[5, 0].BufferSize := round(CombTuningL6 * s);
 fComb[5, 1].BufferSize := round((CombTuningL6 + stereoSpread) * s);
 fComb[6, 0].BufferSize := round(CombTuningL7 * s);
 fComb[6, 1].BufferSize := round((CombTuningL7 + stereoSpread) * s);
 fComb[7, 0].BufferSize := round(CombTuningL8 * s);
 fComb[7, 1].BufferSize := round((CombTuningL8 + stereoSpread) * s);
 fAllpass[0, 0].BufferSize := round(AllpassTuningL1 * s);
 fAllpass[0, 1].BufferSize := round((AllpassTuningL1 + stereoSpread) * s);
 fAllpass[1, 0].BufferSize := round(AllpassTuningL2 * s);
 fAllpass[1, 1].BufferSize := round((AllpassTuningL2 + stereoSpread) * s);
 fAllpass[2, 0].BufferSize := round(AllpassTuningL3 * s);
 fAllpass[2, 1].BufferSize := round((AllpassTuningL3 + stereoSpread) * s);
 fAllpass[3, 0].BufferSize := round(AllpassTuningL4 * s);
 fAllpass[3, 1].BufferSize := round((AllpassTuningL4 + stereoSpread) * s);
end;

procedure TfReeverbVST.ParameterFreezeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Mode := Value;
end;

procedure TfReeverbVST.ParameterStretchChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fStretch := Value;
 BufferRezize;
end;

procedure TfReeverbVST.ParameterDampChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Damp := Value * 0.01;
end;

procedure TfReeverbVST.ParameterNumAllpassesChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  oldLength, i : Integer;
begin
 oldLength := Length(fAllpass);
 if (Value < 1) or (oldLength = Round(Value))
  then exit;

 if (oldLength < Round(Value)) then
  begin
   SetLength(fAllpass, Round(Value));
   for i := oldLength to Length(fAllpass) - 1 do
    begin
     fAllpass[i, 0] := TAllpass.Create(1000);
     fAllpass[i, 1] := TAllpass.Create(1023);
    end;
  end
 else SetLength(fAllpass, Round(Value));
end;

procedure TfReeverbVST.ParameterNumCombsChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  oldLength,i : Integer;
begin
 oldLength := Length(fComb);
 if (Value < 1) or (oldLength = Round(Value))
  then exit;

 if (oldLength<Round(Value)) then
  begin
   SetLength(fComb,Round(Value));
   for i := oldLength to Length(fComb) - 1 do
    begin
     fComb[i,0] := TComb.Create(1000);
     fComb[i,1] := TComb.Create(1023);
    end;
  end
 else
  begin
   for i := oldLength to Round(Value) - 1 do
    begin
     FreeAndNil(fComb[i, 0]);
     FreeAndNil(fComb[i, 1]);
    end;
   SetLength(fComb, Round(Value));
  end;
end;

procedure TfReeverbVST.VSTModuleCreate(Sender: TObject);
var
  i : Integer;
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
 fStretch := 1;
 SetLength(fComb, 8);
 SetLength(fAllpass, 4);

 fComb[0, 0] := TComb.Create(CombTuningL1);
 fComb[0, 1] := TComb.Create(CombTuningL1 + stereoSpread);
 fComb[1, 0] := TComb.Create(CombTuningL2);
 fComb[1, 1] := TComb.Create(CombTuningL2 + stereoSpread);
 fComb[2, 0] := TComb.Create(CombTuningL3);
 fComb[2, 1] := TComb.Create(CombTuningL3 + stereoSpread);
 fComb[3, 0] := TComb.Create(CombTuningL4);
 fComb[3, 1] := TComb.Create(CombTuningL4 + stereoSpread);
 fComb[4, 0] := TComb.Create(CombTuningL5);
 fComb[4, 1] := TComb.Create(CombTuningL5 + stereoSpread);
 fComb[5, 0] := TComb.Create(CombTuningL6);
 fComb[5, 1] := TComb.Create(CombTuningL6 + stereoSpread);
 fComb[6, 0] := TComb.Create(CombTuningL7);
 fComb[6, 1] := TComb.Create(CombTuningL7 + stereoSpread);
 fComb[7, 0] := TComb.Create(CombTuningL8);
 fComb[7, 1] := TComb.Create(CombTuningL8 + stereoSpread);
 fAllpass[0, 0] := TAllpass.Create(AllpassTuningL1);
 fAllpass[0, 1] := TAllpass.Create(AllpassTuningL1 + stereoSpread);
 fAllpass[1, 0] := TAllpass.Create(AllpassTuningL2);
 fAllpass[1, 1] := TAllpass.Create(AllpassTuningL2 + stereoSpread);
 fAllpass[2, 0] := TAllpass.Create(AllpassTuningL3);
 fAllpass[2, 1] := TAllpass.Create(AllpassTuningL3 + stereoSpread);
 fAllpass[3, 0] := TAllpass.Create(AllpassTuningL4);
 fAllpass[3, 1] := TAllpass.Create(AllpassTuningL4 + stereoSpread);
  // Set default values
 for i := 0 to Length(fAllpass)-1 do
  begin
   fAllpass[i,0].Feedback := 0.5;
   fAllpass[i,1].Feedback := 0.5;
  end;
 Wet := 1;
 RoomSize := cInitialRoom;
 Dry := 1;
 Damp := cInitialDamp;
 Width := cInitialWidth;
 Mode := cInitialMode;
 Mute;
end;

procedure TfReeverbVST.VSTModuleDestroy(Sender: TObject);
var
  i: Integer;
begin
 for i := 0 to 3 do
  begin
   if Assigned(fAllpass[i, 0]) then FreeAndNil(fAllpass[i, 0]);
   if Assigned(fAllpass[i, 1]) then FreeAndNil(fAllpass[i, 1]);
  end;
 for i := 0 to 7 do
  begin
   if Assigned(fComb[i, 0]) then FreeAndNil(fComb[i, 0]);
   if Assigned(fComb[i, 1]) then FreeAndNil(fComb[i, 1]);
  end;
end;

procedure TfReeverbVST.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmReverb.Create(Self);
end;

end.
