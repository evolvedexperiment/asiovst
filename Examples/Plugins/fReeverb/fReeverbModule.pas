unit fReeverbModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  fReeverbFilter;

const
  CStereoSpread = 23;
  // These values assume 44.1KHz sample rate
  // they will probably be OK for 48KHz sample rate
  // but would need scaling for 96KHz (or other) sample rates.
  // The values were obtained by listening tests.
  CCombTuningL1 = 1116;
  CCombTuningL2 = 1188;
  CCombTuningL3 = 1277;
  CCombTuningL4 = 1356;
  CCombTuningL5 = 1422;
  CCombTuningL6 = 1491;
  CCombTuningL7 = 1557;
  CCombTuningL8 = 1617;
  CAllpassTuningL1 = 556;
  CAllpassTuningL2 = 441;
  CAllpassTuningL3 = 341;
  CAllpassTuningL4 = 225;

type
  TCombArray    = array [0..1] of TComb;
  TAllpassArray = array [0..1] of TAllpass;

  TfReeverbVST = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const sampleframes: Integer);
    procedure VSTModuleProcessReplacing(const inputs, outputs: TDAVArrayOfSingleDynArray; const sampleframes: Integer);
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
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
  private
    FGain         : Single;
    FRoomSize     : Single;
    FRoomSizeI    : Single;
    FDamp         : Single;
    FDampA        : Single;
    FWet          : Single;
    FWet1         : Single;
    FWet2         : Single;
    FDry          : Single;
    FWidth        : Single;
    FMode         : Single;
    FStretch      : Single;

    FComb         : array of TCombArray; // Comb filters
    FAllpass      : array of TAllpassArray; // Allpass filters
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
    property Width: Single read FWidth write SetWidth;
    property Dry: Single read FDry write FDry;
    property Wet: Single read FWet write SetWet;
    property Damp: Single read GetDamp write SetDamp;
    property RoomSize: Single read GetRoomSize write SetRoomSize;
  end;

implementation

{$R *.DFM}

uses
  Math, fReeverbGUI;

procedure TfReeverbVST.VSTModuleOpen(Sender: TObject);
var
  i : Integer;
begin
 FStretch := 1;
 SetLength(FComb, 8);
 SetLength(FAllpass, 4);

 FComb[0, 0] := TComb.Create(CCombTuningL1);
 FComb[0, 1] := TComb.Create(CCombTuningL1 + CStereoSpread);
 FComb[1, 0] := TComb.Create(CCombTuningL2);
 FComb[1, 1] := TComb.Create(CCombTuningL2 + CStereoSpread);
 FComb[2, 0] := TComb.Create(CCombTuningL3);
 FComb[2, 1] := TComb.Create(CCombTuningL3 + CStereoSpread);
 FComb[3, 0] := TComb.Create(CCombTuningL4);
 FComb[3, 1] := TComb.Create(CCombTuningL4 + CStereoSpread);
 FComb[4, 0] := TComb.Create(CCombTuningL5);
 FComb[4, 1] := TComb.Create(CCombTuningL5 + CStereoSpread);
 FComb[5, 0] := TComb.Create(CCombTuningL6);
 FComb[5, 1] := TComb.Create(CCombTuningL6 + CStereoSpread);
 FComb[6, 0] := TComb.Create(CCombTuningL7);
 FComb[6, 1] := TComb.Create(CCombTuningL7 + CStereoSpread);
 FComb[7, 0] := TComb.Create(CCombTuningL8);
 FComb[7, 1] := TComb.Create(CCombTuningL8 + CStereoSpread);
 FAllpass[0, 0] := TAllpass.Create(CAllpassTuningL1);
 FAllpass[0, 1] := TAllpass.Create(CAllpassTuningL1 + CStereoSpread);
 FAllpass[1, 0] := TAllpass.Create(CAllpassTuningL2);
 FAllpass[1, 1] := TAllpass.Create(CAllpassTuningL2 + CStereoSpread);
 FAllpass[2, 0] := TAllpass.Create(CAllpassTuningL3);
 FAllpass[2, 1] := TAllpass.Create(CAllpassTuningL3 + CStereoSpread);
 FAllpass[3, 0] := TAllpass.Create(CAllpassTuningL4);
 FAllpass[3, 1] := TAllpass.Create(CAllpassTuningL4 + CStereoSpread);

 // Set default values
 for i := 0 to Length(FAllpass)-1 do
  begin
   FAllpass[i, 0].Feedback := 0.5;
   FAllpass[i, 1].Feedback := 0.5;
  end;
 Wet := 1;
 RoomSize := cInitialRoom;
 Dry := 1;
 Damp := cInitialDamp;
 Width := cInitialWidth;
 Mode := cInitialMode;
 Mute;

 // default parameter
 Parameter[0] := 0.5;
 Parameter[1] := 0.5;
 Parameter[2] := 0.5;
 Parameter[3] := 0.5;
 Parameter[4] := 0;
 Parameter[5] := 0;
 Parameter[6] := 0.5;

 // default preset
 with programs[1] do
  begin
   Parameter[0] := 0.5;
   Parameter[1] := 0.5;
   Parameter[2] := 0.5;
   Parameter[3] := 0.5;
   Parameter[4] := 0;
   Parameter[5] := 0;
   Parameter[6] := 0.5;
  end;

 // preset 1
 with programs[1] do
  begin
   Parameter[0] := 0.5;
   Parameter[1] := 0.6;
   Parameter[2] := 0.4;
   Parameter[3] := 0.5;
   Parameter[4] := 0;
   Parameter[5] := 0;
   Parameter[6] := 1;
  end;

 // preset 2
 with programs[2] do
  begin
   Parameter[0] := 0.2;
   Parameter[1] := 0.6;
   Parameter[2] := 0.8;
   Parameter[3] := 1;
   Parameter[4] := 0;
   Parameter[5] := 1;
   Parameter[6] := 1;
  end;

 // preset 3
 with programs[3] do
  begin
   Parameter[0] := random;
   Parameter[1] := random;
   Parameter[2] := random;
   Parameter[3] := random;
   Parameter[4] := 0;
   Parameter[5] := random;
   Parameter[6] := random;
  end;
end;

procedure TfReeverbVST.VSTModuleClose(Sender: TObject);
var
  i: Integer;
begin
 for i := 0 to 3 do
  begin
   if Assigned(FAllpass[i, 0]) then FreeAndNil(FAllpass[i, 0]);
   if Assigned(FAllpass[i, 1]) then FreeAndNil(FAllpass[i, 1]);
  end;
 for i := 0 to 7 do
  begin
   if Assigned(FComb[i, 0]) then FreeAndNil(FComb[i, 0]);
   if Assigned(FComb[i, 1]) then FreeAndNil(FComb[i, 1]);
  end;
end;

procedure TfReeverbVST.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmReverb.Create(Self);
end;

function TfReeverbVST.GetDamp: Single;
begin
 Result := FDamp / cScaleDamp;
end;

function TfReeverbVST.GetMode: Single;
begin
 if FMode >= cFreezeMode
  then Result := 1
  else Result := 0;
end;

function TfReeverbVST.GetRoomSize: Single;
begin
 Result := (FRoomSize - cOffsetRoom) / cScaleRoom;
end;

procedure TfReeverbVST.Mute;
var
  i: Integer;
begin
 if FMode >= cFreezeMode then Exit;
 for i := 0 to Length(FComb) - 1 do
  begin
   FComb[i, 0].Mute;
   FComb[i, 1].Mute;
  end;
 for i := 0 to Length(FAllpass) - 1 do
  begin
   FAllpass[i, 0].Mute;
   FAllpass[i, 1].Mute;
  end;
end;

procedure TfReeverbVST.SetDamp(Value: Single);
begin
 FDamp := Value * cScaleDamp;
 Update;
end;

procedure TfReeverbVST.SetMode(Value: Single);
begin
 FMode := Value;
 Update;
end;

procedure TfReeverbVST.SetRoomSize(Value: Single);
begin
 FRoomSize := (Value * cScaleroom) + cOffsetRoom;
 Update;
end;

procedure TfReeverbVST.SetWet(Value: Single);
begin
 FWet := Value;
 UpdateMix;
end;

procedure TfReeverbVST.SetWidth(Value: Single);
begin
 FWidth := Value;
 UpdateMix;
end;

procedure TfReeverbVST.UpdateMix;
begin
 // Recalculate internal values after parameter change
 FWet1 := FWet * (FWidth * 0.5 + 0.5);
 FWet2 := FWet * ((1 - FWidth) * 0.5);
end;

procedure TfReeverbVST.Update;
var
  i: integer;
begin
 // Recalculate internal values after parameter change
 if FMode >= cFreezeMode then
  begin
   FRoomSizeI := 1;
   FDampA := 0;
   FGain := cMuted;
  end
 else
  begin
   FRoomSizeI := FRoomSize;
   FDampA := FDamp;
   FGain := cFixedGain;
  end;
 for i := 0 to Length(FComb) - 1 do
  begin
   FComb[i, 0].Feedback := FRoomSizeI;
   FComb[i, 1].Feedback := FRoomSizeI;
   FComb[i, 0].Damp := FDampA;
   FComb[i, 1].Damp := FDampA;
  end;
end;

procedure TfReeverbVST.ShuffleAllPassFeedBack;
var
  i : Integer;
begin
 for i := 0 to Length(FAllpass) - 1 do
  begin
   FAllpass[i, 0].Feedback := 0.5 + 0.4 * Random;
   FAllpass[i, 1].Feedback := 0.5 + 0.4 * Random;
  end;
end;

procedure TfReeverbVST.VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const sampleframes: Integer);
var
  OutL, OutR, Inp: Single;
  i, j: integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   OutL := Inputs[0, i];
   OutR := Inputs[1, i];
   Inp := (Inputs[0, i] + Inputs[1, i]) * FGain;
   // Accumulate comb filters in parallel
   for j := 0 to Length(FComb) - 1 do
    begin
     OutL := OutL + FComb[j, 0].Process(inp);
     OutR := OutR + FComb[j, 1].Process(inp);
    end;
   // Feed through allpasses in series
   for j := 0 to Length(FAllpass) - 1 do
    begin
     outL := FAllpass[j, 0].Process(OutL);
     outR := FAllpass[j, 1].Process(OutR);
    end;
   // Calculate output MIXING with anything already there
   Outputs[0,i]  := Outputs[0, i] + OutL * FWet1 + OutR * FWet2 + Inputs[0, i] * FDry;
   Outputs[1,i]  := Outputs[1, i] + OutR * FWet1 + OutL * FWet2 + Inputs[1, i] * FDry;
  end;
end;

procedure TfReeverbVST.VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  OutL, OutR, inp: Single;
  i, j: integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   OutL := 0;
   OutR := 0;
   inp := (Inputs[0, i] + Inputs[1, i]) * FGain;
   // Accumulate comb filters in parallel
   for j := 0 to Length(FComb) - 1 do
    begin
     OutL := OutL + FComb[j, 0].Process(inp);
     OutR := OutR + FComb[j, 1].Process(inp);
    end;
   // Feed through allpasses in series
   for j := 0 to Length(FAllpass) - 1 do
    begin
     outL := FAllpass[j, 0].Process(OutL);
     outR := FAllpass[j, 1].Process(OutR);
    end;
   // Calculate output REPLACING anything already there
   Outputs[0,i] := OutL * FWet1 + OutR * FWet2 + Inputs[0, i] * FDry;
   Outputs[1,i] := OutR * FWet1 + OutL * FWet2 + Inputs[1, i] * FDry;
  end;
end;

procedure TfReeverbVST.ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Dry := Value;
end;

procedure TfReeverbVST.ParameterWetChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Wet := Value;
end;

procedure TfReeverbVST.ParameterWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Width := Value;
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
var
  s : Single;
begin
 s := SampleRate / 44100 * FStretch;

 FComb[0, 0].BufferSize := round(CCombTuningL1 * s);
 FComb[0, 1].BufferSize := round((CCombTuningL1 + CStereoSpread) * s);
 FComb[1, 0].BufferSize := round(CCombTuningL2 * s);
 FComb[1, 1].BufferSize := round((CCombTuningL2 + CStereoSpread) * s);
 FComb[2, 0].BufferSize := round(CCombTuningL3 * s);
 FComb[2, 1].BufferSize := round((CCombTuningL3 + CStereoSpread) * s);
 FComb[3, 0].BufferSize := round(CCombTuningL4 * s);
 FComb[3, 1].BufferSize := round((CCombTuningL4 + CStereoSpread) * s);
 FComb[4, 0].BufferSize := round(CCombTuningL5 * s);
 FComb[4, 1].BufferSize := round((CCombTuningL5 + CStereoSpread) * s);
 FComb[5, 0].BufferSize := round(CCombTuningL6 * s);
 FComb[5, 1].BufferSize := round((CCombTuningL6 + CStereoSpread) * s);
 FComb[6, 0].BufferSize := round(CCombTuningL7 * s);
 FComb[6, 1].BufferSize := round((CCombTuningL7 + CStereoSpread) * s);
 FComb[7, 0].BufferSize := round(CCombTuningL8 * s);
 FComb[7, 1].BufferSize := round((CCombTuningL8 + CStereoSpread) * s);
 FAllpass[0, 0].BufferSize := round(CAllpassTuningL1 * s);
 FAllpass[0, 1].BufferSize := round((CAllpassTuningL1 + CStereoSpread) * s);
 FAllpass[1, 0].BufferSize := round(CAllpassTuningL2 * s);
 FAllpass[1, 1].BufferSize := round((CAllpassTuningL2 + CStereoSpread) * s);
 FAllpass[2, 0].BufferSize := round(CAllpassTuningL3 * s);
 FAllpass[2, 1].BufferSize := round((CAllpassTuningL3 + CStereoSpread) * s);
 FAllpass[3, 0].BufferSize := round(CAllpassTuningL4 * s);
 FAllpass[3, 1].BufferSize := round((CAllpassTuningL4 + CStereoSpread) * s);
end;

procedure TfReeverbVST.ParameterFreezeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Mode := Value;
end;

procedure TfReeverbVST.ParameterStretchChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FStretch := 1 + 9 * Value;
 BufferRezize;
end;

procedure TfReeverbVST.ParameterDampChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Damp := Value;
end;

procedure TfReeverbVST.ParameterNumAllpassesChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  oldLength, i : Integer;
begin
 oldLength := Length(FAllpass);
 if (Value < 1) or (oldLength = Round(Value))
  then exit;

 if (oldLength < Round(Value)) then
  begin
   SetLength(FAllpass, Round(Value));
   for i := oldLength to Length(FAllpass) - 1 do
    begin
     FAllpass[i, 0] := TAllpass.Create(1000);
     FAllpass[i, 1] := TAllpass.Create(1023);
    end;
  end
 else SetLength(FAllpass, Round(Value));
end;

procedure TfReeverbVST.ParameterNumCombsChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  oldLength,i : Integer;
begin
 oldLength := Length(FComb);
 if (Value < 1) or (oldLength = Round(Value))
  then exit;

 if (oldLength < Round(Value)) then
  begin
   SetLength(FComb, Round(Value));
   for i := oldLength to Length(FComb) - 1 do
    begin
     FComb[i, 0] := TComb.Create(1000);
     FComb[i, 1] := TComb.Create(1023);
    end;
  end
 else
  begin
   for i := oldLength to Round(Value) - 1 do
    begin
     FreeAndNil(FComb[i, 0]);
     FreeAndNil(FComb[i, 1]);
    end;
   SetLength(FComb, Round(Value));
  end;
end;

end.
