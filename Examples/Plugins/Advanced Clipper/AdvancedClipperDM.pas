unit AdvancedClipperDM;

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Common,
  DAV_VSTModule, DAV_DSPUpDownsampling;

type
  TAdvancedClipperDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParaOSFactor1Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOSFactor2Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFilterOrder1Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFilterOrder2Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamInputGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOutputGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamBW2Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamBW1Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRoundDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamHardClipDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamHardClipChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FUpDownSampling  : array [0..3] of TDAVUpDownsampling;
    FInputGain       : Single;
    FOutputGain      : Single;
    FHardClip        : Boolean;
    FCriticalSection : TCriticalSection;
  public
  end;

implementation

{$R *.DFM}

uses
  AdvancedClipperGUI, DAV_VSTModuleWithPrograms;

procedure TAdvancedClipperDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TAdvancedClipperDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TAdvancedClipperDataModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 FHardClip   := False;
 FInputGain  := 1;
 FOutputGain := 1;
 for ch := 0 to 3
  do FUpDownSampling[ch] := TDAVUpDownsampling.Create;

 Parameter[0] := -0.1;
 Parameter[1] := 4;
 Parameter[2] := 6;
 Parameter[3] := 99.5;
 Parameter[4] := 1;
 Parameter[5] := 0;
 Parameter[6] := 99.8;
 Parameter[7] := 0;

 with ProgramByName['Default'] do
  begin
   Parameter[0] := -0.1;
   Parameter[1] := 4;
   Parameter[2] := 6;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.8;
   Parameter[7] := 0;
  end;
 with ProgramByName['Bypass'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 1;
   Parameter[2] := 0;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.5;
   Parameter[7] := 0;
  end;
 with ProgramByName['Light'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 2;
   Parameter[2] := 4;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.5;
   Parameter[7] := 0;
  end;
 with ProgramByName['Normal'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 4;
   Parameter[2] := 8;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.5;
   Parameter[7] := 0;
  end;
 with ProgramByName['More'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 8;
   Parameter[2] := 16;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.5;
   Parameter[7] := 0;
  end;
 with ProgramByName['Even More'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 8;
   Parameter[2] := 16;
   Parameter[3] := 99.5;
   Parameter[4] := 8;
   Parameter[5] := 16;
   Parameter[6] := 99.8;
   Parameter[7] := 0;
  end;
 with ProgramByName['True Bypass'] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 1;
   Parameter[2] := 0;
   Parameter[3] := 99.5;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.5;
   Parameter[7] := 0;
  end;
 with ProgramByName['Clip Art!'] do
  begin
   Parameter[0] := 6;
   Parameter[1] := 8;
   Parameter[2] := 16;
   Parameter[3] := 99.9;
   Parameter[4] := 1;
   Parameter[5] := 0;
   Parameter[6] := 99.7;
   Parameter[7] := 0;
  end;
 with ProgramByName['Rippler'] do
  begin
   Parameter[0] := 6;
   Parameter[1] := 8;
   Parameter[2] := 0;
   Parameter[3] := 99.9;
   Parameter[4] := 8;
   Parameter[5] := 16;
   Parameter[6] := 99.7;
   Parameter[7] := -2;
  end;
end;

procedure TAdvancedClipperDataModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to 1
  do FreeAndNil(FUpDownSampling[ch]);
end;

procedure TAdvancedClipperDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmAdvancedClipper.Create(Self);
end;

procedure TAdvancedClipperDataModule.ParaOSFactor1Change(Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 FCriticalSection.Enter;
 try
  for ch := 0 to 1 do
   if assigned(FUpDownSampling[ch])
    then FUpDownSampling[ch].Factor := round(Value);
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmAdvancedClipper then
  with TFmAdvancedClipper(EditorForm) do
   begin
    UpdateOSFactor1;
   end;
end;

procedure TAdvancedClipperDataModule.ParamOSFactor2Change(Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 FCriticalSection.Enter;
 try
  for ch := 2 to 3 do
   if assigned(FUpDownSampling[ch])
    then FUpDownSampling[ch].Factor := round(Value);
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateOSFactor2;
end;

procedure TAdvancedClipperDataModule.ParamInputGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FInputGain := dB_to_Amp(Value);
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateInputGain;
end;

procedure TAdvancedClipperDataModule.ParamRoundDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(Parameter[Index]));
end;

procedure TAdvancedClipperDataModule.ParamHardClipDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Boolean(round(Parameter[Index]))
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TAdvancedClipperDataModule.ParamHardClipChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FHardClip := Boolean(round(Value));
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateHardClip;
end;

procedure TAdvancedClipperDataModule.ParamBW1Change(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 FCriticalSection.Enter;
 try
  for ch := 0 to 1 do
   if assigned(FUpDownSampling[ch])
    then FUpDownSampling[ch].TransitionBandwidth := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAdvancedClipperDataModule.ParamBW2Change(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 FCriticalSection.Enter;
 try
  for ch := 2 to 3 do
   if assigned(FUpDownSampling[ch])
    then FUpDownSampling[ch].TransitionBandwidth := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAdvancedClipperDataModule.ParamOutputGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FOutputGain := dB_to_Amp(Value);
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateOutputGain;
end;

procedure TAdvancedClipperDataModule.ParamFilterOrder1Change(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 FCriticalSection.Enter;
 try
  for ch := 0 to 1 do
   if assigned(FUpDownSampling[ch])
    then FUpDownSampling[ch].Order := round(Value);
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateOrder1;
end;

procedure TAdvancedClipperDataModule.ParamFilterOrder2Change(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 FCriticalSection.Enter;
 try
  for ch := 2 to 3 do
   if assigned(FUpDownSampling[ch])
    then FUpDownSampling[ch].Order := round(Value);
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmAdvancedClipper
  then TFmAdvancedClipper(EditorForm).UpdateOrder2;
end;

procedure TAdvancedClipperDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch, i, j   : Integer;
  InterStage : Double;
  d          : array [0..15] of Double;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   for ch := 0 to 1 do
    begin
     FUpDownSampling[ch].Upsample64(FInputGain * Inputs[ch, i], @d);
     for j := 0 to FUpDownSampling[ch].Factor - 1
      do d[j] := (abs(d[j] + 1) - abs(d[j] - 1)) * 0.5;
     InterStage := FUpDownSampling[ch].Downsample64(@d);

     FUpDownSampling[ch + 2].Upsample64(InterStage, @d);
     for j := 0 to FUpDownSampling[ch + 2].Factor - 1
      do d[j] := (abs(d[j] + 1) - abs(d[j] - 1)) * 0.5;
     d[0] := FUpDownSampling[ch + 2].Downsample64(@d);
     if FHardClip
      then d[0] := (abs(d[0] + 1) - abs(d[0] - 1)) * 0.5;
     Outputs[ch, i] := FOutputGain * d[0];
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAdvancedClipperDataModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleDynArray;
  const SampleFrames: Integer);
var
  ch, i, j   : Integer;
  InterStage : Double;
  d          : array [0..15] of Double;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   for ch := 0 to 1 do
    begin
     FUpDownSampling[ch].Upsample64(FInputGain * Inputs[ch, i], @d);
     for j := 0 to FUpDownSampling[ch].Factor - 1
      do d[j] := (abs(d[j] + 1) - abs(d[j] - 1)) * 0.5;
     InterStage := FUpDownSampling[ch].Downsample64(@d);

     FUpDownSampling[ch + 2].Upsample64(InterStage, @d);
     for j := 0 to FUpDownSampling[ch + 2].Factor - 1
      do d[j] := (abs(d[j] + 1) - abs(d[j] - 1)) * 0.5;
     d[0] := FUpDownSampling[ch + 2].Downsample64(@d);
     if FHardClip
      then d[0] := (abs(d[0] + 1) - abs(d[0] - 1)) * 0.5;
     Outputs[ch, i] := FOutputGain * d[0];
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAdvancedClipperDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to 3 do
  if assigned(FUpDownSampling[ch])
   then FUpDownSampling[ch].SampleRate := SampleRate;
end;

end.