unit DualButterworthFiltersDM;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPFilterButterworth, DAV_DSPFilterLinkwitzRiley;

type
  TDualButterworthFiltersModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessLowpass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessHighpass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessBandpass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterLowpassOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLowpassFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterHighpassFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighpassOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FLowpass   : array of array [0..1] of TButterworthLowPassFilter;
    FHighpass  : array of array [0..1] of TButterworthHighPassFilter;
    FSign      : Single;
  public
    procedure LoadLow(Index: Integer);
    procedure LoadHigh(Index: Integer);
    procedure StoreLow(Index: Integer);
    procedure StoreHigh(Index: Integer);
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Registry, DualButterworthFiltersGui;

const
  CRegistryKey = 'SOFTWARE\Delphi ASIO & VST Project\';

procedure TDualButterworthFiltersModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
begin
 Assert(numOutputs = numInputs);
 SetLength(FLowpass, numInputs);
 for Channel := 0 to Length(FLowpass) - 1 do
  begin
   FLowpass[Channel][0] := TButterworthLowPassFilter.Create;
   FLowpass[Channel][0].SampleRate := SampleRate;
   FLowpass[Channel][1] := TButterworthLowPassFilter.Create;
   FLowpass[Channel][1].SampleRate := SampleRate;
  end;

 SetLength(FHighpass, numInputs);
 for Channel := 0 to Length(FHighpass) - 1 do
  begin
   FHighpass[Channel][0] := TButterworthHighPassFilter.Create;
   FHighpass[Channel][0].SampleRate := SampleRate;
   FHighpass[Channel][1] := TButterworthHighPassFilter.Create;
   FHighpass[Channel][1].SampleRate := SampleRate;
  end;

 {$IFDEF FPC}
 OnProcess := VSTModuleProcess;
 OnProcessReplacing := VSTModuleProcess;
 {$ENDIF}

 FSign := 1;
 Parameter[0] := 10000;
 Parameter[1] := 2;
 Parameter[2] := 100;
 Parameter[3] := 2;
 Parameter[4] := 0;
end;

procedure TDualButterworthFiltersModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLowpass) - 1 do
  begin
   FreeAndNil(FLowpass[0]);
   FreeAndNil(FLowpass[1]);
  end;

 for Channel := 0 to Length(FHighpass) - 1 do
  begin
   FreeAndNil(FHighpass[0]);
   FreeAndNil(FHighpass[1]);
  end;
end;

procedure TDualButterworthFiltersModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 Gui := TFmLinkwitzRiley.Create(Self);
end;

procedure TDualButterworthFiltersModule.LoadLow(Index: Integer);
begin
 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if KeyExists(CRegistryKey + IntToStr(Index)) then
    if OpenKey(CRegistryKey + IntToStr(Index), False) then
     begin
      if ValueExists('Frequency') then Parameter[0] := ReadFloat('Frequency');
      if ValueExists('Order') then Parameter[1] := ReadFloat('Order');
     end;
  finally
   Free;
  end;
end;

procedure TDualButterworthFiltersModule.LoadHigh(Index: Integer);
begin
 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if KeyExists(CRegistryKey + IntToStr(Index)) then
    if OpenKey(CRegistryKey + IntToStr(Index), False) then
     begin
      if ValueExists('Frequency') then Parameter[2] := ReadFloat('Frequency');
      if ValueExists('Order') then Parameter[3] := ReadFloat('Order');
     end;
  finally
   Free;
  end;
end;

procedure TDualButterworthFiltersModule.StoreLow(Index: Integer);
begin
 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if OpenKey(CRegistryKey + IntToStr(Index), True) then
    begin
     WriteFloat('Frequency', Parameter[0]);
     WriteFloat('Order', Parameter[1]);
    end;
  finally
   Free;
  end;
end;

procedure TDualButterworthFiltersModule.StoreHigh(Index: Integer);
begin
 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if OpenKey(CRegistryKey + IntToStr(Index), True) then
    begin
     WriteFloat('Frequency', Parameter[2]);
     WriteFloat('Order', Parameter[3]);
    end;
  finally
   Free;
  end;
end;

procedure TDualButterworthFiltersModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 Predefined := IntToStr(12 * round(Parameter[Index]));
end;

procedure TDualButterworthFiltersModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq : Single;
begin
 Freq := Parameter[Index];
 if Freq >= 1000
  then Predefined := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3);
end;

procedure TDualButterworthFiltersModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TDualButterworthFiltersModule.ParameterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case round(Value) of
  0 : OnProcess := VSTModuleProcessBypass;
  1 : OnProcess := VSTModuleProcessLowpass;
  2 : OnProcess := VSTModuleProcessHighpass;
  3 : OnProcess := VSTModuleProcessBandpass;
 end;
 OnProcessReplacing := OnProcess;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateType;
end;

procedure TDualButterworthFiltersModule.ParameterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0: PreDefined := 'Bypass';
  1: PreDefined := 'Highcut';
  2: PreDefined := 'Lowcut';
  3: PreDefined := 'Bandpass';
 end;
end;

procedure TDualButterworthFiltersModule.ParameterLowpassFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to numInputs - 1 do
  begin
   if assigned(FLowpass[Channel][0])
    then FLowpass[Channel][0].Frequency := Value;
   if assigned(FLowpass[Channel][1])
    then FLowpass[Channel][1].Frequency := Value;
  end;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateLowpassFrequency;
end;

procedure TDualButterworthFiltersModule.ParameterLowpassOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  begin
   if assigned(FLowpass[Channel][0])
    then FLowpass[Channel][0].Order := round(Value);
   if assigned(FLowpass[Channel][1])
    then FLowpass[Channel][1].Order := round(Value);
  end;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateLowpassSlope;
end;

procedure TDualButterworthFiltersModule.ParameterHighpassFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  begin
   if assigned(FHighpass[Channel][0])
    then FHighpass[Channel][0].Frequency := Value;
   if assigned(FHighpass[Channel][1])
    then FHighpass[Channel][1].Frequency := Value;
  end;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateHighpassFrequency;
end;

procedure TDualButterworthFiltersModule.ParameterHighpassOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  begin
   if assigned(FHighpass[Channel][0])
    then FHighpass[Channel][0].Order := round(Value);
   if assigned(FHighpass[Channel][1])
    then FHighpass[Channel][1].Order := round(Value);
  end;

 FSign := 1 - 2 * (round(Value) mod 2);

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateHighpassSlope;
end;

procedure TDualButterworthFiltersModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  begin
   if assigned(FLowpass[Channel][0])
    then FLowpass[Channel][0].SampleRate := SampleRate;
   if assigned(FLowpass[Channel][1])
    then FLowpass[Channel][1].SampleRate := SampleRate;
   if assigned(FHighpass[Channel][0])
    then FHighpass[Channel][0].SampleRate := SampleRate;
   if assigned(FHighpass[Channel][1])
    then FHighpass[Channel][1].SampleRate := SampleRate;
  end;
end;

procedure TDualButterworthFiltersModule.VSTModuleProcessBypass(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLowpass) - 1
  do Move(Inputs[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Single));
end;

procedure TDualButterworthFiltersModule.VSTModuleProcessLowpass(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Channel : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to Length(FLowpass) - 1
   do Outputs[Channel, Sample] := FLowpass[Channel][0].ProcessSample64(
        FLowpass[Channel][1].ProcessSample64(Inputs[Channel, Sample]));
end;

procedure TDualButterworthFiltersModule.VSTModuleProcessHighpass(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Channel : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to Length(FLowpass) - 1
   do Outputs[Channel, Sample] := FSign * FHighpass[Channel][0].ProcessSample64(
        FHighpass[Channel][1].ProcessSample64(Inputs[Channel, Sample]));
end;

procedure TDualButterworthFiltersModule.VSTModuleProcessBandpass(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Channel : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to Length(FLowpass) - 1 do
   begin
    Outputs[Channel, Sample] := FSign * FLowpass[Channel][0].ProcessSample64(
      FLowpass[Channel][1].ProcessSample64(
      FHighpass[Channel][0].ProcessSample64(
      FHighpass[Channel][1].ProcessSample64(Inputs[Channel, Sample]))));
   end;
end;

{$IFDEF FPC}
initialization
  {$i DualButterworthFiltersDM.lrs}
{$ENDIF}

end.
