unit DitherNoiseshaperDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Common,
  DAV_VSTModule, DAV_DspDitherNoiseshaper;

type
  TDitherNoiseshaperModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParameterBitDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBitDepthDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterDitherTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterNoiseshaperTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterNoiseshaperTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterDitherChangeAmplitude(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDitherTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FDitherNoiseshaper : array of TDitherNoiseShaper32;
    FCriticalSection   : TCriticalSection;
  end;

implementation

{$R *.DFM}

uses
  DitherNoiseshaperGUI;

procedure TDitherNoiseshaperModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TDitherNoiseshaperModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TDitherNoiseshaperModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 assert(numInputs = numOutputs);
 assert(numInputs > 0);
 SetLength(FDitherNoiseshaper, numInputs);
 for Channel := 0 to Length(FDitherNoiseshaper) - 1
  do FDitherNoiseshaper[Channel] := TDitherNoiseShaper32.Create;
 Parameter[0] := 16;
 Parameter[1] := 1;
 Parameter[2] := 2;
 Parameter[3] := 1;
 Parameter[4] := 0;
end;

procedure TDitherNoiseshaperModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1
  do FreeAndNil(FDitherNoiseshaper[Channel]);
end;

procedure TDitherNoiseshaperModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmDitherNoiseshaper.Create(Self);
end;

procedure TDitherNoiseshaperModule.ParameterDitherTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].DitherType := TDitherType(round(Value));

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateDitherType;
end;

procedure TDitherNoiseshaperModule.ParameterDitherChangeAmplitude(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].DitherAmplitude := Value;

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateDitherAmplitude;
end;

procedure TDitherNoiseshaperModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].Limit := Boolean(Round(Value));

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateLimit;
end;

procedure TDitherNoiseshaperModule.ParameterBitDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].BitDepth := Round(Value);

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateBitDepth;
end;

procedure TDitherNoiseshaperModule.ParameterNoiseshaperTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].NoiseshaperType := TNoiseshaperType(Round(Value));

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateNoiseShaper;
end;

procedure TDitherNoiseshaperModule.ParameterBitDepthDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure TDitherNoiseshaperModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
const
  COnOff :array [0..1] of string = ('Off', 'On');
begin
 PreDefined := COnOff[round(Parameter[Index])];
end;

procedure TDitherNoiseshaperModule.ParameterDitherTypeDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 case TDitherType(Round(Parameter[Index])) of
        dtNone : PreDefined := 'None';
       dtEqual : PreDefined := 'Rectangular';
  dtTriangular : PreDefined := 'Triangular';
       dtGauss : PreDefined := 'Gauss';
   dtFastGauss : PreDefined := 'Gauss (fast)';
 end;
end;

procedure TDitherNoiseshaperModule.ParameterNoiseshaperTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case TNoiseShaperType(Round(Parameter[Index])) of
          nsNone : PreDefined := 'None';
          nsEFB  : PreDefined := 'Simple Error Feedback (1st Order)';
          ns2Sc  : PreDefined := 'Simple Highpass (2nd Order)';
          ns2MEc : PreDefined := 'mod. E-weighting (2nd Order)';
          ns3MEc : PreDefined := 'mod. E-weighting (3rd Order)';
          ns9MEc : PreDefined := 'mod. E-weighting (9th Order)';
          ns5IEc : PreDefined := 'improved E-weighting (5th Order)';
          ns9IEc : PreDefined := 'improved E-weighting (9th Order)';
          ns3Fc  : PreDefined := 'F-weighting (3rd Order)';
          ns9Fc  : PreDefined := 'F-weighting (9th Order';
          nsSBM  : PreDefined := 'Sony "Super Bit Mapping"';
          nsSBMr : PreDefined := 'Reduced "Super Bit Mapping"';
  nsExperimental : PreDefined := 'Experimental';
 end;
end;

procedure TDitherNoiseshaperModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FDitherNoiseshaper[Channel].ProcessFloat(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TDitherNoiseshaperModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FDitherNoiseshaper[Channel].ProcessFloat(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
