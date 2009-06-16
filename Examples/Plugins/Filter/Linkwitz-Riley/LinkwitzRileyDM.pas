unit LinkwitzRileyDM;

{$I DAV_Compiler.inc}

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFilterLinkwitzRiley;

type
  TLinkwitzRileyModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FLinkwitzRiley : array of TLinkwitzRiley;
  public
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TLinkwitzRileyModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
begin
 assert(numOutputs = 2 * numInputs);
 SetLength(FLinkwitzRiley, numInputs);
 for Channel := 0 to Length(FLinkwitzRiley) - 1 do
  begin
   FLinkwitzRiley[Channel] := TLinkwitzRiley.Create;
   FLinkwitzRiley[Channel].SampleRate := SampleRate;
  end;

 {$IFDEF FPC}
 OnProcess := VSTModuleProcess;
 OnProcessReplacing := VSTModuleProcess;
 {$ENDIF}

 Parameter[0] := 1000;
 Parameter[1] := 2;
end;

procedure TLinkwitzRileyModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLinkwitzRiley);
end;

procedure TLinkwitzRileyModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Channel: Integer;
begin
 // CDenorm32 +
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to Length(FLinkwitzRiley) - 1
   do FLinkwitzRiley[Channel].ProcessSample(Inputs[Channel, Sample],
        Outputs[2 * Channel, Sample], Outputs[2 * Channel + 1, Sample])
end;

procedure TLinkwitzRileyModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 Predefined := IntToStr(2 * round(Parameter[Index]));
end;

procedure TLinkwitzRileyModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq : Single;
begin
 Freq := Parameter[Index];
 if Freq >= 1000
  then Predefined := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3);
end;

procedure TLinkwitzRileyModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TLinkwitzRileyModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FLinkwitzRiley[Channel])
   then FLinkwitzRiley[Channel].Frequency := Value;
end;

procedure TLinkwitzRileyModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FLinkwitzRiley[Channel])
   then FLinkwitzRiley[Channel].Order := round(Value);
end;

procedure TLinkwitzRileyModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FLinkwitzRiley[Channel])
   then FLinkwitzRiley[Channel].SampleRate := SampleRate;
end;

{$IFDEF FPC}
initialization
  {$i LinkwitzRileyDM.lrs}
{$ENDIF}

end.
