unit LinkwitzRileySeparatedDM;

{$I DAV_Compiler.inc}

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFilterLinkwitzRiley;

type
  TLinkwitzRileySeparatedModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FLinkwitzRiley : array of TLinkwitzRiley;
    FSignalMix     : Single;
    function GetLinkwitzRiley(Index: Integer): TLinkwitzRiley;
  public
    property LinkwitzRiley[Index: Integer]: TLinkwitzRiley read GetLinkwitzRiley;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  LinkwitzRileySeparatedGui;

procedure TLinkwitzRileySeparatedModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
begin
 Assert(numOutputs = numInputs);
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
 Parameter[2] := 0;
end;

procedure TLinkwitzRileySeparatedModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLinkwitzRiley);
end;

procedure TLinkwitzRileySeparatedModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 Gui := TFmLinkwitzRiley.Create(Self);
end;

procedure TLinkwitzRileySeparatedModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Channel : Integer;
  Low, High       : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to Length(FLinkwitzRiley) - 1 do
   begin
    FLinkwitzRiley[Channel].ProcessSample(Inputs[Channel, Sample], Low, High);
    Outputs[Channel, Sample] := (1 - FSignalMix) * Low + FSignalMix * High;
   end;
end;

procedure TLinkwitzRileySeparatedModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 Predefined := IntToStr(2 * round(Parameter[Index]));
end;

procedure TLinkwitzRileySeparatedModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq : Single;
begin
 Freq := Parameter[Index];
 if Freq >= 1000
  then Predefined := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3);
end;

procedure TLinkwitzRileySeparatedModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TLinkwitzRileySeparatedModule.ParameterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FSignalMix := Value;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateType;
end;

procedure TLinkwitzRileySeparatedModule.ParameterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'Lowpass'
  else PreDefined := 'Highpass';
end;

function TLinkwitzRileySeparatedModule.GetLinkwitzRiley(Index: Integer): TLinkwitzRiley;
begin
 if (Index >= 0)  or (Index < Length(FLinkwitzRiley))
  then result := FLinkwitzRiley[Index]
  else raise Exception.CreateFmt('Index out of bounds %d', [Index]);
end;

procedure TLinkwitzRileySeparatedModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FLinkwitzRiley[Channel])
   then FLinkwitzRiley[Channel].Frequency := Value;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateFrequency;
end;

procedure TLinkwitzRileySeparatedModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FLinkwitzRiley[Channel])
   then FLinkwitzRiley[Channel].Order := round(Value);

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateOrder;
end;

procedure TLinkwitzRileySeparatedModule.VSTModuleSampleRateChange(Sender: TObject;
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
  {$i LinkwitzRileySeparatedDM.lrs}
{$ENDIF}

end.
