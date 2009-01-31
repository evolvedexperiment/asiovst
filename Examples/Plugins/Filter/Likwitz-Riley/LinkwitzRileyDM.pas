unit LinkwitzRileyDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFilterLinkwitzRiley;

type
  TLinkwitzRileyModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FLinkwitzRiley : TLinkwitzRiley;
  public
  end;

implementation

{$R *.DFM}

procedure TLinkwitzRileyModule.VSTModuleOpen(Sender: TObject);
begin
 FLinkwitzRiley := TLinkwitzRiley.Create;
 FLinkwitzRiley.SampleRate := SampleRate;
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
  Sample: Integer;
begin
 for Sample := 0 to SampleFrames - 1
  do FLinkwitzRiley.ProcessSample(Inputs[0, Sample], Outputs[0, Sample], Outputs[1, Sample])
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
begin
 if assigned(FLinkwitzRiley)
  then FLinkwitzRiley.Frequency := Value;
end;

procedure TLinkwitzRileyModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLinkwitzRiley)
  then FLinkwitzRiley.Order := round(Value);
end;

procedure TLinkwitzRileyModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FLinkwitzRiley.SampleRate := SampleRate;
end;

end.
