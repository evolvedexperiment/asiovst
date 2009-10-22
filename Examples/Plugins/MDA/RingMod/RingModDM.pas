unit RingModDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule;

type
  TRingModDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FPhi      : Single;
    FDeltaPhi : Single;
    FFeedBack : Single;
    FPrev     : Single;
  public
  end;

implementation

{$R *.DFM}

procedure TRingModDataModule.VSTModuleOpen(Sender: TObject);
begin
 FPhi      := 0.0;
 FDeltaPhi := (2 * Pi * 1000) / SampleRate;
 FFeedBack := 0;
 FPrev     := 0;

 // Initial Parameters
 Parameter[0] := 0.0625; //1kHz
 Parameter[1] := 0.0;
 Parameter[2] := 0.0;
end;

procedure TRingModDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FDeltaPhi := (2 * Pi * 100.0 * (Parameter[1] + (160.0 * Parameter[0])) / SampleRate);
end;

procedure TRingModDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  g, fb  : Single;
  p, dp  : Single;
  fp     : array [0..1] of Single;
const
  cTwoPi  : Double = 2 * Pi;
  cTwoPiX : Double = 1 / (2 * Pi);
begin
 p     := FPhi;
 dp    := FDeltaPhi;
 fb    := FFeedBack;
 fp[0] := FPrev;
 fp[1] := FPrev;

 for Sample := 0 to SampleFrames - 1 do
  begin
   g := sin(p);
   p := (p + dp) * cTwoPiX;
   p := (p - round(p - 0.5)) * cTwoPi; // fmod( p + dp, tp );

   fp[0] := (fb * fp[0] + Inputs[0, Sample]) * g;
   fp[1] := (fb * fp[0] + Inputs[1, Sample]) * g;

   Outputs[0, Sample] := fp[0];
   Outputs[1, Sample] := fp[1];
  end;

 FPhi  := p;
 FPrev := fp[0];
end;

procedure TRingModDataModule.ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFeedBack := 0.95 * 0.01 * Value;
end;

end.