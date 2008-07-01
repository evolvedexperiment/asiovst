unit RingModDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TRingModDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer;
      var Value: Single);
  private
    fPhi      : Single;
    fdPhi     : Single;
    fFeedBack : Single;
    fPrev     : Single;
  public
  end;

implementation

{$R *.DFM}

procedure TRingModDataModule.VSTModuleCreate(Sender: TObject);
begin
 fPhi      := 0.0;
 fdPhi     := (2 * Pi * 1000) / SampleRate;
 fFeedBack := 0;
 fPrev     := 0;

 Parameter[0] := 0.0625; //1kHz
 Parameter[1] := 0.0;
 Parameter[2] := 0.0;
end;

procedure TRingModDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 fdPhi := (2 * Pi * 100.0 * (Parameter[1] + (160.0 * Parameter[0])) / SampleRate);
 fFeedBack := 0.95 * 0.01 * Parameter[2];
end;

procedure TRingModDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  g, fb  : Single;
  p, dp  : Single;
  fp     : Array [0..1] of Single;
const
  cTwoPi  : Double = 2 * Pi;
  cTwoPiX : Double = 1 / (2 * Pi);
begin
 p     := fPhi;
 dp    := fdPhi;
 fb    := fFeedBack;
 fp[0] := fPrev;
 fp[1] := fPrev;

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

 fPhi  := p;
 fPrev := fp[0];
end;

end.