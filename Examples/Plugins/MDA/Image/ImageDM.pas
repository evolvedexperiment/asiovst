unit ImageDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TImageDataModule = class(TVSTModule)
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParamModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fLL, fLR,
    fRL, fRR  : Single;
  public
  end;

implementation

{$R *.DFM}

procedure TImageDataModule.ParamModeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[0]) of
  0: PreDefined := 'SM->LR';
  1: PreDefined := 'MS->LR';
  2: PreDefined := 'LR->LR';
  3: PreDefined := 'LR->MS';
 end;
end;

procedure TImageDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  w, k, c, b, g : Single;
begin

 //calcs here
 w := 4 * Parameter[1] - 2;   // width
 k := 2 * Parameter[2];       // balance
 c := 4 * Parameter[3] - 2;   // depth
 b := 2 * Parameter[4];       // pan
 g := dB_to_Amp(Parameter[5]);

 case round(Parameter[0]) of
  0: begin // SM->LR
      fRL :=  g * c * (2 - b);
      fLL :=  g * w * (2 - k);
      fRR :=  g * c * b;
      fLR := -g * w * k;
     end;

  1: begin // MS->LR
      fLL :=  g * c * (2 - b);
      fRL :=  g * w * (2 - k);
      fLR :=  g * c * b;
      fRR := -g * w * k;
     end;

  2: begin // LR->LR
      g   := g * 0.5;
      fLL := g * (c * (2 - b) + w * (2 - k));
      fRL := g * (c * (2 - b) - w * (2 - k));
      fLR := g * (c * b - w * k);
      fRR := g * (c * b + w * k);
     end;

  3: begin //LR->MS
      g   :=  g * 0.5;
      fLL :=  g * (2 - b) * (2 - k);
      fRL :=  g * (2 - b) * k;
      fLR := -g * b * (2 - k);
      fRR :=  g * b * k;
     end;
 end;
end;

procedure TImageDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := fRR * Inputs[0, Sample] + fLR * Inputs[1, Sample];
   Outputs[1, Sample] := fLL * Inputs[1, Sample] + fRL * Inputs[0, Sample];
  end;
end;

end.
