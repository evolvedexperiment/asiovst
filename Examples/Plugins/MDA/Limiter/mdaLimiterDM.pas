unit mdaLimiterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TmdaLimiterDataModule = class(TVSTModule)
    procedure AttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure AttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure KneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure KneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure OutputTrimChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleCreate(Sender: TObject);
  private
    fThreshold_dB : Single;
    fThreshold    : Single;
    fTrim         : Single;
    fGain         : Single;
    fAttack       : Single;
    fRelease      : Single;
    procedure CalculateThreshold;
  public
  end;

implementation

uses
  Math, DAV_VSTModuleWithPrograms;

{$R *.DFM}

procedure TmdaLimiterDataModule.ThresholdChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if fThreshold_dB <> Value then
  begin
   fThreshold_dB := Value;
   CalculateThreshold;
  end;
end;

procedure TmdaLimiterDataModule.OutputTrimChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fTrim := Power(10.0, (2.0 * Value) - 1.0);
end;

procedure TmdaLimiterDataModule.CalculateThreshold;
begin
 if Parameter[4] > 0.5
  then fThreshold := Power(10, 1 - (2 * fThreshold_dB))  //soft knee
  else fThreshold := Power(10, (2 * fThreshold_dB) - 2); //hard knee
end;

procedure TmdaLimiterDataModule.AttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fAttack := Power(10, -2 * Value);
end;

procedure TmdaLimiterDataModule.KneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'HARD'
  else PreDefined := 'SOFT';
end;

procedure TmdaLimiterDataModule.AttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(-301030.1 / (SampleRate * log10(1 - fAttack)), ffGeneral, 4, 4);
end;

procedure TmdaLimiterDataModule.ReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(-301.0301 / (SampleRate * log10(1 - fRelease)), ffGeneral, 4, 4);
end;

procedure TmdaLimiterDataModule.ReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fRelease := Power(10, -2 - (3 * Value));
end;

procedure TmdaLimiterDataModule.KneeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateThreshold;
end;

procedure TmdaLimiterDataModule.VSTModuleCreate(Sender: TObject);
begin
 fAttack  := 0.5;
 fRelease := 0.5;
end;

procedure TmdaLimiterDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  smp      : Integer;
  g, at,
  re, tr,
  th, lev,
  l, r     : Single;
begin
 th := fThreshold;
 g  := fGain;
 at := fAttack;
 re := fRelease;
 tr := fTrim;
 if Parameter[4] < 0.5 then //soft knee
  for smp := 0 to SampleFrames - 1 do
   begin
    l := inputs[0, smp];
    r := inputs[1, smp];
    lev := 1.0 / (1.0 + th * abs(l + r));
    if (g > lev)
     then g := g - at * (g - lev)
     else g := g + re * (lev - g);

    outputs[0, smp] := (l * tr * g);
    outputs[1, smp] := (r * tr * g);
   end
 else
  for smp := 0 to SampleFrames - 1 do
   begin
    l := inputs[0, smp];
    r := inputs[1, smp];

    lev := 0.5 * g * abs(l + r);

    if (lev > th)
     then g := g - (at * (lev - th))
     else g := g + (re * (1.0 - g)); //below threshold

    outputs[0, smp] := (l * tr * g);
    outputs[1, smp] := (r * tr * g);
   end;
 fGain := g;
end;

procedure TmdaLimiterDataModule.VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  smp      : Integer;
  g, at,
  re, tr,
  th, lev,
  l, r     : Double;
begin
 th := fThreshold;
 g  := fGain;
 at := fAttack;
 re := fRelease;
 tr := fTrim;
 if Parameter[4] < 0.5 then //soft knee
  for smp := 0 to SampleFrames - 1 do
   begin
    l := inputs[0, smp];
    r := inputs[1, smp];
    lev := 1 / (1 + th * abs(l + r));
    if (g > lev)
     then g := g - at * (g - lev)
     else g := g + re * (lev - g);

    outputs[0, smp] := (l * tr * g);
    outputs[1, smp] := (r * tr * g);
   end
 else
  for smp := 0 to SampleFrames - 1 do
   begin
    l := inputs[0, smp];
    r := inputs[1, smp];

    lev := 0.5 * g * abs(l + r);

    if (lev > th)
     then g := g - (at * (lev - th))
     else g := g + (re * (1.0 - g)); //below threshold

    outputs[0, smp] := (l * tr * g);
    outputs[1, smp] := (r * tr * g);
   end;
 fGain := g;
end;

end.