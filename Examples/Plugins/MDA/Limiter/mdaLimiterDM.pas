unit mdaLimiterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TmdaLimiterDataModule = class(TVSTModule)
    procedure mdaLimiterDataModuleParameterProperties4CustomParameterDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleProcess(const Inputs,
      Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs,
      Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ThresholdChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure OutputTrimChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
   fThreshold : Single;
   fGain      : Single;
   fAttack    : Single;
   fRelease   : Single;
   fTrim      : Single;
  public
  end;

implementation

uses DVSTModuleWithPrograms;

{$R *.DFM}

procedure TmdaLimiterDataModule.ThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if fThreshold <> Value then
  begin
   fThreshold := Value;
  end;
end;

procedure TmdaLimiterDataModule.OutputTrimChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if fTrim <> Value then
  begin
   fTrim := Value;
  end;
end;

procedure TmdaLimiterDataModule.mdaLimiterDataModuleParameterProperties4CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'HARD'
  else PreDefined := 'SOFT';
end;

procedure TmdaLimiterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
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

procedure TmdaLimiterDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  smp      : Integer;
  g, at,
  re, tr,
  th, lev,
  ol, or_  : Double;
begin
 th := fThreshold;
 g  := fGain;
 at := fAttack;
 re := fRelease;
 tr := fTrim;
 if Parameter[4] < 0.5 then //soft knee
  for smp := 0 to SampleFrames - 1 do
   begin
    ol  := inputs[0, smp];
    or_ := inputs[1, smp];
    lev := 1.0 / (1.0 + th * abs(ol + or_));
    if (g > lev)
     then g := g - at * (g - lev)
     else g := g + re * (lev - g);

    outputs[0, smp] := (ol  * tr * g);
    outputs[1, smp] := (or_ * tr * g);
   end
 else
  for smp := 0 to SampleFrames - 1 do
   begin
    ol  := inputs[0, smp];
    or_ := inputs[1, smp];

    lev := 0.5 * g * abs(ol + or_);

    if (lev > th)
     then g := g - (at * (lev - th))
     else g := g + (re * (1.0 - g)); //below threshold

    outputs[0, smp] := (ol  * tr * g);
    outputs[1, smp] := (or_ * tr * g);
   end;
 fGain := g;
end;

end.