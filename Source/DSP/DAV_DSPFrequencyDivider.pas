unit DAV_DSPFrequencyDivider;

interface

uses
  DAV_Common;

type
  TOcatveDivider = class
  private
    procedure SetShape(Value: Single);
  protected
    FPhaseSign    : Single;
    FPhaseFactor  : Single;
    FShape        : array [0..1] of Single;
  published
  public
    constructor Create; virtual;
    function ProcessSample(Input: Single): Single; overload;
    function ProcessSample(Input: Double): Double; overload;

    property Phase: Single read FPhaseFactor;
  published
    property Shape: Single read FShape[0] write SetShape;
  end;

implementation

{ TOcatveDivider }

constructor TOcatveDivider.Create;
begin
 fPhaseSign   := 1;
 fPhaseFactor := 1;
 FShape[0]    := 0.5;
 FShape[1]    := 0.5;
end;

function TOcatveDivider.ProcessSample(Input: Single): Single;
begin
 if Input * fPhaseSign < 0 then     // Octave Divider
  begin
   fPhaseSign := -fPhaseSign;
   if fPhaseSign < 0 then fPhaseFactor := -fPhaseFactor;
  end;
 result := fPhaseFactor * (FShape[0] + FShape[1] * Input);
end;

function TOcatveDivider.ProcessSample(Input: Double): Double;
begin
 if Input * fPhaseSign < 0 then
  begin
   fPhaseSign := -fPhaseSign;
   if fPhaseSign < 0 then fPhaseFactor := -fPhaseFactor;
  end;
 result := fPhaseFactor * (FShape[0] + FShape[1] * Input);
end;

procedure TOcatveDivider.SetShape(Value: Single);
begin
 Value := Limit(Value, 0, 1);
 if FShape[0] <> Value then
  begin
   FShape[0] := Value;
   FShape[1] := 1 - Value;
  end;
end;

end.
