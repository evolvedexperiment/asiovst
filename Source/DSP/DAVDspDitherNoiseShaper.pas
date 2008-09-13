unit DAVDspDitherNoiseShaper;

interface

{$I ASIOVST.INC}

uses
  DAV_Common;

type
  TDitherType = (dtor9Fc, dtor3Fc, dtor2MEc, dtor3MEc, dtor9MEc, dtor5IEc,
                 dtor9IEc, dtor2Sc);
  TDitherNoiseShaper = class
  private
    fOrder        : Integer;
    fHistoryPos   : Integer;
    fCoefficients : TAVDDoubleDynArray; // Coeffs
    fHistory      : TAVDDoubleDynArray; // Error History
    fDitherType   : TDitherType;
    fBitDepth     : Byte;
    fBitMul       : Double;
    fBitDiv       : Double;
    procedure SetDitherType(v: TDitherType);
    procedure SetBitDepth(Value: Byte);
  public
    constructor Create;
    destructor Destroy; override;
    function ProcessInteger(Input: Double): Integer;
    function ProcessFloat(Input: Double): Double;
    procedure Reset;
  published
    property DitherType: TDitherType read fDitherType write SetDitherType default dtor9Fc;
    property BitDepth: Byte read fBitDepth write SetBitDepth;
  end;

implementation

uses Math;

constructor TDitherNoiseShaper.Create;
begin
 inherited;
 SetLength(fHistory, 2 * fOrder);
 FillChar(fHistory, 2 * fOrder * SizeOf(Double), 0);
 SetBitDepth(16);
 SetDitherType(dtor9Fc);
 fHistoryPos := 8;
 Randomize;
end;

destructor TDitherNoiseShaper.Destroy;
begin
 SetLength(fCoefficients, 0);
 SetLength(fHistory, 0);
 inherited;
end;

procedure TDitherNoiseShaper.SetBitDepth(Value: Byte);
begin
 if Value < 1  then Value := 1 else
 if Value > 32 then Value := 32;
 if fBitDepth <> Value then
  begin
   fBitDepth := Value;
   fBitMul := Power(2, fBitDepth - 1) - 1;
   fBitDiv := 1 / fBitMul;
  end;
end;

procedure TDitherNoiseShaper.SetDitherType(v: TDitherType);
const
  // F-weighted
  or9Fc : array [0..8] of Double = ( 2.412, -3.370,  3.937, -4.174, 3.353,
                                    -2.205,  1.281, -0.569,  0.0847);
  or3Fc : array [0..2] of Double = ( 1.623, -0.982, 0.109);

  // modified-E weighted
  or2MEc : array [0..1] of Double = ( 1.537, -0.8367);
  or3MEc : array [0..2] of Double = ( 1.652, -1.049,    0.1382);
  or9MEc : array [0..8] of Double = ( 1.662, -1.263,    0.4827,  -0.2913, 0.1268,
                                     -0.1124, 0.03252, -0.01265, -0.03524);

  // improved-E weighted
  or5IEc : array [0..4] of Double = ( 2.033, -2.165,  1.959, -1.590, 0.6149);
  or9IEc : array [0..8] of Double = ( 2.847, -4.685,  6.214, -7.184, 6.639,
                                     -5.032,  3.263, -1.632,  0.4191);
  // Simple 2nd order
  or2Sc : array [0..1] of Double = (1.0, -0.5);

begin
 fDitherType := v;
 case v of
  dtor9Fc  : begin
              fOrder := 9;
              SetLength(fCoefficients, fOrder);
              Move(or9Fc[0], fCoefficients[0], fOrder * SizeOf(Double));
             end;
  dtor3Fc  : begin
              fOrder := 3;
              SetLength(fCoefficients, fOrder);
              Move(or3Fc[0], fCoefficients[0], fOrder * SizeOf(Double));
             end;
  dtor2MEc : begin
              fOrder := 2;
              SetLength(fCoefficients, fOrder);
              Move(or2MEc[0], fCoefficients[0], fOrder * SizeOf(Double));
             end;
  dtor3MEc : begin
              fOrder := 3;
              SetLength(fCoefficients, fOrder);
              Move(or3MEc[0], fCoefficients[0], fOrder * SizeOf(Double));
             end;
  dtor9MEc : begin
              fOrder := 9;
              SetLength(fCoefficients, fOrder);
              Move(or9MEc[0], fCoefficients[0], fOrder * SizeOf(Double));
             end;
  dtor5IEc : begin
              fOrder := 5;
              SetLength(fCoefficients, fOrder);
              Move(or5IEc[0], fCoefficients[0], fOrder * SizeOf(Double));
             end;
  dtor9IEc : begin
              fOrder := 9;
              SetLength(fCoefficients, fOrder);
              Move(or9IEc[0], fCoefficients[0], fOrder * SizeOf(Double));
             end;
  dtor2Sc  : begin
              fOrder := 2;
              SetLength(fCoefficients, fOrder);
              Move(or2Sc[0], fCoefficients[0], fOrder * SizeOf(Double));
             end;
 end;
 SetLength(fHistory, 2 * fOrder);
 Reset;
end;

procedure TDitherNoiseShaper.Reset;
begin
 FillChar(fHistory[0], 2 * fOrder * SizeOf(Double), 0);
 fHistoryPos := 8;
end;

function TDitherNoiseShaper.ProcessInteger(Input: Double):Integer;
begin
 // scale input to bit range
 Input := fBitMul * Input;

 // Unrolled loop for faster execution
 Input := Input - fCoefficients[8] * fHistory[fHistoryPos + 8] +
                  fCoefficients[7] * fHistory[fHistoryPos + 7] +
                  fCoefficients[6] * fHistory[fHistoryPos + 6] +
                  fCoefficients[5] * fHistory[fHistoryPos + 5] +
                  fCoefficients[4] * fHistory[fHistoryPos + 4] +
                  fCoefficients[3] * fHistory[fHistoryPos + 3] +
                  fCoefficients[2] * fHistory[fHistoryPos + 2] +
                  fCoefficients[1] * fHistory[fHistoryPos + 1] +
                  fCoefficients[0] * fHistory[fHistoryPos];

 // add triangular distributed noise
 result := round(Input + (random - random));
 fHistoryPos := ((fHistoryPos + 8) mod fOrder);

 // Update buffer (both copies)
 fHistory[fHistoryPos]     := Result - Input;
 fHistory[fHistoryPos + 9] := fHistory[fHistoryPos];
end;

function TDitherNoiseShaper.ProcessFloat(Input: Double): Double;
begin
 result := ProcessInteger(Input) * fBitDiv;
end;

end.
