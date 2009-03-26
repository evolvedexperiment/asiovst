unit DAV_DspDitherNoiseShaper;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon;

type
  TDitherType = (dtor9Fc, dtor3Fc, dtor2MEc, dtor3MEc, dtor9MEc, dtor5IEc,
                 dtor9IEc, dtor2Sc);
  TDitherNoiseShaper = class(TDspObject)
  private
    FOrder        : Integer;
    FHistoryPos   : Integer;
    FCoefficients : TDAVDoubleDynArray; // Coeffs
    FHistory      : TDAVDoubleDynArray; // Error History
    FDitherType   : TDitherType;
    FBitDepth     : Byte;
    FBitMul       : Double;
    FBitDiv       : Double;
    procedure SetDitherType(v: TDitherType);
    procedure SetBitDepth(Value: Byte);
  protected  
    procedure DitherTypeChanged;
    procedure BitDepthChanged;
  public
    constructor Create;
    destructor Destroy; override;
    function ProcessInteger(Input: Double): Integer;
    function ProcessFloat(Input: Double): Double;
    procedure Reset;
  published
    property DitherType: TDitherType read FDitherType write SetDitherType default dtor9Fc;
    property BitDepth: Byte read FBitDepth write SetBitDepth;
  end;

implementation

uses Math;

constructor TDitherNoiseShaper.Create;
begin
 inherited;
 SetLength(FHistory, 2 * FOrder);
 FillChar(FHistory, 2 * FOrder * SizeOf(Double), 0);
 SetBitDepth(16);
 SetDitherType(dtor9Fc);
 FHistoryPos := 8;
 Randomize;
end;

destructor TDitherNoiseShaper.Destroy;
begin
 SetLength(FCoefficients, 0);
 SetLength(FHistory, 0);
 inherited;
end;

procedure TDitherNoiseShaper.SetBitDepth(Value: Byte);
begin
 if Value < 1  then Value := 1 else
 if Value > 32 then Value := 32;
 if FBitDepth <> Value then
  begin
   FBitDepth := Value;
   BitDepthChanged;
  end;
end;

procedure TDitherNoiseShaper.BitDepthChanged;
begin
 FBitMul := Power(2, FBitDepth - 1) - 1;
 FBitDiv := 1 / FBitMul;
end;

procedure TDitherNoiseShaper.DitherTypeChanged;
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
 case FDitherType of
  dtor9Fc  : begin
              FOrder := 9;
              SetLength(FCoefficients, FOrder);
              Move(or9Fc[0], FCoefficients[0], FOrder * SizeOf(Double));
             end;
  dtor3Fc  : begin
              FOrder := 3;
              SetLength(FCoefficients, FOrder);
              Move(or3Fc[0], FCoefficients[0], FOrder * SizeOf(Double));
             end;
  dtor2MEc : begin
              FOrder := 2;
              SetLength(FCoefficients, FOrder);
              Move(or2MEc[0], FCoefficients[0], FOrder * SizeOf(Double));
             end;
  dtor3MEc : begin
              FOrder := 3;
              SetLength(FCoefficients, FOrder);
              Move(or3MEc[0], FCoefficients[0], FOrder * SizeOf(Double));
             end;
  dtor9MEc : begin
              FOrder := 9;
              SetLength(FCoefficients, FOrder);
              Move(or9MEc[0], FCoefficients[0], FOrder * SizeOf(Double));
             end;
  dtor5IEc : begin
              FOrder := 5;
              SetLength(FCoefficients, FOrder);
              Move(or5IEc[0], FCoefficients[0], FOrder * SizeOf(Double));
             end;
  dtor9IEc : begin
              FOrder := 9;
              SetLength(FCoefficients, FOrder);
              Move(or9IEc[0], FCoefficients[0], FOrder * SizeOf(Double));
             end;
  dtor2Sc  : begin
              FOrder := 2;
              SetLength(FCoefficients, FOrder);
              Move(or2Sc[0], FCoefficients[0], FOrder * SizeOf(Double));
             end;
 end;
 SetLength(FHistory, 2 * FOrder);
 Reset;
end;

procedure TDitherNoiseShaper.SetDitherType(v: TDitherType);
begin
 if FDitherType <> v then
  begin
   FDitherType := v;
   DitherTypeChanged;
  end;
end;

procedure TDitherNoiseShaper.Reset;
begin
 FillChar(FHistory[0], 2 * FOrder * SizeOf(Double), 0);
 FHistoryPos := 8;
end;

function TDitherNoiseShaper.ProcessInteger(Input: Double): Integer;
begin
 // scale input to bit range
 Input := FBitMul * Input;

 // Unrolled loop for faster execution
 Input := Input - FCoefficients[8] * FHistory[FHistoryPos + 8] +
                  FCoefficients[7] * FHistory[FHistoryPos + 7] +
                  FCoefficients[6] * FHistory[FHistoryPos + 6] +
                  FCoefficients[5] * FHistory[FHistoryPos + 5] +
                  FCoefficients[4] * FHistory[FHistoryPos + 4] +
                  FCoefficients[3] * FHistory[FHistoryPos + 3] +
                  FCoefficients[2] * FHistory[FHistoryPos + 2] +
                  FCoefficients[1] * FHistory[FHistoryPos + 1] +
                  FCoefficients[0] * FHistory[FHistoryPos];

 // add triangular distributed noise
 result := round(Input + (random - random));
 FHistoryPos := ((FHistoryPos + 8) mod FOrder);

 // Update buffer (both copies)
 FHistory[FHistoryPos]     := Result - Input;
 FHistory[FHistoryPos + 9] := FHistory[FHistoryPos];
end;

function TDitherNoiseShaper.ProcessFloat(Input: Double): Double;
begin
 result := ProcessInteger(Input) * FBitDiv;
end;

end.
