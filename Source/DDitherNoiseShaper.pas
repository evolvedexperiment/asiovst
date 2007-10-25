unit DDitherNoiseShaper;

interface

uses DAVDCommon;

type
  TDitherType = (dtor9Fc, dtor3Fc, dtor2MEc, dtor3MEc, dtor9MEc, dtor5IEc,
                 dtor9IEc, dtor2Sc);
  TDDitherNoiseShaper = class
  private
    fOrder        : Integer;
    fHistoryPos   : Integer;
    fCoefficients : TAVDSingleDynArray; // Coeffs
    fHistory      : TAVDSingleDynArray; // Error History
    fDitherType   : TDitherType;
    procedure SetDitherType(v: TDitherType);
  public
    constructor Create;
    destructor Destroy; override;
    function ProcessS(Input: Single): SmallInt;
    function ProcessI(Input: Single): Integer;
    procedure Reset;
  published
    property DitherType: TDitherType read fDitherType write SetDitherType default dtor9Fc;
  end;

implementation

uses Math;

constructor TNS9dither16.Create;
begin
 inherited;
 fOrder := 9;
 SetLength(fCoefficients, fOrder);
 SetLength(fHistory, 2 * fOrder);
 Move(or9Fc[0], fCoefficients[0], fOrder * sizeof(Single));
 FillChar(fHistory, 2 * fOrder * sizeof(Single), 0);
 fHistoryPos := 8;
 Randomize;
end;

destructor TNS9dither16.Destroy;
begin
 SetLength(fCoefficients, 0);
 SetLength(fHistory, 0);
 inherited;
end;

procedure TNS9dither16.SetDitherType(v: TDitherType);
const
  // F-weighted
  or9Fc : array [0..8] of Single = ( 2.412, -3.370,  3.937, -4.174, 3.353,
                                    -2.205,  1.281, -0.569,  0.0847);
  or3Fc : array [0..2] of Single = ( 1.623, -0.982, 0.109);

  // modified-E weighted
  or2MEc : array [0..1] of Single = ( 1.537, -0.8367);
  or3MEc : array [0..2] of Single = ( 1.652, -1.049,    0.1382);
  or9MEc : array [0..8] of Single = ( 1.662, -1.263,    0.4827,  -0.2913, 0.1268,
                                     -0.1124, 0.03252, -0.01265, -0.03524);

  // improved-E weighted
  or5IEc : array [0..4] of Single = ( 2.033, -2.165,  1.959, -1.590, 0.6149);
  or9IEc : array [0..8] of Single = ( 2.847, -4.685,  6.214, -7.184, 6.639,
                                     -5.032,  3.263, -1.632,  0.4191);
  // Simple 2nd order
  or2Sc : array [0..1] of Single = (1.0, -0.5);

begin
 fDitherType := v;
 case v of
  dtor9Fc  : begin
              fOrder := 9;
              SetLength(fCoefficients, fOrder);
              Move(or9Fc[0], fCoefficients[0], fOrder * sizeof(Single));
             end;
  dtor3Fc  : begin
              fOrder := 3;
              SetLength(fCoefficients, fOrder);
              Move(or3Fc[0], fCoefficients[0], fOrder * sizeof(Single));
             end;
  dtor2MEc : begin
              fOrder := 2;
              SetLength(fCoefficients, fOrder);
              Move(or2MEc[0], fCoefficients[0], fOrder * sizeof(Single));
             end;
  dtor3MEc : begin
              fOrder := 3;
              SetLength(fCoefficients, fOrder);
              Move(or3MEc[0], fCoefficients[0], fOrder * sizeof(Single));
             end;
  dtor9MEc : begin
              fOrder := 9;
              SetLength(fCoefficients, fOrder);
              Move(or9MEc[0], fCoefficients[0], fOrder * sizeof(Single));
             end;
  dtor5IEc : begin
              fOrder := 5;
              SetLength(fCoefficients, fOrder);
              Move(or5IEc[0], fCoefficients[0], fOrder * sizeof(Single));
             end;
  dtor9IEc : begin
              fOrder := 9;
              SetLength(fCoefficients, fOrder);
              Move(or9IEc[0], fCoefficients[0], fOrder * sizeof(Single));
             end;
  dtor2Sc  : begin
              fOrder := 2;
              SetLength(fCoefficients, fOrder);
              Move(or2Sc[0], fCoefficients[0], fOrder * sizeof(Single));
             end;
 end;
 SetLength(fHistory, 2 * fOrder);
 Reset;
end;

procedure TNS9dither16.Reset;
begin
 FillChar(fHistory, 2 * fOrder * SizeOf(Single), 0);
 fHistoryPos := 8;
end;

function TNS9dither16.processS(Samp: Single): SmallInt;
begin
 // Unrolled loop for faster execution
 samp := samp - fCoefficients[8] * fHistory[fHistoryPos+8] +
                fCoefficients[7] * fHistory[fHistoryPos+7] +
                fCoefficients[6] * fHistory[fHistoryPos+6] +
                fCoefficients[5] * fHistory[fHistoryPos+5] +
                fCoefficients[4] * fHistory[fHistoryPos+4] +
                fCoefficients[3] * fHistory[fHistoryPos+3] +
                fCoefficients[2] * fHistory[fHistoryPos+2] +
                fCoefficients[1] * fHistory[fHistoryPos+1] +
                fCoefficients[0] * fHistory[fHistoryPos];

 result := round(samp + (random - random));
 fHistoryPos:= ((fHistoryPos + 8) mod fOrder);

 // Update buffer (both copies)
 fHistory[fHistoryPos]:=Result - samp;
 fHistory[fHistoryPos+9]:=fHistory[fHistoryPos];
end;

function TNS9dither16.processI(Samp: Single):Integer;
begin
 // Unrolled loop for faster execution
 samp := samp - fCoefficients[8] * fHistory[fHistoryPos+8] +
                fCoefficients[7] * fHistory[fHistoryPos+7] +
                fCoefficients[6] * fHistory[fHistoryPos+6] +
                fCoefficients[5] * fHistory[fHistoryPos+5] +
                fCoefficients[4] * fHistory[fHistoryPos+4] +
                fCoefficients[3] * fHistory[fHistoryPos+3] +
                fCoefficients[2] * fHistory[fHistoryPos+2] +
                fCoefficients[1] * fHistory[fHistoryPos+1] +
                fCoefficients[0] * fHistory[fHistoryPos];

 result := round(samp + (random - random));
 fHistoryPos:= ((fHistoryPos + 8) mod fOrder);

 // Update buffer (both copies)
 fHistory[fHistoryPos]     := Result - samp;
 fHistory[fHistoryPos + 9] := fHistory[fHistoryPos];
end;

end.
