unit DAV_LMS;

interface

type
  TLMS = class(TObject)
  private
  protected
    FCoeffs   : array[0..7] of Double;
    FHistory  : array[0..15] of Single;
    FMu       : Double;
    FDelta    : Double;
    FHistPos  : Integer;
    FDeltaMul : Single;
    FXVol     : Single;
    procedure SetMu(const Value: Double);
  public
    constructor Create;
    function Process(const Input: Single): Single;
  published
    property Mu: Double read FMu write SetMu;
    property DeltaFactor: Single read FDeltaMul write FDeltaMul;
    property XVol: Single read FXVol write FXVol;
  end;

implementation

constructor TLMS.Create;
begin
  inherited Create;
  FMu := 0;
  FHistPos := 8;
  FCoeffs[0] := 1;
  FCoeffs[1] := 0;
  FCoeffs[2] := 0;
  FCoeffs[3] := 0;
  FCoeffs[4] := 0;
  FCoeffs[5] := 0;
  FCoeffs[6] := 0;
  FCoeffs[7] := 0;
end;

procedure TLMS.SetMu(const Value: Double);
begin
 if FMu <> Value then
  begin
   FMu := Value;
  end;
end;

function TLMS.Process(const Input: Single): Single;
var
  theta: Double;
begin
  Result := FHistory[FHistPos] * FCoeffs[0] +
    FHistory[FHistPos + 1] * FCoeffs[1] +
    FHistory[FHistPos + 2] * FCoeffs[2] +
    FHistory[FHistPos + 3] * FCoeffs[3] +
    FHistory[FHistPos + 4] * FCoeffs[4] +
    FHistory[FHistPos + 5] * FCoeffs[5] +
    FHistory[FHistPos + 6] * FCoeffs[6] +
    FHistory[FHistPos + 7] * FCoeffs[7];
  FDelta := Input - Result;

  theta := sqr(FHistory[FHistPos    ]) + sqr(FHistory[FHistPos + 1]) +
           sqr(FHistory[FHistPos + 2]) + sqr(FHistory[FHistPos + 3]) +
           sqr(FHistory[FHistPos + 4]) + sqr(FHistory[FHistPos + 5]) +
           sqr(FHistory[FHistPos + 6]) + sqr(FHistory[FHistPos + 7]);
  theta := 0.9 / theta;
  if FMu < theta then theta := FMu;
  FCoeffs[0] := FCoeffs[0] + 2 * theta * FDelta * FHistory[FHistPos    ];
  FCoeffs[1] := FCoeffs[1] + 2 * theta * FDelta * FHistory[FHistPos + 1];
  FCoeffs[2] := FCoeffs[2] + 2 * theta * FDelta * FHistory[FHistPos + 2];
  FCoeffs[3] := FCoeffs[3] + 2 * theta * FDelta * FHistory[FHistPos + 3];
  FCoeffs[4] := FCoeffs[4] + 2 * theta * FDelta * FHistory[FHistPos + 4];
  FCoeffs[5] := FCoeffs[5] + 2 * theta * FDelta * FHistory[FHistPos + 5];
  FCoeffs[6] := FCoeffs[6] + 2 * theta * FDelta * FHistory[FHistPos + 6];
  FCoeffs[7] := FCoeffs[7] + 2 * theta * FDelta * FHistory[FHistPos + 7];

  Dec(FHistPos);
  if FHistPos < 0 then
   begin
    FHistPos := 7;
    Move(FHistory[0], FHistory[8], 8 * SizeOf(Single));
   end;
  FHistory[FHistPos] := Input;
  Result := FDeltaMul * FDelta + FXVol * Result;
end;

end.
