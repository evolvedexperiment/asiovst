unit DAV_DspFilterChebyshev;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_DspFilter;

type
  TCustomChebyshevFilterClass = class of TCustomChebyshevFilter;
  TCustomChebyshevFilter = class(TCustomOrderFilter)
  protected
    FHypFactors   : TDAV2DoubleArray;
    FFilterGain   : Double;
    FTanW0Half    : Double;
    FOrderInv     : Double;
    FExpOrdPiHalf : TComplexDouble;
    procedure CalculateW0; override;
    procedure CalculateHypFactors; virtual; abstract;
    procedure CalculateExpOrdPiHalf; virtual; 
    procedure OrderChanged; override;
  public
    constructor Create(const Order: Integer = 0); override;
    procedure ResetStatesInt64; override;
    function Real(const Frequency: Double): Double; override;
    function Imaginary(const Frequency: Double): Double; override;
  end;

  TCustomDownsampledChebyshevFilter = class(TCustomChebyshevFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    FDownsamplePow : Integer;
    FDownsampleFak : Integer;
    procedure CalculateW0; override;
  public
    constructor Create(const Order: Integer = 0); override;
    property DownsampleAmount : Integer read FDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read FDownsampleFak;
  end;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math;

{ TCustomChebyshevFilter }

constructor TCustomChebyshevFilter.Create(const Order: Integer = 0);
begin
 FOrder  := Order;
 OrderChanged;
 inherited Create(Order);
end;

procedure TCustomChebyshevFilter.CalculateW0;
begin
 // inherited; FTanW0Half := FExpW0.Im / (1 + FExpW0.Re);
 FW0 := Pi * FSRR * FFrequency;
 FTanW0Half := tan(FW0);
end;

function TCustomChebyshevFilter.Imaginary(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, Temp, result);
end;

procedure TCustomChebyshevFilter.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   CalculateExpOrdPiHalf;
   CalculateHypFactors;
   ResetStates;
   inherited;
  end
 else
  begin
   FOrderInv := 1;
   FFilterGain := 1;
  end;
 Changed; 
end;

procedure TCustomChebyshevFilter.CalculateExpOrdPiHalf;
begin
 GetSinCos(Pi * CHalf64 * FOrderInv, FExpOrdPiHalf.Im, FExpOrdPiHalf.Re);
end;

function TCustomChebyshevFilter.Real(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, result, Temp);
end;

procedure TCustomChebyshevFilter.ResetStatesInt64;
begin
 inherited;
 ResetStates;
end;

{ TCustomDownsampledChebyshevFilter }

constructor TCustomDownsampledChebyshevFilter.Create(const Order: Integer = 0);
begin
 FDownsamplePow := 0;
 FDownsampleFak := 1;
 inherited Create(Order);
end;

procedure TCustomDownsampledChebyshevFilter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FDownsamplePow <> Value then
  begin
   FDownsamplePow := Value;
   FDownsampleFak := round(IntPower(2, FDownsamplePow));
   CalculateW0;
  end;
end;

procedure TCustomDownsampledChebyshevFilter.CalculateW0;
begin
 // inherited; FTanW0Half := FExpW0.Im / (1 + FExpW0.Re);
 FW0 := Pi * FSRR * FFrequency * FDownsampleFak;
 FTanW0Half := tan(FW0);
end;

end.
