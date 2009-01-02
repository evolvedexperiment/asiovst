unit DAV_DspPolyphaseFilter;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspPolyphaseIirDesigner;

type
  TCustomPolyphaseFilter = class
  private
    procedure SetNumberOfCoeffs(const Value: Integer);
    procedure SetTransition(const Value: Double);
  protected
    FCoefficients   : PDAVDoubleFixedArray;
    FNumberOfCoeffs : Integer;
    FTransition     : Double;
    FAttenuation    : Double;
    procedure NumberOfCoeffsChanged; virtual;
    procedure TransitionChanged; virtual;
    procedure SetProcedures; virtual; abstract;
    procedure SetCoefficients(const Coefficients: TDAVDoubleDynArray); overload; virtual;
    procedure SetCoefficients(const NumberOfCoefficients: Integer = 8; const Transition: Double = 0.1); overload; virtual;

    property Coefficients: PDAVDoubleFixedArray read FCoefficients;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property NumberOfCoefficients: Integer read FNumberOfCoeffs write SetNumberOfCoeffs;
    property Attenuation: Double read FAttenuation;
    property Transition: Double read FTransition write SetTransition;
  end;

implementation

constructor TCustomPolyphaseFilter.Create;
begin
 inherited;
 FCoefficients := nil;
 FNumberOfCoeffs := 8;
 FTransition := 0.1;
end;

destructor TCustomPolyphaseFilter.Destroy;
begin
 Dispose(FCoefficients);
 inherited;
end;

procedure TCustomPolyphaseFilter.SetCoefficients(const Coefficients: TDAVDoubleDynArray);
begin
  // make sure the coefficients are not empty
  assert(Length(Coefficients) > 0);

  // check if the number of coeffitients changed
  if FNumberOfCoeffs <> Length(Coefficients) then
   begin
    FNumberOfCoeffs := Length(Coefficients);
    NumberOfCoeffsChanged;
   end;

  // actually copy the coefficients
  move(Coefficients[0], FCoefficients[0], FNumberOfCoeffs * SizeOf(Double));
end;

procedure TCustomPolyphaseFilter.SetCoefficients(
  const NumberOfCoefficients: Integer = 8;
  const Transition: Double = 0.1);
var
  tmpCoeffs: TDAVDoubleDynArray;
begin
  SetLength(tmpCoeffs, NumberOfCoefficients);
  TPolyphaseIirDesigner.ComputeCoeffsSpecOrderTBW(tmpCoeffs, NumberOfCoefficients, Transition);
  FAttenuation := TPolyphaseIirDesigner.ComputeAttenuationFromOrderTBW(FNumberOfCoeffs, FTransition);
  SetCoefficients(tmpCoeffs);
end;

procedure TCustomPolyphaseFilter.NumberOfCoeffsChanged;
begin
 ReallocMem(FCoefficients, FNumberOfCoeffs * SizeOf(Double));
 SetCoefficients(FNumberOfCoeffs, FTransition);
end;

procedure TCustomPolyphaseFilter.TransitionChanged;
begin
 SetCoefficients(FNumberOfCoeffs, FTransition);
end;

procedure TCustomPolyphaseFilter.SetNumberOfCoeffs(const Value: Integer);
begin
 if FNumberOfCoeffs <> Value then
  begin
   FNumberOfCoeffs := Value;
   NumberOfCoeffsChanged;
  end;
end;

procedure TCustomPolyphaseFilter.SetTransition(const Value: Double);
begin
 if FTransition <> Value then
  begin
   FTransition := Value;
   TransitionChanged;
  end;
end;

end.
