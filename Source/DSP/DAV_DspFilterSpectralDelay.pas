unit DAV_DspFilterSpectralDelay;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspFilter;

type
  TSpectralDelayFilter = class(TCustomIIRFilter)
  private
    FFilterCount : Integer;
    FFilters     : array of TFirstOrderAllpassFilter;
    procedure SetFilterCount(const Value: Integer);
  protected
    procedure FilterCountChanged;
    procedure SampleRateChanged; override;
    procedure CalculateCoefficients; override;
    procedure FrequencyChanged; override;
  public
    constructor Create; override;
    function ProcessSample(const Input: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Reset; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure PushStates; override;
    procedure PopStates; override;
  published
    property FilterCount: Integer read FFilterCount write SetFilterCount;
  end;

implementation

uses
  Math, SysUtils;

{ TSpectralDelayFilter }

procedure TSpectralDelayFilter.CalculateCoefficients;
begin
 inherited;
 // do nothing!
end;

constructor TSpectralDelayFilter.Create;
begin
 inherited;
 FFilterCount := 16;
 FilterCountChanged;
end;

function TSpectralDelayFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := Log10(MagnitudeSquared(Frequency));
end;

function TSpectralDelayFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  Filter : Integer;
begin
 Result := 1;
 for Filter := 0 to Length(FFilters) - 1
  do Result := Result * FFilters[Filter].MagnitudeSquared(Frequency);
end;

procedure TSpectralDelayFilter.PopStates;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].PopStates;
end;

function TSpectralDelayFilter.ProcessSample(const Input: Double): Double;
var
  Filter : Integer;
begin
 Result := Input;
 for Filter := 0 to Length(FFilters) - 1
  do Result := FFilters[Filter].ProcessSample(Result);
end;

procedure TSpectralDelayFilter.PushStates;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].PushStates;
end;

procedure TSpectralDelayFilter.Reset;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].Reset;
end;

procedure TSpectralDelayFilter.ResetStates;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].ResetStates;
end;

procedure TSpectralDelayFilter.ResetStatesInt64;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].ResetStatesInt64;
end;

procedure TSpectralDelayFilter.SampleRateChanged;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].SampleRate := SampleRate;
end;

procedure TSpectralDelayFilter.SetFilterCount(const Value: Integer);
begin
 if FFilterCount <> Value then
  begin
   FFilterCount := Value;
   FilterCountChanged;
  end;
end;

procedure TSpectralDelayFilter.FilterCountChanged;
var
  Filter    : Integer;
  OldLength : Integer;
begin
 if Length(FFilters) < FFilterCount then
  begin
   OldLength := Length(FFilters);
   SetLength(FFilters, FFilterCount);
   for Filter := OldLength to FFilterCount - 1 do
    begin
     FFilters[Filter] := TFirstOrderAllpassFilter.Create;
     with FFilters[Filter] do
      begin
       SampleRate := Self.SampleRate;
       Frequency := 0.9;
      end;
    end;
  end
 else
  begin
   for Filter := FFilterCount to Length(FFilters) - 1
    do FreeAndNil(FFilters[Filter]);
   SetLength(FFilters, FFilterCount);
  end;
end;

procedure TSpectralDelayFilter.FrequencyChanged;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].Frequency := Frequency;
end;

end.
