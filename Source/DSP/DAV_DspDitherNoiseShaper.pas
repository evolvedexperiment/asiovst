unit DAV_DspDitherNoiseShaper;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_DspFilterBasics;

const
  {$A4}
  // simple error feedback
  CNoiseShaperCoefficientsEFB  : array [0..0] of Single = (1);

  // Simple 2nd order
  CNoiseShaperCoefficients2Sc  : array [0..1] of Single = (1.0, -0.5);

  // modified-E weighted
  CNoiseShaperCoefficients2MEc : array [0..1] of Single = (1.537, -0.8367);
  CNoiseShaperCoefficients3MEc : array [0..2] of Single = (1.652, -1.049,
    0.1382);
  CNoiseShaperCoefficients9MEc : array [0..8] of Single = (1.662, -1.263,
    0.4827,  -0.2913, 0.1268, -0.1124, 0.03252, -0.01265, -0.03524);

  // improved-E weighted
  CNoiseShaperCoefficients5IEc : array [0..4] of Single = (2.033, -2.165,
    1.959, -1.590, 0.6149);
  CNoiseShaperCoefficients9IEc : array [0..8] of Single = (2.847, -4.685,
    6.214, -7.184, 6.639, -5.032,  3.263, -1.632,  0.4191);

  // F-weighted
  CNoiseShaperCoefficients3Fc  : array [0..2] of Single = (1.623, -0.982,
    0.109);
  CNoiseShaperCoefficients9Fc  : array [0..8] of Single = (2.412, -3.370,
    3.937, -4.174, 3.353, -2.205,  1.281, -0.569,  0.0847);

  // Sony "super bit mapping"
  CNoiseShaperCoefficientsSBM  : array [0..11] of Single = (1.47933, -1.59032,
    1.64436, -1.36613, 9.26704E-1, -5.57931E-1,  2.6786E-1, -1.06726E-1,
    2.8516E-2, 1.23066E-3, -6.16555E-3, 3.067E-3);

  // reduced super bit mapping"
  CNoiseShaperCoefficientsSBMr : array [0..9] of Single = (1.47933, -1.59032,
    1.64436, -1.36613, 9.26704E-1, -5.57931E-1,  2.6786E-1, -1.06726E-1,
    2.8516E-2, 1.23066E-3);

  // Experimental
  CNoiseShaperCoefficientsEX  : array [0..8] of Single = (1.2194769820734,
    -1.77912468394129, 2.18256539389233, -2.33622087251503, 2.2010985277411,
    -1.81964871362306, 1.29830681491534, -0.767889385169331, 0.320990893363264);

  CNoiseShaperCoefficients14kSharp44100 : array [0..6] of Single = (
    1.62019206878484, -2.26551157411517, 2.50884415683988, -2.25007947643775,
    1.62160867255441, -0.899114621685913, 0.35350816625238);

  CNoiseShaperCoefficients15kSharpEx44100  : array [0..7] of Single = (
    2.13029284627951, -3.37834026386511, 4.18650513140503, -4.13744252026737,
    3.33572681086378, -2.10101859689547, 1.01512367881576, -0.286474308856534);

  CNoiseShaperCoefficients15kSharp44100 : array [0..7] of Single = (
    1.34860378444905, -1.80123976889643, 2.04804746376671, -1.93234174830592,
    1.59264693241396, -1.04979311664936, 0.599422666305319, -0.213194268754789);

  CNoiseShaperCoefficients16kSharp44100 : array [0..8] of Single = (
    1.07618924753262, -1.41232919229157, 1.61374140100329, -1.5996973679788,
    1.42711666927426, -1.09986023030973, 0.750589080482029, -0.418709259968069,
    0.185132272731155);

(*
  CNoiseShaperCoefficientsEX   : array [0..7] of Single = (-0.952727263532277,
    0.601561451347511,  0.109057946728173,   0.0260489037563895,
    0.126886385245994,  0.0164113078217921, -0.132348709265001,
    0.151840173811289);
*)

type
  TNoiseShaperType = (nsNone, nsEFB, ns2Sc, ns2MEc, ns3MEc, ns9MEc, ns5IEc,
    ns9IEc, ns3Fc, ns9Fc, nsSBM, nsSBMr, nsSharp14k7thOrder,
    nsSharp15k8thOrder, nsSharp16k9thOrder, nsExperimental);

  TDitherType = (dtNone, dtEqual, dtTriangular, dtGauss, dtFastGauss);  

  TCustomDitherNoiseShaper = class(TDspObject)
  private
    FBitDepth   : Byte;
    FDitherType : TDitherType;
    FLimit      : Boolean;
    FLimits     : array [0..1] of Integer;
    procedure SetBitDepth(Value: Byte);
    procedure SetDitherType(const Value: TDitherType);
  protected
    procedure BitDepthChanged; virtual; abstract;
    procedure DitherTypeChanged; virtual;
    procedure Reset; virtual; abstract;
  public
    constructor Create; virtual;

    property BitDepth: Byte read FBitDepth write SetBitDepth default 16;
    property Limit: Boolean read FLimit write FLimit default True;
    property DitherType: TDitherType read FDitherType write SetDitherType default dtTriangular;
  end;

  TCustomFIRDitherNoiseShaper = class(TCustomDitherNoiseShaper)
  private
    FOrder           : Integer;
    FHistoryPos      : Integer;
    FNoiseshaperType : TNoiseShaperType;
    procedure SetNoiseshaperType(Value: TNoiseShaperType);
    procedure SetOrder(const Value: Integer);
  protected
    procedure NoiseshaperTypeChanged; virtual;
    procedure OrderChanged; virtual; abstract;
    procedure Reset; override;

    property Order: Integer read FOrder write SetOrder;
  public
    constructor Create; override;

    property NoiseshaperType: TNoiseShaperType read FNoiseshaperType write SetNoiseshaperType default ns9Fc;
  end;

  TCustomIIRDitherNoiseShaper = class(TCustomDitherNoiseShaper)
  private
    FFilter : TCustomFilter;
  protected
    procedure Reset; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TDitherNoiseShaper32 = class(TCustomFIRDitherNoiseShaper)
  private
    FBitMul, FBitDiv : Single;
    FDitherAmplitude : Single;
    procedure ChooseNoiseshaper;
  protected
    FCoefficients : PDAVSingleFixedArray; // Coefficients
    FHistory      : PDAVSingleFixedArray; // Error History
    procedure NoiseshaperTypeChanged; override;
    procedure BitDepthChanged; override;
    procedure OrderChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessInteger(Input: Single): Integer;
    function ProcessFloat(Input: Single): Single;
    procedure Reset; override;
  published
    property BitDepth;
    property DitherAmplitude: Single read FDitherAmplitude write FDitherAmplitude;
    property DitherType;
    property Limit;
    property NoiseshaperType;
  end;

  TDitherNoiseShaper64 = class(TCustomFIRDitherNoiseShaper)
  private
    FBitMul, FBitDiv : Double;
    FDitherAmplitude : Double;
    procedure ChooseNoiseshaper;
  protected
    FCoefficients : PDAVDoubleFixedArray; // Coefficients
    FHistory      : PDAVDoubleFixedArray; // Error History
    procedure NoiseshaperTypeChanged; override;
    procedure BitDepthChanged; override;
    procedure OrderChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessInteger(Input: Double): Integer;
    function ProcessFloat(Input: Double): Double;
    procedure Reset; override;
  published
    property BitDepth;
    property DitherAmplitude: Double read FDitherAmplitude write FDitherAmplitude;
    property DitherType;
    property Limit;
    property NoiseshaperType;
  end;

  TDitherHighShelfNoiseShaper32 = class(TCustomIIRDitherNoiseShaper)
  private
    FBitMul, FBitDiv : Single;
    FDitherAmplitude : Single;
    FLastSample      : Single;
    FSampleRate: Single;
    FFrequency: Single;
    procedure SetFrequency(const Value: Single);
    procedure SetSampleRate(const Value: Single);
  protected
    procedure BitDepthChanged; override;
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; virtual;
  public
    constructor Create; override;

    function ProcessInteger(Input: Single): Integer;
    function ProcessFloat(Input: Single): Single;
  published
    property DitherAmplitude: Single read FDitherAmplitude write FDitherAmplitude;
    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property BitDepth;
    property DitherType;
    property Limit;
  end;

implementation

uses
  Math, SysUtils, DAV_Approximations;

{ TCustomDitherNoiseShaper }

constructor TCustomDitherNoiseShaper.Create;
begin
 inherited;
 FBitDepth := 16;
 FDitherType := dtTriangular;

 Randomize;
 BitDepthChanged;
end;

procedure TCustomDitherNoiseShaper.SetBitDepth(Value: Byte);
begin
 if Value < 1  then Value := 1 else
 if Value > 32 then Value := 32;
 if FBitDepth <> Value then
  begin
   FBitDepth := Value;
   BitDepthChanged;
  end;
end;

procedure TCustomDitherNoiseShaper.SetDitherType(const Value: TDitherType);
begin
 if FDitherType <> Value then
  begin
   FDitherType := Value;
   DitherTypeChanged;
  end;
end;

procedure TCustomDitherNoiseShaper.DitherTypeChanged;
begin
 Reset;
end;

{ TCustomFIRDitherNoiseShaper }

constructor TCustomFIRDitherNoiseShaper.Create;
begin
 inherited;
 FHistoryPos := 0;
 FNoiseshaperType := ns9Fc;
 NoiseshaperTypeChanged;
end;

procedure TCustomFIRDitherNoiseShaper.SetNoiseshaperType(Value: TNoiseShaperType);
begin
 if FNoiseshaperType <> Value then
  begin
   FNoiseshaperType := Value;
   NoiseshaperTypeChanged;
  end;
end;

procedure TCustomFIRDitherNoiseShaper.SetOrder(const Value: Integer);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

procedure TCustomFIRDitherNoiseShaper.NoiseshaperTypeChanged;
begin
 Reset;
end;

procedure TCustomFIRDitherNoiseShaper.Reset;
begin
 FHistoryPos := 0;
end;

{ TCustomIIRDitherNoiseShaper }

constructor TCustomIIRDitherNoiseShaper.Create;
begin
 inherited;

end;

destructor TCustomIIRDitherNoiseShaper.Destroy;
begin
 FreeAndNil(FFilter);
 inherited;
end;

procedure TCustomIIRDitherNoiseShaper.Reset;
begin
 FFilter.ResetStates;
end;

{ TDitherNoiseShaper32 }

constructor TDitherNoiseShaper32.Create;
begin
 inherited;
 FDitherAmplitude := 1;
 GetMem(FHistory, FOrder * SizeOf(Single));
 Reset;
end;

destructor TDitherNoiseShaper32.Destroy;
begin
 Dispose(FHistory);
 inherited;
end;

procedure TDitherNoiseShaper32.BitDepthChanged;
begin
 FBitMul := IntPower(2, FBitDepth - 1) - CHalf32;
 FBitDiv := 1 / FBitMul;
 FLimits[0] := round(-FBitMul - CHalf32);
 FLimits[1] := round( FBitMul - CHalf32);
end;

procedure TDitherNoiseShaper32.NoiseshaperTypeChanged;
begin
 ChooseNoiseshaper;
 inherited;
end;

procedure TDitherNoiseShaper32.OrderChanged;
begin
 ReallocMem(FCoefficients, FOrder * SizeOf(Single));
 ReallocMem(FHistory, FOrder * SizeOf(Single));
end;

procedure TDitherNoiseShaper32.ChooseNoiseshaper;
begin
 case FNoiseshaperType of
  nsNone :
   begin
    Order := 1;
    FCoefficients[0] := 0;
   end;
  nsEFB :
   begin
    Order := Length(CNoiseShaperCoefficientsEFB);
    Move(CNoiseShaperCoefficientsEFB[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns2Sc :
   begin
    Order := Length(CNoiseShaperCoefficients2Sc);
    Move(CNoiseShaperCoefficients2Sc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns2MEc :
   begin
    Order := Length(CNoiseShaperCoefficients2MEc);
    Move(CNoiseShaperCoefficients2MEc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns3MEc :
   begin
    Order := Length(CNoiseShaperCoefficients3MEc);
    Move(CNoiseShaperCoefficients3MEc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns9MEc :
   begin
    Order := Length(CNoiseShaperCoefficients9MEc);
    Move(CNoiseShaperCoefficients9MEc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns5IEc :
   begin
    Order := Length(CNoiseShaperCoefficients5IEc);
    Move(CNoiseShaperCoefficients5IEc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns9IEc :
   begin
    Order := Length(CNoiseShaperCoefficients9IEc);
    Move(CNoiseShaperCoefficients9IEc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns3Fc :
   begin
    Order := Length(CNoiseShaperCoefficients3Fc);
    Move(CNoiseShaperCoefficients3Fc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns9Fc :
   begin
    Order := Length(CNoiseShaperCoefficients9Fc);
    Move(CNoiseShaperCoefficients9Fc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsSBM :
   begin
    Order := Length(CNoiseShaperCoefficientsSBM);
    Move(CNoiseShaperCoefficientsSBM[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsSBMr :
   begin
    Order := Length(CNoiseShaperCoefficientsSBMr);
    Move(CNoiseShaperCoefficientsSBMr[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsSharp14k7thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients14kSharp44100);
    Move(CNoiseShaperCoefficients14kSharp44100[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsSharp15k8thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients15kSharp44100);
    Move(CNoiseShaperCoefficients15kSharp44100[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsSharp16k9thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients16kSharp44100);
    Move(CNoiseShaperCoefficients16kSharp44100[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsExperimental :
   begin
    Order := Length(CNoiseShaperCoefficientsEX);
    Move(CNoiseShaperCoefficientsEX[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
 end;
end;

function TDitherNoiseShaper32.ProcessFloat(Input: Single): Single;
{.$DEFINE RenderFIR}
{$IFDEF RenderFIR}
var
  Coef : Integer;
{$ENDIF}
begin
 {$IFDEF RenderFIR}
 // render FIR filter
 Result := Input;
 for Coef := 0 to FOrder - 1
  do Result := Result - FCoefficients[Coef] * FHistory[(FOrder + FHistoryPos - Coef) mod FOrder];
 FHistoryPos := (FHistoryPos + 1) mod FOrder;
 FHistory[FHistoryPos] := Input;
 {$ELSE}
 result := (ProcessInteger(Input) + CHalf32) * FBitDiv;
 {$ENDIF}
end;

function TDitherNoiseShaper32.ProcessInteger(Input: Single): Integer;
var
  Coef : Integer;
begin
 // scale input to bit range
 Input := FBitMul * Input;

 // Direct FIR filter implementation
 for Coef := 0 to FOrder - 1
  do Input := Input - FCoefficients[Coef] * FHistory[(FOrder + FHistoryPos - Coef) mod FOrder];

 FHistoryPos := (FHistoryPos + 1) mod FOrder;

 // add triangular distributed noise
 case FDitherType of
        dtNone : Result := round(Input - CHalf32);
       dtEqual : Result := round(Input - CHalf32 + FDitherAmplitude * FastRandom);
  dtTriangular : Result := round(Input - CHalf32 + FDitherAmplitude * (random - random));
       dtGauss : Result := round(Input - CHalf32 + FDitherAmplitude * RandomGauss);
   dtFastGauss : Result := round(Input - CHalf32 + FDitherAmplitude * FastRandomGauss);
  else Result := 0;
 end;

 if FLimit then
  if Result < FLimits[0] then Result := FLimits[0] else
  if Result > FLimits[1] then Result := FLimits[1];

 // update buffer
 FHistory[FHistoryPos] := Result - Input;
end;

procedure TDitherNoiseShaper32.Reset;
begin
 FillChar(FHistory^[0], FOrder * SizeOf(Single), 0);
end;


{ TDitherNoiseShaper64 }

constructor TDitherNoiseShaper64.Create;
begin
 inherited;
 FDitherAmplitude := 1;
 GetMem(FHistory, FOrder * SizeOf(Double));
 Reset;
end;

destructor TDitherNoiseShaper64.Destroy;
begin
 Dispose(FHistory);
 inherited;
end;

procedure TDitherNoiseShaper64.BitDepthChanged;
begin
 FBitMul := IntPower(2, FBitDepth - 1) - CHalf64;
 FBitDiv := 1 / FBitMul;
 FLimits[0] := round(-FBitMul - CHalf64);
 FLimits[1] := round( FBitMul - CHalf64);
end;

procedure TDitherNoiseShaper64.NoiseshaperTypeChanged;
begin
 ChooseNoiseshaper;
 inherited;
end;

procedure TDitherNoiseShaper64.ChooseNoiseshaper;
var
  Coef : Integer;
begin
 case FNoiseshaperType of
  nsEFB :
   begin
    Order := Length(CNoiseShaperCoefficientsEFB);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients2Sc[Coef];
   end;
  ns2Sc :
   begin
    Order := Length(CNoiseShaperCoefficients2Sc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients2Sc[Coef];
   end;
  ns2MEc :
   begin
    Order := Length(CNoiseShaperCoefficients2MEc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients2MEc[Coef];
   end;
  ns3Fc  :
   begin
    Order := Length(CNoiseShaperCoefficients3Fc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients3Fc[Coef];
   end;
  ns3MEc :
   begin
    Order := Length(CNoiseShaperCoefficients3MEc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients3MEc[Coef];
   end;
  ns5IEc :
   begin
    Order := Length(CNoiseShaperCoefficients5IEc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients5IEc[Coef];
   end;
  ns9Fc  :
   begin
    Order := Length(CNoiseShaperCoefficients9Fc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients9Fc[Coef];
   end;
  ns9MEc :
   begin
    Order := Length(CNoiseShaperCoefficients9MEc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients9MEc[Coef];
   end;
  ns9IEc :
   begin
    Order := Length(CNoiseShaperCoefficients9IEc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients9IEc[Coef];
   end;
  nsSBM :
   begin
    Order := Length(CNoiseShaperCoefficientsSBM);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficientsSBM[Coef];
   end;
  nsSBMr :
   begin
    Order := Length(CNoiseShaperCoefficientsSBMr);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficientsSBMr[Coef];
   end;
  nsSharp14k7thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients14kSharp44100);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients14kSharp44100[Coef];
   end;
  nsSharp15k8thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients15kSharp44100);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients15kSharp44100[Coef];
   end;
  nsSharp16k9thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients16kSharp44100);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients16kSharp44100[Coef];
   end;
  nsExperimental :
   begin
    Order := Length(CNoiseShaperCoefficientsEX);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficientsEX[Coef];
   end;
 end;
end;

procedure TDitherNoiseShaper64.OrderChanged;
begin
 ReallocMem(FCoefficients, FOrder * SizeOf(Double));
 ReallocMem(FHistory, FOrder * SizeOf(Double));
end;

function TDitherNoiseShaper64.ProcessFloat(Input: Double): Double;
begin
 result := ProcessInteger(Input) * FBitDiv;
end;

function TDitherNoiseShaper64.ProcessInteger(Input: Double): Integer;
var
  Coef : Integer;
begin
 // scale input to bit range
 Input := FBitMul * Input;

 // Direct FIR filter implementation
 for Coef := 0 to FOrder - 1
  do Input := Input - FCoefficients[Coef] * FHistory[(FOrder + FHistoryPos - Coef) mod FOrder];

 FHistoryPos := (FHistoryPos + 1) mod FOrder;

 // add triangular distributed noise
 case FDitherType of
        dtNone : Result := round(Input - CHalf64);
       dtEqual : Result := round(Input - CHalf64 + FDitherAmplitude * (2 * random - 1));
  dtTriangular : Result := round(Input - CHalf64 + FDitherAmplitude * (random - random));
       dtGauss : Result := round(Input - CHalf64 + FDitherAmplitude * RandomGauss);
   dtFastGauss : Result := round(Input - CHalf64 + FDitherAmplitude * FastRandomGauss);
  else Result := 0;
 end;

 if FLimit then
  if Result < FLimits[0] then Result := FLimits[0] else
  if Result > FLimits[1] then Result := FLimits[1];

 // update buffer
 FHistory[FHistoryPos] := Result - Input;
end;

procedure TDitherNoiseShaper64.Reset;
begin
 FillChar(FHistory^[0], FOrder * SizeOf(Double), 0);
end;

{ TDitherHighShelfNoiseShaper32 }

constructor TDitherHighShelfNoiseShaper32.Create;
begin
 inherited;
 FFrequency := 10000;
 FSampleRate := 44100;

 FFilter := TBasicLowShelfFilter.Create;
 with TBasicLowShelfFilter(FFilter) do
  begin
   Frequency := Self.Frequency;
   SampleRate := Self.SampleRate;
   Bandwidth := 1;
   Gain := -5;
  end;
end;

procedure TDitherHighShelfNoiseShaper32.BitDepthChanged;
begin
 FBitMul := IntPower(2, FBitDepth - 1) - CHalf32;
 FBitDiv := 1 / FBitMul;
 FLimits[0] := round(-FBitMul - CHalf32);
 FLimits[1] := round( FBitMul - CHalf32);
end;

function TDitherHighShelfNoiseShaper32.ProcessFloat(Input: Single): Single;
{-$DEFINE RenderFIR}
{$IFDEF RenderFIR}
var
  Coef : Integer;
{$ENDIF}
begin
 {$IFDEF RenderFIR}
 // render FIR filter
 Result := Input - FFilter.ProcessSample(FLastSample);
 FLastSample := Input;
 {$ELSE}
 result := (ProcessInteger(Input) + CHalf32) * FBitDiv;
 {$ENDIF}
end;

function TDitherHighShelfNoiseShaper32.ProcessInteger(Input: Single): Integer;
begin
 // scale input to bit range
 Input := FBitMul * Input;

 // Direct FIR filter implementation
 Input := Input - FFilter.ProcessSample(FLastSample);

 // add triangular distributed noise
 case FDitherType of
        dtNone : Result := round(Input - CHalf32);
       dtEqual : Result := round(Input - CHalf32 + FDitherAmplitude * FastRandom);
  dtTriangular : Result := round(Input - CHalf32 + FDitherAmplitude * (random - random));
       dtGauss : Result := round(Input - CHalf32 + FDitherAmplitude * RandomGauss);
   dtFastGauss : Result := round(Input - CHalf32 + FDitherAmplitude * FastRandomGauss);
  else Result := 0;
 end;

 if FLimit then
  if Result < FLimits[0] then Result := FLimits[0] else
  if Result > FLimits[1] then Result := FLimits[1];

 // update buffer
 FLastSample := Result - Input;
end;

procedure TDitherHighShelfNoiseShaper32.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TDitherHighShelfNoiseShaper32.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TDitherHighShelfNoiseShaper32.FrequencyChanged;
begin
 TBasicLowShelfFilter(FFilter).Frequency := Frequency;
end;

procedure TDitherHighShelfNoiseShaper32.SampleRateChanged;
begin
 FFilter.SampleRate := SampleRate;
end;

end.
