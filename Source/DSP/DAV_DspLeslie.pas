unit DAV_DspLeslie;

interface

uses
  DAV_Common, DAV_DspCommon;

type
  TLeslieSpeed = (lsStop, lsSlow, lsFast);

  TLeslieRotator = class(TDspObject)
  private
    FLeslieSpeed   : TLeslieSpeed;
    FLowWidth      : Single;
    FLowThrob      : Single;
    FHighWidth     : Single;
    FHighDepth     : Single;
    FHighThrob     : Single;
    FCrossover     : Single;
    FOutputGain    : Single;
    FSpeed         : Single;
    FSampleRate    : Single;
    FInvSampleRate : Single;
    procedure SetSampleRate(const Value: Single);
    procedure SetCrossover(const Value: Single);
    procedure SetHighDepth(const Value: Single);
    procedure SetHighThrob(const Value: Single);
    procedure SetHighWidth(const Value: Single);
    procedure SetLeslieSpeed(const Value: TLeslieSpeed);
    procedure SetLowThrob(const Value: Single);
    procedure SetLowWidth(const Value: Single);
    procedure SetOutputGain(const Value: Single);
    procedure SetSpeed(const Value: Single);

    procedure CalculateInvSamplerate;
    procedure SetInternalSpeedFactors;
  protected
    FGainFactor  : Single;
    FFilo        : Single;
    FLoWid       : Single;
    FLowLevel    : Single;
    FHiWid       : Single;
    FHDep        : Single;
    FHighLevel   : Single;
    FHiMom       : Single;
    FLoMom       : Single;
    FHiSet       : Single;
    FLoSet       : Single;
    FHMom        : Single;
    FLMom        : Single;
    FHSet        : Single;
    FLSet        : Single;
    FLoSpd       : Single;
    FHiSpd       : Single;
    FLoPhi       : Single;
    FHiPhi       : Single;
    FSpd         : Single;
    FHiPos       : Integer;
    FDelayBuffer : PDAVSingleFixedArray;
    FState       : Array [0..1] of Single;

    procedure CrossoverChanged; virtual;
    procedure GainChanged; virtual;
    procedure HighDepthChanged; virtual;
    procedure HighThrobChanged; virtual;
    procedure HighWidthChanged; virtual;
    procedure LeslieSpeedChanged; virtual;
    procedure LowThrobChanged; virtual;
    procedure LowWidthChanged; virtual;
    procedure MomChanged; virtual;
    procedure OutputGainChanged; virtual;
    procedure SamplerateChanged; virtual;
    procedure SpeedChanged; virtual;
    procedure SpeedParametersChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Process(Input: Single; out Left, Right: Single);

    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Crossover: Single read FCrossover write SetCrossover;
    property LeslieSpeed: TLeslieSpeed read FLeslieSpeed write SetLeslieSpeed default lsFast;
    property LowThrob: Single read FLowThrob write SetLowThrob;
    property LowWidth: Single read FLowWidth write SetLowWidth;
    property HighThrob: Single read FHighThrob write SetHighThrob;
    property HighDepth: Single read FHighDepth write SetHighDepth;
    property HighWidth: Single read FHighWidth write SetHighWidth;
    property OutputGain: Single read FOutputGain write SetOutputGain;
    property Speed: Single read FSpeed write SetSpeed;
  end;

implementation

uses
  Math;

{ TLeslieRotator }

constructor TLeslieRotator.Create;
begin
 inherited;
 FLeslieSpeed := lsFast;
 FHiPos       := 0;
 FHiPhi       := 1.6;
 FLowThrob    := 60;
 FLowWidth    := 70;
 FHighThrob   := 60;
 FHighDepth   := 70;
 FHighWidth   := 0.48;
 FOutputGain  := 0;
 FSpeed       := 100;

 FSampleRate  := 44100;
 CalculateInvSamplerate;

 SetInternalSpeedFactors;
 OutputGainChanged;
 GainChanged;
 MomChanged;
 CrossoverChanged; 
 GainChanged; 
 HighDepthChanged; 
 HighThrobChanged; 
 HighWidthChanged; 
 LeslieSpeedChanged; 
 LowThrobChanged; 
 LowWidthChanged;
 SpeedParametersChanged;

 GetMem(FDelayBuffer, 256 * SizeOf(Single));
end;

destructor TLeslieRotator.Destroy;
begin
 Dispose(FDelayBuffer);
 inherited;
end;

procedure TLeslieRotator.SetCrossover(const Value: Single);
begin
 if FCrossover <> Value then
  begin
   FCrossover := Value;
   CrossoverChanged;
  end;
end;

procedure TLeslieRotator.SetHighDepth(const Value: Single);
begin
 if FHighDepth <> Value then
  begin
   FHighDepth := Value;
   HighDepthChanged;
  end;
end;

procedure TLeslieRotator.SetHighThrob(const Value: Single);
begin
 if FHighThrob <> Value then
  begin
   FHighThrob := Value;
   HighThrobChanged;
  end;
end;

procedure TLeslieRotator.SetHighWidth(const Value: Single);
begin
 if FHighWidth <> Value then
  begin
   FHighWidth := Value;
   HighWidthChanged;
  end;
end;

procedure TLeslieRotator.SetLeslieSpeed(const Value: TLeslieSpeed);
begin
 if FLeslieSpeed <> Value then
  begin
   FLeslieSpeed := Value;
   LeslieSpeedChanged;
  end;
end;

procedure TLeslieRotator.SetLowThrob(const Value: Single);
begin
 if FLowThrob <> Value then
  begin
   FLowThrob := Value;
   LowThrobChanged;
  end;
end;

procedure TLeslieRotator.SetLowWidth(const Value: Single);
begin
 if FLowWidth <> Value then
  begin
   FLowWidth := Value;
   LowWidthChanged;
  end;
end;

procedure TLeslieRotator.SetOutputGain(const Value: Single);
begin
 if FOutputGain <> Value then
  begin
   FOutputGain := Value;
   OutputGainChanged;
  end;
end;

procedure TLeslieRotator.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SamplerateChanged;
  end;
end;

procedure TLeslieRotator.SetSpeed(const Value: Single);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   SpeedChanged;
  end;
end;

procedure TLeslieRotator.CrossoverChanged;
begin
 FFilo := 1 - Power(10, FCrossover * (2.27 - 0.54 * FCrossover) - 1.92);
end;

procedure TLeslieRotator.HighDepthChanged;
begin
 FHDep := sqr(0.01 * FHighDepth) * SampleRate / 760;
end;

procedure TLeslieRotator.HighThrobChanged;
begin
 FHighLevel := FGainFactor * 0.9 * sqr(0.01 * FHighThrob);
end;

procedure TLeslieRotator.HighWidthChanged;
begin
 FHiWid := sqr(0.01 * FHighWidth);
end;

procedure TLeslieRotator.SetInternalSpeedFactors;
begin
 case FLeslieSpeed of
  lsStop :
   begin
    FLoSet := 0.00; FHiSet := 0.0;
    FLoMom := 0.12; FHiMom := 0.1;
   end;
  lsSlow :
   begin
    FLoSet := 0.49; FHiSet := 0.66;
    FLoMom := 0.27; FHiMom := 0.18;
   end;
  lsFast :
   begin
    FLoSet := 5.31; FHiSet := 6.40;
    FLoMom := 0.14; FHiMom := 0.09;
   end;
 end;
end;

procedure TLeslieRotator.LeslieSpeedChanged;
begin
 SetInternalSpeedFactors;
 MomChanged;
 SpeedParametersChanged;
end;

procedure TLeslieRotator.SpeedParametersChanged;
begin
 FSpd := 4 * Pi * (0.01 * FSpeed) * FInvSampleRate;
 FHSet := FHiSet * FSpd;
 FLSet := FLoSet * FSpd;
end;

procedure TLeslieRotator.MomChanged;
const
  COne32th : Single = 0.03125;
begin
 FHMom := Power(10, -FInvSampleRate / FHiMom);
 FLMom := Power(10, -FInvSampleRate / FLoMom);

 FHMom := Power(FHMom, COne32th);
 FLMom := Power(FLMom, COne32th);
end;

procedure TLeslieRotator.LowThrobChanged;
begin
 FLowLevel := FGainFactor * 0.9 * Sqr(0.01 * FLowThrob);
end;

procedure TLeslieRotator.LowWidthChanged;
begin
 FLoWid := Sqr(0.01 * FLowWidth);
end;

procedure TLeslieRotator.OutputGainChanged;
begin
 FGainFactor := dB_to_Amp(FOutputGain);
 GainChanged;
end;

procedure TLeslieRotator.GainChanged;
begin
 FLowLevel := FGainFactor * 0.9 * sqr(0.01 * FLowThrob);
 FHighLevel := FGainFactor * 0.9 * sqr(0.01 * FHighThrob);
end;

procedure TLeslieRotator.CalculateInvSamplerate;
begin
 FInvSampleRate := 1 / FSampleRate;
end;

procedure TLeslieRotator.SamplerateChanged;
begin
 MomChanged;
 HighDepthChanged;
 SpeedParametersChanged;
end;

procedure TLeslieRotator.SpeedChanged;
begin
 SpeedParametersChanged;
end;

procedure TLeslieRotator.Process(Input: Single; out Left, Right: Single);
var
  High, Low          : Single;
  hint               : Single;
  hdd, hdd2          : Integer;
  chp, clp, shp, slp : Single;
begin
 // set LFO values
 GetSinCos(FHiPhi, shp, chp);
 GetSinCos(FLoPhi, slp, clp);
 chp := sqr(chp) * chp;

 // calculate current speed
 FLoSpd := (FLMom * FLoSpd) + FLSet * (1 - FLMom); // tend to required speed
 FHiSpd := (FHMom * FHiSpd) + FHSet * (1 - FHMom);

 // advance low phase by current speed
 FLoPhi := FLoPhi + FLoSpd;
 while FLoPhi > 2 * Pi do FLoPhi := FLoPhi - 2 * Pi;

 // advance high phase by current speed
 FHiPhi := FHiPhi + FHiSpd;
 while FHiPhi > 2 * Pi do FHiPhi := FHiPhi - 2 * Pi;

 // crossover
 FState[0] := FFilo * (FState[0] - Input) + Input;
 FState[1] := FFilo * (FState[1] - FState[0]) + FState[0];

 // catch denormals
 if (abs(FState[0]) < 1E-10) then FState[0] := 0;
 if (abs(FState[1]) < 1E-10) then FState[1] := 0;

 // Volume
 High := (FGainFactor - FHighLevel * chp) * (Input - FState[1]);
 Low := (FGainFactor - FLowLevel * clp) * FState[1];

 // delay input pos
 if FHiPos > 0
  then Dec(FHiPos)
  else FHiPos := 200;

 // delay output pos
 hint := FHiPos + FHDep * (1 + chp);
 hdd  := round(hint);

 // linear interpolation
 hint := hint - hdd;
 hdd2 := hdd + 1;
 if (hdd > 199) then
  begin
   if (hdd > 200)
    then hdd := hdd - 201;
   hdd2 := hdd2 - 201;
  end;

 //delay input
 FDelayBuffer[FHiPos] := High;
 Input := FDelayBuffer[hdd];

 //delay output
 High := High + Input + hint * (FDelayBuffer[hdd2] - Input);

 Left := Low + High;
 Right := Low + High;
 High := High * FHiWid * shp;
 Low := Low * FLoWid * slp;

 // output
 Right := Right + Low - High;
 Left := Left + High - Low;
end;

end.
