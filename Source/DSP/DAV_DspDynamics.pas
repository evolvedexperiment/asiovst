unit DAV_DspDynamics;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspButterworthFilter;

type
  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  IDynamicProcessorKnee                                                   //
  //  ---------------------                                                   //
  //                                                                          //
  //  Interface used to grant access to all classes that features the knee    //
  //  property. (not in use yet!)                                             //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  IDynamicProcessorKnee = interface(IInterface)
    function GetKnee_dB: Double;
    procedure SetKnee_dB(Value: Double);
    property Knee_dB: Double read GetKnee_dB write SetKnee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomDynamicProcessor                                                 //
  //  -----------------------                                                 //
  //                                                                          //
  //  Base class for all dynamic processors. It features a pure abstract      //
  //  sample process function which is reserved for the sample based DSP      //
  //  processing.                                                             //
  //  Also an interface for calculation of the characteristic curve is        //
  //  provided. The user needs to fill the TranslatePeakToGain with live in   //
  //  order to use the functionallity properly. For feedback compressor       //
  //  designs the CharacteristicCurve function must be overriden as well.     //
  //                                                                          //
  //  Also the threshold property is located in this base class, since it is  //
  //  common for all dynamics processors.                                     //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomDynamicProcessor = class(TDspObject)
  private
    function GetKnee_dB: Double;
    procedure SetThreshold(const Value: Double);
    procedure SetKnee_dB(Value: Double);
  protected
    FGain         : Double;
    FThreshold    : Double;
    FThreshold_dB : Double;
    FKnee_dB      : Double;
    procedure CalculateThreshold; virtual;
    procedure KneeChanged; virtual;
    procedure ThresholdChanged; virtual;
  public
    constructor Create; virtual;
    function ProcessSample(const Input : Double): Double; virtual; abstract;
    function GainSample(const Input : Double): Double; virtual;
    procedure InputSample(const Input : Double); virtual; abstract;
    function TranslatePeakToGain(const PeakLevel: Double): Double; virtual; abstract;
    function CharacteristicCurve(const InputLevel: Double): Double; virtual;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; virtual;

    property Threshold_dB: Double read FThreshold_dB write SetThreshold;  // in dB
    property Knee_dB: Double read GetKnee_dB write SetKnee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSimpleDirectGate                                                       //
  //  -----------------                                                       //
  //                                                                          //
  //  Very basic gate that removes anything below the given threshold.        //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TSimpleDirectGate = class(TCustomDynamicProcessor)
  protected
    // hidden properties
    property Knee_dB;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function ProcessSample(const Input : Double):Double; override;
  published
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSoftDirectGate                                                         //
  //  ---------------                                                         //
  //                                                                          //
  //  Very basic gate that removes anything softly below the given threshold. //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TSoftDirectGate = class(TCustomDynamicProcessor)
  private
    procedure CalculateKneedThreshold;
  protected
    FSoftKnee       : array [0..1] of Double;
    FKneedThreshold : Double;
    procedure SetSoftKnee_dB(const Value: Double);
    procedure CalculateSoftKnee;
    procedure ThresholdChanged; override;
    procedure KneeChanged; override;
  public
    constructor Create; override;
    function ProcessSample(const Input: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property Threshold_dB;
    property Knee_dB;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomBrickwallLimiter                                                 //
  //  -----------------------                                                 //
  //                                                                          //
  //  Base class for brickwall limiters. Properties introduced here are:      //
  //  AutoMakeUp and MakeUpGain_dB. Also the reciprocal of the threshold is   //
  //  calculated.                                                             //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomBrickwallLimiter = class(TCustomDynamicProcessor)
  private
    FMakeUpGain_dB : Double;
    FAutoMakeUp    : Boolean;
    procedure SetAutoMakeUp(const Value: Boolean);
    procedure SetMakeUpGain_dB(const Value: Double);
  protected
    FThresholdReciprocal : Double;
    FMakeUpGain          : Double;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; virtual;
    procedure MakeUpGainChanged; virtual;

    // hidden properties
    property Knee_dB;
  public
    constructor Create; override;

    function ProcessSample(const Input: Double): Double; override;
    procedure InputSample(const Input: Double); override;

    property AutoMakeUp : Boolean read FAutoMakeUp write SetAutoMakeUp;
    property MakeUpGain_dB : Double read FMakeUpGain_dB write SetMakeUpGain_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TBrickwallLimiter                                                       //
  //  -----------------                                                       //
  //                                                                          //
  //  Very basic limiter that simply clips the samples that lie beyond the    //
  //  given threshold.                                                        //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TBrickwallLimiter = class(TCustomBrickwallLimiter)
  public
    function ProcessSample(const Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function CharacteristicCurve(const InputLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSoftBrickwallLimiter                                                   //
  //  ---------------------                                                   //
  //                                                                          //
  //  Soft limiter that uses a tanh(x) waveshaper to clip the input signal    //
  //  beyond a given threshold. The shape can be adjusted using the knee      //
  //  control.                                                                //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TSoftBrickwallLimiter = class(TCustomBrickwallLimiter)
  private
    FSoftKnee : array [0..1] of Double;
    procedure CalculateSoftKnee;
  protected
    procedure KneeChanged; override;
  public
    constructor Create; override;
    function ProcessSample(const Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Knee_dB;
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSimpleSoftBrickwallLimiter                                             //
  //  ---------------------------                                             //
  //                                                                          //
  //  Soft limiter that uses a simple, but common formula to control the      //
  //  waveshape. It features a knee property that can controls the dip around //
  //  the threshold in dB.                                                    //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TSimpleSoftBrickwallLimiter = class(TCustomBrickwallLimiter)
  private
    FSoftKnee : array [0..1] of Double;
    procedure CalculateSoftKnee;
  protected
    procedure KneeChanged; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
    property Knee_dB;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomTimeConstantDynamics                                             //
  //  ---------------------------                                             //
  //                                                                          //
  //  Time constans and thus a time variant behaviour is introduced with this //
  //  base class. Also the sample rate needs to be known here.                //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomTimeConstantDynamics = class(TCustomDynamicProcessor)
  private
    procedure SetAttack(const Value: Double);
    procedure SetRelease(const Value: Double);
    procedure SetSampleRate(const Value: Double);
    function GetGainReductiondB: Double;
  protected
    FPeak          : Double;
    FLevel         : Double;
    FSampleRate    : Double;
    FSampleRateRez : Double;
    FRelease       : Double;
    FAttack        : Double;
    FReleaseFactor : Double;
    FAttackFactor  : Double;
    procedure SampleRateChanged; virtual;
    procedure AttackChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;

    // hidden properties
    property Knee_dB;
  public
    constructor Create; override;

    property Attack: Double read FAttack write SetAttack;             // in ms
    property Release: Double read FRelease write SetRelease;          // in ms
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property GainReductionFactor: Double read FGain;                  // in dB
    property GainReductiondB: Double read GetGainReductiondB;         // in dB
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomLimiter                                                          //
  //  --------------                                                          //
  //                                                                          //
  //  Base class for time variant limiters. Properties introduced here are:   //
  //  AutoMakeUp and MakeUpGain_dB. Also the reciprocal of the threshold is   //
  //  calculated.                                                             //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomLimiter = class(TCustomTimeConstantDynamics)
  private
    FMakeUpGain_dB : Double;
    FAutoMakeUp    : Boolean;
    procedure SetAutoMakeUp(const Value: Boolean);
    procedure SetMakeUpGain_dB(const Value: Double);
  protected
    FThresholdReciprocal : Double;
    FMakeUpGain          : Double;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; virtual;
    procedure MakeUpGainChanged; virtual;
  public
    constructor Create; override;

    function ProcessSample(const Input: Double): Double; override;
    procedure InputSample(const Input: Double); override;

    property AutoMakeUp : Boolean read FAutoMakeUp write SetAutoMakeUp;
    property MakeUpGain_dB : Double read FMakeUpGain_dB write SetMakeUpGain_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TLimiter                                                                //
  //  --------                                                                //
  //                                                                          //
  //  Very basic hard knee limiter.                                           //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TLimiter = class(TCustomLimiter)
  public
    function ProcessSample(const Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function CharacteristicCurve(const InputLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSoftKneeLimiter                                                        //
  //  ----------------                                                        //
  //                                                                          //
  //  Soft limiter that uses a tanh(x) waveshaper to clip the input signal    //
  //  beyond a given threshold. The shape can be adjusted using the knee      //
  //  control.                                                                //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomKneeLimiter = class(TCustomLimiter)
  protected
    procedure KneeChanged; override;
  public
    property Knee_dB;
  end;

  TSoftKneeLimiter = class(TCustomKneeLimiter)
  private
    FSoftKnee : array [0..1] of Double;
    procedure CalculateSoftKnee;
  protected
    procedure KneeChanged; override;
  public
    constructor Create; override;
    function ProcessSample(const Input: Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
    property Knee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TSimpleSoftKneeLimiter                                                  //
  //  ----------------------                                                  //
  //                                                                          //
  //  Soft limiter that uses a simple, but common formula to control the      //
  //  waveshape. It features a knee property that can controls the dip around //
  //  the threshold in dB.                                                    //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TSimpleSoftKneeLimiter = class(TCustomKneeLimiter)
  private
    FSoftKnee : array [0..1] of Double;
    procedure CalculateSoftKnee;
  protected
    procedure KneeChanged; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample(const Input: Double): Double; override;
  published
    property AutoMakeUp;
    property MakeUpGain_dB;
    property Threshold_dB;
    property Knee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TFastSoftKneeLimiter                                                    //
  //  --------------------                                                    //
  //                                                                          //
  //  Fast soft knee limiter that uses approximations to obtain a             //
  //  controllable knee [in dB] around a given threshold.                     //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TFastSoftKneeLimiter = class(TCustomKneeLimiter)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
  protected
    FThrshlddB   : Single;
    FMkpdB       : Single;
    FKneeFactor  : Single;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure CalculateMakeUpGain; virtual;
    procedure AutoMakeUpChanged; override;
    procedure MakeUpGainChanged; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample(const Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomClassicGate                                                      //
  //  ------------------                                                      //
  //                                                                          //
  //  This base class encapsulates a straight forward classic gate            //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomClassicGate = class(TCustomTimeConstantDynamics)
  public
    function ProcessSample(const Input: Double): Double; override;
  end;

  TClassicGate = class(TCustomClassicGate)
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    procedure InputSample(const Input : Double); override;
    function ProcessSample(const Input: Double): Double; override;
  published
    property Attack;
    property Release;
    property SampleRate;
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomClassicRangeGate                                                 //
  //  -----------------------                                                 //
  //                                                                          //
  //  This base class encapsulates a gate that allows to attenuate the        //
  //  signal below the given threshold.                                       //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomClassicRangeGate = class(TCustomClassicGate)
  private
    procedure SetRange_dB(const Value: Double);
  protected
    FRange_dB : Double;
    FRange    : Double;
    procedure CalculateRange; virtual;
  public
    constructor Create; override;
    property Range_dB: Double read FRange_dB write SetRange_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TClassicSoftRangeGate                                                   //
  //  ---------------------                                                   //
  //                                                                          //
  //  An implementation of a RangeGate with soften edges.                     //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TClassicSoftRangeGate = class(TCustomClassicRangeGate)
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    procedure InputSample(const Input : Double); override;
  published
    property Attack;
    property Release;
    property SampleRate;
    property Range_dB;
    property Threshold_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftKneeGate                                                     //
  //  -------------------                                                     //
  //                                                                          //
  //  The base class for all soft knee gates. Features a soft knee property.  //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftKneeGate = class(TCustomClassicGate)
  protected
    FSoftKnee : array [0..1] of Double;
    procedure CalculateSoftKnee; virtual;
    procedure KneeChanged; override;
  public
    constructor Create; override;
    property Knee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TClassicSoftKneeGate                                                    //
  //  --------------------                                                    //
  //                                                                          //
  //  The basic implementation of a classic soft knee.                        //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TClassicSoftKneeGate = class(TCustomSoftKneeGate)
  private
    procedure CalculateKneedThreshold;
  protected
    FKneedThreshold : Double;
    procedure CalculateSoftKnee; override;
    procedure ThresholdChanged; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    procedure InputSample(const Input : Double); override;
  published
    property Attack;
    property Release;
    property SampleRate;
    property Knee_dB;
    property Threshold_dB;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomTimeConstantRatioDynamics                                        //
  //  --------------------------------                                        //
  //                                                                          //
  //  This base class introduces a ratio property and calculate its           //
  //  reciprocal value. Furthermore the reciprocal of the Threshold is        //
  //  calculated.                                                             //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomTimeConstantRatioDynamics = class(TCustomTimeConstantDynamics)
  private
    procedure SetRatio(const Value: Double);
  protected
    FRatio               : Double;
    FRatioReciprocal     : Double;
    FThresholdReciprocal : Double;
    procedure RatioChanged; virtual;
    procedure ThresholdChanged; override;
  public
    constructor Create; override;

    property Ratio: Double read FRatio write SetRatio;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomGate                                                             //
  //  -----------                                                             //
  //                                                                          //
  //  The base class for all full featured gates. Includes a Hold property,   //
  //  a Range property and a Knee property.                                   //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomGate = class(TCustomTimeConstantRatioDynamics)
  private
    procedure CalculateSoftKnee;
    procedure SetHold(Value: Double);
    procedure SetRange_dB(const Value: Double);
  protected
    FHold        : Double;
    FSoftKnee    : array [0..1] of Double;
    FRange_dB    : Double;
    FRangeFactor : Double;
    FHoldSmplCnt : Integer;
    FHoldSamples : Double;
    procedure CalculateHoldSamples;
    procedure CalculateRangeFactor;
    procedure KneeChanged; override;
    procedure RangeChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample(const Input : Double):Double; override;
    procedure InputSample(const Input: Double); override;

    property Hold: Double read FHold write SetHold;              // in s
    property Range_dB: Double read FRange_dB write SetRange_dB;  // in dB
    property Knee_dB;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TAdvancedGate                                                           //
  //  -------------                                                           //
  //                                                                          //
  //  This class features several additional properties necessary for a full  //
  //  featured gate.                                                          //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TAdvancedGate = class(TCustomGate)
  private
    FThresholdKneeFactor  : Double;
    FRangeThresholdFactor : Double;
    function GetHighCut: Double;
    function GetLowCut: Double;
    procedure CalculateRangeThresholdFactor;
    procedure CalculateThresholdKneeFactor;
    procedure SetHighCut(const Value: Double);
    procedure SetLowCut(const Value: Double);
  protected
    FSideChain   : Double;
    FDuck        : Boolean;
    FLowCut      : TButterworthHP;
    FHighCut     : TButterworthLP;
    procedure SampleRateChanged; override;
    procedure ThresholdChanged; override;
    procedure RangeChanged; override;
    procedure KneeChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure InputSample(const Input: Double); override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property Duck : Boolean read FDuck write FDuck;       // not implemented yet
    property SideChainLowCut : Double read GetLowCut write SetLowCut;     // in Hz
    property SideChainHighCut : Double read GetHighCut write SetHighCut;  // in Hz
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomCompressor                                                       //
  //  -----------------                                                       //
  //                                                                          //
  //  The base class for all compressor implementations.                      //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomCompressor = class(TCustomTimeConstantRatioDynamics)
  private
    procedure SetAutoMakeUp(const Value: Boolean);
    procedure SetMakeUpGain_dB(const Value: Double);
  protected
    FAutoMakeUp    : Boolean;
    FMakeUpGain_dB : Double;
    FMakeUpGain    : Double;
    procedure RatioChanged; override;
    procedure ThresholdChanged; override;
    procedure CalculateMakeUpGain; virtual;
    procedure AutoMakeUpChanged; virtual;
    procedure MakeUpGainChanged; virtual;
  public
    constructor Create; override;
    function ProcessSample(const Input : Double): Double; override;
    procedure InputSample(const Input: Double); override;

    property AutoMakeUp : Boolean read FAutoMakeUp write SetAutoMakeUp;
    property MakeUpGain_dB : Double read FMakeUpGain_dB write SetMakeUpGain_dB; //in dB
  end;

  TSimpleCompressor = class(TCustomCompressor)
  protected
    FMakeUpGain    : TDAV2DoubleArray;
    procedure CalculateMakeUpGain; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  end;

  TCustomKneeCompressor = class(TCustomCompressor)
  protected
    procedure KneeChanged; override;
  public
    property Knee_dB;
  end;

  TSoftKneeCompressor = class(TCustomKneeCompressor)
  protected
    FMakeUpGain : TDAV2DoubleArray;
    procedure CalculateMakeUpGain; override;
    procedure ThresholdChanged; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property Knee_dB;
  end;

  TFastCompressor = class(TCustomKneeCompressor)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
  protected
    FRatioFactor : Single;
    FThrshlddB   : Single;
    FMkpdB       : Single;
    FKneeFactor  : Single;
    procedure RatioChanged; override;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure CalculateMakeUpGain; override;
    procedure AutoMakeUpChanged; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample(const Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TCustomFeedbackCompressor = class(TCustomCompressor)
  protected
    FPreviousAbsSample : Double;
  public
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  end;

  TSimpleFeedbackCompressor = class(TCustomFeedbackCompressor)
  protected
    FMakeUpGains    : TDAV2DoubleArray;
    procedure RatioChanged; override;
    procedure CalculateMakeUpGain; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
  public
    function ProcessSample(const Input : Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function CharacteristicCurve(const InputLevel: Double): Double; override;
  end;

  TSoftKneeFeedbackCompressor = class(TSimpleFeedbackCompressor)
  public
    function ProcessSample(const Input : Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  end;

  TSimpleRMSCompressor = class(TSimpleCompressor)
  private
    FRMSTime    : Double;
    procedure SetRMSTime(const Value: Double);
    procedure UpdateRMSBuffer;
  protected
    FRMSSize    : Integer;
    FRMSPos     : Integer;
    FRMSFactor  : Double;
    FRMSBuffer  : TDAVDoubleDynArray;
    FCurrentRMS : Double;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    function ProcessSample(const Input : Double):Double; override;
    procedure InputSample(const Input: Double); override;
  published
    property RMSTime : Double read FRMSTime write SetRMSTime;  // in ms
  end;

  TCompressor = class(TSimpleRMSCompressor)
  private
    function GetHighCut: Double;
    function GetLowCut: Double;
    procedure SetHighCut(const Value: Double);
    procedure SetLowCut(const Value: Double);
  protected
    FLowCut  : TButterworthHP;
    FHighCut : TButterworthLP;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure InputSample(const Input : Double); override;
    function ProcessSample(const Input : Double):Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property SideChainLowCut : Double read GetLowCut write SetLowCut;     // in Hz
    property SideChainHighCut : Double read GetHighCut write SetHighCut;  // in Hz
  end;

(*
  TSoftKneeFeedbackLimiter = class(TSoftKneeLimiter)
  protected
    FOversample : Integer;
    FFilter     : TButterworthLowCut;
    FAttackFac2 : Double;
    FReleaseFac2  : Double;
    FPeak2      : Double;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
    procedure SampleRateChanged; override;
  public
    function ProcessSample(const Input : Double):Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    constructor Create; override;
  end;
*)

implementation

uses
  SysUtils, Math, DAV_Approximations;

{ TCustomDynamicProcessor }

constructor TCustomDynamicProcessor.Create;
begin
  inherited;
  FThreshold_dB := -40;
  FThreshold    := dB_to_Amp(FThreshold_dB);
  FKnee_dB      := 0;
end;

procedure TCustomDynamicProcessor.CalculateThreshold;
begin
  FThreshold := dB_to_Amp(FThreshold_dB);
end;

function TCustomDynamicProcessor.CharacteristicCurve(const InputLevel: Double): Double;
begin
 result := TranslatePeakToGain(abs(InputLevel)) * InputLevel;
end;

function TCustomDynamicProcessor.CharacteristicCurve_dB(const InputLevel_dB: Double): Double;
begin
 result := Amp_to_dB(cDenorm32 + CharacteristicCurve(dB_to_Amp(InputLevel_dB)));
end;

function TCustomDynamicProcessor.GainSample(const Input: Double): Double;
begin
 result := FGain * Input;
end;

function TCustomDynamicProcessor.GetKnee_dB: Double;
begin
 result := FKnee_dB;
end;

procedure TCustomDynamicProcessor.KneeChanged;
begin
 raise Exception.Create('The knee property is not in use in this class');
end;

procedure TCustomDynamicProcessor.SetKnee_dB(Value: Double);
begin
 if Value < 0 then Value := 0;
 if FKnee_dB <> Value then
  begin
   FKnee_dB := Value;
   KneeChanged;
  end;
end;

procedure TCustomDynamicProcessor.SetThreshold(const Value: Double);
begin
  if FThreshold_dB <> Value then
  begin
    FThreshold_dB := Value;
    ThresholdChanged;
  end;
end;

procedure TCustomDynamicProcessor.ThresholdChanged;
begin
 CalculateThreshold;
end;

{ TSimpleDirectGate }

function TSimpleDirectGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
  if PeakLevel < FThreshold
   then Result := 0
   else Result := 1;
end;

procedure TSimpleDirectGate.InputSample(const Input: Double);
begin
 FGain := TranslatePeakToGain(abs(Input));
end;

function TSimpleDirectGate.ProcessSample(const Input: Double): Double;
begin
  if abs(Input) < FThreshold
   then Result := 0
   else Result := Input;
end;

{ TSoftDirectGate }

constructor TSoftDirectGate.Create;
begin
 inherited;
 FKnee_dB := 1;
 CalculateSoftKnee;
end;

procedure TSoftDirectGate.KneeChanged;
begin
 CalculateSoftKnee;
end;

procedure TSoftDirectGate.CalculateSoftKnee;
const
  CdBScale : Double = 0.16609640474436811739351597147447;
begin
 FSoftKnee[0] := FKnee_dB * CdBScale;
 FSoftKnee[1] := 1 / FSoftKnee[0];
 CalculateKneedThreshold;
end;

procedure TSoftDirectGate.SetSoftKnee_dB(const Value: Double);
begin
 if FKnee_dB <> Value then
  begin
   FKnee_dB := Value;
   CalculateSoftKnee;
  end;
end;

procedure TSoftDirectGate.ThresholdChanged;
begin
 inherited;
 CalculateKneedThreshold;
end;

procedure TSoftDirectGate.CalculateKneedThreshold;
begin
 FKneedThreshold := Power(FThreshold, FSoftKnee[1]);
end;

procedure TSoftDirectGate.InputSample(const Input: Double);
begin
 FGain := TranslatePeakToGain(abs(Input));
end;

function TSoftDirectGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := Power((PeakLevel * Power(Power(PeakLevel, FSoftKnee[1]) + FKneedThreshold, -FSoftKnee[0])), 1 / ((abs(PeakLevel - FThreshold) + (FSoftKnee[0] + PeakLevel - FThreshold))));
end;

function TSoftDirectGate.ProcessSample(const Input: Double): Double;
begin
 InputSample(Input);
 result := GainSample(Input);
end;

{ TCustomBrickwallLimiter }

constructor TCustomBrickwallLimiter.Create;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
 FMakeUpGain          := 1;
 FMakeUpGain_dB       := 0;
end;

procedure TCustomBrickwallLimiter.AutoMakeUpChanged;
begin
 if FAutoMakeUp
  then FMakeUpGain := FThreshold
  else FMakeUpGain := dB_to_Amp(FMakeUpGain_dB);
end;

procedure TCustomBrickwallLimiter.MakeUpGainChanged;
begin
 if not FAutoMakeUp
  then FMakeUpGain := dB_to_Amp(FMakeUpGain_dB);
end;

procedure TCustomBrickwallLimiter.SetAutoMakeUp(const Value: Boolean);
begin
 if FAutoMakeUp <> Value then
  begin
   FAutoMakeUp := Value;
   AutoMakeUpChanged;
  end;
end;

procedure TCustomBrickwallLimiter.SetMakeUpGain_dB(const Value: Double);
begin
 if FMakeUpGain_dB <> Value then
  begin
   FMakeUpGain_dB := Value;
   MakeUpGainChanged;
  end;
end;

procedure TCustomBrickwallLimiter.ThresholdChanged;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
 if FAutoMakeUp
  then FMakeUpGain := FThresholdReciprocal
end;

procedure TCustomBrickwallLimiter.InputSample(const Input: Double);
begin
 FGain := TranslatePeakToGain(abs(Input));
end;

function TCustomBrickwallLimiter.ProcessSample(const Input: Double): Double;
begin
 InputSample(Input);
 result := GainSample(Input);
end;

{ TBrickwallLimiter }

function TBrickwallLimiter.ProcessSample(const Input: Double): Double;
begin
 if Input > FThreshold
  then result := FThreshold else
 if Input < -FThreshold
  then result := -FThreshold
  else result := Input;
 result := FMakeUpGain * result;
end;

function TBrickwallLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel > FThreshold
  then result := FThreshold / PeakLevel
  else result := 1;
 result := FMakeUpGain * result;
end;

function TBrickwallLimiter.CharacteristicCurve(const InputLevel: Double): Double;
begin
 if InputLevel > FThreshold
  then result := FThreshold else
 if InputLevel < -FThreshold
  then result := -FThreshold
  else result := InputLevel;
 result := FMakeUpGain * result;
end;

{ TSoftBrickwallLimiter }

constructor TSoftBrickwallLimiter.Create;
begin
 inherited;
 FKnee_dB := 1;
 KneeChanged;
end;

procedure TSoftBrickwallLimiter.KneeChanged;
begin
 CalculateSoftKnee;
end;

function TSoftBrickwallLimiter.ProcessSample(const Input: Double): Double;
begin
 FGain := TranslatePeakToGain(abs(Input));
 result := Input * FGain;
end;

function TSoftBrickwallLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  a, b : Double;
begin
 a := Power(abs(PeakLevel * FThresholdReciprocal), FSoftKnee[1]);
 b :=  1.03971379878158321 + a *
     ( 0.54953758170495126 + a *
     ( 2.13184139104070569 + a *
     (-1.46060069227128242 + a *
     ( 0.91996358346770157))));
 Result := FMakeUpGain * Power(a + 1 / b, -FSoftKnee[0]);
end;

procedure TSoftBrickwallLimiter.CalculateSoftKnee;
begin
 FSoftKnee[0] := FKnee_dB * 0.4211083378;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

{ TSimpleSoftBrickwallLimiter }

constructor TSimpleSoftBrickwallLimiter.Create;
begin
 inherited;
 FKnee_dB := 1;
 KneeChanged;
end;

procedure TSimpleSoftBrickwallLimiter.KneeChanged;
begin
 CalculateSoftKnee;
end;

function TSimpleSoftBrickwallLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := Power(1 + Power(PeakLevel * FThresholdReciprocal, FSoftKnee[1]), -FSoftKnee[0]);
end;

procedure TSimpleSoftBrickwallLimiter.CalculateSoftKnee;
const
  CdBScale : Double = 0.16609640474436811739351597147447;
begin
 FSoftKnee[0] := FKnee_dB * CdBScale;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

{ TCustomTimeConstantDynamics }

constructor TCustomTimeConstantDynamics.Create;
begin
  inherited;
  FSampleRate := 44100;
  FSampleRateRez := 1 / FSampleRate;
  FAttack := 5;
  FRelease := 5;
  FLevel := 0;
  CalculateThreshold;
  CalculateAttackFactor;
  CalculateReleaseFactor;
end;

procedure TCustomTimeConstantDynamics.SetSampleRate(const Value: Double);
begin
  if FSampleRate <> Value then
  begin
    FSampleRate := Value;
    FSampleRateRez := 1 / FSampleRate;
    SampleRateChanged;
  end;
end;

function TCustomTimeConstantDynamics.GetGainReductiondB: Double;
begin
 result := Amp_to_dB(FGain);
end;

procedure TCustomTimeConstantDynamics.ReleaseChanged;
begin
 CalculateReleaseFactor;
end;

procedure TCustomTimeConstantDynamics.AttackChanged;
begin
 CalculateAttackFactor;
end;

procedure TCustomTimeConstantDynamics.SampleRateChanged;
begin
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TCustomTimeConstantDynamics.SetAttack(const Value: Double);
begin
  if FAttack <> Value then
  begin
    FAttack := abs(Value);
    AttackChanged;
  end;
end;

procedure TCustomTimeConstantDynamics.SetRelease(const Value: Double);
begin
  if FRelease <> Value then
  begin
    FRelease := abs(Value);
    ReleaseChanged;
  end;
end;

procedure TCustomTimeConstantDynamics.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - exp( -ln2 / (FAttack * 0.001 * SampleRate));
end;

procedure TCustomTimeConstantDynamics.CalculateReleaseFactor;
begin
  if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / (FRelease * 0.001 * SampleRate));
end;

{ TCustomLimiter }

constructor TCustomLimiter.Create;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
 FMakeUpGain          := 1;
 FMakeUpGain_dB        := 0;
end;

procedure TCustomLimiter.AutoMakeUpChanged;
begin
 if FAutoMakeUp
  then FMakeUpGain := FThreshold
  else FMakeUpGain := dB_to_Amp(FMakeUpGain_dB);
end;

procedure TCustomLimiter.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);
end;

procedure TCustomLimiter.MakeUpGainChanged;
begin
 if not FAutoMakeUp
  then FMakeUpGain := dB_to_Amp(FMakeUpGain_dB);
end;

function TCustomLimiter.ProcessSample(const Input: Double): Double;
begin
 InputSample(Input);
 result := GainSample(Input);
end;

procedure TCustomLimiter.SetAutoMakeUp(const Value: Boolean);
begin
 if FAutoMakeUp <> Value then
  begin
   FAutoMakeUp := Value;
   AutoMakeUpChanged;
  end;
end;

procedure TCustomLimiter.SetMakeUpGain_dB(const Value: Double);
begin
 if FMakeUpGain_dB <> Value then
  begin
   FMakeUpGain_dB := Value;
   MakeUpGainChanged;
  end;
end;

procedure TCustomLimiter.ThresholdChanged;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
 if FAutoMakeUp
  then FMakeUpGain := FThresholdReciprocal
end;

{ TLimiter }

function TLimiter.CharacteristicCurve(const InputLevel: Double): Double;
begin
 if InputLevel > FThreshold
  then result := FThreshold else
 if InputLevel < -FThreshold
  then result := -FThreshold
  else result := InputLevel;
 result := FMakeUpGain * result;
end;

function TLimiter.ProcessSample(const Input: Double): Double;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 if FPeak > FThreshold
  then result := FThreshold else
 if FPeak < -FThreshold
  then result := -FThreshold
  else result := FPeak;
 result := FMakeUpGain * result;
end;

function TLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel > FThreshold
  then result := FThreshold / PeakLevel
  else result := 1;
 result := FMakeUpGain * result;
end;

{ TCustomKneeLimiter }

procedure TCustomKneeLimiter.KneeChanged;
begin
 // nothing here
end;

{ TSoftKneeLimiter }

constructor TSoftKneeLimiter.Create;
begin
 inherited;
 FKnee_dB := 1;
 KneeChanged;
end;

procedure TSoftKneeLimiter.KneeChanged;
begin
 CalculateSoftKnee;
end;

procedure TSoftKneeLimiter.CalculateSoftKnee;
begin
 FSoftKnee[0] := FKnee_dB * 0.4211083378;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

function TSoftKneeLimiter.ProcessSample(const Input: Double): Double;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 result := Input * TranslatePeakToGain(FPeak);
end;

function TSoftKneeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  a, b : Double;
begin
 a := Power(abs(PeakLevel * FThresholdReciprocal), FSoftKnee[1]);
 b :=  1.03971379878158321 + a *
     ( 0.54953758170495126 + a *
     ( 2.13184139104070569 + a *
     (-1.46060069227128242 + a *
     ( 0.91996358346770157))));
 Result := FMakeUpGain * Power(a + 1 / b, -FSoftKnee[0]);
end;

{ TSimpleSoftKneeLimiter }

constructor TSimpleSoftKneeLimiter.Create;
begin
 inherited;
 FKnee_dB := 1;
 KneeChanged;
end;

procedure TSimpleSoftKneeLimiter.KneeChanged;
begin
 CalculateSoftKnee;
end;

procedure TSimpleSoftKneeLimiter.CalculateSoftKnee;
const
  CdBScale : Double = 0.16609640474436811739351597147447;
begin
 FSoftKnee[0] := FKnee_dB * CdBScale;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

function TSimpleSoftKneeLimiter.ProcessSample(const Input: Double): Double;

 function Exp2(const X: Extended): Extended;
 asm
  FLD     X
  FLD     ST(0)       { i := round(y);     }
  FRNDINT
  FSUB    ST(1), ST   { f := y - i;        }
  FXCH    ST(1)       { z := 2**f          }
  F2XM1
  FLD1
  FADD
  FSCALE              { result := z * 2**i }
  FSTP    ST(1)
 end;

begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 result := Input * Exp2(-FSoftKnee[0] * log2(1 + Exp2(FSoftKnee[1] * log2(FPeak * FThresholdReciprocal))));
end;

function TSimpleSoftKneeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := Power(1 + Power(PeakLevel * FThresholdReciprocal, FSoftKnee[1]), -FSoftKnee[0]);
end;

{ TFastSoftKneeLimiter }

procedure TFastSoftKneeLimiter.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
end;

procedure TFastSoftKneeLimiter.MakeUpGainChanged;
begin
 inherited;
 CalculateMakeUpGain;
end;

procedure TFastSoftKneeLimiter.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else CalculateMakeUpGain;
end;

procedure TFastSoftKneeLimiter.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * -CHalf32;
 FMakeUpGain_dB := FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) - Temp;
 CalculateMakeUpGain;
end;

procedure TFastSoftKneeLimiter.CalculateKneeFactor;
begin
 FKneeFactor := sqr(2 * CdBtoAmpExpGain32 * FKnee_dB);
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TFastSoftKneeLimiter.ThresholdChanged;
begin
 inherited;
 FThrshlddB := Threshold_dB / CFactor2IndB32;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

function TFastSoftKneeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := PeakLevel;
 result := FastLog2ContinousError5(result) - FThrshlddB;
 result := FastPower2MinError3(CHalf32 * (FMkpdB - result - FastSqrtBab2(sqr(result) + FKneeFactor)));
end;

procedure TFastSoftKneeLimiter.CalculateMakeUpGain;
begin
 inherited;
 FMkpdB := 2 * FMakeUpGain_dB * CdBtoAmpExpGain32;
end;

function TFastSoftKneeLimiter.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := -CHalf32 * (InputLevel_dB - FThreshold_dB);
 result := Temp - FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TFastSoftKneeLimiter.ProcessSample(const Input: Double): Double;
{$IFDEF XPUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + abs(Input);

 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FastLog2ContinousError5(FPeak) - FThrshlddB;
 FGain := FastPower2MinError3(CHalf32 * (FMkpdB - Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor)));

 result := FGain * Input;
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
 // Temp := CDenorm32 + abs(Input);
 mov   edx, eax               // edx = Self
 fld   Input
 fabs
 fadd  CDenorm32              // Stack: temp

 fcom  [edx.FPeak].Double    // Stack: temp
 fstsw ax
 sahf
 jbe   @Release
@Attack:
 fsub  [edx.FPeak]
 fmul  [edx.FAttackFactor]
 fadd  [edx.FPeak]
 fst   [edx.FPeak]
 jmp   @AmpTodB
@Release:
 fld   [edx.FPeak]            // Stack: FPeak, temp
 fsubr st(0), st(1)           // Stack: (FPeak - temp), temp
 fmul  [edx.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
 faddp                        // Stack: (FPeak - temp) * FReleaseFactor + temp
 fst   [edx.FPeak]

@AmpTodB:
 fstp  IntCast                // Stack: (empty)
 mov   eax, IntCast
 mov   ecx, eax               // copy eax to ecx
 and   eax, $807fffff
 add   eax, $3f800000
 mov   IntCast, eax
 fld   CastedSingle
 fmul  [CL2Continous5        ].Single
 fadd  [CL2Continous5 + 4    ].Single
 fmul  CastedSingle
 fadd  [CL2Continous5 + 4 * 2].Single
 fmul  CastedSingle
 fadd  [CL2Continous5 + 4 * 3].Single
 fmul  CastedSingle
 fadd  [CL2Continous5 + 4 * 4].Single

 shr   ecx, $17
 and   ecx, $000000ff
 sub   ecx, $00000080
 mov   IntCast, ecx
 fild  IntCast
 faddp


 fsub  [edx.FThrshlddB]        // Stack : Temp

 // FGain := FastPower2MinError3(CHalf32 * (FMkpdB - Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor)));
 fld   st(0)                   // Stack : Temp, Temp
 fmul  st(0), st(0)
 fadd  [edx.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
 fld   st(0)                   // Stack : Intemp, Intemp, Temp
 fst   CastedSingle            // Stack : Intemp, Intemp, Temp

 mov   eax, IntCast
 sub   eax, $00800000
 shr   eax, 1
 add   eax, $20000000
 mov   IntCast, eax
 fdiv  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
 fadd  CastedSingle            // Stack: newresult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
 fld   st(0)                   // Stack: newresult, newresult, Intemp, Temp
 fmul  CQuarter32              // Stack: CQuarter32 * newresult, newresult, Intemp, Temp
 fxch                          // Stack: newresult, CQuarter32 * newresult, Intemp, Temp
 fdivp st(2), st(0)            // Stack: Intemp / newresult, CQuarter32 * newresult, Temp
 faddp                         // Stack: Intemp / newresult + CQuarter32 * newresult, Temp

 faddp                         // Stack: Temp + SqrtTemp
 fsubr [edx.FMkpdB]            // Stack: FMkpdB - (Temp + SqrtTemp)
 fmul  CHalf32                 // Stack: CHalf32 * (FMkpdB - (Temp + SqrtTemp))

 fld   st(0)                   // Stack: temp, temp
 frndint                       // Stack: round(temp), temp

 fist  IntCast                 // Stack: round(temp), temp
 fsubp                         // Stack: newtemp = temp - round(temp)

 mov   eax, IntCast            // EAX
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                         // Stack: newtemp, newtemp
 fmul  [CP2MinError3 + 4 * 2].Single // Stack: CP2MinError3[2] * newtemp, newtemp
 fadd  [CP2MinError3 + 4    ].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
 fmul  st(0), st(1)                  // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fadd  [CP2MinError3        ].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fmulp                               // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fld1
 faddp                               // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fmul  CastedSingle                  // == 1.976848

 fst  [edx.FGain]
 fmul Input
end;
{$ENDIF}


{ TCustomTimeConstantRatioDynamics }

constructor TCustomTimeConstantRatioDynamics.Create;
begin
  inherited;
  FRatio := 1;
  RatioChanged;
end;

procedure TCustomTimeConstantRatioDynamics.RatioChanged;
begin
 FRatioReciprocal :=  1 / Ratio;
end;

procedure TCustomTimeConstantRatioDynamics.SetRatio(const Value: Double);
begin
 if FRatio <> Value then
  begin
   FRatio := Value;
   RatioChanged;
  end;
end;


procedure TCustomTimeConstantRatioDynamics.ThresholdChanged;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
end;

{ TCustomClassicGate }

function TCustomClassicGate.ProcessSample(const Input: Double): Double;
begin
 InputSample(Input);
 result := GainSample(Input);
end;

{ TClassicGate }

procedure TClassicGate.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);
end;

function TClassicGate.ProcessSample(const Input: Double): Double;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 result := Input * TranslatePeakToGain(FPeak);
end;

function TClassicGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
  if PeakLevel < FThreshold
   then Result := 0
   else Result := 1;
end;

{ TCustomClassicRangeGate }

constructor TCustomClassicRangeGate.Create;
begin
 inherited;
 FRange_dB := -10;
 CalculateRange;
end;

procedure TCustomClassicRangeGate.CalculateRange;
begin
 FRange := dB_to_Amp(FRange_dB);
end;

procedure TCustomClassicRangeGate.SetRange_dB(const Value: Double);
begin
 if FRange_dB <> Value then
  begin
   FRange_dB := Value;
   CalculateRange;
  end;
end;

{ TClassicSoftRangeGate }

procedure TClassicSoftRangeGate.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);
end;

function TClassicSoftRangeGate.TranslatePeakToGain(const PeakLevel: Double): Double;
(*
const
  cScale = 1 / 100;
begin
 result := Power((Power(PeakLevel, 1 / cScale) + Power(FRange, 1 / cScale) * FThreshold) / (Power(PeakLevel, 1 / cScale) + FThreshold), cScale);
*)
begin
 result := (PeakLevel + FRange * FThreshold) / (PeakLevel + FThreshold);
end;

{ TCustomSoftKneeGate }

constructor TCustomSoftKneeGate.Create;
begin
 inherited;
 FKnee_dB := 1;
 KneeChanged;
end;

procedure TCustomSoftKneeGate.KneeChanged;
begin
 CalculateSoftKnee;
end;

procedure TCustomSoftKneeGate.CalculateSoftKnee;
begin
 FSoftKnee[0] := FKnee_dB * 0.4211083378;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

{ TClassicSoftKneeGate }

procedure TClassicSoftKneeGate.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);
end;

procedure TClassicSoftKneeGate.ThresholdChanged;
begin
 inherited;
 CalculateKneedThreshold;
end;

procedure TClassicSoftKneeGate.CalculateSoftKnee;
begin
 inherited;
 CalculateKneedThreshold;
end;

procedure TClassicSoftKneeGate.CalculateKneedThreshold;
begin
 FKneedThreshold := Power(FThreshold, FSoftKnee[1]);
end;

function TClassicSoftKneeGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
// result := Power((PeakLevel * Power(Power(PeakLevel, FSoftKnee[1]) + FKneedThreshold, -FSoftKnee[0])), 1 / ((abs(PeakLevel - FThreshold) + (FSoftKnee[0] + PeakLevel - FThreshold))));
 result := Power((PeakLevel * Power(Power(PeakLevel, FSoftKnee[1]) + FKneedThreshold, -FSoftKnee[0])), 1 / ((abs(PeakLevel - FThreshold) + (FSoftKnee[0] + PeakLevel - FThreshold))));
end;

{ TCustomGate }

constructor TCustomGate.Create;
begin
  inherited;
  FGain        := 1;
  FRange_dB    := 10;
  FHold        := 0;
  FHoldSamples := 0;
  FHoldSmplCnt := 0;
  CalculateRangeFactor;
  CalculateHoldSamples;
end;

procedure TCustomGate.CalculateHoldSamples;
begin
  FHoldSamples := FHold * FSampleRate;
end;

procedure TCustomGate.CalculateRangeFactor;
begin
 FRangeFactor := dB_to_Amp(FRange_dB);
end;

procedure TCustomGate.CalculateSoftKnee;
begin
 FSoftKnee[0] := abs(FKnee_dB) + 0.1;
 FSoftKnee[1] := 1 / FSoftKnee[0];
end;

procedure TCustomGate.KneeChanged;
begin
 CalculateSoftKnee;
end;

procedure TCustomGate.SampleRateChanged;
begin
 inherited;
 CalculateHoldSamples;
end;

procedure TCustomGate.SetHold(Value: Double);
begin
 if Value < 0 then Value := 0;
 if FHold <> Value then
  begin
   FHold := Value;
   CalculateHoldSamples;
  end;
end;

procedure TCustomGate.SetRange_dB(const Value: Double);
begin
 if FRange_dB <> Value then
  begin
   FRange_dB := Value;
   RangeChanged;
  end;
end;

procedure TCustomGate.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor else
 if FHoldSmplCnt > FHoldSamples
  then FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor
  else inc(FHoldSmplCnt);

 FGain := TranslatePeakToGain(FPeak);
end;

function TCustomGate.ProcessSample(const Input: Double): Double;
begin
 InputSample(Input);
 result := GainSample(Input);
end;

procedure TCustomGate.RangeChanged;
begin
 CalculateRangeFactor;
end;

function TCustomGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then result := Power(FThreshold, 1 - FRatio) * Power(PeakLevel, FRatio - 1) * (1 - FRangeFactor) + FRangeFactor
  else
   begin
    FHoldSmplCnt := 0; // start hold phase
    result := FRangeFactor + (1 - FRangeFactor);
   end;
end;

{ TAdvancedGate }

constructor TAdvancedGate.Create;
begin
  inherited;
  FGain              := 1;
  FLowCut            := TButterworthHP.Create;
  FHighCut           := TButterworthLP.Create;
  FLowCut.Frequency  := 20;
  FHighCut.Frequency := 20000;
end;

destructor TAdvancedGate.Destroy;
begin
  FreeAndNil(FLowCut);
  FreeAndNil(FHighCut);
  inherited;
end;

procedure TAdvancedGate.CalculateRangeThresholdFactor;
begin
 FRangeThresholdFactor := Power(FRangeFactor * FThreshold, FSoftKnee[1]);
end;

procedure TAdvancedGate.CalculateThresholdKneeFactor;
begin
 FThresholdKneeFactor := Power(FThreshold, FSoftKnee[1]);
end;

function TAdvancedGate.GetHighCut: Double;
begin
 result := FHighCut.Frequency;
end;

function TAdvancedGate.GetLowCut: Double;
begin
 result := FLowCut.Frequency;
end;

procedure TAdvancedGate.InputSample(const Input: Double);
begin
 inherited InputSample(FHighCut.ProcessSample(FLowCut.ProcessSample(Input)));
end;

procedure TAdvancedGate.KneeChanged;
begin
 inherited;
 CalculateRangeThresholdFactor;
 CalculateThresholdKneeFactor;
end;

procedure TAdvancedGate.RangeChanged;
begin
 inherited;
 CalculateRangeThresholdFactor;
end;

procedure TAdvancedGate.SampleRateChanged;
begin
 inherited;
 FLowCut.SampleRate := FSampleRate;
 FHighCut.SampleRate := FSampleRate;
end;

procedure TAdvancedGate.SetHighCut(const Value: Double);
begin
 if FHighCut.Frequency <> Value then
  begin
   FHighCut.Frequency := Value;
  end;
end;

procedure TAdvancedGate.SetLowCut(const Value: Double);
begin
 if FLowCut.Frequency <> Value then
  begin
   FLowCut.Frequency := Value;
  end;
end;

procedure TAdvancedGate.ThresholdChanged;
begin
 inherited;
 CalculateThresholdKneeFactor;
 CalculateRangeThresholdFactor;
end;

function TAdvancedGate.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  ScaledPeakLevel : Double;
begin
 ScaledPeakLevel := Power(PeakLevel, FSoftKnee[1]);
 result := Power((ScaledPeakLevel + FRangeThresholdFactor) / (ScaledPeakLevel + FThresholdKneeFactor), FSoftKnee[0]);
 if PeakLevel > FThreshold
  then FHoldSmplCnt := 0; // start hold phase
end;


{ TCustomCompressor }

procedure TCustomCompressor.CalculateMakeUpGain;
begin
 FMakeUpGain := dB_to_Amp(FMakeUpGain_dB);
end;

constructor TCustomCompressor.Create;
begin
  inherited;
  FMakeUpGain_dB := 0;
  FAutoMakeUp := False;
  CalculateMakeUpGain;
end;

procedure TCustomCompressor.ThresholdChanged;
begin
 inherited;
 CalculateMakeUpGain;
end;

procedure TCustomCompressor.RatioChanged;
begin
 inherited;
 CalculateMakeUpGain;
end;

procedure TCustomCompressor.AutoMakeUpChanged;
begin
 if FAutoMakeUp
  then CalculateMakeUpGain;
end;

procedure TCustomCompressor.MakeUpGainChanged;
begin
 if not FAutoMakeUp
  then CalculateMakeUpGain;
end;

procedure TCustomCompressor.SetAutoMakeUp(const Value: Boolean);
begin
 if FAutoMakeUp <> Value then
  begin
   FAutoMakeUp := Value;
   AutoMakeUpChanged;
  end;
end;

procedure TCustomCompressor.SetMakeUpGain_dB(const Value: Double);
begin
 if FMakeUpGain_dB <> Value then
  begin
   FMakeUpGain_dB := Value;
   MakeUpGainChanged;
  end;
end;

procedure TCustomCompressor.InputSample(const Input: Double);
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);
end;

function TCustomCompressor.ProcessSample(const Input: Double): Double;
begin
 InputSample(Input);
 result := GainSample(Input);
end;

{ TSimpleCompressor }

procedure  TSimpleCompressor.CalculateMakeUpGain;
var
  dbl : Double;
begin
 inherited;
 dbl := Power(FThreshold, 1 - FRatio);
 if FAutoMakeUp
  then FMakeUpGain[0] := 1 / dbl
  else FMakeUpGain[0] := dB_to_Amp(FMakeUpGain_dB);
 FMakeUpGain[1] := FMakeUpGain[0] * dbl;
end;

function TSimpleCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then result := FMakeUpGain[0]
  else result := FMakeUpGain[1] * Power(PeakLevel, FRatio - 1);
end;

{ TCustomKneeCompressor }

procedure TCustomKneeCompressor.KneeChanged;
begin
 // nothing here yet!
end;

{ TSoftKneeCompressor }

procedure TSoftKneeCompressor.CalculateMakeUpGain;
var
  dbl : Double;
begin
 inherited;
 dbl := Power(FThreshold, 1 - FRatio);
 if FAutoMakeUp
  then FMakeUpGain[0] := 1 / dbl
  else FMakeUpGain[0] := dB_to_Amp(FMakeUpGain_dB);
 FMakeUpGain[1] := FMakeUpGain[0] * dbl;
end;

procedure TSoftKneeCompressor.ThresholdChanged;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
end;

function TSoftKneeCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then result := FMakeUpGain[0]
  else result := FMakeUpGain[1] * Power(PeakLevel, FRatio - 1);
end;


{ TFastCompressor }

procedure TFastCompressor.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
end;

procedure TFastCompressor.RatioChanged;
begin
 inherited;
 FRatioFactor := CHalf32 * (1 / Ratio - 1);
 CalculateKneeFactor;
end;

procedure TFastCompressor.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else CalculateMakeUpGain;
end;

procedure TFastCompressor.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * FRatioFactor;
 FMakeUpGain_dB := FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) - Temp;
 CalculateMakeUpGain;
end;

procedure TFastCompressor.CalculateKneeFactor;
begin
 FKneeFactor := sqr(CdBtoAmpExpGain32 * FKnee_dB);
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TFastCompressor.ThresholdChanged;
begin
 inherited;
 FThrshlddB := Threshold_dB / CFactor2IndB32;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

function TFastCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := PeakLevel;
 result := FRatioFactor * (FastLog2ContinousError5(result) - FThrshlddB);
 result := FastPower2MinError3(result - FastSqrtBab2(sqr(result) + FKneeFactor) + FMkpdB);
end;

procedure TFastCompressor.CalculateMakeUpGain;
begin
 inherited;
 FMkpdB := FMakeUpGain_dB * CdBtoAmpExpGain32;
end;

function TFastCompressor.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := FRatioFactor * (InputLevel_dB - FThreshold_dB);
 result := Temp - FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

(*
function Diode(const Value: Single): Single; inline;
begin
 result := (abs(Value) + Value);
end;
*)

function TFastCompressor.ProcessSample(const Input: Double): Double;
var
  Temp : Single;
begin
 Temp := CDenorm32 + abs(Input);

 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FRatioFactor * (FastLog2ContinousError5(FPeak) - FThrshlddB);
 FGain := FastPower2MinError3(Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor) + FMkpdB);

 result := FGain * Input;
end;


{ TCustomFeedbackCompressor }

function TCustomFeedbackCompressor.GainSample(const Input: Double): Double;
begin
 result := FMakeUpGain * FGain * Input;
end;

procedure TCustomFeedbackCompressor.InputSample(const Input: Double);
begin
 if FPreviousAbsSample > FPeak
  then FPeak := FPeak + (FPreviousAbsSample - FPeak) * FAttackFactor
  else FPeak := FPreviousAbsSample + (FPeak - FPreviousAbsSample) * FReleaseFactor;

 FGain := TranslatePeakToGain(abs(FPeak));
 FPreviousAbsSample := abs(Input * FGain);
end;

{ TSimpleFeedbackCompressor }

procedure TSimpleFeedbackCompressor.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - exp( -ln2 / (FAttack * 0.001 * SampleRate * FRatioReciprocal));
end;

procedure TSimpleFeedbackCompressor.CalculateReleaseFactor;
begin
  if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / (FRelease * 0.001 * SampleRate * FRatioReciprocal));
end;

procedure TSimpleFeedbackCompressor.RatioChanged;
begin
 inherited;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

function TSimpleFeedbackCompressor.ProcessSample(const Input: Double): Double;
begin
 if FPreviousAbsSample > FPeak
  then FPeak := FPeak + (FPreviousAbsSample - FPeak) * FAttackFactor
  else FPeak := FPreviousAbsSample + (FPeak - FPreviousAbsSample) * FReleaseFactor;

 FPreviousAbsSample := Input * TranslatePeakToGain(abs(FPeak));

 result := FMakeUpGains[0] * FPreviousAbsSample;
 FPreviousAbsSample := abs(FPreviousAbsSample);
end;

procedure TSimpleFeedbackCompressor.CalculateMakeUpGain;
var
  dbl : Double;
begin
 dbl := Power(FThreshold, FRatio - 1);
 if FAutoMakeUp
  then FMakeUpGains[0] := dbl
  else FMakeUpGains[0] := dB_to_Amp(FMakeUpGain_dB);
 FMakeUpGains[1] := Power(FThreshold, (1 - FRatio) * FRatioReciprocal);
end;

function TSimpleFeedbackCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then result := 1
  else result := FMakeUpGains[1] * Power(PeakLevel, 1 - FRatioReciprocal);
end;

function TSimpleFeedbackCompressor.CharacteristicCurve(const InputLevel: Double): Double;
begin
 result := FMakeUpGains[0] * Power(TranslatePeakToGain(abs(InputLevel)), FRatio) * InputLevel;
end;

{ TSoftKneeFeedbackCompressor }

function TSoftKneeFeedbackCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
(*
var
  a, b, c : Double;
*)
begin
(*
 a := PeakLevel/FThreshold;
 result := 1 + 0.5 * (1 + abs(a - 1) / (a - 1)) * (FMakeUpGain[1] * Power(PeakLevel, 1 - FRatioReciprocal) - 1);
*)

(*
 b := PeakLevel / FThreshold;
 a := 0.5 * (abs(b - 1) / (b - 1) + 1);
 result := 1 + a * (Power(b, 1 - FRatioReciprocal) - 1);
*)

(*
 b := PeakLevel / FThreshold;
 a := Power(b, 1 - FRatioReciprocal);
 result := 1 + 0.5 * (abs(a - 1) + (a - 1));
*)

 if PeakLevel < FThreshold
  then result := 1
  else result := FMakeUpGains[1] * Power(PeakLevel, 1 - FRatioReciprocal);

(*
 a := Power(FPeak, FRatioReciprocal);
 result := (1 - a / (a + 3)) * FThresholdReciprocal;
*)
end;

function TSoftKneeFeedbackCompressor.ProcessSample(const Input: Double): Double;
begin
 if FPreviousAbsSample > FPeak
  then FPeak := FPeak + (FPreviousAbsSample - FPeak) * FAttackFactor
  else FPeak := FPreviousAbsSample + (FPeak - FPreviousAbsSample) * FReleaseFactor;

 FPreviousAbsSample := Input * TranslatePeakToGain(abs(FPeak));

 result := FMakeUpGains[0] * FPreviousAbsSample;
 FPreviousAbsSample := abs(FPreviousAbsSample);
end;

{ TSimpleRMSCompressor }

constructor TSimpleRMSCompressor.Create;
begin
 inherited;
 FCurrentRMS := 0;
 FRMSTime := 10;
 UpdateRMSBuffer;
end;

procedure TSimpleRMSCompressor.InputSample(const Input: Double);
begin
 // calculate RMS stuff
 FCurrentRMS := FCurrentRMS - FRMSBuffer[FRMSPos] + sqr(Input);
 FRMSBuffer[FRMSPos] := sqr(Input);
 if FRMSPos < FRMSSize then inc(FRMSPos) else FRMSPos := 0;

 inherited InputSample(Sqrt(FCurrentRMS * FRMSFactor));
end;

function TSimpleRMSCompressor.ProcessSample(const Input: Double): Double;
var
  Temp: Double;
begin
 // calculate RMS stuff
 FCurrentRMS := FCurrentRMS - FRMSBuffer[FRMSPos] + sqr(Input);
 FRMSBuffer[FRMSPos] := sqr(Input);
 if FRMSPos < FRMSSize then inc(FRMSPos) else FRMSPos := 0;

 // time constants
 Temp := Sqrt(FCurrentRMS * FRMSFactor);
 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);

 Result := FGain * Input;
end;

procedure TSimpleRMSCompressor.UpdateRMSBuffer;
var
  i : Integer;
begin
 i := Round(FSampleRate * 0.001 * FRMSTime);
 SetLength(FRMSBuffer, i);
 if i > FRMSSize then FillChar(FRMSBuffer[FRMSSize], (i - FRMSSize) * SizeOf(Double), 0);
 FRMSSize := i; FRMSFactor := 1 / FRMSSize;
 if FRMSPos > FRMSSize then FRMSPos := 0;
end;

procedure TSimpleRMSCompressor.SampleRateChanged;
begin
 inherited;
 UpdateRMSBuffer;
end;

procedure TSimpleRMSCompressor.SetRMSTime(const Value: Double);
begin
 if FRMSTime <> Value then
  begin
   FRMSTime := Value;
   UpdateRMSBuffer;
  end;
end;

{ TCompressor }

constructor TCompressor.Create;
begin
 inherited;
 FLowCut  := TButterworthHP.Create;
 FHighCut := TButterworthLP.Create;
 FLowCut.Frequency := 20;
 FHighCut.Frequency := 20000;
end;

destructor TCompressor.Destroy;
begin
 FreeAndNil(FLowCut);
 FreeAndNil(FHighCut);
 inherited;
end;

function TCompressor.GetHighCut: Double;
begin
 result := FHighCut.Frequency;
end;

function TCompressor.GetLowCut: Double;
begin
 result := FLowCut.Frequency;
end;

function TCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
(*
var
  Soft : Double;
*)
begin
// Soft := FThreshold / (PeakLevel + FThreshold);

 if PeakLevel < FThreshold
  then result := FMakeUpGain[0]
  else result := FMakeUpGain[1] * Power(PeakLevel, FRatio - 1);

// result := sqrt(Soft * result);
end;

procedure TCompressor.InputSample(const Input: Double);
begin
 inherited InputSample(FHighCut.ProcessSample(FLowCut.ProcessSample(Input)));
end;

function TCompressor.ProcessSample(const Input: Double): Double;
var
  Temp: Double;
begin
 // calculate RMS stuff
 FCurrentRMS := FCurrentRMS - FRMSBuffer[FRMSPos] + sqr(Input);
 FRMSBuffer[FRMSPos] := sqr(Input);
 if FRMSPos < FRMSSize then inc(FRMSPos) else FRMSPos := 0;

 // time constants
 Temp := Sqrt(FCurrentRMS * FRMSFactor);
 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 FGain := TranslatePeakToGain(FPeak);

 Result := FGain * Input;
end;

procedure TCompressor.SetHighCut(const Value: Double);
begin
 if FHighCut.Frequency <> Value
  then FHighCut.Frequency := Value;
end;

procedure TCompressor.SetLowCut(const Value: Double);
begin
 if FLowCut.Frequency <> Value
  then FLowCut.Frequency := Value;
end;

(*
{ TBrickwallLimiter }

function TBrickwallLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then result := 1
  else result := FThresholdRatioFactor * Power(PeakLevel, FRatio - 1);
end;

function TBrickwallLimiter.ProcessSample(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;

 FGain := CharacteristicCurve(FPeak);

 result := FGain * Input;
end;
{$ELSE}
asm
 fld Input                        // Input
 fabs                             // abs(Input)
 fld [self.FPeak].Double          // FPeak, abs(Input)
 mov edx, eax                     // edx = self
 fcom st(1)                       // FPeak, abs(Input)
 fstsw ax                         // ax = FPU Status Word
 sahf                             // ax -> EFLAGS register
 jbe @Attack                      // goto Attack
@Release:
 fsub st(0), st(1)                // FPeak - abs(Input), abs(Input)
 fmul [edx.FReleaseFactor].Double   // (FPeak - abs(Input)) * FReleaseFactor, abs(Input)
 faddp                            // (FPeak - abs(Input)) * FReleaseFactor + abs(Input)
 fst [self.FPeak].Double          // FPeak := (FPeak - abs(Input)) * FReleaseFactor + abs(Input)
 jmp @EndAttack
@Attack:
 fxch                             // abs(Input), FPeak
 fsub st(0), st(1)                // abs(Input) - FPeak, FPeak
 fmul [edx.FAttackFactor].Double  // (abs(Input) - FPeak) * FAttackFactor, FPeak
 faddp                            // (abs(Input) - FPeak) * FAttackFactor + FPeak
 fst  [self.FPeak].Double         // FPeak := (abs(Input) - FPeak) * FAttackFactor + FPeak
@EndAttack:

 fld [edx.FThreshold].Double      // FThreshold, FPeak
 fcom st(1)                       // FThreshold, FPeak
 fstsw ax                         // ax = FPU Status Word
 sahf                             // ax -> EFLAGS register
 fstp st(0)                       // FPeak
 jbe @Limit                       // goto Limit
 fstp st(0)                       // --
 fld Input                        // Input
 jmp @Exit
@Limit:

 fld [edx.FRatio].Double          // FRatio, FPeak
 fld1                             // 1, FRatio, FPeak
 fsubp                            // FRatio - 1, FPeak
 fxch
 fldln2                           // {
 fxch                             //
 fyl2x                            //
 fxch                             //
 fmulp   st(1), st                //  P
 fldl2e                           //  O
 fmulp   st(1), st                //  W
 fld     st(0)                    //  E
 frndint                          //  R
 fsub    st(1), st                //
 fxch    st(1)                    //
 f2xm1                            //
 fld1                             //
 faddp   st(1), st                //
 fscale                           // }
 fstp    st(1)

 fmul [edx.FThresholdRatioFactor].Double // FThresholdRatioFactor * Power(FPeak, FRatio - 1)
 fmul Input                              // Input * FThresholdRatioFactor * Power(FPeak, FRatio - 1)


@Exit:
end;
{$ENDIF}

procedure TBrickwallLimiter.ThresholdChanged;
begin
 inherited;
 CalculateThresholdRatioFactor;
end;

procedure TBrickwallLimiter.RatioChanged;
begin
 inherited;
 CalculateThresholdRatioFactor;
end;

procedure TBrickwallLimiter.CalculateThresholdRatioFactor;
begin
 FThresholdRatioFactor := Power(FThreshold, 1 - FRatio);
end;

{ TSoftKneeLimiter }

constructor TSoftKneeLimiter.Create;
begin
 inherited;
 FSoftKnee := 1;
end;

function TSoftKneeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  InternalRatio, Knee : Double;
begin
 Knee := 0.5 *(1 + Tanh2c(FSoftKnee * log10(PeakLevel / FThreshold)));
 InternalRatio := 1 + Knee * (FRatio - 1);
 result := Power(FThreshold, 1 - InternalRatio) * Power(PeakLevel, InternalRatio - 1);
end;

function TSoftKneeLimiter.ProcessSample(const Input: Double): Double;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FReleaseFactor;
 FGain := CharacteristicCurve(FPeak);
 result := FGain * Input;
end;

procedure TSoftKneeLimiter.SetSoftKnee(const Value: Double);
begin
 if FSoftKnee <> Value then
  begin
   FSoftKnee := Value;
  end;
end;
*)

(*
{ TSoftKneeFeedbackLimiter }

constructor TSoftKneeFeedbackLimiter.Create;
begin
 inherited;
 FGain := 1;
 FOversample := Round(1E5 / Samplerate + 0.5);
 FFilter := TButterworthLowCut.Create;
 FFilter.Frequency := 13.8;
 FFilter.Order := 1;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TSoftKneeFeedbackLimiter.CalculateAttackFactor;
begin
 if FAttack = 0
  then FAttackFactor := 0
  else FAttackFactor := exp( -ln2 / (FOversample * FAttack * 0.001 * SampleRate));
 FAttackFac2 := exp( -ln2 / (0.48 * SampleRate));
end;

procedure TSoftKneeFeedbackLimiter.CalculateReleaseFactor;
begin
 if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / ( {FOversample *} FRelease * 0.001 * SampleRate));
 FReleaseFac2 := exp( -ln2 / (0.98 * SampleRate));
end;

function TSoftKneeFeedbackLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 // yet to do
 result := 1;
end;

function TSoftKneeFeedbackLimiter.ProcessSample(const Input: Double): Double;
var
  InternalRatio    : Double;
  PeakdB           : Double;
begin
 Input := FFilter.ProcessSample(const Input);
{

// threshold = -13.4 .. - 13

 result := FGain * Input;
 if abs(result)>FPeak2
  then FPeak2 := abs(result) + (FPeak2 - abs(result)) * FAttackFac2
  else FPeak2 := abs(result) + (FPeak2 - abs(result)) * FReleaseFac2;

 if abs(result)>FPeak
  then
   begin
    FPeak := abs(result) + (FPeak - abs(result)) * FAttackFactor;
    PeakdB := Amp_to_dB(abs(0.3 * FPeak2 + 0.7 * FPeak + 1E-32));
    InternalRatio := - (3 + 3 * (PeakdB - FThreshold_dB - 0.5) / (abs(PeakdB - FThreshold_dB) + 1));
    FGain := dB_to_Amp(PeakdB * InternalRatio - FThreshold_dB * InternalRatio);
    for OversampleCount := 1 to FOversample - 1 do
     begin
      result := FGain * Input;
      FPeak := abs(result) + (FPeak - abs(result)) * FAttackFactor;
      PeakdB := Amp_to_dB(abs(0.3 * FPeak2 + 0.7 * FPeak + 1E-32));
      InternalRatio := - (3 + 3 * (PeakdB - FThreshold_dB - 0.5) / (abs(PeakdB - FThreshold_dB) + 1));
      FGain := dB_to_Amp(PeakdB * InternalRatio - FThreshold_dB * InternalRatio);
     end
   end
  else
   begin
    FPeak := abs(result) + (FPeak - abs(result)) * FReleaseFactor;
    PeakdB := Amp_to_dB(abs(0.3 * FPeak2 + 0.7 * FPeak + 1E-32));
    InternalRatio := - (3 + 3 * (PeakdB - FThreshold_dB - 0.5) / (abs(PeakdB - FThreshold_dB) + 1));
    FGain := dB_to_Amp(PeakdB * InternalRatio - FThreshold_dB * InternalRatio);
   end;

}

 result := FGain * Input;
{
 if abs(result)>FPeak
  then FPeak := abs(result) + (FPeak - abs(result)) * FAttackFactor
  else FPeak := abs(result) + (FPeak - abs(result)) * FReleaseFactor;
}
 FPeak := FReleaseFactor * FPeak;
 if abs(result) > FPeak
  then FPeak := abs(result) + (FPeak - abs(result)) * FAttackFactor;

 FPeak2 := FReleaseFac2 * FPeak2;
 if abs(result) > FPeak2
  then FPeak2 := abs(result) + (FPeak2 - abs(result)) * FAttackFac2;

 PeakdB := Amp_to_dB(abs(0.3 * FPeak + 0.7 * FPeak2 + 1E-32));
// InternalRatio := - (3 + 3 * (PeakdB - FThreshold_dB - 0.5) / (abs(PeakdB - FThreshold_dB) + 1));
 InternalRatio := - (2.6 + 2.6 * (PeakdB - FThreshold_dB - 1.5) / (abs(PeakdB - FThreshold_dB) + 2));
 FGain := dB_to_Amp(PeakdB * InternalRatio - FThreshold_dB * InternalRatio);
end;

procedure TSoftKneeFeedbackLimiter.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := FSampleRate;
 FOversample := Round(1E5 / FSampleRate + 1.5);
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;
*)

end.
