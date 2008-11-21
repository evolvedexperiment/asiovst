unit SEDynamicsModule;

interface

uses
  DAV_Common, DAV_DSPDynamics, DAV_SECommon, DAV_SEModule;

type
  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomDynamicsSEModule                                                 //
  //  -----------------------                                                 //
  //                                                                          //
  //  Base class for all dynamics, simple features one input and one output.  //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomDynamicsSEModule = class(TSEModuleBase)
  protected
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    procedure Open; override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleDirectGateSEModule                                         //
  //  -------------------------------                                         //
  //                                                                          //
  //  Base class for the simple direct gate.                                  //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleDirectGateSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor: TSimpleDirectGate;
    procedure Open; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TSimpleDirectGateStaticSEModule = class(TCustomSimpleDirectGateSEModule)
  protected
    FThreshold : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSimpleDirectGateAutomatableSEModule = class(TCustomSimpleDirectGateSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftDirectGateSEModule                                           //
  //  -----------------------------                                           //
  //                                                                          //
  //  Base class for the soft direct gate.                                    //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftDirectGateSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor: TSoftDirectGate;
    procedure Open; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TSoftDirectGateStaticSEModule = class(TCustomSoftDirectGateSEModule)
  protected
    FThreshold : Single;
    FKnee_dB   : Single;      
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSoftDirectGateAutomatableSEModule = class(TCustomSoftDirectGateSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override; 
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomBrickwallLimiterSEModule                                         //
  //  -------------------------------                                         //
  //                                                                          //
  //  Base class for all brickwall limiters.                                  //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomBrickwallLimiterSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor : TCustomBrickwallLimiter;
    FAutoMakeUp      : Boolean;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleBrickwallLimiterSEModule                                   //
  //  -------------------------------------                                   //
  //                                                                          //
  //  Base class for the simple brickwall limiter.                            //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleBrickwallLimiterSEModule = class(TCustomBrickwallLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TBrickwallLimiterStaticSEModule = class(TCustomSimpleBrickwallLimiterSEModule)
  protected
    FThreshold : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TBrickwallLimiterAutomatableSEModule = class(TCustomSimpleBrickwallLimiterSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftBrickwallLimiterSEModule                                     //
  //  -----------------------------------                                     //
  //                                                                          //
  //  Base class for the soft brickwall limiter.                              //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftBrickwallLimiterSEModule = class(TCustomBrickwallLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TBrickwallSoftLimiterStaticSEModule = class(TCustomSoftBrickwallLimiterSEModule)
  protected
    FThreshold : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TBrickwallSoftLimiterAutomatableSEModule = class(TCustomSoftBrickwallLimiterSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleSoftBrickwallLimiterSEModule                               //
  //  -----------------------------------------                               //
  //                                                                          //
  //  Base class for the simple soft brickwall limiter.                       //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleSoftBrickwallLimiterSEModule = class(TCustomBrickwallLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TBrickwallSimpleSoftLimiterStaticSEModule = class(TCustomSimpleSoftBrickwallLimiterSEModule)
  protected
    FThreshold : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TBrickwallSimpleSoftLimiterAutomatableSEModule = class(TCustomSimpleSoftBrickwallLimiterSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomRangeGateSEModule                                                //
  //  ------------------------                                                //
  //                                                                          //
  //  Base class for the classic gate.                                        //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomRangeGateSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor: TClassicSoftRangeGate;
    procedure Open; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TRangeGateStaticSEModule = class(TCustomRangeGateSEModule)
  protected
    FThreshold : Single;
    FKnee      : Single;
    FRange     : Single;
    FAttack    : Single;
    FRelease   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TRangeGateAutomatableSEModule = class(TCustomRangeGateSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee      : PDAVSingleFixedArray;
    FRange     : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomClassicGateSEModule                                              //
  //  --------------------------                                              //
  //                                                                          //
  //  Base class for the classic gate.                                        //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomClassicGateSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor: TClassicGate;
    procedure Open; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TClassicGateStaticSEModule = class(TCustomClassicGateSEModule)
  protected
    FThreshold : Single;
    FKnee      : Single;
    FAttack    : Single;
    FRelease   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TClassicGateAutomatableSEModule = class(TCustomClassicGateSEModule)
  private
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee      : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftClassicGateSEModule                                           //
  //  -----------------------------                                           //
  //                                                                          //
  //  Base class for the soft direct gate.                                    //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftClassicGateSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor: TClassicSoftKneeGate;
    procedure Open; override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TSoftClassicGateStaticSEModule = class(TCustomSoftClassicGateSEModule)
  protected
    FThreshold : Single;
    FKnee_dB   : Single;      
    FAttack    : Single;
    FRelease   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSoftClassicGateAutomatableSEModule = class(TCustomSoftClassicGateSEModule)
  protected
    FThreshold : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomLimiterSEModule                                                  //
  //  ----------------------                                                  //
  //                                                                          //
  //  Base class for all time constant limiters.                              //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomLimiterSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor : TCustomLimiter;
    FAutoMakeUp      : Boolean;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleLimiterSEModule                                            //
  //  ----------------------------                                            //
  //                                                                          //
  //  Base class for the simple limiter.                                      //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleLimiterSEModule = class(TCustomLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TLimiterStaticSEModule = class(TCustomSimpleLimiterSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TLimiterAutomatableSEModule = class(TCustomSimpleLimiterSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftLimiterSEModule                                              //
  //  --------------------------                                              //
  //                                                                          //
  //  Base class for the soft  limiter.                                       //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftLimiterSEModule = class(TCustomLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TSoftLimiterStaticSEModule = class(TCustomSoftLimiterSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSoftLimiterAutomatableSEModule = class(TCustomSoftLimiterSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;


  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleSoftLimiterSEModule                                        //
  //  --------------------------------                                        //
  //                                                                          //
  //  Base class for the simple soft limiter.                                 //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleSoftLimiterSEModule = class(TCustomLimiterSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TSimpleSoftLimiterStaticSEModule = class(TCustomSimpleSoftLimiterSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FKnee_dB   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSimpleSoftLimiterAutomatableSEModule = class(TCustomSimpleSoftLimiterSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FKnee_dB   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomCompressorSEModule                                               //
  //  -------------------------                                               //
  //                                                                          //
  //  Base class for all time constant compressors.                           //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomCompressorSEModule = class(TCustomDynamicsSEModule)
  protected
    FDynamicProcesor : TCustomCompressor;
    FAutoMakeUp      : Boolean;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    destructor Destroy; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSimpleCompressorSEModule                                         //
  //  -------------------------------                                         //
  //                                                                          //
  //  Base class for the Simple Soft Compressor.                              //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSimpleCompressorSEModule = class(TCustomCompressorSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TSimpleCompressorStaticSEModule = class(TCustomSimpleCompressorSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FRatio     : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSimpleCompressorAutomatableSEModule = class(TCustomSimpleCompressorSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FRatio     : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomSoftKneeCompressorSEModule                                         //
  //  -------------------------------                                         //
  //                                                                          //
  //  Base class for the SoftKnee Soft Compressor.                              //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomSoftKneeCompressorSEModule = class(TCustomCompressorSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TSoftKneeCompressorStaticSEModule = class(TCustomSoftKneeCompressorSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FRatio     : Single;
    FKnee      : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TSoftKneeCompressorAutomatableSEModule = class(TCustomSoftKneeCompressorSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FRatio     : PDAVSingleFixedArray;
    FKnee      : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TCustomRMSCompressorSEModule                                            //
  //  ----------------------------                                            //
  //                                                                          //
  //  Base class for the RMS Soft Compressor.                                 //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TCustomRMSCompressorSEModule = class(TCustomCompressorSEModule)
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
  end;

  TRMSCompressorStaticSEModule = class(TCustomRMSCompressorSEModule)
  protected
    FThreshold : Single;
    FAttack    : Single;
    FRelease   : Single;
    FRatio     : Single;
    FRMSTime   : Single;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
  end;

  TRMSCompressorAutomatableSEModule = class(TCustomRMSCompressorSEModule)
  private
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
  protected
    FThreshold : PDAVSingleFixedArray;
    FAttack    : PDAVSingleFixedArray;
    FRelease   : PDAVSingleFixedArray;
    FRatio     : PDAVSingleFixedArray;
    FRMSTime   : PDAVSingleFixedArray;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

implementation

uses
  SysUtils;

procedure TCustomDynamicsSEModule.Open;
begin
 inherited Open;

 // let 'downstream' modules know audio data is coming
 Pin[1].TransmitStatusChange(SampleClock, stRun);
end;

// describe your module
class procedure TCustomDynamicsSEModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomDynamicsSEModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Input';
       VariableAddress := @FInputBuffer;
       Direction       := drIn;
       Datatype        := dtFSample;
       DefaultValue    := '0';
       Flags           := [iofLinearInput];
      end;

  // typical output plug
  1: with Properties^ do
      begin
       Name            := 'Output';
       VariableAddress := @FOutputBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;

  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TCustomSimpleDirectGateSEModule }

constructor TCustomSimpleDirectGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSimpleDirectGate.Create;
end;

destructor TCustomSimpleDirectGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomSimpleDirectGateSEModule.Open;
begin
 inherited;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TCustomSimpleDirectGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TSimpleDirectGateStaticSEModule }

constructor TSimpleDirectGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
end;

class procedure TSimpleDirectGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Direct Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Direct Gate (Static)';
  end;
end;

function TSimpleDirectGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSimpleDirectGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.Threshold_dB := FThreshold;
 end;
 inherited;
end;

procedure TSimpleDirectGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TSimpleDirectGateAutomatableSEModule }

class procedure TSimpleDirectGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Direct Gate (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Direct Gate (Automatable)';
  end;
end;

function TSimpleDirectGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
 end;
end;

procedure TSimpleDirectGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case CurrentPin.PinID of
  2: if (Pin[2].Status <> stRun)
      then OnProcess := SubProcessStatic
      else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TSimpleDirectGateAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Thres  : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres  := PDAVSingleFixedArray(@FThreshold[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

procedure TSimpleDirectGateAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TCustomSoftDirectGateSEModule }

constructor TCustomSoftDirectGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSoftDirectGate.Create;
end;

destructor TCustomSoftDirectGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomSoftDirectGateSEModule.Open;
begin
 inherited;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TCustomSoftDirectGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TSoftDirectGateStaticSEModule }

constructor TSoftDirectGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FKnee_dB := 1;
end;

class procedure TSoftDirectGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Soft Direct Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Soft Direct Gate (Static)';
  end;
end;

function TSoftDirectGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSoftDirectGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.Threshold_dB := FThreshold;
  3: FDynamicProcesor.Knee_dB := FKnee_dB;
 end;
 inherited;
end;

procedure TSoftDirectGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TSoftDirectGateAutomatableSEModule }

class procedure TSoftDirectGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Soft Direct Gate (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Soft Direct Gate (Automatable)';
  end;
end;

function TSoftDirectGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
 end;
end;

procedure TSoftDirectGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case CurrentPin.PinID of
  2: if (Pin[2].Status <> stRun) and (Pin[3].Status <> stRun)
      then OnProcess := SubProcessStatic
      else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TSoftDirectGateAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Thres  : PDAVSingleFixedArray;
  Knee   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres  := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Knee   := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Knee_dB := Knee[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

procedure TSoftDirectGateAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TCustomBrickwallLimiterSEModule }

destructor TCustomBrickwallLimiterSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomBrickwallLimiterSEModule.Open;
begin
 inherited;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

function TCustomBrickwallLimiterSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Auto Make Up';
       VariableAddress := @FAutoMakeUp;
       Direction       := drParameter;
       DataType        := dtBoolean;
       DefaultValue    := '0';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TCustomBrickwallLimiterSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.AutoMakeUp := FAutoMakeUp;
 end;
 inherited;
end;

procedure TCustomBrickwallLimiterSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TCustomSimpleBrickwallLimiterSEModule }

constructor TCustomSimpleBrickwallLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TBrickwallLimiter.Create;
end;

{ TBrickwallLimiterStaticSEModule }

constructor TBrickwallLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
end;

class procedure TBrickwallLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Brickwall Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Brickwall Limiter (Static)';
  end;
end;

function TBrickwallLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TBrickwallLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
 end;
 inherited;
end;

procedure TBrickwallLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TBrickwallLimiterAutomatableSEModule }

class procedure TBrickwallLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Brickwall Limiter (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Brickwall Limiter (Automatable)';
  end;
end;

function TBrickwallLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
 end;
end;

procedure TBrickwallLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case CurrentPin.PinID of
  3: if (Pin[3].Status <> stRun)
      then OnProcess := SubProcessStatic
      else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TBrickwallLimiterAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Thres  : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres  := PDAVSingleFixedArray(@FThreshold[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

procedure TBrickwallLimiterAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TCustomSoftBrickwallLimiterSEModule }

constructor TCustomSoftBrickwallLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSoftBrickwallLimiter.Create;
end;

{ TBrickwallSoftLimiterStaticSEModule }

constructor TBrickwallSoftLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FKnee_dB := 1;
end;

class procedure TBrickwallSoftLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Brickwall Soft Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Brickwall Soft Limiter (Static)';
  end;
end;

function TBrickwallSoftLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TBrickwallSoftLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Knee_dB := FKnee_dB;
 end;
 inherited;
end;

procedure TBrickwallSoftLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TBrickwallSoftLimiterAutomatableSEModule }

class procedure TBrickwallSoftLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Brickwall Soft Limiter (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Brickwall Soft Limiter (Automatable)';
  end;
end;

function TBrickwallSoftLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
 end;
end;

procedure TBrickwallSoftLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case CurrentPin.PinID of
  3..4: if (Pin[3].Status <> stRun) and (Pin[4].Status <> stRun)
         then OnProcess := SubProcessStatic
         else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TBrickwallSoftLimiterAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Thres  : PDAVSingleFixedArray;
  Knee   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres  := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Knee   := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Knee_dB := Knee[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

procedure TBrickwallSoftLimiterAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TCustomSimpleSoftBrickwallLimiterSEModule }

constructor TCustomSimpleSoftBrickwallLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSimpleSoftBrickwallLimiter.Create;
end;

{ TBrickwallSimpleSoftLimiterStaticSEModule }

constructor TBrickwallSimpleSoftLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FKnee_dB := 1;
end;

class procedure TBrickwallSimpleSoftLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Brickwall Simple Soft Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Brickwall Simple Soft Limiter (Static)';
  end;
end;

function TBrickwallSimpleSoftLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TBrickwallSimpleSoftLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Knee_dB := FKnee_dB;
 end;
 inherited;
end;

procedure TBrickwallSimpleSoftLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TBrickwallSimpleSoftLimiterAutomatableSEModule }

class procedure TBrickwallSimpleSoftLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Brickwall Simple Soft Limiter (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Brickwall Simple Soft Limiter (Automatable)';
  end;
end;

function TBrickwallSimpleSoftLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
 end;
end;

procedure TBrickwallSimpleSoftLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case CurrentPin.PinID of
  3..4: if (Pin[3].Status <> stRun) and (Pin[4].Status <> stRun)
         then OnProcess := SubProcessStatic
         else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TBrickwallSimpleSoftLimiterAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Thres  : PDAVSingleFixedArray;
  Knee   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres  := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Knee   := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Knee_dB := Knee[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

procedure TBrickwallSimpleSoftLimiterAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;


{ TCustomRangeGateSEModule }

constructor TCustomRangeGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TClassicSoftRangeGate.Create;
end;

destructor TCustomRangeGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomRangeGateSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

procedure TCustomRangeGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TRangeGateStaticSEModule }

constructor TRangeGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FRange     := -10;
 FAttack    :=  10;
 FRelease   := 100;
end;

class procedure TRangeGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Range Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Range Gate (Static)';
  end;
end;

function TRangeGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Range [dB]';
       VariableAddress := @FRange;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TRangeGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.Threshold_dB := FThreshold;
  3: FDynamicProcesor.Range_dB     := FRange;
  4: FDynamicProcesor.Attack       := FAttack;
  5: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TRangeGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TRangeGateAutomatableSEModule }

class procedure TRangeGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Range Gate (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Range Gate (Automatable)';
  end;
end;

function TRangeGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Range [dB]';
       VariableAddress := @FRange;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

procedure TRangeGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case CurrentPin.PinID of
  2..5: if (Pin[2].Status <> stRun) and
           (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun)
         then OnProcess := SubProcessStatic
         else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TRangeGateAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Range   : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Range   := PDAVSingleFixedArray(@FRange[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Range_dB := Range[Sample];
   FDynamicProcesor.Attack := Attack[Sample];
   FDynamicProcesor.Release := Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

procedure TRangeGateAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TCustomClassicGateSEModule }

constructor TCustomClassicGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TClassicGate.Create;
end;

destructor TCustomClassicGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomClassicGateSEModule.Open;
begin
 inherited;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TCustomClassicGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TClassicGateStaticSEModule }

constructor TClassicGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRelease   := 100;
end;

class procedure TClassicGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Classic Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Classic Gate (Static)';
  end;
end;

function TClassicGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TClassicGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.Threshold_dB := FThreshold;
  3: FDynamicProcesor.Attack       := FAttack;
  4: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TClassicGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TClassicGateAutomatableSEModule }

class procedure TClassicGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Classic Gate (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Classic Gate (Automatable)';
  end;
end;

function TClassicGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

procedure TClassicGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case CurrentPin.PinID of
  2..4: if (Pin[2].Status <> stRun) and
           (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun)
         then OnProcess := SubProcessStatic
         else OnProcess := SubProcess;
 end;
 inherited;
end;

procedure TClassicGateAutomatableSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Attack := Attack[Sample];
   FDynamicProcesor.Release := Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

procedure TClassicGateAutomatableSEModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TCustomSoftClassicGateSEModule }

constructor TCustomSoftClassicGateSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TClassicSoftKneeGate.Create;
end;

destructor TCustomSoftClassicGateSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomSoftClassicGateSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

procedure TCustomSoftClassicGateSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TSoftClassicGateStaticSEModule }

constructor TSoftClassicGateStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FKnee_dB   :=   1;
 FAttack    :=  10;
 FRelease   := 100;
end;

class procedure TSoftClassicGateStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Classic Soft Knee Gate (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Classic Soft Knee Gate (Static)';
  end;
end;

function TSoftClassicGateStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSoftClassicGateStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.Threshold_dB := FThreshold;
  3: FDynamicProcesor.Knee_dB      := FKnee_dB;
  4: FDynamicProcesor.Attack       := FAttack;
  5: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TSoftClassicGateStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TSoftClassicGateAutomatableSEModule }

class procedure TSoftClassicGateAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Classic Soft Knee Gate (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Classic Soft Knee Gate (Automatable)';
  end;
end;

function TSoftClassicGateAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  3: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

procedure TSoftClassicGateAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case CurrentPin.PinID of
  2..5: if (Pin[2].Status <> stRun) and
           (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSoftClassicGateAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Knee    : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Knee    := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Knee_dB := Knee[Sample];
   FDynamicProcesor.Attack := Attack[Sample];
   FDynamicProcesor.Release := Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

{ TCustomLimiterSEModule }

destructor TCustomLimiterSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomLimiterSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

function TCustomLimiterSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Auto Make Up';
       VariableAddress := @FAutoMakeUp;
       Direction       := drParameter;
       DataType        := dtBoolean;
       DefaultValue    := '0';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TCustomLimiterSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.AutoMakeUp := FAutoMakeUp;
 end;
 inherited;
end;

procedure TCustomLimiterSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TCustomSimpleLimiterSEModule }

constructor TCustomSimpleLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TLimiter.Create;
end;

{ TLimiterStaticSEModule }

constructor TLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRelease   := 100;
end;

class procedure TLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Limiter (Static)';
  end;
end;

function TLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Attack       := FAttack;
  5: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TLimiterAutomatableSEModule }

class procedure TLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Limiter (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Limiter (Automatable)';
  end;
end;

function TLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

procedure TLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  3..5: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TLimiterAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Attack := Attack[Sample];
   FDynamicProcesor.Release := Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

{ TCustomSoftLimiterSEModule }

constructor TCustomSoftLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSoftKneeLimiter.Create;
end;

{ TSoftLimiterStaticSEModule }

constructor TSoftLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRelease   := 100;
 FKnee_dB   :=   1;
end;

class procedure TSoftLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Soft Knee Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Soft Knee Limiter (Static)';
  end;
end;

function TSoftLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSoftLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Knee_dB := FKnee_dB;
  5: FDynamicProcesor.Attack := FAttack;
  6: FDynamicProcesor.Release := FRelease;
 end;
 inherited;
end;

procedure TSoftLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TSoftLimiterAutomatableSEModule }

class procedure TSoftLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Soft Knee Limiter (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Soft Knee Limiter (Automatable)';
  end;
end;

function TSoftLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

procedure TSoftLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case CurrentPin.PinID of
  3..6: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSoftLimiterAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Knee    : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Knee    := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Knee_dB := Knee[Sample];
   FDynamicProcesor.Attack := Attack[Sample];
   FDynamicProcesor.Release := Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

{ TCustomSimpleSoftLimiterSEModule }

constructor TCustomSimpleSoftLimiterSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSimpleSoftKneeLimiter.Create;
end;

{ TSimpleSoftLimiterStaticSEModule }

constructor TSimpleSoftLimiterStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRelease   := 100;
 FKnee_dB   :=   1;
end;

class procedure TSimpleSoftLimiterStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'SimpleSoft Knee Limiter (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV SimpleSoft Knee Limiter (Static)';
  end;
end;

function TSimpleSoftLimiterStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSimpleSoftLimiterStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Knee_dB := FKnee_dB;
  5: FDynamicProcesor.Attack := FAttack;
  6: FDynamicProcesor.Release := FRelease;
 end;
 inherited;
end;

procedure TSimpleSoftLimiterStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TSimpleSoftLimiterAutomatableSEModule }

class procedure TSimpleSoftLimiterAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Soft Knee Limiter (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Soft Knee Limiter (Automatable)';
  end;
end;

function TSimpleSoftLimiterAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee_dB;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

procedure TSimpleSoftLimiterAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case CurrentPin.PinID of
  3..6: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSimpleSoftLimiterAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Knee    : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Knee    := PDAVSingleFixedArray(@FKnee_dB[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Knee_dB := Knee[Sample];
   FDynamicProcesor.Attack := Attack[Sample];
   FDynamicProcesor.Release := Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

{ TCustomCompressorSEModule }

destructor TCustomCompressorSEModule.Destroy;
begin
 FreeAndNil(FDynamicProcesor);
 inherited;
end;

procedure TCustomCompressorSEModule.Open;
begin
 inherited;
 OnProcess := SubProcess;
end;

function TCustomCompressorSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  2: with Properties^ do
      begin
       Name            := 'Auto Make Up';
       VariableAddress := @FAutoMakeUp;
       Direction       := drParameter;
       DataType        := dtBoolean;
       DefaultValue    := '0';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TCustomCompressorSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  2: FDynamicProcesor.AutoMakeUp := FAutoMakeUp;
 end;
 inherited;
end;

procedure TCustomCompressorSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TCustomSimpleCompressorSEModule }

constructor TCustomSimpleCompressorSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSimpleCompressor.Create;
end;

{ TSimpleCompressorStaticSEModule }

constructor TSimpleCompressorStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRatio     :=   8;
 FRelease   := 100;
end;

class procedure TSimpleCompressorStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Compressor (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Compressor (Static)';
  end;
end;

function TSimpleCompressorStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '8';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSimpleCompressorStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Ratio        := FRatio;
  5: FDynamicProcesor.Attack       := FAttack;
  6: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TSimpleCompressorStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TSimpleCompressorAutomatableSEModule }

class procedure TSimpleCompressorAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Compressor (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Compressor (Automatable)';
  end;
end;

function TSimpleCompressorAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '8';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

procedure TSimpleCompressorAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  3..6: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSimpleCompressorAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Attack := Attack[Sample];
   FDynamicProcesor.Release := Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

{ TCustomSoftKneeCompressorSEModule }

constructor TCustomSoftKneeCompressorSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSoftKneeCompressor.Create;
end;

{ TSoftKneeCompressorStaticSEModule }

constructor TSoftKneeCompressorStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRatio     :=   8;
 FKnee      :=   1;
 FRelease   := 100;
end;

class procedure TSoftKneeCompressorStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Soft Knee Compressor (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Soft Knee Compressor (Static)';
  end;
end;

function TSoftKneeCompressorStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '8';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSoftKneeCompressorStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Ratio        := FRatio;
  5: FDynamicProcesor.Knee_dB      := FKnee;
  6: FDynamicProcesor.Attack       := FAttack;
  7: FDynamicProcesor.Release      := FRelease;
 end;
 inherited;
end;

procedure TSoftKneeCompressorStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TSoftKneeCompressorAutomatableSEModule }

class procedure TSoftKneeCompressorAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Soft Knee Compressor (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Soft Knee Compressor (Automatable)';
  end;
end;

function TSoftKneeCompressorAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '8';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'Knee [dB]';
       VariableAddress := @FKnee;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

procedure TSoftKneeCompressorAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  3..7: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun) and
           (Pin[7].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSoftKneeCompressorAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  Knee    : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 Knee    := PDAVSingleFixedArray(@FKnee[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   FDynamicProcesor.Knee_dB := Knee[Sample];
   FDynamicProcesor.Attack := Attack[Sample];
   FDynamicProcesor.Release := Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

{ TCustomRMSCompressorSEModule }

constructor TCustomRMSCompressorSEModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(AudioMaster, Reserved);
 FDynamicProcesor := TSimpleRMSCompressor.Create;
end;

{ TRMSCompressorStaticSEModule }

constructor TRMSCompressorStaticSEModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FThreshold := -10;
 FAttack    :=  10;
 FRatio     :=   8;
 FRMSTime   :=   1;
 FRelease   := 100;
end;

class procedure TRMSCompressorStaticSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'RMS Compressor (Static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV RMS Compressor (Static)';
  end;
end;

function TRMSCompressorStaticSEModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '8';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'RMS Time [ms]';
       VariableAddress := @FRMSTime;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drParameter;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TRMSCompressorStaticSEModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 // has user altered Dynamics time parameter?
 case CurrentPin.PinID of
  3: FDynamicProcesor.Threshold_dB := FThreshold;
  4: FDynamicProcesor.Ratio        := FRatio;
  6: FDynamicProcesor.Attack       := FAttack;
  7: FDynamicProcesor.Release      := FRelease;
  5: TSimpleRMSCompressor(FDynamicProcesor).RMSTime := FRMSTime;
 end;
 inherited;
end;

procedure TRMSCompressorStaticSEModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
end;

{ TRMSCompressorAutomatableSEModule }

class procedure TRMSCompressorAutomatableSEModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'RMS Compressor (Automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV RMS Compressor (Automatable)';
  end;
end;

function TRMSCompressorAutomatableSEModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case index of
  3: with Properties^ do
      begin
       Name            := 'Threshold [dB]';
       VariableAddress := @FThreshold;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '-10';
       result          := True;
      end;
  4: with Properties^ do
      begin
       Name            := 'Ratio (1:x)';
       VariableAddress := @FRatio;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '8';
       result          := True;
      end;
  5: with Properties^ do
      begin
       Name            := 'RMS time [ms]';
       VariableAddress := @FRMSTime;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '1';
       result          := True;
      end;
  6: with Properties^ do
      begin
       Name            := 'Attack [ms]';
       VariableAddress := @FAttack;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '10';
       result          := True;
      end;
  7: with Properties^ do
      begin
       Name            := 'Release [ms]';
       VariableAddress := @FRelease;
       Direction       := drIn;
       DataType        := dtSingle;
       DefaultValue    := '100';
       result          := True;
      end;
 end;
end;

procedure TRMSCompressorAutomatableSEModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  3..7: if (Pin[3].Status <> stRun) and
           (Pin[4].Status <> stRun) and
           (Pin[5].Status <> stRun) and
           (Pin[6].Status <> stRun) and
           (Pin[7].Status <> stRun)
         then OnProcess := SubProcess
         else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TRMSCompressorAutomatableSEModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Input   : PDAVSingleFixedArray;
  Output  : PDAVSingleFixedArray;
  Thres   : PDAVSingleFixedArray;
  RMSTime : PDAVSingleFixedArray;
  Attack  : PDAVSingleFixedArray;
  Release : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input   := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Output  := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Thres   := PDAVSingleFixedArray(@FThreshold[BufferOffset]);
 RMSTime := PDAVSingleFixedArray(@FRMSTime[BufferOffset]);
 Attack  := PDAVSingleFixedArray(@FAttack[BufferOffset]);
 Release := PDAVSingleFixedArray(@FRelease[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FDynamicProcesor.Threshold_dB := Thres[Sample];
   TSimpleRMSCompressor(FDynamicProcesor).RMSTime := RMSTime[Sample];
   FDynamicProcesor.Attack := Attack[Sample];
   FDynamicProcesor.Release := Release[Sample];
   Output^[Sample] := FDynamicProcesor.ProcessSample(Input[Sample])
  end;
end;

end.