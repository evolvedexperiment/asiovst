unit SETrigoMathModule;

interface

uses
  DAV_Types, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_DSPSineLFO;

type
  TSETrigoMathModuleClass = class of TCustomSETrigoMathModule;
  TCustomSETrigoMathModule = class(TSEModuleBase)
  protected
    procedure Open; override;
    class function MathFuncText: string; virtual; abstract;
    class function PrecisionText: string; virtual; abstract;
  public
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
  end;

  TCustomSETrigoMathTwoArgumentsFloatModule = class(TCustomSETrigoMathModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    class function PrecisionText: string; override;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TCustomSETrigoMathTwoArgumentsDoubleModule = class(TCustomSETrigoMathModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    class function PrecisionText: string; override;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEArcCosFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCosDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcSinFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcSinDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSESinFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSESinDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECosFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECosDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSETanFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSETanDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECotanFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECotanDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSESecantFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSESecantDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECosecantFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECosecantDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSESinhFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSESinhDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECoshFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECoshDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSETanhFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSETanhDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECotHFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECotHDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSESecHFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSESecHDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECscHFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSECscHDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCotFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCotDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcSecFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcSecDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCscFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCscDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCoshFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCoshDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcSinhFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcSinhDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcTanhFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcTanhDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCotHFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCotHDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcSecHFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcSecHDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCscHFloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSEArcCscHDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSELog10FloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSELog10HDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSELog2FloatModule = class(TCustomSETrigoMathTwoArgumentsFloatModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;

  TSELog2HDoubleModule = class(TCustomSETrigoMathTwoArgumentsDoubleModule)
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    class function MathFuncText: string; override;
  end;


implementation

uses
  Math, SysUtils;

{ TCustomSETrigoMathModule }

class procedure TCustomSETrigoMathModule.GetModuleProperties(
  Properties: PSEModuleProperties);
var
  Caption : string;
begin
 with Properties^ do
  begin
   Caption := MathFuncText + ' ' + PrecisionText;

   // describe the plugin, this is the name the end-user will see.
   Name := PChar(Caption);

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := PChar('DAV + ' + Caption);

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TCustomSETrigoMathModule.Open;
begin
 inherited Open;

 OnProcess := SubProcessStatic;

 // 'transmit' new output status to next module 'downstream'
 if assigned(Pin[0]) then Pin[0].TransmitStatusChange(SampleClock, stStatic);
 if assigned(Pin[1]) then Pin[1].TransmitStatusChange(SampleClock, stStatic);
end;

// The most important part, processing the audio
procedure TCustomSETrigoMathModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;


{ TCustomSETrigoMathTwoArgumentsFloatModule }

// describe the pins (plugs)
function TCustomSETrigoMathTwoArgumentsFloatModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Float';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Float';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

class function TCustomSETrigoMathTwoArgumentsFloatModule.PrecisionText: string;
begin
 result := 'Float';
end;


{ TCustomSETrigoMathTwoArgumentsDoubleModule }

// describe the pins (plugs)
function TCustomSETrigoMathTwoArgumentsDoubleModule.GetPinProperties(
  const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Double';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtDouble;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Double';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtDouble;
       DefaultValue    := '0';
      end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

class function TCustomSETrigoMathTwoArgumentsDoubleModule.PrecisionText: string;
begin
 result := 'Double';
end;


{ TSEArcCosFloatModule }

class function TSEArcCosFloatModule.MathFuncText: string;
begin
 result := 'ArcCos';
end;

procedure TSEArcCosFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcCos(FFloatIn);
end;


{ TSEArcCosDoubleModule }

class function TSEArcCosDoubleModule.MathFuncText: string;
begin
 result := 'ArcCos';
end;

procedure TSEArcCosDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := ArcCos(FDoubleIn);
end;


{ TSEArcSinFloatModule }

class function TSEArcSinFloatModule.MathFuncText: string;
begin
 result := 'ArcSin';
end;

procedure TSEArcSinFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcSin(FFloatIn);
end;

{ TSEArcSinDoubleModule }

class function TSEArcSinDoubleModule.MathFuncText: string;
begin
 result := 'ArcSin';
end;

procedure TSEArcSinDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := ArcSin(FDoubleIn);
end;

{ TSESinFloatModule }

class function TSESinFloatModule.MathFuncText: string;
begin
 result := 'Sin';
end;

procedure TSESinFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Sin(FFloatIn);
end;

{ TSESinDoubleModule }

class function TSESinDoubleModule.MathFuncText: string;
begin
 result := 'Sin';
end;

procedure TSESinDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Sin(FDoubleIn);
end;

{ TSECosFloatModule }

class function TSECosFloatModule.MathFuncText: string;
begin
 result := 'Cos';
end;

procedure TSECosFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Cos(FFloatIn);
end;

{ TSECosDoubleModule }

class function TSECosDoubleModule.MathFuncText: string;
begin
 result := 'Cos';
end;

procedure TSECosDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Cos(FDoubleIn);
end;

{ TSETanFloatModule }

class function TSETanFloatModule.MathFuncText: string;
begin
 result := 'Tan';
end;

procedure TSETanFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Tan(FFloatIn);
end;

{ TSETanDoubleModule }

class function TSETanDoubleModule.MathFuncText: string;
begin
 result := 'Tan';
end;

procedure TSETanDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Tan(FDoubleIn);
end;

{ TSECotanFloatModule }

class function TSECotanFloatModule.MathFuncText: string;
begin
 result := 'Cotan';
end;

procedure TSECotanFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Cotan(FFloatIn);
end;

{ TSECotanDoubleModule }

class function TSECotanDoubleModule.MathFuncText: string;
begin
 result := 'Cotan';
end;

procedure TSECotanDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Cotan(FDoubleIn);
end;

{ TSESecantFloatModule }

class function TSESecantFloatModule.MathFuncText: string;
begin
 result := 'Secant';
end;

procedure TSESecantFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Secant(FFloatIn);
end;

{ TSESecantDoubleModule }

class function TSESecantDoubleModule.MathFuncText: string;
begin
 result := 'Secant';
end;

procedure TSESecantDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Secant(FDoubleIn);
end;

{ TSECosecantFloatModule }

class function TSECosecantFloatModule.MathFuncText: string;
begin
 result := 'Cosecant';
end;

procedure TSECosecantFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Cosecant(FFloatIn);
end;

{ TSECosecantDoubleModule }

class function TSECosecantDoubleModule.MathFuncText: string;
begin
 result := 'Cosecant';
end;

procedure TSECosecantDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Cosecant(FDoubleIn);
end;

{ TSESinhFloatModule }

class function TSESinhFloatModule.MathFuncText: string;
begin
 result := 'Sinh';
end;

procedure TSESinhFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Sinh(FFloatIn);
end;

{ TSESinhDoubleModule }

class function TSESinhDoubleModule.MathFuncText: string;
begin
 result := 'Sinh';
end;

procedure TSESinhDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Sinh(FDoubleIn);
end;

{ TSECoshFloatModule }

class function TSECoshFloatModule.MathFuncText: string;
begin
 result := 'Cosh';
end;

procedure TSECoshFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Cosh(FFloatIn);
end;

{ TSECoshDoubleModule }

class function TSECoshDoubleModule.MathFuncText: string;
begin
 result := 'Cosh';
end;

procedure TSECoshDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Cosh(FDoubleIn);
end;

{ TSETanhFloatModule }

class function TSETanhFloatModule.MathFuncText: string;
begin
 result := 'Tanh';
end;

procedure TSETanhFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Tanh(FFloatIn);
end;

{ TSETanhDoubleModule }

class function TSETanhDoubleModule.MathFuncText: string;
begin
 result := 'Tanh';
end;

procedure TSETanhDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Tanh(FDoubleIn);
end;

{ TSECotHFloatModule }

class function TSECotHFloatModule.MathFuncText: string;
begin
 result := 'CotH';
end;

procedure TSECotHFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := CotH(FFloatIn);
end;

{ TSECotHDoubleModule }

class function TSECotHDoubleModule.MathFuncText: string;
begin
 result := 'CotH';
end;

procedure TSECotHDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := CotH(FDoubleIn);
end;

{ TSESecHFloatModule }

class function TSESecHFloatModule.MathFuncText: string;
begin
 result := 'SecH';
end;

procedure TSESecHFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := SecH(FFloatIn);
end;

{ TSESecHDoubleModule }

class function TSESecHDoubleModule.MathFuncText: string;
begin
 result := 'SecH';
end;

procedure TSESecHDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := SecH(FDoubleIn);
end;

{ TSECscHFloatModule }

class function TSECscHFloatModule.MathFuncText: string;
begin
 result := 'CscH';
end;

procedure TSECscHFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := CscH(FFloatIn);
end;

{ TSECscHDoubleModule }

class function TSECscHDoubleModule.MathFuncText: string;
begin
 result := 'CscH';
end;

procedure TSECscHDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := CscH(FDoubleIn);
end;

{ TSEArcCotFloatModule }

class function TSEArcCotFloatModule.MathFuncText: string;
begin
 result := 'ArcCot';
end;

procedure TSEArcCotFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcCot(FFloatIn);
end;

{ TSEArcCotDoubleModule }

class function TSEArcCotDoubleModule.MathFuncText: string;
begin
 result := 'ArcCot';
end;

procedure TSEArcCotDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := ArcCot(FDoubleIn);
end;

{ TSEArcSecFloatModule }

class function TSEArcSecFloatModule.MathFuncText: string;
begin
 result := 'ArcSec';
end;

procedure TSEArcSecFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcSec(FFloatIn);
end;

{ TSEArcSecDoubleModule }

class function TSEArcSecDoubleModule.MathFuncText: string;
begin
 result := 'ArcSec';
end;

procedure TSEArcSecDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := ArcSec(FDoubleIn);
end;

{ TSEArcCscFloatModule }

class function TSEArcCscFloatModule.MathFuncText: string;
begin
 result := 'ArcCsc';
end;

procedure TSEArcCscFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcCsc(FFloatIn);
end;

{ TSEArcCscDoubleModule }

class function TSEArcCscDoubleModule.MathFuncText: string;
begin
 result := 'ArcCsc';
end;

procedure TSEArcCscDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := ArcCsc(FDoubleIn);
end;

{ TSEArcCoshFloatModule }

class function TSEArcCoshFloatModule.MathFuncText: string;
begin
 result := 'ArcCosh';
end;

procedure TSEArcCoshFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcCosh(FFloatIn);
end;

{ TSEArcCoshDoubleModule }

class function TSEArcCoshDoubleModule.MathFuncText: string;
begin
 result := 'ArcCosh';
end;

procedure TSEArcCoshDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := ArcCosh(FDoubleIn);
end;

{ TSEArcSinhFloatModule }

class function TSEArcSinhFloatModule.MathFuncText: string;
begin
 result := 'ArcSinh';
end;

procedure TSEArcSinhFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcSinh(FFloatIn);
end;

{ TSEArcSinhDoubleModule }

class function TSEArcSinhDoubleModule.MathFuncText: string;
begin
 result := 'ArcSinh';
end;

procedure TSEArcSinhDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := ArcSinh(FDoubleIn);
end;

{ TSEArcTanhFloatModule }

class function TSEArcTanhFloatModule.MathFuncText: string;
begin
 result := 'ArcTanh';
end;

procedure TSEArcTanhFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcTanh(FFloatIn);
end;

{ TSEArcTanhDoubleModule }

class function TSEArcTanhDoubleModule.MathFuncText: string;
begin
 result := 'ArcTanh';
end;

procedure TSEArcTanhDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := ArcTanh(FDoubleIn);
end;

{ TSEArcCotHFloatModule }

class function TSEArcCotHFloatModule.MathFuncText: string;
begin
 result := 'ArcCotH';
end;

procedure TSEArcCotHFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcCotH(FFloatIn);
end;

{ TSEArcCotHDoubleModule }

class function TSEArcCotHDoubleModule.MathFuncText: string;
begin
 result := 'ArcCotH';
end;

procedure TSEArcCotHDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := ArcCotH(FDoubleIn);
end;

{ TSEArcSecHFloatModule }

class function TSEArcSecHFloatModule.MathFuncText: string;
begin
 result := 'ArcSecH';
end;

procedure TSEArcSecHFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcSecH(FFloatIn);
end;

{ TSEArcSecHDoubleModule }

class function TSEArcSecHDoubleModule.MathFuncText: string;
begin
 result := 'ArcSecH';
end;

procedure TSEArcSecHDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := ArcSecH(FDoubleIn);
end;

{ TSEArcCscHFloatModule }

class function TSEArcCscHFloatModule.MathFuncText: string;
begin
 result := 'ArcCscH';
end;

procedure TSEArcCscHFloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := ArcCscH(FFloatIn);
end;

{ TSEArcCscHDoubleModule }

class function TSEArcCscHDoubleModule.MathFuncText: string;
begin
 result := 'ArcCscH';
end;

procedure TSEArcCscHDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := CscH(FDoubleIn);
end;

{ TSELog10FloatModule }

class function TSELog10FloatModule.MathFuncText: string;
begin
 result := 'Log10';
end;

procedure TSELog10FloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Log10(FFloatIn);
end;

{ TSELog10HDoubleModule }

class function TSELog10HDoubleModule.MathFuncText: string;
begin
 result := 'Log10';
end;

procedure TSELog10HDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Log10(FDoubleIn);
end;

{ TSELog2FloatModule }

class function TSELog2FloatModule.MathFuncText: string;
begin
 result := 'Log2';
end;

procedure TSELog2FloatModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FFloatOut := Log2(FFloatIn);
end;

{ TSELog2HDoubleModule }

class function TSELog2HDoubleModule.MathFuncText: string;
begin
 result := 'Log2';
end;

procedure TSELog2HDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0
  then FDoubleOut := Log2(FDoubleIn);
end;

end.
