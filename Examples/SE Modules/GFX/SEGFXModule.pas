unit SEGFXModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule;

type
  TSEGFXBaseModuleClass = class of TSEGFXBaseModule;
  TSEGFXBaseModule = class(TSEModuleBase)
  protected
    FLock : Boolean;
    procedure Open; override;
    class function GetGfxName: string; virtual; abstract;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

  TSEGFXAmountModule = class(TSEGFXBaseModule)
  protected
    FAmount : Integer;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEGFXAddColorNoiseModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXAddMonoNoiseModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXContrastModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXEmbossModule = class(TSEGFXBaseModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXGrayScaleModule = class(TSEGFXBaseModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXHueModule = class(TSEGFXAmountModule)
  protected
    FDither : Boolean;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXInvertModule = class(TSEGFXBaseModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXFishEyeModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXLightnessModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXDarknessModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSaturationModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSplitBlurModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSplitBlur2Module = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXGaussianBlurModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXMosaicModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXTwistModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSplitlightModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXTileModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSpotlightModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXTraceModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXSolarizeModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

  TSEGFXPosterizeModule = class(TSEGFXAmountModule)
  public
    class function GetGfxName: string; override;
  end;

implementation

function TSEGFXBaseModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Lock';
       VariableAddress := @FLock;
       Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
       Direction       := drIn;
       Datatype        := dtBoolean;
       DefaultValue    := '0';
      end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEGFXBaseModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEGFXBaseModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;

// describe your module
class procedure TSEGFXBaseModule.getModuleProperties(Properties : PSEModuleProperties);
var
  str : string;
begin
 // describe the plugin, this is the name the end-user will see.
 with Properties^ do
  begin
   str  := 'GFX ' + GetGfxName;
   Name := PChar(str);
   ID   := PChar(str);

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   Flags      := [];
   GuiFlags   := [gfControlView, gfStructureView];
   SdkVersion := CSeSdkVersion;
  end;
end;

{ TSEGFXAmountModule }

// describe the pins (plugs)
function TSEGFXAmountModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 if Index = 1 then
  with Properties^ do
   begin
    Name            := 'Amount';
    VariableAddress := @FAmount;
    Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
    Direction       := drIn;
    Datatype        := dtEnum;
    DefaultValue    := '0';
    DatatypeExtra   := 'range 0,255';
    result          := True;
   end;
end;

{ TSEGFXInvertModule }

class function TSEGFXInvertModule.GetGfxName: string;
begin
 result := 'Invert';
end;

{ TSEGFXGrayScaleModule }

class function TSEGFXGrayScaleModule.GetGfxName: string;
begin
 result := 'GrayScale';
end;

{ TSEGFXEmbossModule }

class function TSEGFXEmbossModule.GetGfxName: string;
begin
 result := 'Emboss';
end;

{ TSEGFXAddColorNoiseModule }

class function TSEGFXAddColorNoiseModule.GetGfxName: string;
begin
 result := 'AddColorNoise';
end;

{ TSEGFXAddMonoNoiseModule }

class function TSEGFXAddMonoNoiseModule.GetGfxName: string;
begin
 result := 'AddMonoNoise';
end;

{ TSEGFXContrastModule }

class function TSEGFXContrastModule.GetGfxName: string;
begin
 result := 'Contrast';
end;

{ TSEGFXHueModule }

function TSEGFXHueModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 if Index = 2 then
  with Properties^ do
   begin
    Name            := 'Dither';
    VariableAddress := @FDither;
    Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
    Direction       := drIn;
    Datatype        := dtBoolean;
    DefaultValue    := '3';
    result          := True;
   end;
end;

class function TSEGFXHueModule.GetGfxName: string;
begin
 result := 'Hue';
end;

{ TSEGFXFishEyeModule }

class function TSEGFXFishEyeModule.GetGfxName: string;
begin
 result := 'FishEye';
end;

{ TSEGFXLightnessModule }

class function TSEGFXLightnessModule.GetGfxName: string;
begin
 result := 'Lightness';
end;

{ TSEGFXDarknessModule }

class function TSEGFXDarknessModule.GetGfxName: string;
begin
 result := 'Darkness';
end;

{ TSEGFXSaturationModule }

class function TSEGFXSaturationModule.GetGfxName: string;
begin
 result := 'Saturation';
end;

{ TSEGFXSplitBlurModule }

class function TSEGFXSplitBlurModule.GetGfxName: string;
begin
 result := 'SplitBlur';
end;

{ TSEGFXSplitBlur2Module }

class function TSEGFXSplitBlur2Module.GetGfxName: string;
begin
 result := 'SplitBlur²';
end;

{ TSEGFXGaussianBlurModule }

class function TSEGFXGaussianBlurModule.GetGfxName: string;
begin
 result := 'GaussianBlur';
end;

{ TSEGFXMosaicModule }

class function TSEGFXMosaicModule.GetGfxName: string;
begin
 result := 'Mosaic';
end;

{ TSEGFXTwistModule }

class function TSEGFXTwistModule.GetGfxName: string;
begin
 result := 'Twist';
end;

{ TSEGFXSplitlightModule }

class function TSEGFXSplitlightModule.GetGfxName: string;
begin
 result := 'Splitlight';
end;

{ TSEGFXTraceModule }

class function TSEGFXTraceModule.GetGfxName: string;
begin
 result := 'Trace';
end;

{ TSEGFXSolarizeModule }

class function TSEGFXSolarizeModule.GetGfxName: string;
begin
 result := 'Solarize';
end;

{ TSEGFXPosterizeModule }

class function TSEGFXPosterizeModule.GetGfxName: string;
begin
 result := 'Posterize';
end;

{ TSEGFXTileModule }

class function TSEGFXTileModule.GetGfxName: string;
begin
 result := 'Tile';
end;

{ TSEGFXSpotlightModule }

class function TSEGFXSpotlightModule.GetGfxName: string;
begin
 result := 'Spotlight';
end;

end.
