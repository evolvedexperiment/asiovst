unit SELabelModule;

interface

uses
  Graphics, DAV_Common, DAV_SECommon, DAV_SEModule;

const
  CFontList : array [0..10] of TFontName = ('Arial', 'Comic Sans MS',
    'Courier New', 'Georgia', 'Impact', 'Script', 'Tahoma', 'Times New Roman',
    'Trebuchet MS', 'Verdana', 'WST_Engl');
type
  TSELabelModule = class(TSEModuleBase)
  private
    FText       : PChar;
    FFontColor  : Integer;
    FFontName   : Integer;
    FFontSize   : Integer;
    FFontBold   : Boolean;
    FFontItalic : Boolean;
    FFontShadow : Integer;
  protected
    procedure Open; override;
  public
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

procedure TSELabelModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSELabelModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;

// describe your module
class procedure TSELabelModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'Label Example';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.
 Properties.ID := 'Synthedit Label Example';

 // Info, may include Author, Web page whatever
 Properties.About := 'by Christian-W. Budde';

 Properties.Flags      := [];
 Properties.GuiFlags   := [gfControlView, gfStructureView];
 Properties.SdkVersion := CSeSdkVersion;
end;

// describe the pins (plugs)
function TSELabelModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Text';
       VariableAddress := @FText;
       Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
       Direction       := drIn;
       Datatype        := dtText;
       DefaultValue    := 'Label';
      end;
  1: with Properties^ do
      begin
       Name            := 'Font Color';
       VariableAddress := @FFontColor;
       Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
       Datatype        := dtInteger;
       Direction       := drIn;
      end;
  2: with Properties^ do
      begin
       Name            := 'Font Name';
       VariableAddress := @FFontName;
       Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
       Datatype        := dtEnum;
       Direction       := drIn;
       DatatypeExtra   := 'Arial, Comic Sans MS, Courier New, Georgia, Impact, Script, Tahoma, Times New Roman, Trebuchet MS, Verdana, WST_Engl';
      end;
  3: with Properties^ do
      begin
       Name            := 'Font Size';
       VariableAddress := @FFontSize;
       Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
       Datatype        := dtEnum;
       Direction       := drIn;
       DatatypeExtra   := '8=8,9=9,10=10,11=11,12=12,14=14,16=16,18=18,20=20,24=24,32=32,48=48,72=72';
       DefaultValue    := '24';
      end;
  4: with Properties^ do
      begin
       Name            := 'Bold';
       VariableAddress := @FFontBold;
       Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
       Datatype        := dtBoolean;
       Direction       := drIn;
      end;
  5: with Properties^ do
      begin
       Name            := 'Italic';
       VariableAddress := @FFontItalic;
       Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
       Datatype        := dtBoolean;
       Direction       := drIn;
      end;
  6: with Properties^ do
      begin
       Name            := 'Shadow Length';
       VariableAddress := @FFontShadow;
       Flags           := [iofUICommunication, iofLinearInput, iofPatchStore];
       Datatype        := dtEnum;
       Direction       := drIn;
       DatatypeExtra   := 'range 0,32';
      end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

end.
