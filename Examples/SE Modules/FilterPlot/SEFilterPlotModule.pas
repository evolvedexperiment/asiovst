unit SEFilterPlotModule;

interface

uses
  Windows, Graphics, DAV_Types, DAV_SECommon, DAV_SEModule, DAV_DspFilter;

type
  TSEBlob = record
    Size : Integer;
    Data : Pointer;
  end;

  TSEFilterPlotModule = class(TSEModuleBase)
  protected
    FColorLine    : TColor;
    FColorCurve   : TColor;
    FFilterRef    : Pointer;
    FFilterRefGUI : TCustomFilter;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;

    procedure SubProcessSleep(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils, Dialogs;

constructor TSEFilterPlotModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FColorLine  := clGray;
 FColorCurve := clSilver;
 FFilterRef  := nil;
 FFilterRefGUI := nil;
end;

procedure TSEFilterPlotModule.Open;
begin
 inherited Open;
 if FFilterRef = nil
  then FFilterRefGUI := nil
  else FFilterRefGUI := TCustomFilter(FFilterRef^);
 OnProcess := SubProcessSleep;
end;

procedure TSEFilterPlotModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case CurrentPin.PinID of
  1 : Pin[0].TransmitStatusChange(SampleClock, stStatic);
(*
  2 : FHue := Limit(FHue, 0, 1);
  3 : FLuminance := Limit(FLuminance, 0, 1);
*)
 end;
end;

// The most important part, processing the audio
procedure TSEFilterPlotModule.SubProcessSleep(const BufferOffset, SampleFrames: Integer);
begin
 Pin[0].TransmitStatusChange(SampleClock, stRun);
 CallHost(SEAudioMasterSleepMode);
end;

// describe your module
class procedure TSEFilterPlotModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Filter Plot';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Synthedit Filter Plot Example';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   Flags      := [];
   GuiFlags   := [gfControlView, gfStructureView];
   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEFilterPlotModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Internal Filter Reference (DSP)';
       VariableAddress := @FFilterRefGUI;
       Direction       := drOut;
       Datatype        := dtInteger;
       Flags           := [iofPatchStore, iofHidePin]; 
      end;
  1: with Properties^ do
      begin
       Name            := 'Internal Filter Reference (GUI)';
       Direction       := drIn;
       Datatype        := dtInteger;
       Flags           := [iofUICommunication, iofPatchStore, iofHidePin];
      end;
  2: with Properties^ do
      begin
       Name            := 'Filter Reference';
       VariableAddress := @FFilterRef;
       Direction       := drIn;
       Datatype        := dtFilterReference;
      end;
  3: with Properties^ do
      begin
       Name            := 'Line Color';
       VariableAddress := @FColorLine;
       Direction       := drIn;
       Datatype        := dtInteger;
       Flags           := [iofUICommunication, iofPatchStore];
       DefaultValue    := '0.1';
      end;
  4: with Properties^ do
      begin
       Name            := 'Curve Color';
       VariableAddress := @FColorCurve;
       Direction       := drIn;
       Datatype        := dtInteger;
       Flags           := [iofUICommunication, iofPatchStore];
       DefaultValue    := '0.1';
      end;
  else result := False;
 end;
end;

end.
