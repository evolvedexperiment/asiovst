library SEWaveshaper;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEGUI,
  SEWaveshaperModule in 'SEWaveshaperModule.pas',
  SEWaveshaperGUI in 'SEWaveshaperGUI.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Properties.SDKVersion := SDK_VERSION;
 result := True;

 case Index of // !!TODO!! list your in / out plugs
  0: begin
      Properties.SdkVersion := SDK_VERSION;

      case Index of                // !!TODO!! list your modules
       0: with Properties^ do
           begin
            // describe the plugin, this is the name the end-user will see.
            Name := 'Waveshaper Example';

            // return a unique string 32 characters max
            // if posible include manufacturer and plugin identity
            // this is used internally by SE to identify the plug.
            // No two plugs may have the same id.
            ID := 'Synthedit Waveshaper (DAV)';

            // Info, may include Author, Web page whatever
            About := 'by Christian-W. Budde';
            GuiFlags := CF_CONTROL_VIEW or CF_STRUCTURE_VIEW;
           end;
      end;
     end
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  Effect : TSEModuleBase;
  GUI    : TSEGUIBase;
begin
 result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        Effect := TSEWaveshaperModule.Create(SEAudioMaster, Reserved);
        if assigned(Effect)
         then result := Effect.getEffect;
       end else
      if (ProcessType = 2) then // GUI Object
       begin
        GUI := TSEWaveshaperGui.Create(TSEGuiCallback(SEAudioMaster), Reserved); //nasty!
        if assigned(GUI)
         then result := GUI.getEffect;
       end;
     end;
 end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
