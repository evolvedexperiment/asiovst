library SEIntToList;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEGUI,
  SEIntToListModule in 'SEIntToListModule.pas',
  SEIntToListGUI in 'SEIntToListGUI.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Properties.SDKVersion := SDK_VERSION;
 result := True;

 case Index of // !!TODO!! list your in / out plugs
  0: with Properties^ do
      begin
       // describe the plugin, this is the name the end-user will see.
       Name := 'Int To List';

       // return a unique string 32 characters max
       // include manufacturer and plugin identity
       // this is used internally by SE to identify the plug.
       // No two plugs may have the same id.
       ID := 'Synthedit Int To List';

       // Info, may include Author, Web page whatever
       About := 'by Christian-W. Budde';
       GuiFlags := CF_STRUCTURE_VIEW;
      end;
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; p_resvd1: Pointer): Pointer; cdecl; export;
var
  Effect : TSEIntToListModule;
  GUI    : TSEIntToListGui;
begin
 result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        Effect := TSEIntToListModule.Create(SEAudioMaster, p_resvd1);
        if assigned(Effect)
         then result := Effect.getEffect;
       end else
      if (ProcessType = 1) then // GUI Object
       begin
        GUI := TSEIntToListGui.Create(TSEGuiCallback(SEAudioMaster), p_resvd1); //nasty!
        if assigned(GUI)
         then result := GUI.getEffect;
       end;
     end;
 end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
