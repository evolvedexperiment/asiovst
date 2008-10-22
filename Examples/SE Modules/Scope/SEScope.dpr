library SEScope;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEGUI,
  SEScopeModule in 'SEScopeModule.pas',
  SEScopeGUI in 'SEScopeGUI.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEScopeModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase : TSEModuleBase;
  GUI          : TSEGUIBase;
begin
 result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        SEModuleBase := TSEScopeModule.Create(SEAudioMaster, Reserved);
        if assigned(SEModuleBase)
         then result := SEModuleBase.Effect;
       end else
      if (ProcessType = 2) then // GUI Object
       begin
        GUI := TSEScopeGui.Create(TSEGuiCallback(SEAudioMaster), Reserved); //nasty!
        if assigned(GUI)
         then result := GUI.getEffect;
       end;
     end;
 end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
