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
 result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEIntToListModule.GetModuleProperties(Properties)
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: Pointer; Reserved: Pointer): Pointer; cdecl; export;
var
  SEIntToListModule : TSEIntToListModule;
  GUI               : TSEIntToListGui;
begin
 result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        SEIntToListModule := TSEIntToListModule.Create(SEAudioMaster, Reserved);
        if assigned(SEIntToListModule)
         then result := SEIntToListModule.Effect;
       end else
      if (ProcessType = 1) then // GUI Object
       begin
        GUI := TSEIntToListGui.Create(TSEGuiCallback(SEAudioMaster), Reserved); //nasty!
        if assigned(GUI)
         then result := GUI.SEGUIStructBase;
       end;
     end;
 end;
end;

exports makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
