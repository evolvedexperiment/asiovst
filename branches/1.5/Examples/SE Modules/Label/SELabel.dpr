library SELabel;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEGUI,
  SELabelModule in 'SELabelModule.pas',
  SELabelGUI in 'SELabelGUI.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
  Result := True;
  case Index of
    0:
      TSELabelModuleGUI.GetModuleProperties(Properties);
    else
      Result := False;
 end;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
  GUI: TSEGUIBase;
begin
  Result := nil;
  case Index of
    0:
      if (ProcessType = 1) then// Audio Processing Object
      begin
        SEModuleBase := TSELabelModuleGUI.Create(SEAudioMaster, Reserved);
        if Assigned(SEModuleBase) then
          Result := SEModuleBase.Effect;
      end else
      if (ProcessType = 2) then // GUI Object
      begin
        GUI := TSELabelGui.Create(TSEGuiCallback(SEAudioMaster), Reserved);
        if Assigned(GUI) then
          Result := GUI.SEGUIStructBase;
      end;
  end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
