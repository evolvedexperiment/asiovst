library SEFilterPlot;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEGUI,
  SEFilterPlotModule in 'SEFilterPlotModule.pas',
  SEFilterPlotGUI in 'SEFilterPlotGUI.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of
  0: TSEFilterPlotModule.GetModuleProperties(Properties);
  else result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase : TSEModuleBase;
  GUI          : TSEGUIBase;
begin
 result := nil;
 case Index of
  0: if (ProcessType = 1) then
      begin
       SEModuleBase := TSEFilterPlotModule.Create(SEAudioMaster, Reserved);
       if assigned(SEModuleBase)
        then result := SEModuleBase.Effect;
      end else
     if (ProcessType = 2) then
      begin
       GUI := TSEFilterPlotGui.Create(TSEGuiCallback(SEAudioMaster), Reserved);
       if assigned(GUI)
        then result := GUI.SEGUIStructBase;
      end;
 end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
