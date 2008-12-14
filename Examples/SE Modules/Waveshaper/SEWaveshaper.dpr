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
 result := True;
 case Index of
  0: TSEWaveshaperModule.GetModuleProperties(Properties);
  else result := False;
 end;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase : TSEModuleBase;
  GUI          : TSEGUIBase;
begin
 result := nil;
 case Index of
  0: if (ProcessType = 1) then// Audio Processing Object
      begin
       SEModuleBase := TSEWaveshaperModule.Create(SEAudioMaster, Reserved);
       if assigned(SEModuleBase)
        then result := SEModuleBase.Effect;
      end else
     if (ProcessType = 2) then // GUI Object
      begin
       GUI := TSEWaveshaperGui.Create(TSEGuiCallback(SEAudioMaster), Reserved); //nasty!
       if assigned(GUI)
        then result := GUI.SEGUIStructBase;
      end;
 end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
