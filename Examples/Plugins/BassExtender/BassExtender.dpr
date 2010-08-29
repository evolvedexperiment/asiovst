{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BassExtender;

// if the file below is missing please execute the batch file in this
// directory first to compile the resource file

{$R 'BassExtender.res' 'BassExtender.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  DAV_SeCommon,
  DAV_SeModule,
  DAV_SeGUI,
  BassExtenderDM in 'BassExtenderDM.pas' {BassExtenderModule: TVSTModule},
  BassExtenderGUI in 'BassExtenderGUI.pas' {FmBassExtender};

{
function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
(*
 result := True;
 case Index of
  0: TSEWaveshaperModule.GetModuleProperties(Properties);
  else result := False;
 end;
*)
 result := False;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
(*
var
  SEModuleBase : TSEModuleBase;
  GUI          : TSEGUIBase;
*)
begin
 result := nil;
(*
 case Index of
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
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
*)
end;

exports makeModule name 'makeModule';
exports GetModuleProperties name 'getModuleProperties';
}

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TBassExtenderModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TBassExtenderModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
