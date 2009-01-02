{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BassExtender;

{$R 'BassExtender.res' 'BassExtender.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  DAV_SeCommon,
  DAV_SeModule,
  DAV_SeGUI,
  DAV_WinAmp,
  BassExtenderDM in 'BassExtenderDM.pas' {BassExtenderModule: TVSTModule},
  BassExtenderGUI in 'BassExtenderGUI.pas' {FmBassExtender},
  DAV_DSPFrequencyDivider in '..\..\..\Source\DSP\DAV_DSPFrequencyDivider.pas',
  DAV_DspFilterLinkwitzRiley in '..\..\..\Source\DSP\DAV_DspFilterLinkwitzRiley.pas',
  DAV_PluginWrapper in 'DAV_PluginWrapper.pas';

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TBassExtenderModule.Create(Application) do
     begin
      AudioMaster := AudioMasterCallback;
      Result := Effect;
     end;
  except
    Result := nil;
  end;
end;

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

function GetWinampModule(Which : Integer) : PWinAmpDSPModule; cdecl;
begin
 case Which of
   0 : begin
        Result := @WADSPModule;
        Result^.Description := PChar(TBassExtenderModule.GetStaticDescription);
       end
 else
  Result := nil;
 end;
end;

var
  WADSPHeader  : TWinAmpDSPheader;

function winampDSPGetHeader2 : PWinAmpDSPHeader; cdecl;
begin
 try
  with WADSPHeader do
   begin
    Version     := $20;
    Description := PChar(TBassExtenderModule.GetStaticDescription);
    GetModule   := GetWinampModule;
   end;
  Result := @WADSPHeader;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';
exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';
exports winampDSPGetHeader2 name 'winampDSPGetHeader2';

begin
end.