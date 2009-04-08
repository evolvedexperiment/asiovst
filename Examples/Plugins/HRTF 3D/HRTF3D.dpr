{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library HRTF3D;

{$R 'Head.res' 'Head.rc'}

uses
  FastMM4,
  FastMove,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  HRTF3DModule in 'HRTF3DModule.pas' {VSTHRTF3DModule: TVST2Module},
  HRTF3DGUI in 'HRTF3DGUI.pas' {VSTGUI};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TVSTHRTF3DModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TVSTHRTF3DModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.

