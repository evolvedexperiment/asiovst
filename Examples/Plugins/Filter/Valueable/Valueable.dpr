{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Valueable;

{$R 'Valueable.res' 'Valueable.rc'}

uses
  FastMM4,
  FastMove,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  VTModule in 'VTModule.pas' {VTVSTModule: TVST2Module},
  VTGUI in 'VTGUI.pas' {FmVT};

function Main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VTVSTModule: TVTVSTModule;
begin
 try
  VTVSTModule := TVTVSTModule.Create(Application);
  VTVSTModule.Effect^.user := VTVSTModule;
  VTVSTModule.AudioMaster := audioMaster;
  Result := VTVSTModule.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
