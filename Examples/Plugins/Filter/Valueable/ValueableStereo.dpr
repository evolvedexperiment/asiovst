{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ValueableStereo;

{$R 'Valueable.res' 'Valueable.rc'}

uses
  FastMM4,
  FastMove,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  VTModuleStereo in 'VTModuleStereo.pas' {VTVSTModule: TVST2Module},
  VTGUIStereo in 'VTGUIStereo.pas' {FmVT};

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
