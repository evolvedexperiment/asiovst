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

function Main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TVTVSTModule.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
