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
