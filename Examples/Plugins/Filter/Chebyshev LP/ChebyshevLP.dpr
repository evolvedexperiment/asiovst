{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ChebyshevLP;

{$R 'Chebyshev.res' 'Chebyshev.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ChebyshevDM in 'ChebyshevDM.pas' {ChebyshevLPModule: TVSTModule},
  ChebyshevGUI in 'ChebyshevGUI.pas' {FmChebyshev};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TChebyshevLPModule.Create(Application) do
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