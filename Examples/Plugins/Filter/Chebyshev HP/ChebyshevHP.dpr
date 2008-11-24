{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ChebyshevHP;

{$R 'Chebyshev.res' 'Chebyshev.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ChebyshevDM in 'ChebyshevDM.pas' {ChebyshevHPModule: TVSTModule},
  ChebyshevGUI in 'ChebyshevGUI.pas' {FmChebyshev};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  ChebyshevHPModule: TChebyshevHPModule;
begin
  try
    ChebyshevHPModule := TChebyshevHPModule.Create(Application);
    ChebyshevHPModule.Effect^.user := ChebyshevHPModule;
    ChebyshevHPModule.AudioMaster := audioMaster;
    Result := ChebyshevHPModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
