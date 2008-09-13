{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Decimator;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  DecimatorModule in 'DecimatorModule.pas' {VSTDecimator: TVSTModule},
  DecimatorGUI in 'DecimatorGUI.pas' {VSTGUI};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VSTDecimator : TVSTDecimator;
begin
 try
  VSTDecimator := TVSTDecimator.Create(Application);
  VSTDecimator.Effect^.user := VSTDecimator;
  VSTDecimator.AudioMaster := audioMaster;
  Result := VSTDecimator.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.

