{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaCombo;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ComboDM in 'ComboDM.pas' {ComboDataModule: TVSTModule},
  ComboGUI in 'ComboGUI.pas' {FmCombo};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  ComboDataModule: TComboDataModule;
begin
  try
    ComboDataModule := TComboDataModule.Create(Application);
    ComboDataModule.Effect^.user := ComboDataModule;
    ComboDataModule.AudioMaster := audioMaster;
    Result := ComboDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.