{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaCombo;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  ComboDM in 'ComboDM.pas' {ComboDataModule: TVSTModule};

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