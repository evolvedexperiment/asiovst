{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library UniQuE;

{$R 'UniQuE.res' 'UniQuE.rc'}

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  UniQuEDM in 'UniQuEDM.pas' {UniQuEDataModule: TVSTModule},
  UniQuEGUI in 'UniQuEGUI.pas' {FmUniQuE};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  UniQuEDataModule: TUniQuEDataModule;
begin
  try
    UniQuEDataModule := TUniQuEDataModule.Create(Application);
    UniQuEDataModule.Effect^.user := UniQuEDataModule;
    UniQuEDataModule.AudioMaster := audioMaster;
    Result := UniQuEDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.