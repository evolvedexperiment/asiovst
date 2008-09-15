{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaThruZero;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ThruZeroDM in 'ThruZeroDM.pas' {ThruZeroDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  ThruZeroDataModule: TThruZeroDataModule;
begin
  try
    ThruZeroDataModule := TThruZeroDataModule.Create(Application);
    ThruZeroDataModule.Effect^.user := ThruZeroDataModule;
    ThruZeroDataModule.AudioMaster := audioMaster;
    Result := ThruZeroDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.