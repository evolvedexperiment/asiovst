{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library VSTPascalScript;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  PSDM in 'C:\Code\VSTPack\Examples\Plugins\PascalScript\PSDM.pas' {PascalScriptDataModule: TVSTModule},
  PSGUI in 'C:\Code\VSTPack\Examples\Plugins\PascalScript\PSGUI.pas' {FmPascalScript};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  PascalScriptDataModule: TPascalScriptDataModule;
begin
  try
    PascalScriptDataModule := TPascalScriptDataModule.Create(Application);
    PascalScriptDataModule.Effect^.user := PascalScriptDataModule;
    PascalScriptDataModule.AudioMaster := audioMaster;
    Result := PascalScriptDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.