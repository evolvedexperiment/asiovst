{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaLimiter;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  mdaLimiterDM in 'mdaLimiterDM.pas' {mdaLimiterDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  mdaLimiterDataModule: TmdaLimiterDataModule;
begin
  try
    mdaLimiterDataModule := TmdaLimiterDataModule.Create(Application);
    mdaLimiterDataModule.Effect^.user := mdaLimiterDataModule;
    mdaLimiterDataModule.AudioMaster := audioMaster;
    Result := mdaLimiterDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.