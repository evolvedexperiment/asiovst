{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleLimiter;

{$R 'SimpleLimiter.res' 'SimpleLimiter.rc'}

uses
  FastMM4,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SimpleLimiterDM in 'SimpleLimiterDM.pas' {SimpleLimiterDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  SimpleLimiterDataModule: TSimpleLimiterDataModule;
begin
  try
    SimpleLimiterDataModule := TSimpleLimiterDataModule.Create(Application);
    SimpleLimiterDataModule.AudioMaster := audioMaster;
    with SimpleLimiterDataModule do
    begin
      Effect^.user := SimpleLimiterDataModule;
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