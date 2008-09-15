{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SoftKneeLimiter;

{$R 'SoftKneeKnob.res' 'SoftKneeKnob.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SKLDM in 'SKLDM.pas' {SoftKneeLimiterDataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  SoftKneeLimiterDataModule: TSoftKneeLimiterDataModule;
begin
  try
    SoftKneeLimiterDataModule := TSoftKneeLimiterDataModule.Create(Application);
    SoftKneeLimiterDataModule.AudioMaster := audioMaster;
    with SoftKneeLimiterDataModule do
    begin
      Effect^.user := SoftKneeLimiterDataModule;
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
