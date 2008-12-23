{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleChorus;

{$R 'Chorus.res' 'Chorus.rc'}

uses
  FastMM4,
  FastMove,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SimpleChorusDM in 'SimpleChorusDM.pas' {SimpleChorusModule: TVSTModule},
  SimpleChorusGUI in 'SimpleChorusGUI.pas' {FmSimpleChorus};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  SimpleChorusModule: TSimpleChorusModule;
begin
  try
    SimpleChorusModule := TSimpleChorusModule.Create(Application);
    SimpleChorusModule.Effect^.user := SimpleChorusModule;
    SimpleChorusModule.AudioMaster := audioMaster;
    Result := SimpleChorusModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.