{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ButterworthLP;

{$R 'Butterworth.res' 'Butterworth.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ButterworthDM in 'ButterworthDM.pas' {ButterworthLPModule: TVSTModule},
  ButterworthGUI in 'ButterworthGUI.pas' {FmButterworth};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  ButterworthLPModule: TButterworthLPModule;
begin
  try
    ButterworthLPModule := TButterworthLPModule.Create(Application);
    ButterworthLPModule.Effect^.user := ButterworthLPModule;
    ButterworthLPModule.AudioMaster := audioMaster;
    Result := ButterworthLPModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
