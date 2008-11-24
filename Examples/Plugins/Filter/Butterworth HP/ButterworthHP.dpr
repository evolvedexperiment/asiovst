{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ButterworthHP;

{$R 'Butterworth.res' 'Butterworth.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ButterworthDM in 'ButterworthDM.pas' {ButterworthHPModule: TVSTModule},
  ButterworthGUI in 'ButterworthGUI.pas' {FmButterworth};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  ButterworthHPModule: TButterworthHPModule;
begin
  try
    ButterworthHPModule := TButterworthHPModule.Create(Application);
    ButterworthHPModule.Effect^.user := ButterworthHPModule;
    ButterworthHPModule.AudioMaster := audioMaster;
    Result := ButterworthHPModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
