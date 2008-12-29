{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ButterworthLP;

{$R 'Butterworth.res' 'Butterworth.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ButterworthDM in 'ButterworthDM.pas' {ButterworthLPModule: TVSTModule},
  ButterworthGUI in 'ButterworthGUI.pas' {FmButterworth};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TButterworthLPModule.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
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
