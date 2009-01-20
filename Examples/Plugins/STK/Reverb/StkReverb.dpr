{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library StkReverb;

uses
  FastMM4,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  StkReverbDM in 'StkReverbDM.pas' {StkReverbModule: TVSTModule},
  StkReverbGUI in 'StkReverbGUI.pas' {FmStkReverb};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TStkReverbModule.Create(Application) do
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