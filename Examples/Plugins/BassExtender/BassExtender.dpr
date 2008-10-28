{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BassExtender;

{$R 'BassExtender.res' 'BassExtender.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  BassExtenderDM in 'BassExtenderDM.pas' {BassExtenderModule: TVSTModule},
  BassExtenderGUI in 'BassExtenderGUI.pas' {FmBassExtender},
  DAV_DSPFrequencyDivider in '..\..\..\Source\DSP\DAV_DSPFrequencyDivider.pas',
  DAV_DspFilterLinkwitzRiley in '..\..\..\Source\DSP\DAV_DspFilterLinkwitzRiley.pas';

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  BassExtenderModule: TBassExtenderModule;
begin
  try
    BassExtenderModule := TBassExtenderModule.Create(Application);
    BassExtenderModule.Effect^.user := BassExtenderModule;
    BassExtenderModule.AudioMaster := audioMaster;
    Result := BassExtenderModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.