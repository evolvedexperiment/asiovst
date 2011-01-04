{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SampleDelay;

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SampleDelayModule in 'SampleDelayModule.pas' {SampleDelayVST: TVST2Module};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TSampleDelayVST);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

begin
end.
