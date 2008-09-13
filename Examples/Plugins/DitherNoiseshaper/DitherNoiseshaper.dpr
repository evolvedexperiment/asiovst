{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library DitherNoiseshaper;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  DitherNoiseshaperDM in 'DitherNoiseshaperDM.pas' {DitherNoiseshaperModule: TVSTModule},
  DitherNoiseshaperGUI in 'DitherNoiseshaperGUI.pas' {FmDitherNoiseshaper};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  DitherNoiseshaperModule: TDitherNoiseshaperModule;
begin
 try
  DitherNoiseshaperModule := TDitherNoiseshaperModule.Create(Application);
  DitherNoiseshaperModule.Effect^.user := DitherNoiseshaperModule;
  DitherNoiseshaperModule.AudioMaster := audioMaster;
  Result := DitherNoiseshaperModule.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.