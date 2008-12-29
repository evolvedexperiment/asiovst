{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SpinBugLite;

uses
  FastMM4,
  Forms,
  DAV_VSTEffect,
  DAV_VSTCustomModule,
  DAV_VSTModule,
  DAV_VSTParameters,
  SpinBugLiteModule in 'SpinBugLiteModule.pas' {SpinBugLiteModule: TVSTModule},
  SpinBugLiteGUI in 'SpinBugLiteGUI.pas' {SpinBugLiteGUI: TFmSpinBugLite};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TSpinBugLiteModule.Create(Application) do
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

