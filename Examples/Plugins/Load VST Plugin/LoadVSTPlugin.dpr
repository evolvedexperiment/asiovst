{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LoadVSTPlugin;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  LoadVSTModule in 'LoadVSTModule.pas' {PlugInPlugModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var PlugInPlugModule : TPlugInPlugModule;
begin
 try
  PlugInPlugModule:=TPlugInPlugModule.Create(Application);
  PlugInPlugModule.Effect^.user:=PlugInPlugModule;
  PlugInPlugModule.AudioMaster:=audioMaster;
  Result := PlugInPlugModule.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.                                                    