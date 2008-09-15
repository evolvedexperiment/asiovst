{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library MIDIPlugIn;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  MIDIPlugInModule in 'MIDIPlugInModule.pas' {MIDIModule: TVSTModule},
  MIDIPlugInGUI in 'MIDIPlugInGUI.pas' {VSTGUI};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var MIDIModule : TMIDIModule;
begin
 try
  MIDIModule:=TMIDIModule.Create(Application);
  MIDIModule.Effect^.user:=MIDIModule;
  MIDIModule.AudioMaster:=audioMaster;
  Result := MIDIModule.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.