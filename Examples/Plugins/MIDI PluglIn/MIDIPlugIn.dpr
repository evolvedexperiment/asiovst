{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library MIDIPlugIn;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  MIDIPlugInModule in 'MIDIPlugInModule.pas' {MIDIModule: TVSTModule},
  MIDIPlugInGUI in 'MIDIPlugInGUI.pas' {VSTGUI};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TMIDIModule.Create(Application) do
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