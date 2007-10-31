{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Wavedisplay;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  WavedisplayModule in 'WavedisplayModule.pas' {WavedisplayModule: TVSTModule},
  WavedisplayGUI in 'WavedisplayGUI.pas' {WavedisplayGUI};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  WavedisplayModule: TWavedisplayModule;
begin
  try
    WavedisplayModule := TWavedisplayModule.Create(Application);
    WavedisplayModule.Effect^.user := WavedisplayModule;
    WavedisplayModule.AudioMaster := audioMaster;
    Result := WavedisplayModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.