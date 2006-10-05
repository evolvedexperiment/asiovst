{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library NoGUIFilter;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  FilterModule in 'FilterModule.pas' {VSTFilter: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var VSTModule1 : TVSTFilter;
begin
 try
  VSTModule1:=TVSTFilter.Create(Application);
  VSTModule1.Effect^.user:=VSTModule1;
  VSTModule1.AudioMaster:=audioMaster;
  Result := VSTModule1.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.