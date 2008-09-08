{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LA2094;

{$R 'LA2094.res' 'LA2094.rc'}

uses
  FastMM4,
  Forms,
  DVSTEffect,
  DVSTModule,
  LA2094DM in 'LA2094DM.pas' {LA2094DataModule: TVSTModule},
  EditorFrm in 'EditorFrm.pas' {EditorForm},
  DDSPLevelingAmplifier in '..\..\..\..\Source\DDSPLevelingAmplifier.pas',
  DGuiVUMeter in '..\..\..\..\Source\DGuiVUMeter.pas';

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  LA2094DataModule: TLA2094DataModule;
begin
 try
  LA2094DataModule := TLA2094DataModule.Create(Application);
  LA2094DataModule.AudioMaster := audioMaster;
  with LA2094DataModule do
   begin
    Effect^.user := LA2094DataModule;
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
