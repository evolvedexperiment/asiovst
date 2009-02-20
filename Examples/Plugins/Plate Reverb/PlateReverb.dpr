{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library PlateReverb;

{$R 'PlateReverbKnob.res' 'PlateReverbKnob.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  PlateReverbModule in 'PlateReverbModule.pas' {PlateReverbVST: TVSTModule},
  PlateReverbGUI in 'PlateReverbGUI.pas' {FmReverb};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TPlateReverbVST.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VstPluginMain';

begin
end.
