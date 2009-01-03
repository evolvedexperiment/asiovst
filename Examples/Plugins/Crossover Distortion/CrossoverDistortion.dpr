{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library CrossoverDistortion;

{$R 'CrossoverDistortion.res' 'CrossoverDistortion.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept,
  madLinkDisAsm,
  madListModules,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  CrossoverDistortionDM in 'CrossoverDistortionDM.pas' {CrossoverDistortionDataModule: TVSTModule},
  CrossoverDistortionGUI in 'CrossoverDistortionGUI.pas' {FmCrossoverDistortion};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TCrossoverDistortionDataModule.Create(Application) do
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