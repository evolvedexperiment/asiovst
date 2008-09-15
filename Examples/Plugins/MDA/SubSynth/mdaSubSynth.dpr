{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaSubSynth;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SubSynthDM in 'SubSynthDM.pas' {SubSynthDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  SubSynthDataModule: TSubSynthDataModule;
begin
  try
    SubSynthDataModule := TSubSynthDataModule.Create(Application);
    SubSynthDataModule.Effect^.user := SubSynthDataModule;
    SubSynthDataModule.AudioMaster := audioMaster;
    Result := SubSynthDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.