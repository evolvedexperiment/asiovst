{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaBandisto;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  BandistoDM in 'BandistoDM.pas' {BandistoDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  BandistoDataModule: TBandistoDataModule;
begin
  try
    BandistoDataModule := TBandistoDataModule.Create(Application);
    BandistoDataModule.Effect^.user := BandistoDataModule;
    BandistoDataModule.AudioMaster := audioMaster;
    Result := BandistoDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.