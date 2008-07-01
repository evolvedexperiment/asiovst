{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaSplitter;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  SplitterDM in 'SplitterDM.pas' {SplitterDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  SplitterDataModule: TSplitterDataModule;
begin
  try
    SplitterDataModule := TSplitterDataModule.Create(Application);
    SplitterDataModule.Effect^.user := SplitterDataModule;
    SplitterDataModule.AudioMaster := audioMaster;
    Result := SplitterDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.