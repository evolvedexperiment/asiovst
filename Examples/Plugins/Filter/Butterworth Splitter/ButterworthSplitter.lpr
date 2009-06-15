{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ButterworthSplitter;

{$I DAV_Compiler.inc}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ButterworthSplitterDM in 'ButterworthSplitterDM.pas' {ButterworthSplitterModule: TVSTModule};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TButterworthSplitterModule.Create(Application) do
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
