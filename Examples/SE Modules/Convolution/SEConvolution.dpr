library SEConvolution;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEConvolutionModule in 'SEConvolutionModule.pas';

{$E sem}

{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of
  0: TSEConvolutionModule.GetModuleProperties(Properties);
  else result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 SEModuleBase := nil;
 case Index of
  0: if (ProcessType = 1)
      then SEModuleBase := TSEConvolutionModule.Create(SEAudioMaster, Reserved);
 end;
 if assigned(SEModuleBase)
  then result := SEModuleBase.Effect
  else result := nil;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
