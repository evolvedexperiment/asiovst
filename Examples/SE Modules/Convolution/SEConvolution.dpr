library SEConvolution;

{$I DAV_Compiler.INC}

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEConvolutionModule in 'SEConvolutionModule.pas',
  SELowLatencyConvolutionModule in 'SELowLatencyConvolutionModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of
  0: TSEConvolutionModule.GetModuleProperties(Properties);
  1: TSELowLatencyConvolutionModule.GetModuleProperties(Properties);
  else result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 SEModuleBase := nil;
 if (ProcessType = 1) then
  case Index of
   0: SEModuleBase := TSEConvolutionModule.Create(SEAudioMaster, Reserved);
   1: SEModuleBase := TSELowLatencyConvolutionModule.Create(SEAudioMaster, Reserved);
  end;
 if assigned(SEModuleBase)
  then result := SEModuleBase.Effect
  else result := nil;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
