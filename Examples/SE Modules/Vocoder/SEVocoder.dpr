library SEVocoder;

{$I DAV_Compiler.inc}

uses
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEVocoderModule in 'SEVocoderModule.pas',
  SESimpleVocoderModule in 'SESimpleVocoderModule.pas',
  SEBarkVocoderModule in 'SEBarkVocoderModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses: array [0..8] of TSEModuleBaseClass = (
    TSEVocoderStaticModule,
    TSEVocoderControllableModule,
    TSEVocoderAutomatableModule,
    TSESimpleVocoderStaticModule,
    TSESimpleVocoderControllableModule,
    TSESimpleVocoderAutomatableModule,
    TSEBarkVocoderStaticModule,
    TSEBarkVocoderControllableModule,
    TSEBarkVocoderAutomatableModule
  );

function GetModuleProperties(Index: Integer;
  Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
  Result := False;
  if (Index >= 0) and (Index < Length(CModuleClasses)) then
  begin
    CModuleClasses[Index].GetModuleProperties(Properties);
    Result := True;
  end;
end;

function MakeModule(Index, ProcessType: Integer;
  SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
  Result := nil;
  if (Index >= 0) and (Index < Length(CModuleClasses)) and (ProcessType = 1) then
    Result := CModuleClasses[Index].Create(SEAudioMaster, Reserved).Effect;
end;

exports
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
