library SEDitherNoiseshaper;

{$I DAV_Compiler.inc}

uses
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEDitherNoiseshaperModule in 'SEDitherNoiseshaperModule.pas',
  SEDitherHighshelfNoiseshaperModule in 'SEDitherHighshelfNoiseshaperModule.pas',
  SEDitherSharpNoiseshaperModule in 'SEDitherSharpNoiseshaperModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses: array [0..5] of TSEModuleBaseClass = (
    TSEDitherNoiseshaperStaticModule,
    TSEDitherNoiseshaperControllableModule,
    TSEDitherHighshelfNoiseshaperStaticModule,
    TSEDitherHighshelfNoiseshaperControllableModule,
    TSEDitherSharpNoiseshaperStaticModule,
    TSEDitherSharpNoiseshaperControllableModule
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
