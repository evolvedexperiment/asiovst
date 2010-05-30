library SETools;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEToolsModule in 'SEToolsModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses : array [0..23] of TSEToolsModuleClass = (
    TSELimitFloatModule, TSELimitDoubleModule, TSELimitIntegerModule,
    TSEIsPowerOf2Module, TSERoundToPowerOf2Module, TSETruncToPowerOf2Module,
    TSEExtendToPowerOf2Module, TSEFlipIntegerBytesModule, TSESincSingleModule,
    TSESincDoubleModule, TSESigmoidSingleModule, TSESigmoidDoubleModule,
    TSEFractionalSingleModule, TSEFractionalDoubleModule,
    TSEModuloSingleModule, TSEModuloDoubleModule,
    TSEMinimumIntModule, TSEMinimumSingleModule, TSEMinimumDoubleModule,
    TSEMaximumIntModule, TSEMaximumSingleModule, TSEMaximumDoubleModule,
    TSEGetSinCosSingleModule, TSEGetSinCosDoubleModule);

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 if (Index >= 0) and (Index < Length(CModuleClasses)) then
  begin
   CModuleClasses[Index].GetModuleProperties(Properties);
   result := True;
  end
 else result := False;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 if (Index >= 0) and (Index < Length(CModuleClasses)) and (ProcessType = 1)
  then result := CModuleClasses[Index].Create(SEAudioMaster, Reserved).Effect
  else result := nil;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
