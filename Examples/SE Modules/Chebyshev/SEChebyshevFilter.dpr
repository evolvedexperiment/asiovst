library SEChebyshevFilter;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEChebyshev1FilterModule in 'SEChebyshev1FilterModule.pas',
  SEChebyshev2FilterModule in 'SEChebyshev2FilterModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses : array [0..15] of TSEModuleBaseClass = (
    TSEStaticChebyshev1FilterLPModule, TSEStaticChebyshev1FilterHPModule,
    TSEControlableChebyshev1FilterLPModule,
    TSEControlableChebyshev1FilterHPModule,
    TSEAutomatebleChebyshev1FilterLPModule,
    TSEAutomatebleChebyshev1FilterHPModule,
    TSEAutomatebleXChebyshev1FilterLPModule,
    TSEAutomatebleXChebyshev1FilterHPModule,
    TSEStaticChebyshev2FilterLPModule, TSEStaticChebyshev2FilterHPModule,
    TSEControlableChebyshev2FilterLPModule,
    TSEControlableChebyshev2FilterHPModule,
    TSEAutomatebleChebyshev2FilterLPModule,
    TSEAutomatebleChebyshev2FilterHPModule,
    TSEAutomatebleXChebyshev2FilterLPModule,
    TSEAutomatebleXChebyshev2FilterHPModule);

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
