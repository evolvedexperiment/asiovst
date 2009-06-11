library SEChebyshevFilter;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEChebyshevFilterModule in 'SEChebyshevFilterModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses : array [0..7] of TSEModuleBaseClass = (
    TSEStaticChebyshevFilterLPModule, TSEStaticChebyshevFilterHPModule,
    TSEControlableChebyshevFilterLPModule,
    TSEControlableChebyshevFilterHPModule,
    TSEAutomatebleChebyshevFilterLPModule,
    TSEAutomatebleChebyshevFilterHPModule,
    TSEAutomatebleXChebyshevFilterLPModule,
    TSEAutomatebleXChebyshevFilterHPModule);

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 if Index in [0..Length(CModuleClasses) - 1] then
  begin
   CModuleClasses[Index].GetModuleProperties(Properties);
   result := True;
  end
 else result := False;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 if Index in [0..Length(CModuleClasses) - 1]
  then result := CModuleClasses[Index].Create(SEAudioMaster, Reserved).Effect
  else result := nil;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
