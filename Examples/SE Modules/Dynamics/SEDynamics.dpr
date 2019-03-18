library SEDynamics;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEDynamicsModule in 'SEDynamicsModule.pas';

{$E sem}
{$R *.res}

const
  ModuleClasses : array [0..41] of TCustomDynamicsSEModuleClass =
    (TSimpleDirectGateStaticSEModule, TSimpleDirectGateParamStaticSEModule,
     TSimpleDirectGateAutomatableSEModule, TSoftDirectGateStaticSEModule,
     TSoftDirectGateParamStaticSEModule, TSoftDirectGateAutomatableSEModule,
     TBrickwallLimiterStaticSEModule, TBrickwallLimiterParamStaticSEModule,
     TBrickwallLimiterAutomatableSEModule, TBrickwallSoftLimiterStaticSEModule,
     TBrickwallSoftLimiterParamStaticSEModule,
     TBrickwallSoftLimiterAutomatableSEModule,
     TBrickwallSimpleSoftLimiterStaticSEModule,
     TBrickwallSimpleSoftLimiterParamStaticSEModule,
     TBrickwallSoftLimiterAutomatableSEModule, TClassicGateStaticSEModule,
     TClassicGateParamStaticSEModule, TClassicGateAutomatableSEModule,
     TSoftClassicGateStaticSEModule, TSoftClassicGateParamStaticSEModule,
     TSoftClassicGateAutomatableSEModule, TLimiterStaticSEModule,
     TLimiterParamStaticSEModule, TLimiterAutomatableSEModule,
     TSoftLimiterStaticSEModule, TSoftLimiterParamStaticSEModule,
     TSoftLimiterAutomatableSEModule, TSimpleSoftLimiterStaticSEModule,
     TSimpleSoftLimiterParamStaticSEModule,
     TSimpleSoftLimiterAutomatableSEModule, TRangeGateStaticSEModule,
     TRangeGateParamStaticSEModule, TRangeGateAutomatableSEModule,
     TSimpleCompressorStaticSEModule, TSimpleCompressorParamStaticSEModule,
     TSimpleCompressorAutomatableSEModule, TSoftKneeCompressorStaticSEModule,
     TSoftKneeCompressorParamStaticSEModule,
     TSoftKneeCompressorAutomatableSEModule, TRMSCompressorStaticSEModule,
     TRMSCompressorParamStaticSEModule, TRMSCompressorAutomatableSEModule);

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
