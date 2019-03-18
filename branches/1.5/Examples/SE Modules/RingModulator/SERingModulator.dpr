library SERingModulator;

uses
  FastMove,
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SERingModulatorModule in 'SERingModulatorModule.pas',
  SEAnalogRingModulatorModule in 'SEAnalogRingModulatorModule.pas',
  SELightweightAnalogRingModulatorModule in 'SELightweightAnalogRingModulatorModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses: array [0..5] of TSEModuleBaseClass = (
   TSERingModulatorStaticModule,
   TSERingModulatorControllableModule,
   TSEAnalogRingModulatorStaticModule,
   TSEAnalogRingModulatorControllableModule,
   TSELightweightAnalogRingModulatorStaticModule,
   TSELightweightAnalogRingModulatorControllableModule
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
