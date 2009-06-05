library SEButterworth;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEButterworthModule in 'SEButterworthModule.pas';

{$E sem}

{$R *.res}

const
  CModuleClasses : array [0..5] of TSEModuleBaseClass = (
    TSEStaticButterworthLPModule, TSEStaticButterworthHPModule,
    TSEStaticControlableButterworthLPModule,
    TSEStaticControlableButterworthHPModule,
    TSEAutomatableButterworthLPModule, TSEAutomatableButterworthHPModule);
//    TSEAutomatableXButterworthLPModule, TSEAutomatableXButterworthHPModule);

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
