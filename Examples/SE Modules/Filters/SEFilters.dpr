library SEFilters;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEFiltersModule in 'SEFiltersModule.pas',
  SELightweightFiltersModule in 'SELightweightFiltersModule.pas';

{$E sem}
{$R *.res}

{$UNDEF FilterReference}

const
  CX = {$IFDEF FilterReference}1{$ELSE}0{$ENDIF};
  CModuleClasses : array [0..25 + CX] of TSEModuleBaseClass = (
    TSEBasicLowpassModule, TSEBasicHighpassModule, TSEBasicBandpassModule,
    TSEBasicNotchModule, TSEBasicLowshelfModule, TSEBasicLowshelfAModule,
    TSEBasicLowshelfBModule, TSEBasicHighshelfModule, TSEBasicHighshelfAModule,
    TSEBasicHighshelfBModule, TSEBasicPeakModule, TSEBasicAllpassModule,
    TSEBasicShapeModule, TSELightweightLowpassModule,
    TSELightweightHighpassModule, TSELightweightBandpassModule,
    TSELightweightNotchModule, TSELightweightLowshelfModule,
    TSELightweightLowshelfAModule, TSELightweightLowshelfBModule,
    TSELightweightHighshelfModule, TSELightweightHighshelfAModule,
    TSELightweightHighshelfBModule, TSELightweightPeakModule,
    TSELightweightAllpassModule, TSELightweightShapeModule
    {$IFDEF FilterReference}, TFilterCascadeModule{$ENDIF});

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
