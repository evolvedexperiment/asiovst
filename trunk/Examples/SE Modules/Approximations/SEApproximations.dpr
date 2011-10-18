library SEApproximations;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEApproximationsModule in 'SEApproximationsModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses : array [0..15] of TSEApproximationsModuleClass = (
    TSEFastSineApproximationsSingleModule,
    TSEFastSineApproximationsDoubleModule,
    TSEFastCosineApproximationsSingleModule,
    TSEFastCosineApproximationsDoubleModule,
    TSEFastTangensApproximationsSingleModule,
    TSEFastTangensApproximationsDoubleModule,
    TSEFastCotangensApproximationsSingleModule,
    TSEFastCotangensApproximationsDoubleModule,
    TSEFastArcTanApproximationsSingleModule,
    TSEFastArcTanApproximationsDoubleModule,
    TSEFastArcCotanApproximationsSingleModule,
    TSEFastArcCotanApproximationsDoubleModule,
    TSEFastLog2ApproximationsSingleModule,
    TSEFastLog2ApproximationsDoubleModule,
    TSEFastPower2ApproximationsSingleModule,
    TSEFastPower2ApproximationsDoubleModule);

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

exports makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
