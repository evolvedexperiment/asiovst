library SEFilters;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEFiltersModule in 'SEFiltersModule.pas';

{$E sem}

{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of
  0: TSEBasicLowpassModule.GetModuleProperties(Properties);
  1: TSEBasicHighpassModule.GetModuleProperties(Properties);
  2: TSEBasicBandpassModule.GetModuleProperties(Properties);
  3: TSEBasicNotchModule.GetModuleProperties(Properties);
  4: TSEBasicLowshelfModule.GetModuleProperties(Properties);
  5: TSEBasicHighshelfModule.GetModuleProperties(Properties);
  6: TSEBasicPeakModule.GetModuleProperties(Properties);
  7: TSEBasicAllpassModule.GetModuleProperties(Properties);
  8: TSEBasicShapeModule.GetModuleProperties(Properties);
  else result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 SEModuleBase := nil;
 if (ProcessType = 1) then
  case Index of
   0: SEModuleBase := TSEBasicLowpassModule.Create(SEAudioMaster, Reserved);
   1: SEModuleBase := TSEBasicHighpassModule.Create(SEAudioMaster, Reserved);
   2: SEModuleBase := TSEBasicBandpassModule.Create(SEAudioMaster, Reserved);
   3: SEModuleBase := TSEBasicNotchModule.Create(SEAudioMaster, Reserved);
   4: SEModuleBase := TSEBasicLowshelfModule.Create(SEAudioMaster, Reserved);
   5: SEModuleBase := TSEBasicHighshelfModule.Create(SEAudioMaster, Reserved);
   6: SEModuleBase := TSEBasicPeakModule.Create(SEAudioMaster, Reserved);
   7: SEModuleBase := TSEBasicAllpassModule.Create(SEAudioMaster, Reserved);
   8: SEModuleBase := TSEBasicShapeModule.Create(SEAudioMaster, Reserved);
  end;
 if assigned(SEModuleBase)
  then result := SEModuleBase.Effect
  else result := nil;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
