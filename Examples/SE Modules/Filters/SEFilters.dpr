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
  0: TSESimpleLowpassModule.GetModuleProperties(Properties);
  1: TSESimpleHighpassModule.GetModuleProperties(Properties);
  2: TSESimpleBandpassModule.GetModuleProperties(Properties);
  3: TSESimpleNotchModule.GetModuleProperties(Properties);
  4: TSESimpleLowshelfModule.GetModuleProperties(Properties);
  5: TSESimpleHighshelfModule.GetModuleProperties(Properties);
  6: TSESimplePeakModule.GetModuleProperties(Properties);
  7: TSESimpleAllpassModule.GetModuleProperties(Properties);
  8: TSESimpleShapeModule.GetModuleProperties(Properties);
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
   0: SEModuleBase := TSESimpleLowpassModule.Create(SEAudioMaster, Reserved);
   1: SEModuleBase := TSESimpleHighpassModule.Create(SEAudioMaster, Reserved);
   2: SEModuleBase := TSESimpleBandpassModule.Create(SEAudioMaster, Reserved);
   3: SEModuleBase := TSESimpleNotchModule.Create(SEAudioMaster, Reserved);
   4: SEModuleBase := TSESimpleLowshelfModule.Create(SEAudioMaster, Reserved);
   5: SEModuleBase := TSESimpleHighshelfModule.Create(SEAudioMaster, Reserved);
   6: SEModuleBase := TSESimplePeakModule.Create(SEAudioMaster, Reserved);
   7: SEModuleBase := TSESimpleAllpassModule.Create(SEAudioMaster, Reserved);
   8: SEModuleBase := TSESimpleShapeModule.Create(SEAudioMaster, Reserved);
  end;
 if assigned(SEModuleBase)
  then result := SEModuleBase.Effect
  else result := nil;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
