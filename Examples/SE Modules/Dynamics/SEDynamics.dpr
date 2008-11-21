library SEDynamics;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEDynamicsModule in 'SEDynamicsModule.pas';

{$E sem}

{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of
    0: TSimpleDirectGateStaticSEModule.GetModuleProperties(Properties);
    1: TSimpleDirectGateAutomatableSEModule.GetModuleProperties(Properties);
    2: TSoftDirectGateStaticSEModule.GetModuleProperties(Properties);
    3: TSoftDirectGateAutomatableSEModule.GetModuleProperties(Properties);
    4: TBrickwallLimiterStaticSEModule.GetModuleProperties(Properties);
    5: TBrickwallLimiterAutomatableSEModule.GetModuleProperties(Properties);
    6: TBrickwallSoftLimiterStaticSEModule.GetModuleProperties(Properties);
    7: TBrickwallSoftLimiterAutomatableSEModule.GetModuleProperties(Properties);
    8: TBrickwallSimpleSoftLimiterStaticSEModule.GetModuleProperties(Properties);
    9: TBrickwallSoftLimiterAutomatableSEModule.GetModuleProperties(Properties);
   10: TClassicGateStaticSEModule.GetModuleProperties(Properties);
   11: TClassicGateAutomatableSEModule.GetModuleProperties(Properties);
   12: TSoftClassicGateStaticSEModule.GetModuleProperties(Properties);
   13: TSoftClassicGateAutomatableSEModule.GetModuleProperties(Properties);
   14: TLimiterStaticSEModule.GetModuleProperties(Properties);
   15: TLimiterAutomatableSEModule.GetModuleProperties(Properties);
   16: TSoftLimiterStaticSEModule.GetModuleProperties(Properties);
   17: TSoftLimiterAutomatableSEModule.GetModuleProperties(Properties);
   18: TSimpleSoftLimiterStaticSEModule.GetModuleProperties(Properties);
   19: TSimpleSoftLimiterAutomatableSEModule.GetModuleProperties(Properties);
   20: TRangeGateStaticSEModule.GetModuleProperties(Properties);
   21: TRangeGateAutomatableSEModule.GetModuleProperties(Properties);
   22: TSimpleCompressorStaticSEModule.GetModuleProperties(Properties);
   23: TSimpleCompressorAutomatableSEModule.GetModuleProperties(Properties);
   24: TSoftKneeCompressorStaticSEModule.GetModuleProperties(Properties);
   25: TSoftKneeCompressorAutomatableSEModule.GetModuleProperties(Properties);
   26: TRMSCompressorStaticSEModule.GetModuleProperties(Properties);
   27: TRMSCompressorAutomatableSEModule.GetModuleProperties(Properties);
  else result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 result := nil;
 if (ProcessType = 1) then
  case Index of
     0: SEModuleBase := TSimpleDirectGateStaticSEModule.Create(SEAudioMaster, Reserved);
     1: SEModuleBase := TSimpleDirectGateAutomatableSEModule.Create(SEAudioMaster, Reserved);
     2: SEModuleBase := TSoftDirectGateStaticSEModule.Create(SEAudioMaster, Reserved);
     3: SEModuleBase := TSoftDirectGateAutomatableSEModule.Create(SEAudioMaster, Reserved);
     4: SEModuleBase := TBrickwallLimiterStaticSEModule.Create(SEAudioMaster, Reserved);
     5: SEModuleBase := TBrickwallLimiterAutomatableSEModule.Create(SEAudioMaster, Reserved);
     6: SEModuleBase := TBrickwallSoftLimiterStaticSEModule.Create(SEAudioMaster, Reserved);
     7: SEModuleBase := TBrickwallSoftLimiterAutomatableSEModule.Create(SEAudioMaster, Reserved);
     8: SEModuleBase := TBrickwallSimpleSoftLimiterStaticSEModule.Create(SEAudioMaster, Reserved);
     9: SEModuleBase := TBrickwallSimpleSoftLimiterAutomatableSEModule.Create(SEAudioMaster, Reserved);
    10: SEModuleBase := TClassicGateStaticSEModule.Create(SEAudioMaster, Reserved);
    11: SEModuleBase := TClassicGateAutomatableSEModule.Create(SEAudioMaster, Reserved);
    12: SEModuleBase := TSoftClassicGateStaticSEModule.Create(SEAudioMaster, Reserved);
    13: SEModuleBase := TSoftClassicGateAutomatableSEModule.Create(SEAudioMaster, Reserved);
    14: SEModuleBase := TLimiterStaticSEModule.Create(SEAudioMaster, Reserved);
    15: SEModuleBase := TLimiterAutomatableSEModule.Create(SEAudioMaster, Reserved);
    16: SEModuleBase := TSoftLimiterStaticSEModule.Create(SEAudioMaster, Reserved);
    17: SEModuleBase := TSoftLimiterAutomatableSEModule.Create(SEAudioMaster, Reserved);
    18: SEModuleBase := TSimpleSoftLimiterStaticSEModule.Create(SEAudioMaster, Reserved);
    19: SEModuleBase := TSimpleSoftLimiterAutomatableSEModule.Create(SEAudioMaster, Reserved);
    20: SEModuleBase := TRangeGateStaticSEModule.Create(SEAudioMaster, Reserved);
    21: SEModuleBase := TRangeGateAutomatableSEModule.Create(SEAudioMaster, Reserved);
    22: SEModuleBase := TSimpleCompressorStaticSEModule.Create(SEAudioMaster, Reserved);
    23: SEModuleBase := TSimpleCompressorAutomatableSEModule.Create(SEAudioMaster, Reserved);
    24: SEModuleBase := TSoftKneeCompressorStaticSEModule.Create(SEAudioMaster, Reserved);
    25: SEModuleBase := TSoftKneeCompressorAutomatableSEModule.Create(SEAudioMaster, Reserved);
    26: SEModuleBase := TRMSCompressorStaticSEModule.Create(SEAudioMaster, Reserved);
    27: SEModuleBase := TRMSCompressorAutomatableSEModule.Create(SEAudioMaster, Reserved);
   else SEModuleBase := nil;
  end else SEModuleBase := nil;
 if assigned(SEModuleBase) then result := SEModuleBase.Effect;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
