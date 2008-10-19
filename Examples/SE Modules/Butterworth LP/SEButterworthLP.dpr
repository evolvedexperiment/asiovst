library SEButterworthLP;

uses
  SysUtils,
  Classes,
  SECommon,
  SEDSP,
  SEButterworthLPModule in 'SEButterworthLPModule.pas';

{$E sem}

{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Properties.SDKVersion := SDK_VERSION;
 result := True;

 case Index of // !!TODO!! list your in / out plugs
  0: TSEButterworthLPModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; p_resvd1: Pointer): Pointer; cdecl; export;
var
  Effect: TSEModuleBase;
begin
 result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        Effect := TSEButterworthLPModule.Create(SEAudioMaster, p_resvd1);
        if assigned(Effect)
         then result := Effect.getEffect;
       end;
     end;
 end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
