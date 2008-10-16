library SEGain;

uses
  SysUtils,
  Classes,
  SEModStruct,
  SEGainModule in 'SEGainModule.pas';

{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Integer; cdecl; export;
begin
 Properties.SDKVersion = SDK_VERSION;
 result := True;

 case Index of // !!TODO!! list your in / out plugs
  0: TSEGainModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer, ProcessType: Integer; SEAudioMaster: SEAudioMasterCallback2; p_resvd1: Pointer): Pointer; cdecl; export;
var
  Effect: TSEModuleBase;
begin
 result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        Effect := TSEGainModule.Create(SEAudioMaster, p_resvd1);
        if assigned(Effect)
         then result := Effect.getAeffect;
       end;
     end;
 end;
end;

begin
end.
