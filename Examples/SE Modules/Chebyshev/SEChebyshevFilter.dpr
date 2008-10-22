library SEChebyshevFilter;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEChebyshevFilterModule in 'SEChebyshevFilterModule.pas';

{$E sem}

{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEChebyshevFilterLPModule.GetModuleProperties(Properties);
  1: TSEChebyshevFilterHPModule.GetModuleProperties(Properties);
  else result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        SEModuleBase := TSEChebyshevFilterLPModule.Create(SEAudioMaster, Reserved);
        if assigned(SEModuleBase)
         then result := SEModuleBase.Effect;
       end;
     end;
  1: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        SEModuleBase := TSEChebyshevFilterHPModule.Create(SEAudioMaster, Reserved);
        if assigned(SEModuleBase)
         then result := SEModuleBase.Effect;
       end;
     end;
 end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
