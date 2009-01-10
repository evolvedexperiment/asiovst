library SELinkwitzRiley;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SELinkwitzRileyModule in 'SELinkwitzRileyModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of
  0: TSELinkwitzRileyStaticModule.GetModuleProperties(Properties);
  1: TSELinkwitzRileyControlableModule.GetModuleProperties(Properties);
  2: TSELinkwitzRileyAutomatableModule.GetModuleProperties(Properties);
  else result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 result := nil;
 case Index of
  0: begin
      if (ProcessType = 1) then
       begin
        SEModuleBase := TSELinkwitzRileyStaticModule.Create(SEAudioMaster, Reserved);
        if assigned(SEModuleBase)
         then result := SEModuleBase.Effect;
       end;
     end;
  1: begin
      if (ProcessType = 1) then
       begin
        SEModuleBase := TSELinkwitzRileyControlableModule.Create(SEAudioMaster, Reserved);
        if assigned(SEModuleBase)
         then result := SEModuleBase.Effect;
       end;
     end;
  2: begin
      if (ProcessType = 1) then
       begin
        SEModuleBase := TSELinkwitzRileyAutomatableModule.Create(SEAudioMaster, Reserved);
        if assigned(SEModuleBase)
         then result := SEModuleBase.Effect;
       end;
     end;
 end;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
