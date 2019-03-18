library SEEnvelope;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEEnvelopeModule in 'SEEnvelopeModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses: array [0..1] of TSEModuleBaseClass = (
    TSEEnvelopeModule,
    TSEHilbertModule
  );

function GetModuleProperties(Index: Integer;
  Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
  Result := False;
  if (Index >= 0) and (Index < Length(CModuleClasses)) then
  begin
    CModuleClasses[Index].GetModuleProperties(Properties);
    Result := True;
  end;
end;

function MakeModule(Index, ProcessType: Integer;
  SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
  Result := nil;
  if (Index >= 0) and (Index < Length(CModuleClasses)) and (ProcessType = 1) then
    Result := CModuleClasses[Index].Create(SEAudioMaster, Reserved).Effect;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
