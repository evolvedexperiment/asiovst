library LazVSTPlugin;

uses
  SysUtils,
{$IFDEF LCL}
  LclType,
  Interfaces,
{$ELSE}
  Windows,
{$ENDIF}
  Forms,
  DVSTModule,
  DVSTEffect,
  DDSPBase,
  LazPlugin,
  VSTPluginLaz;

function Main(audioMaster: TAudioMasterCallbackFunc ): PVSTEffect; cdecl; export;
var VSTModule1 : TVSTModule1;
begin
  Application.Initialize;
 try
  VSTModule1:=TVSTModule1.Create(nil);
  VSTModule1.Effect^.user:=VSTModule1;
  VSTModule1.AudioMaster:=audioMaster;
//  VSTModule1.Effect^.UniqueID:=FourCharToLong('A','F','F','E');
  with VSTModule1.ParameterProperties.Add do
   begin
    DisplayName:='Einfach Toll';
   end;
  Result := VSTModule1.Effect;
 except
  Result := nil;
 end;
end;

(*
function Main(audioMaster: Pointer): Pointer; cdecl; export;
begin
 Application.Initialize;
 result:=nil;
 ShowMessage('Phuh');
end;
*)

exports Main name 'main';
exports Main name 'VSTPluginMain';

end.
