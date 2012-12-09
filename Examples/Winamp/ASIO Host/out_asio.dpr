library out_asio;

{$I DAV_Compiler.inc}

uses
{$IFDEF UseFastMM}
  FastMM4,
{$ENDIF}
{$IFDEF UseFastMove}
  FastMove,
{$ENDIF}
  SysUtils,
  Classes,
  OutputAsio in 'OutputAsio.pas' {FmASIOConfig};

{$R *.res}

function winampGetOutModule: POut_Module; cdecl;
begin
  OutMod.Version := $10;
  GetMem(OutMod.Description, 32);
  FillChar(OutMod.Description^, 32, 0);
  StrPCopy(OutMod.Description, 'ASIO Output Plugin');
  OutMod.Config := Config;
  OutMod.About := About;
  OutMod.Init := Init;
  OutMod.Quit := Quit;
  OutMod.Open := Open;
  OutMod.Close := Close;
  OutMod.Write := Write;
  OutMod.CanWrite := CanWrite;
  OutMod.IsPlaying := IsPlaying;
  OutMod.Pause := Pause;
  OutMod.SetVolume := SetVolume;
  OutMod.SetPan := SetPan;
  OutMod.Flush := Flush;
  OutMod.GetOutputTime := GetOutputTime;
  OutMod.GetWrittenTime := GetOutputTime;
  Result := @OutMod;
end;

exports
  winampGetOutModule;

begin

end.
