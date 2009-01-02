unit OpAmpGUI;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Classes, Messages,
  Forms, Controls, StdCtrls, DAV_Common, DAV_VSTModule;

type
  TVSTGUI = class(TForm)
    SBGain: TScrollBar;
    LbGain: TLabel;
    procedure SBGainChange(Sender: TObject);
    procedure LbGainClick(Sender: TObject);
  private
  public
    theModule: TVSTModule;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Dialogs, SysUtils, OpAmpModule;

procedure TVSTGUI.LbGainClick(Sender: TObject);
var
  b : PChar;
begin
(*
 // Example on how to query the DLL name of the plugin
 GetMem(b, 255);
 FillChar(b^, 255, 0);
 try
  ShowMessage('Instance: ' + IntToStr(hInstance));
  GetModuleFileName(hInstance, b, 255);
  ShowMessage(StrPas(b));
 finally
  Dispose(b);
 end;
*)
end;

procedure TVSTGUI.SBGainChange(Sender: TObject);
begin
 TVSTOpAmp(Owner).Parameter[0] := SBGain.Position * 0.1;
end;

{$IFDEF FPC}
initialization
  {$i OpAmpGUI.lrs}
{$ENDIF}

end.
