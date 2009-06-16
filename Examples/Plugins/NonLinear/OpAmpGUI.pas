unit OpAmpGUI;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Classes, Messages,
  Forms, Controls, StdCtrls, DAV_Common, DAV_VSTModule;

type

  { TVSTGUI }

  TVSTGUI = class(TForm)
    SBGain: TScrollBar;
    LbGain: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBGainChange(Sender: TObject);
    procedure LbGainClick(Sender: TObject);
  public
    procedure UpdateGain;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Dialogs, SysUtils, OpAmpModule;

procedure TVSTGUI.FormCreate(Sender: TObject);
begin
 LbGain.Caption  := 'OpAmp Gain';
 SbGain.Max      := 1000;
 SbGain.Min      := 100;
 SbGain.Position := 100;
end;

procedure TVSTGUI.FormShow(Sender: TObject);
begin
 UpdateGain;
end;

procedure TVSTGUI.LbGainClick(Sender: TObject);
//var
//  b : PChar;
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

procedure TVSTGUI.UpdateGain;
begin
 with TVSTOpAmp(Owner) do
  begin
   if round(10 * Parameter[0]) <> SBGain.Position
    then SBGain.Position := round(10 * Parameter[0]);
  end;
end;

procedure TVSTGUI.SBGainChange(Sender: TObject);
begin
 with TVSTOpAmp(Owner) do
  begin
   if Parameter[0] <> SBGain.Position * 0.1
    then Parameter[0] := SBGain.Position * 0.1;
  end;
end;

{$IFDEF FPC}
initialization
  {$i OpAmpGUI.lrs}
{$ENDIF}

end.
