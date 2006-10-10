unit AboutForm;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf,  LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TAbout = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    procedure Label4Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  end;

var About: TAbout;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses ShellAPI, MiniHostForm;

procedure TAbout.Label4Click(Sender: TObject);
begin
{$IFNDEF FPC}
 ShellExecute(GetDesktopWindow(), 'open',
  PChar('http://www.tobybear.de'), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TAbout.Label3Click(Sender: TObject);
begin
{$IFNDEF FPC}
 ShellExecute(GetDesktopWindow(), 'open',
  PChar('mailto:tobybear@web.de'), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TAbout.Button1Click(Sender: TObject);
begin
 close;
end;

procedure TAbout.FormShow(Sender: TObject);
begin
 Label1.caption := 'Tobybear ' + appname + ' ' + appversion;
end;

procedure TAbout.Label5Click(Sender: TObject);
begin
 {$IFNDEF FPC}
 ShellExecute(GetDesktopWindow(), 'open',
  PChar('https://www.paypal.com/xclick/business=tobybear%40web.de&item_name=MiniHost'), nil, nil, SW_SHOWNORMAL);
 {$ENDIF}
end;

{$IFDEF FPC}
initialization
  {$i aboutform.lrs}
  {$i aboutform.lrs}
{$ENDIF}

end.

