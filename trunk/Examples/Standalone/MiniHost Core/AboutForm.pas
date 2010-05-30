unit AboutForm;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC}LCLIntf,  LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls;

type
  TFmAbout = class(TForm)
    BtOK: TButton;
    LbTitle: TLabel;
    LbCopyright: TLabel;
    LbMail: TLabel;
    LbWeb: TLabel;
    LbDonate: TLabel;
    Lbml: TLabel;
    LbTrademarks: TLabel;
    LbHours: TLabel;
    LbReadManual: TLabel;
    LbWb: TLabel;
    procedure LbWebClick(Sender: TObject);
    procedure LbMailClick(Sender: TObject);
    procedure BtOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbDonateClick(Sender: TObject);
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  ShellAPI, MiniHostForm;

procedure TFmAbout.LbWebClick(Sender: TObject);
begin
{$IFNDEF FPC}
 ShellExecute(GetDesktopWindow, 'open',
  PChar('http://www.tobybear.de'), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TFmAbout.LbMailClick(Sender: TObject);
begin
{$IFNDEF FPC}
 ShellExecute(GetDesktopWindow, 'open',
  PChar('mailto:tobybear@web.de'), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TFmAbout.BtOKClick(Sender: TObject);
begin
 Close;
end;

procedure TFmAbout.FormShow(Sender: TObject);
begin
 LbTitle.caption := 'Tobybear ' + appname + ' ' + appversion;
end;

procedure TFmAbout.LbDonateClick(Sender: TObject);
begin
 {$IFNDEF FPC}
 ShellExecute(GetDesktopWindow(), 'open',
   PChar('https://www.paypal.com/xclick/business=tobybear%40web.de&item_name=MiniHost'), nil, nil, SW_SHOWNORMAL);
 {$ENDIF}
end;

{$IFDEF FPC}
initialization
  {$i AboutForm.lrs}
{$ENDIF}

end.

