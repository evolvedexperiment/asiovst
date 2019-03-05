unit AboutForm;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf,  LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls;

type
  TFormAbout = class(TForm)
    ButtonOK: TButton;
    LabelTitle: TLabel;
    LabelCopyright: TLabel;
    LabelMail: TLabel;
    LabelWebSite: TLabel;
    LabelDonate: TLabel;
    LabelEmail: TLabel;
    LabelTrademarks: TLabel;
    LabelHours: TLabel;
    LabelReadManual: TLabel;
    LabelWeb: TLabel;
    procedure LabelWebSiteClick(Sender: TObject);
    procedure LabelMailClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelDonateClick(Sender: TObject);
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  ShellAPI, MiniHostForm;

procedure TFormAbout.LabelWebSiteClick(Sender: TObject);
begin
{$IFNDEF FPC}
  ShellExecute(Self.WindowHandle, 'open', PChar('http://www.tobybear.de'), nil,
    nil, SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TFormAbout.LabelMailClick(Sender: TObject);
begin
{$IFNDEF FPC}
  ShellExecute(Self.WindowHandle, 'open', PChar('mailto:tobybear@web.de'), nil,
    nil, SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TFormAbout.ButtonOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  LabelTitle.caption := 'Tobybear ' + appname + ' ' + appversion;
end;

procedure TFormAbout.LabelDonateClick(Sender: TObject);
begin
{$IFNDEF FPC}
  ShellExecute(Self.WindowHandle, 'open',
    PChar('https://www.paypal.com/xclick/business=tobybear%40web.de&item_name=MiniHost'), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
end;

end.
