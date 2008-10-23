unit SEabout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_GuiBaseControl, DAV_GuiLabel, ExtCtrls;

type
  TFmAbout = class(TForm)
    ImSinthEdit: TImage;
    LbSem: TGuiLabel;
    LbAbout: TLabel;
    LbDelphiASIOVST: TLabel;
    LbPainting: TLabel;
    LbSemShadow: TGuiLabel;
    LbExplorer: TGuiLabel;
    LbExplorerShadow: TGuiLabel;
    procedure FormClick(Sender: TObject);
    procedure ImSinthEditClick(Sender: TObject);
    procedure LbDelphiASIOVSTClick(Sender: TObject);
    procedure LbAboutClick(Sender: TObject);
  end;

var
  FmAbout: TFmAbout;

implementation

uses
  ShellAPI;

{$R *.dfm}

procedure TFmAbout.FormClick(Sender: TObject);
begin
 Close;
end;

procedure TFmAbout.ImSinthEditClick(Sender: TObject);
begin
 ShellExecute(Handle, 'open', 'http://jacky.theprize.googlepages.com/paintings', nil, nil, SW_SHOW);
end;

procedure TFmAbout.LbAboutClick(Sender: TObject);
begin
 ShellExecute(Handle, 'open', 'http://www.savioursofsoul.de/Christian', nil, nil, SW_SHOW);
end;

procedure TFmAbout.LbDelphiASIOVSTClick(Sender: TObject);
begin
 ShellExecute(Handle, 'open', 'http://delphiasiovst.sourceforge.net', nil, nil, SW_SHOW);
end;

end.
