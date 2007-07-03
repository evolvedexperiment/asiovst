unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule, Controls,
  StdCtrls;

type
  TEditorForm = class(TForm)
    LbThreshold: TLabel;
    ScrollBar: TScrollBar;
    LbdB: TLabel;
    procedure ScrollBarChange(Sender: TObject);
  public
  end;

implementation

{$R *.DFM}

uses SimpleLimiterDM;

procedure TEditorForm.ScrollBarChange(Sender: TObject);
begin
 TSimpleLimiterDataModule(Owner).Parameter[0] := ScrollBar.Position;
 LbdB.Caption := IntToStr(ScrollBar.Position) + ' dB';
end;

end.