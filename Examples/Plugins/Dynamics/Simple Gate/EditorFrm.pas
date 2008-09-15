unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Common,
  DAV_VSTModule;

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

uses
  SimpleGateDM;

procedure TEditorForm.ScrollBarChange(Sender: TObject);
begin
 TSimpleGateDataModule(Owner).Parameter[0] := ScrollBar.Position;
 LbdB.Caption := IntToStr(ScrollBar.Position) + ' dB';
end;

end.