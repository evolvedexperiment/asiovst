unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Types,
  DAV_VSTModule;

type
  TEditorForm = class(TForm)
    LbThreshold: TLabel;
    ScrollBar: TScrollBar;
    LbdB: TLabel;
    procedure ScrollBarChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateScrollBar;
  end;

implementation

{$R *.DFM}

uses
  SimpleGateDM;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateScrollBar;
end;

procedure TEditorForm.ScrollBarChange(Sender: TObject);
begin
 TSimpleGateDataModule(Owner).Parameter[0] := ScrollBar.Position;
 LbdB.Caption := IntToStr(ScrollBar.Position) + ' dB';
end;

procedure TEditorForm.UpdateScrollBar;
begin
 with Owner as TSimpleGateDataModule do
  begin
   if Round(Parameter[0]) <> ScrollBar.Position
    then ScrollBar.Position := Round(Parameter[0]);
   LbdB.Caption := IntToStr(ScrollBar.Position) + ' dB';
  end;
end;

end.