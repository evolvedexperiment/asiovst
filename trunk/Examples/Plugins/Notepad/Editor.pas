unit Editor;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  Controls, StdCtrls;

type
  TFmNotepad = class(TForm)
    MeNotepad: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MeNotepadChange(Sender: TObject);
  end;

implementation

uses
  PluginDM;

{$R *.DFM}

procedure TFmNotepad.FormShow(Sender: TObject);
begin
 if Owner is TPluginDataModule then
  with TPluginDataModule(Owner)
   do MeNotepad.Text := Text;
end;

procedure TFmNotepad.MeNotepadChange(Sender: TObject);
begin
 if Owner is TPluginDataModule then
  with TPluginDataModule(Owner)
   do Text := MeNotepad.Text;
end;

procedure TFmNotepad.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if Owner is TPluginDataModule then
  with TPluginDataModule(Owner)
   do Text := MeNotepad.Text;
end;

end.
