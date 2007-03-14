unit LunchBoxVST;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFmVST = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    EdRealtimeVST: TEdit;
    Label1: TLabel;
    BtRealtimeVST: TButton;
    EdOutputVST: TEdit;
    Label2: TLabel;
    BtOutputVST: TButton;
    BtOutputEditor: TButton;
    BtRealtimeEditor: TButton;
    procedure EdRealtimeVSTChange(Sender: TObject);
    procedure BtOutputEditorClick(Sender: TObject);
    procedure BtRealtimeEditorClick(Sender: TObject);
    procedure EdOutputVSTChange(Sender: TObject);
    procedure BtRealtimeVSTClick(Sender: TObject);
    procedure BtOutputVSTClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmVST: TFmVST;

implementation

uses LunchBoxMain;

{$R *.dfm}

procedure TFmVST.BtOutputVSTClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   Name := 'OD';
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select a VST Plugin';
   if Execute then
    begin
     EdOutputVST.Text:=FileName;
    end;
  finally
   Free;
  end;
end;

procedure TFmVST.BtRealtimeVSTClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   Name := 'OD';
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select a VST Plugin';
   if Execute then
    begin
     EdRealtimeVST.Text:=FileName;
    end;
  finally
   Free;
  end;
end;

procedure TFmVST.BtRealtimeEditorClick(Sender: TObject);
begin
 if FmLunchBox.VSTHost[0].Active
  then FmLunchBox.VSTHost[0].ShowEdit;
end;

procedure TFmVST.BtOutputEditorClick(Sender: TObject);
begin
 if FmLunchBox.VSTHost[1].Active
  then FmLunchBox.VSTHost[1].ShowEdit;
end;

procedure TFmVST.EdRealtimeVSTChange(Sender: TObject);
begin
 with FmLunchBox.VSTHost[0] do
  begin
   Active:=False;
   if FileExists(EdRealtimeVST.Text) then
    begin
     DLLFileName:=EdRealtimeVST.Text;
     Active:=True;
    end;
   BtRealtimeEditor.Enabled:=Active;
  end;
end;

procedure TFmVST.EdOutputVSTChange(Sender: TObject);
begin
 with FmLunchBox.VSTHost[1] do
  begin
   Active:=False;
   if FileExists(EdOutputVST.Text) then
    begin
     DLLFileName:=EdOutputVST.Text;
     Active:=True;
    end;
   BtOutputEditor.Enabled:=Active;
  end;
end;

end.
