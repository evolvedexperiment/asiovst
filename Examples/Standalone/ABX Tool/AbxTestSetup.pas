unit AbxTestSetup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TFmTestSetup = class(TForm)
    BtCancel: TButton;
    BtGo: TButton;
    CBAllowNavigation: TCheckBox;
    EdNameID: TEdit;
    LbNameID: TLabel;
    LbNumberOfTrials: TLabel;
    SENumberOfTrials: TSpinEdit;
    procedure BtGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

implementation

uses
  IniFiles, AbxTest, AbxMain;

{$R *.dfm}

procedure TFmTestSetup.BtGoClick(Sender: TObject);
begin
 with TFmAbxTest.Create(Self) do
  try
   TrialCount := SENumberOfTrials.Value;
   NameID := EdNameID.Text;
   ShowModal;
  finally
   Free; 
  end;
end;

function GetUserName: string;
var
  UserName  : array [0..255] of Char;
  dw        : DWord;
begin
 dw := SizeOf(UserName);
 Windows.GetUserName(@UserName, dw);
 result := Username;
end;

procedure TFmTestSetup.FormCreate(Sender: TObject);
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   if not ValueExists('Test Setup', 'Name/ID') then
    try
     EdNameID.Text := GetUserName;
    except
    end;
   SENumberOfTrials.Value := ReadInteger('Test Setup', 'Number of Trials', SENumberOfTrials.Value);
   EdNameID.Text := ReadString('Test Setup', 'Name/ID', EdNameID.Text);
   CBAllowNavigation.Checked := ReadBool('Test Setup', 'Allow Navigation', CBAllowNavigation.Checked);
  finally
   Free;
  end;
end;

procedure TFmTestSetup.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   WriteInteger('Test Setup', 'Number of Trials', SENumberOfTrials.Value);
   WriteString('Test Setup', 'Name/ID', EdNameID.Text);
   WriteBool('Test Setup', 'Allow Navigation', CBAllowNavigation.Checked);
  finally
   Free;
  end;
end;

end.
