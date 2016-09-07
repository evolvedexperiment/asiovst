unit SaveAnimationUnit;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, Spin, Vcl.ExtCtrls;

type
  TFmSaveAnimation = class(TForm)
    BtCancel: TButton;
    BtOK: TButton;
    BtSelectDirectory: TButton;
    EdDirectory: TEdit;
    LbDimensions: TLabel;
    LbDirectory: TLabel;
    LbScaleFactor: TLabel;
    LbScaleFactorUnit: TLabel;
    SEScale: TSpinEdit;
    CbStyle: TComboBox;
    LbStyle: TLabel;
    PnSettingsAnimated: TPanel;
    SEHalfLife: TSpinEdit;
    LbHalfLife: TLabel;
    CbSkipFrames: TCheckBox;
    PnSettingsAdvanced: TPanel;
    LbMaxItems: TLabel;
    SeMaxAnimatedPrimitives: TSpinEdit;
    PnBlow: TPanel;
    CbInverted: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtOKClick(Sender: TObject);
    procedure BtSelectDirectoryClick(Sender: TObject);
    procedure EdDirectoryChange(Sender: TObject);
    procedure SEScaleChange(Sender: TObject);
    procedure CbStyleChange(Sender: TObject);
  private
    procedure CalculateAndDisplayDimensions;
  end;

implementation

{$R *.dfm}

uses
  Filectrl, Inifiles, MainUnit;

procedure TFmSaveAnimation.FormCreate(Sender: TObject);
begin
 EdDirectory.Text := ExtractFileDir(ParamStr(0));
 CalculateAndDisplayDimensions;
end;

procedure TFmSaveAnimation.FormShow(Sender: TObject);
begin
 with TIniFile.Create(FmPrimitivePictureEvolution.IniFileName) do
  try
   EdDirectory.Text := ReadString('Animation', 'Directory', EdDirectory.Text);
   CbStyle.ItemIndex := CbStyle.Items.IndexOf(ReadString('Animation', 'Style', CbStyle.Text));
   CbSkipFrames.Checked := ReadBool('Animation', 'Skip Frames', CbSkipFrames.Checked);
   SEHalfLife.Value := ReadInteger('Animation', 'Halflife', SEHalfLife.Value);
   SEScale.Value := ReadInteger('Animation', 'Scale', SEScale.Value);
   SeMaxAnimatedPrimitives.Value := ReadInteger('Animation', 'Maximum Primitives', SeMaxAnimatedPrimitives.Value);
  finally
   Free;
  end;
end;

procedure TFmSaveAnimation.BtOKClick(Sender: TObject);
begin
 with TIniFile.Create(FmPrimitivePictureEvolution.IniFileName) do
  try
   WriteString('Animation', 'Directory', EdDirectory.Text);
   WriteString('Animation', 'Style', CbStyle.Text);
   WriteBool('Animation', 'Skip Frames', CbSkipFrames.Checked);
   WriteInteger('Animation', 'Halflife', SEHalfLife.Value);
   WriteInteger('Animation', 'Maximum Primitives', SeMaxAnimatedPrimitives.Value);
   WriteInteger('Animation', 'Scale', SEScale.Value);
  finally
   Free;
  end;
end;

procedure TFmSaveAnimation.BtSelectDirectoryClick(Sender: TObject);
var
  Dir : string;
begin
 Dir := ExtractFileDir(ParamStr(0));
 if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0)
  then EdDirectory.Text := Dir;
end;

procedure TFmSaveAnimation.EdDirectoryChange(Sender: TObject);
begin
 BtOK.Enabled := DirectoryExists(EdDirectory.Text);
end;

procedure TFmSaveAnimation.CalculateAndDisplayDimensions;
begin
 with FmPrimitivePictureEvolution do
  begin
   LbDimensions.Caption := 'Dimensions: ' +
     IntToStr(Round(0.01 * SEScale.Value * ImageWidth)) + ' x ' +
     IntToStr(Round(0.01 * SEScale.Value * ImageHeight));
  end;
end;

procedure TFmSaveAnimation.CbStyleChange(Sender: TObject);
begin
  PnSettingsAnimated.Visible := CbStyle.ItemIndex = 1;
  PnSettingsAdvanced.Visible := CbStyle.ItemIndex = 2;
  PnBlow.Visible := CbStyle.ItemIndex = 3;
end;

procedure TFmSaveAnimation.SEScaleChange(Sender: TObject);
begin
 CalculateAndDisplayDimensions;
end;

end.

