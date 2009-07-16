unit AbxResultTableSetup;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons,
  ExtCtrls, Mask;

type
  TFmResultTableSetup = class(TForm)
    BtOK: TButton;
    BtCancel: TButton;
    Bevel: TBevel;
    LbColumnColor: TLabel;
    RBColorEverySecond: TRadioButton;
    RBColorAboveThreshold: TRadioButton;
    RBColorBelowThreshold: TRadioButton;
    LbRatingThreshold: TLabel;
    MERatingThreshold: TMaskEdit;
    procedure FormCreate(Sender: TObject);
    procedure MERatingThresholdChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FRatingThreshold: Single;
  public
    property RatingThreshold: Single read FRatingThreshold;
  end;

var
  FmResultTableSetup: TFmResultTableSetup;

implementation

uses
  IniFiles, AbxMain, AbxProject;

{$R *.dfm}

procedure TFmResultTableSetup.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i : Integer;
begin
 for i := 0 to FmAbxMain.MDIChildCount - 1
  do TFmProject(FmAbxMain.MDIChildren[i]).DBGridPro.Invalidate;
end;

procedure TFmResultTableSetup.FormCreate(Sender: TObject);
var
  str : string;
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   FRatingThreshold := ReadFloat('Table Setup', 'Rating Threshold', 0.25);
   str := ReadString('Table Setup', 'Color Columns', 'Every Second');
   RBColorEverySecond.Checked := str = 'Every Second';
   RBColorAboveThreshold.Checked := str = 'Above Rating Threshold';
   RBColorBelowThreshold.Checked := str = 'Below Rating Threshold';
  finally
   Free;
  end;
end;

procedure TFmResultTableSetup.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   WriteFloat('Table Setup', 'Rating Threshold', FRatingThreshold);
   if RBColorEverySecond.Checked
    then WriteString('Table Setup', 'Color Columns', 'Every Second');
   if RBColorAboveThreshold.Checked
    then WriteString('Table Setup', 'Color Columns', 'Above Rating Threshold');
   if RBColorBelowThreshold.Checked
    then WriteString('Table Setup', 'Color Columns', 'Below Rating Threshold');
  finally
   Free;
  end;
end;

procedure TFmResultTableSetup.MERatingThresholdChange(Sender: TObject);
begin
 try
  FRatingThreshold := StrToFloat(MERatingThreshold.Text);
  MERatingThreshold.Text := FloatToStr(FRatingThreshold);
 except
  MERatingThreshold.Text := '0,25';
 end;
end;

end.
