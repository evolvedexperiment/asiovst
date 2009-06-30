unit NSFDmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_Common, DAV_DspNoiseShapingFilterDesigner, Menus, StdCtrls;

type
  TFmNoiseshapingFilterDesigner = class(TForm)
    Memo: TMemo;
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    MISaveAs: TMenuItem;
    MICalculation: TMenuItem;
    procedure MIExitClick(Sender: TObject);
    procedure MICalculationClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
  private
    procedure CoefficientUptdate(Sender: TObject; Coefficients: PDAVDoubleFixedArray; Best: Double);
  end;

var
  FmNoiseshapingFilterDesigner: TFmNoiseshapingFilterDesigner;

implementation

{$R *.dfm}

procedure TFmNoiseshapingFilterDesigner.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmNoiseshapingFilterDesigner.MISaveAsClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := 'txt';
   Filter := 'Text (*.txt)|*.txt';
   if Execute then
    begin
     Memo.Lines.SaveToFile(Filename);
    end;
  finally
   Free
  end;
end;

procedure TFmNoiseshapingFilterDesigner.CoefficientUptdate(Sender: TObject; Coefficients: PDAVDoubleFixedArray; Best: Double);
var
  Sample : Integer;
begin
 with Sender as TNoiseShapingFilterDesigner do
  begin
   for Sample := 0 to SampleFrames - 1
    do Memo.Lines.Add('Coefficient[' + IntToStr(Sample) + '] = ' + FloatToStr(Coefficients[Sample]));
   Memo.Lines.Add('Best = ' + FloatToStr(Best));
  end;
 Application.ProcessMessages;
end;

procedure TFmNoiseshapingFilterDesigner.MICalculationClick(Sender: TObject);
begin
 with TNoiseShapingFilterDesigner.Create do
  try
   OnCoefficientUpdate := CoefficientUptdate;
   Calculate;
  finally
   Free;
  end;
end;

end.
