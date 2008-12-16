unit FPmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiBaseControl, DAV_GuiGraphXY;

type
  TFmFunctionPlot = class(TForm)
    GuiGraphXY: TGuiGraphXY;
    function FunctionPlotEvaluate(Sender: TObject; X: Double): Double;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  FmFunctionPlot: TFmFunctionPlot;

implementation

uses
  Math;

{$R *.dfm}

procedure TFmFunctionPlot.FormCreate(Sender: TObject);
begin
 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series) do
   begin
    OnEvaluate := FunctionPlotEvaluate;
   end;
end;

function TFmFunctionPlot.FunctionPlotEvaluate(Sender: TObject; X: Double): Double;
begin
 result := tanh(x);
end;

end.
