unit PTGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule, Controls,
  StdCtrls, ExtCtrls;

type
  TFmPerformanceTest = class(TForm)
    BtPatchFunctionCalls: TButton;
    LbPerformance: TLabel;
    LbCycles: TLabel;
    Timer: TTimer;
    procedure BtPatchFunctionCallsClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses PTDM;

procedure TFmPerformanceTest.BtPatchFunctionCallsClick(Sender: TObject);
begin
 with TPerformanceTestModule(Owner)
  do PatchProcessCalls;
end;

procedure TFmPerformanceTest.TimerTimer(Sender: TObject);
begin
 with TPerformanceTestModule(Owner)
  do LbCycles.Caption := FloatToStrF(Cycles, ffGeneral, 8, 8);
end;

end.