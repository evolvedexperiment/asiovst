unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule, Controls,
  StdCtrls;

type
  TEditorForm = class(TForm)
    LbThreshold: TLabel;
    SBThreshold: TScrollBar;
    LbdB: TLabel;
    Label1: TLabel;
    LbRatio: TLabel;
    SBRatio: TScrollBar;
    procedure SBThresholdChange(Sender: TObject);
    procedure SBRatioChange(Sender: TObject);
  public
  end;

implementation

{$R *.DFM}

uses Math, SimpleLimiterDM;

procedure TEditorForm.SBThresholdChange(Sender: TObject);
begin
 TSimpleLimiterDataModule(Owner).Parameter[0] := SBThreshold.Position;
 LbdB.Caption := IntToStr(SBThreshold.Position) + ' dB';
end;

procedure TEditorForm.SBRatioChange(Sender: TObject);
begin
 with TSimpleLimiterDataModule(Owner) do
  begin
   Parameter[1] := Power(10, 0.01*SBRatio.Position);
   LbRatio.Caption := '1 : ' + FloatToStrF(Parameter[1], ffGeneral, 4, 4);
  end;
end;

end.