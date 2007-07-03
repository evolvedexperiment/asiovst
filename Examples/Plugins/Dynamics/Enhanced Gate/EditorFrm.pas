unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule, Controls,
  StdCtrls, DDial, Gauges;

type
  TEditorForm = class(TForm)
    GBMain: TGroupBox;
    LbThreshold: TLabel;
    DialThreshold: TDial;
    LbAttack: TLabel;
    DialAttack: TDial;
    LbHold: TLabel;
    DialHold: TDial;
    LbDecay: TLabel;
    DialDecay: TDial;
    EdThreshold: TEdit;
    EdAttack: TEdit;
    EdHold: TEdit;
    EdDecay: TEdit;
    CBOnOff: TCheckBox;
    CBDuck: TCheckBox;
    CBStereoLink: TCheckBox;
    GBSideChain: TGroupBox;
    LBLowCut: TLabel;
    DialLoCut: TDial;
    LBHighCut: TLabel;
    DialHiCut: TDial;
    EdLoCut: TEdit;
    EdHiCut: TEdit;
    CBSideChain: TComboBox;
    LbSource: TLabel;
    GBDynamics: TGroupBox;
    LbRatio: TLabel;
    DialRatio: TDial;
    LbKnee: TLabel;
    DialKnee: TDial;
    EdRatio: TEdit;
    EdKnee: TEdit;
    LbRange: TLabel;
    DialRange: TDial;
    EdRange: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    GaugeL: TGauge;
    Gauge1: TGauge;
    procedure CBOnOffClick(Sender: TObject);
    procedure CBDuckClick(Sender: TObject);
    procedure CBStereoLinkClick(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialHoldChange(Sender: TObject);
    procedure DialDecayChange(Sender: TObject);
    procedure DialLoCutChange(Sender: TObject);
    procedure DialHiCutChange(Sender: TObject);
  private
  public
    procedure UpdateThreshold;
    procedure UpdateAttack;
    procedure UpdateHold;
    procedure UpdateDecay;
    procedure UpdateHiCut;
    procedure UpdateLoCut;
  end;

implementation

{$R *.DFM}

uses Math, EnhancedGateDM;

procedure TEditorForm.CBOnOffClick(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner)
  do Parameter[0] := Integer(CBOnOff.Checked);
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner)
  do Parameter[1] := DialThreshold.Position;
 UpdateThreshold;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner)
  do Parameter[2] := Power(10, DialAttack.Position);
 UpdateAttack;
end;

procedure TEditorForm.DialHoldChange(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner)
  do Parameter[3] := Power(10, DialHold.Position);
 UpdateHold;
end;

procedure TEditorForm.CBDuckClick(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner)
  do Parameter[5] := Integer(CBDuck.Checked);
end;

procedure TEditorForm.CBStereoLinkClick(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner)
  do Parameter[6] := Integer(CBStereoLink.Checked);
end;

procedure TEditorForm.DialDecayChange(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner)
  do Parameter[4] := Power(10, DialDecay.Position);
 UpdateDecay;
end;

procedure TEditorForm.DialLoCutChange(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner)
  do Parameter[8] := Power(10, DialLoCut.Position);
 UpdateLoCut;
end;

procedure TEditorForm.DialHiCutChange(Sender: TObject);
begin
 with TEnhancedGateDataModule(Owner)
  do Parameter[9] := 0.001 * Power(10, DialHiCut.Position);
 UpdateHiCut;
end;

procedure TEditorForm.UpdateThreshold;
var i : Integer;
begin
 with TEnhancedGateDataModule(Owner) do
  begin
   if DialThreshold.Position <> Parameter[1]
    then DialThreshold.Position := Parameter[1];
   EdThreshold.Text := FloatToStrF(DialThreshold.Position, ffFixed, 5, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateAttack;
var i : Integer;
begin
 with TEnhancedGateDataModule(Owner) do
  begin
   if DialAttack.Position <> Log10(Parameter[2])
    then DialAttack.Position := Log10(Parameter[2]);
   i := Round(1.499999-DialAttack.Position);
   if i<0 then i:=0 else if i>2 then i:=2;
   EdAttack.Text := FloatToStrF(Parameter[2], ffFixed, 5, i) + ' ms';
  end;
end;

procedure TEditorForm.UpdateHold;
var i : Integer;
begin
 with TEnhancedGateDataModule(Owner) do
  begin
   if DialHold.Position <> Log10(Parameter[3])
    then DialHold.Position := Log10(Parameter[3]);
   i := Round(1.499999-DialHold.Position);
   if i<0 then i:=0 else if i>2 then i:=2;
   EdHold.Text := FloatToStrF(Parameter[3], ffFixed, 5, i) + ' s';
  end;
end;

procedure TEditorForm.UpdateDecay;
var i : Integer;
begin
 with TEnhancedGateDataModule(Owner) do
  begin
   if DialDecay.Position <> Log10(Parameter[4])
    then DialDecay.Position := Log10(Parameter[4]);
   i := Round(1.499999-DialDecay.Position);
   if i<0 then i:=0 else if i>2 then i:=2;
   EdDecay.Text := FloatToStrF(Parameter[4], ffFixed, 5, i) + ' ms';
  end;
end;

procedure TEditorForm.UpdateLoCut;
begin
 with TEnhancedGateDataModule(Owner) do
  begin
   if DialLoCut.Position <> Log10(Parameter[8])
    then DialLoCut.Position := Log10(Parameter[8]);
   if Parameter[8]<1000
    then EdLoCut.Text := FloatToStrF(Parameter[8], ffFixed, 5, Round(2.49999-Log10(Parameter[8]))) + ' Hz'
    else EdLoCut.Text := FloatToStrF(0.001*Parameter[8], ffFixed, 5, 1) + ' kHz';
  end;
end;

procedure TEditorForm.UpdateHiCut;
begin
 with TEnhancedGateDataModule(Owner) do
  begin
   if DialHiCut.Position <> Log10(1000*Parameter[9])
    then DialHiCut.Position := Log10(1000*Parameter[9]);
   if Parameter[9]<1000
    then EdHiCut.Text := FloatToStrF(1000*Parameter[9], ffFixed, 5, 0) + ' Hz'
    else EdHiCut.Text := FloatToStrF(Parameter[9], ffFixed, 5, Round(4.49999-Log10(Parameter[9])) ) + ' kHz';
  end;
end;

end.
