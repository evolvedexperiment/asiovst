unit SimpleSampleDelayGUI;

interface

uses
  Windows, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_VSTModule,
  DAV_Common;

type
  TFmSimpleSampleDelay = class(TForm)
    SampleBar: TScrollBar;
    LbSamples: TLabel;
    LbFeedbackValue: TLabel;
    SBFeedback: TScrollBar;
    LbDryMixValue: TLabel;
    SBDryMix: TScrollBar;
    LbWetMixValue: TLabel;
    SBWetMix: TScrollBar;
    CBFeedbackInv: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure SampleBarChange(Sender: TObject);
    procedure SBFeedbackChange(Sender: TObject);
    procedure SBDryMixChange(Sender: TObject);
    procedure SBWetMixChange(Sender: TObject);
    procedure CBFeedbackInvClick(Sender: TObject);
  private
  public
    procedure UpdateDelayLength;
    procedure UpdateFeedback;
    procedure UpdateFeedbackInvert;
    procedure UpdateDryMix;
    procedure UpdateWetMix;
  end;

implementation

{$R *.DFM}

uses
  SimpleSampleDelayModule;

procedure TFmSimpleSampleDelay.FormShow(Sender: TObject);
begin
 UpdateDelayLength;
end;

procedure TFmSimpleSampleDelay.SampleBarChange(Sender: TObject);
begin
 with TSimpleSampleDelayVST(Owner) do
  begin
   if Parameter[0] <> SampleBar.Position
    then Parameter[0] := SampleBar.Position;
  end;
end;

procedure TFmSimpleSampleDelay.SBFeedbackChange(Sender: TObject);
begin
 with TSimpleSampleDelayVST(Owner) do
  begin
   if Parameter[1] <> 0.1 * SBFeedback.Position
    then Parameter[1] := 0.1 * SBFeedback.Position;
  end;
end;

procedure TFmSimpleSampleDelay.CBFeedbackInvClick(Sender: TObject);
begin
 with TSimpleSampleDelayVST(Owner) do
  begin
   if Parameter[2] <> Integer(CBFeedbackInv.Checked)
    then Parameter[2] := Integer(CBFeedbackInv.Checked);
  end;
end;

procedure TFmSimpleSampleDelay.SBDryMixChange(Sender: TObject);
begin
 with TSimpleSampleDelayVST(Owner) do
  begin
   if Parameter[3] <> 0.1 * SBDryMix.Position
    then Parameter[3] := 0.1 * SBDryMix.Position;
  end;
end;

procedure TFmSimpleSampleDelay.SBWetMixChange(Sender: TObject);
begin
 with TSimpleSampleDelayVST(Owner) do
  begin
   if Parameter[4] <> 0.1 * SBWetMix.Position
    then Parameter[4] := 0.1 * SBWetMix.Position;
  end;
end;

procedure TFmSimpleSampleDelay.UpdateDelayLength;
begin
 with TSimpleSampleDelayVST(Owner) do
  begin
   if round(Parameter[0]) <> SampleBar.Position
    then SampleBar.Position := round(Parameter[0]);
   LbSamples.Caption := 'Delay: ' + IntToStr(round(Parameter[0])) + ' samples ' +
                        '(= ' + FloatToStrF(1000 * Parameter[0] / SampleRate, ffGeneral, 4, 4) + ' ms)';
  end;
end;

procedure TFmSimpleSampleDelay.UpdateFeedback;
begin
 with TSimpleSampleDelayVST(Owner) do
  begin
   if round(10 * Parameter[1]) <> SBFeedback.Position
    then SBFeedback.Position := round(10 * Parameter[1]);
   UpdateFeedbackInvert;
   LbFeedbackValue.Caption := 'Feedback: ' + FloatToStrF(Parameter[1], ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmSimpleSampleDelay.UpdateFeedbackInvert;
begin
 CBFeedbackInv.Checked := TSimpleSampleDelayVST(Owner).Parameter[2] > 0.5;
end;

procedure TFmSimpleSampleDelay.UpdateDryMix;
begin
 with TSimpleSampleDelayVST(Owner) do
  begin
   if round(10 * Parameter[3]) <> SBDryMix.Position
    then SBDryMix.Position := round(10 * Parameter[3]);
   LbDryMixValue.Caption := 'Dry Mix: ' + FloatToStrF(Parameter[3], ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmSimpleSampleDelay.UpdateWetMix;
begin
 with TSimpleSampleDelayVST(Owner) do
  begin
   if round(10 * Parameter[4]) <> SBWetMix.Position
    then SBWetMix.Position := round(10 * Parameter[4]);
   LbWetMixValue.Caption := 'Wet Mix: ' + FloatToStrF(Parameter[4], ffGeneral, 3, 3) + ' %';
  end;
end;

end.
