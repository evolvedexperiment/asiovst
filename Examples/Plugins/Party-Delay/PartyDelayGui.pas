unit PartyDelayGui;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Graphics, Controls, StdCtrls,
  ComCtrls, DAV_Common, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmPartyDelay = class(TForm)
    CbActive: TCheckBox;
    CbFilterType: TComboBox;
    CbInvert: TCheckBox;
    DialFreqShift: TGuiDial;
    LbBalance: TLabel;
    LbBandwidth: TLabel;
    LbDelay: TLabel;
    LbDrive: TLabel;
    LbFeedback: TLabel;
    LbFilterType: TLabel;
    LbFreqShift: TLabel;
    LbFrequency: TLabel;
    LbGain: TLabel;
    LbLevel: TLabel;
    LbPan: TLabel;
    SbBalance: TScrollBar;
    SbDelay: TScrollBar;
    SbDrive: TScrollBar;
    SbFeedback: TScrollBar;
    SbFilterBW: TScrollBar;
    SbFilterGain: TScrollBar;
    SbFreqShift: TScrollBar;
    SbFrequency: TScrollBar;
    SbLevel: TScrollBar;
    SbPan: TScrollBar;
    TC: TTabControl;
    StatusBar: TStatusBar;
    procedure CbActiveClick(Sender: TObject);
    procedure CbInvertClick(Sender: TObject);
    procedure SbPanChange(Sender: TObject);
    procedure SbLevelChange(Sender: TObject);
    procedure SbDelayChange(Sender: TObject);
    procedure SbFeedbackChange(Sender: TObject);
    procedure CbFilterTypeChange(Sender: TObject);
    procedure SbFrequencyChange(Sender: TObject);
    procedure SbFilterGainChange(Sender: TObject);
    procedure SbFilterBWChange(Sender: TObject);
    procedure DialFreqShiftChange(Sender: TObject);
    procedure SbFreqShiftChange(Sender: TObject);
    procedure SbDriveChange(Sender: TObject);
    procedure SbBalanceChange(Sender: TObject);
    procedure TCChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateAll;
    procedure UpdateActive;
    procedure UpdateGain;
    procedure UpdatePan;
    procedure UpdateInvert;
    procedure UpdateDelay;
    procedure UpdateFeedback;
    procedure UpdateFilterType;
    procedure UpdateFilterFrequency;
    procedure UpdateFilterGain;
    procedure UpdateFilterBandwidth;
    procedure UpdateFrequencyShifter;
    procedure UpdateShiftFrequency;
    procedure UpdateDrive;
    procedure UpdateBalance;
  end;

implementation

{$R *.DFM}

uses
  PartyDelayDM;

procedure TFmPartyDelay.FormShow(Sender: TObject);
begin
 UpdateAll;
end;

procedure TFmPartyDelay.TCChange(Sender: TObject);
begin
 UpdateAll;
end;

procedure TFmPartyDelay.CbActiveClick(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 0;
   if Parameter[ParamNo] <> Integer(CbActive.Checked)
    then Parameter[ParamNo] := Integer(CbActive.Checked);
  end;
end;

procedure TFmPartyDelay.SbPanChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 1;
   if Parameter[ParamNo] <> SbPan.Position
    then Parameter[ParamNo] := SbPan.Position;
  end;
end;

procedure TFmPartyDelay.SbLevelChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 2;
   if Parameter[ParamNo] <> SbLevel.Position
    then Parameter[ParamNo] := SbLevel.Position;
  end;
end;

procedure TFmPartyDelay.CbInvertClick(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 3;
   if Parameter[ParamNo] <> Integer(CbActive.Checked)
    then Parameter[ParamNo] := Integer(CbActive.Checked);
  end;
end;

procedure TFmPartyDelay.SbDelayChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 4;
   if Parameter[ParamNo] <> SbDelay.Position
    then Parameter[ParamNo] := SbDelay.Position;
  end;
end;

procedure TFmPartyDelay.SbFeedbackChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 5;
   if Parameter[ParamNo] <> 0.1 * SbFeedback.Position
    then Parameter[ParamNo] := 0.1 * SbFeedback.Position;
  end;
end;

procedure TFmPartyDelay.CbFilterTypeChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 6;
   if Parameter[ParamNo] <> CbFilterType.ItemIndex
    then Parameter[ParamNo] := CbFilterType.ItemIndex;
  end;
end;

procedure TFmPartyDelay.SbFrequencyChange(Sender: TObject);
var
  ParamNo : Integer;
  FreqLin : Single;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 7;
   FreqLin := FreqLogToLinear(0.0001 * SbFrequency.Position);
   if Parameter[ParamNo] <> FreqLin
    then Parameter[ParamNo] := FreqLin;
  end;
end;

procedure TFmPartyDelay.SbFilterGainChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 8;
   if Parameter[ParamNo] <> 0.1 * SbFilterGain.Position
    then Parameter[ParamNo] := 0.1 * SbFilterGain.Position;
  end;
end;

procedure TFmPartyDelay.SbFilterBWChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 9;
   if Parameter[ParamNo] <> 0.1 * SbFilterBW.Position
    then Parameter[ParamNo] := 0.1 * SbFilterBW.Position;
  end;
end;

procedure TFmPartyDelay.DialFreqShiftChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 10;
   if Parameter[ParamNo] <> DialFreqShift.Position
    then Parameter[ParamNo] := DialFreqShift.Position;
  end;
end;

procedure TFmPartyDelay.SbFreqShiftChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 11;
   if Parameter[ParamNo] <> 0.1 * SbFreqShift.Position
    then Parameter[ParamNo] := 0.1 * SbFreqShift.Position;
  end;
end;

procedure TFmPartyDelay.SbDriveChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 13;
   if Parameter[ParamNo] <> 0.1 * SbDrive.Position
    then Parameter[ParamNo] := 0.1 * SbDrive.Position;
  end;
end;

procedure TFmPartyDelay.SbBalanceChange(Sender: TObject);
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 14;
   if Parameter[ParamNo] <> 0.1 * SbBalance.Position
    then Parameter[ParamNo] := 0.1 * SbBalance.Position;
  end;
end;


procedure TFmPartyDelay.UpdateAll;
begin
 UpdateActive;
 UpdateGain;
 UpdatePan;
 UpdateInvert;
 UpdateDelay;
 UpdateFeedback;
 UpdateFilterType;
 UpdateFilterFrequency;
 UpdateFilterGain;
 UpdateFilterBandwidth;
 UpdateFrequencyShifter;
 UpdateShiftFrequency;
 UpdateDrive;
 UpdateBalance;
end;

procedure TFmPartyDelay.UpdateActive;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 0;
   if CbActive.Checked <> (Parameter[ParamNo] > 0.5)
    then CbActive.Checked := Parameter[ParamNo] > 0.5;
  end;
end;

procedure TFmPartyDelay.UpdatePan;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 1;
   if SbPan.Position <> round(10 * Parameter[ParamNo])
    then SbPan.Position := round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateGain;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 2;
   if SbLevel.Position <> round(10 * Parameter[ParamNo])
    then SbLevel.Position := round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateInvert;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 3;
   if CbInvert.Checked <> (Parameter[ParamNo] > 0.5)
    then CbInvert.Checked := Parameter[ParamNo] > 0.5;
  end;
end;

procedure TFmPartyDelay.UpdateDelay;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 4;
   if SbDelay.Position <> round(10 * Parameter[ParamNo])
    then SbDelay.Position := round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateFeedback;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 5;
   if SbFeedback.Position <> round(10 * Parameter[ParamNo])
    then SbFeedback.Position := round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateFilterType;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 6;
   if CbFilterType.ItemIndex <> round(Parameter[ParamNo])
    then CbFilterType.ItemIndex := round(Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateFilterFrequency;
var
  ParamNo : Integer;
  FreqLog : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 7;
   FreqLog := round(10000 * FreqLinearToLog(Parameter[ParamNo]));
   if SbFrequency.Position <> FreqLog
    then SbFrequency.Position := FreqLog;
  end;
end;

procedure TFmPartyDelay.UpdateFilterGain;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 8;
   if SbFilterGain.Position <> round(10 * Parameter[ParamNo])
    then SbFilterGain.Position := round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateFilterBandwidth;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 9;
   if SbFilterBW.Position <> round(10 * Parameter[ParamNo])
    then SbFilterBW.Position := round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateFrequencyShifter;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 10;
   if DialFreqShift.Position <> Parameter[ParamNo]
    then DialFreqShift.Position := Parameter[ParamNo];
  end;
end;

procedure TFmPartyDelay.UpdateShiftFrequency;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 11;
   if SbFreqShift.Position <> round(10 * Parameter[ParamNo])
    then SbFreqShift.Position := round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateDrive;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 13;
   if SbDrive.Position <> round(10 * Parameter[ParamNo])
    then SbDrive.Position := round(10 * Parameter[ParamNo]);
  end;
end;

procedure TFmPartyDelay.UpdateBalance;
var
  ParamNo : Integer;
begin
 with TPartyDelayDataModule(Owner) do
  begin
   ParamNo := TC.TabIndex * ParametersPerBand + 14;
   if SbBalance.Position <> round(10 * Parameter[ParamNo])
    then SbBalance.Position := round(10 * Parameter[ParamNo]);
  end;
end;

end.
