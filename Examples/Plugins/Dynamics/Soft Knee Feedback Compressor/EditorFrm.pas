unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Types,
  DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TEditorForm = class(TForm)
    DialAttack: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    DialThreshold: TGuiDial;
    LbAttack: TGuiLabel;
    LbAttackValue: TLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TLabel;
    LbMakeUpValue: TLabel;
    DialMakeUp: TGuiDial;
    LbMakeUp: TGuiLabel;
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialMakeUpChange(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateMakeUpGain;
    procedure UpdateRatio;
    procedure UpdateRelease;
    procedure UpdateThreshold;
  end;

implementation

{$R *.DFM}

uses
  Math, SoftKneeFeedbackCompressorDM;

procedure TEditorForm.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'RoundKnob', 'BMP');
 try
  DialThreshold.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DialRatio.DialBitmap.Assign(DialThreshold.DialBitmap);
  DialAttack.DialBitmap.Assign(DialThreshold.DialBitmap);
  DialRelease.DialBitmap.Assign(DialThreshold.DialBitmap);
  DialMakeUp.DialBitmap.Assign(DialThreshold.DialBitmap);
 finally
  RS.Free;
 end;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateMakeUpGain;
 UpdateRatio;
 UpdateRelease;
 UpdateThreshold;
end;

procedure TEditorForm.UpdateThreshold;
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   if Parameter[0] <> DialThreshold.Position
    then DialThreshold.Position := Parameter[0];
   LbThresholdValue.Caption := FloatToStrF(Parameter[0], ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateAttack;
var
  AttackTemp : Single;
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   AttackTemp := 100 * Log10(Parameter[2]);
   if DialAttack.Position <> AttackTemp
    then DialAttack.Position := AttackTemp;
   LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateMakeUpGain;
var
  MakeUpTemp : Single;
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   MakeUpTemp := Parameter[4];
   if DialMakeUp.Position <> MakeUpTemp
    then DialMakeUp.Position := MakeUpTemp;
   LbMakeUpValue.Caption := FloatToStrF(Parameter[4], ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateRatio;
var
  RatioTemp : Single;
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   RatioTemp := 100 * Log10(Parameter[1]);
   if DialRatio.Position <> RatioTemp
    then DialRatio.Position := RatioTemp;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[1], ffFixed, 3, 1);
  end;
end;

procedure TEditorForm.UpdateRelease;
var
  ReleaseTemp : Single;
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   ReleaseTemp := 1000 * Log10(Parameter[3]);
   if DialRelease.Position <> ReleaseTemp
    then DialRelease.Position := ReleaseTemp;
   LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 5) + ' ms';
  end;
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[0] := DialThreshold.Position;
  end;
end;

procedure TEditorForm.DialRatioChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[1] := Power(10, 0.01 * DialRatio.Position);
  end;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[2] := Power(10, 0.01 * DialAttack.Position);
  end;
end;

procedure TEditorForm.DialReleaseChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[3] := Power(10, 0.001 * DialRelease.Position);
  end;
end;

procedure TEditorForm.DialMakeUpChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[4] := DialMakeUp.Position;
  end;
end;

end.
