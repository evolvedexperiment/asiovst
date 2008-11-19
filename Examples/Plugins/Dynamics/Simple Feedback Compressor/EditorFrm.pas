unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, ExtCtrls, Controls, StdCtrls,
  DAV_Common, DAV_VSTModule, DAV_GuiLabel, DAV_GuiPanel, DAV_GuiBaseControl,
  DAV_GuiDial;

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
    Panel: TGuiPanel;
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateThreshold;
    procedure UpdateAttack;
    procedure UpdateRatio;
    procedure UpdateRelease;
  end;

implementation

{$R *.DFM}

uses
  Math, SimpleFeedbackCompressorDM;

procedure TEditorForm.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'SimpleFeedbackCompressor', 'BMP');
 try
  DialThreshold.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DialRatio.DialBitmap.LoadFromStream(RS);     RS.Position := 0;
  DialAttack.DialBitmap.LoadFromStream(RS);    RS.Position := 0;
  DialRelease.DialBitmap.LoadFromStream(RS);   RS.Position := 0;
 finally
  RS.Free;
 end;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateThreshold;
 UpdateAttack;
 UpdateRatio;
 UpdateRelease;
end;

procedure TEditorForm.UpdateThreshold;
begin
 LbThresholdValue.Caption := FloatToStrF(DialThreshold.Position, ffFixed, 3, 1) + ' dB';
end;

procedure TEditorForm.UpdateRatio;
begin
 with TSimpleFeedbackCompressorDataModule(Owner)
  do LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[1], ffFixed, 3, 1);
end;

procedure TEditorForm.UpdateAttack;
var
  TempAttack : Single;
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
  begin
   TempAttack := 100 * Log10(Parameter[2]);
   if Parameter[2] <> TempAttack
    then DialAttack.Position := TempAttack;
   if Parameter[2] < 1
    then LbAttackValue.Caption := FloatToStrF(1000 * Parameter[2], ffGeneral, 4, 2) + ' µs'
    else LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateRelease;
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
 if Parameter[3] < 1000
   then LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 5) + ' ms'
   else LbReleaseValue.Caption := FloatToStrF(0.001 * Parameter[3], ffGeneral, 4, 5) + ' s'
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[0] := DialThreshold.Position;
   UpdateThreshold;
  end;
end;

procedure TEditorForm.DialRatioChange(Sender: TObject);
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[1] := Power(10, 0.01 * DialRatio.Position);
   UpdateRatio;
  end;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[2] := Power(10, 0.01 * DialAttack.Position);
   UpdateAttack;
  end;
end;

procedure TEditorForm.DialReleaseChange(Sender: TObject);
begin
 with TSimpleFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[3] := Power(10, 0.001 * DialRelease.Position);
   UpdateRelease;
  end;
end;

end.
