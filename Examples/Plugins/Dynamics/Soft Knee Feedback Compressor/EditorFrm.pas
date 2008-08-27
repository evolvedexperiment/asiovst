unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule, Controls,
  StdCtrls, DGuiBaseControl, DGuiDial, DGuiLabel;

type
  TEditorForm = class(TForm)
    LbThresholdValue: TLabel;
    LbRatioValue: TLabel;
    LbAttackValue: TLabel;
    LbReleaseValue: TLabel;
    DialThreshold: TGuiDial;
    DialRatio: TGuiDial;
    DialAttack: TGuiDial;
    DialRelease: TGuiDial;
    LbRatio: TGuiLabel;
    LbAttack: TGuiLabel;
    LbRelease: TGuiLabel;
    LbThreshold: TGuiLabel;
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
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
  DialRatio.DialBitmap.LoadFromStream(RS);     RS.Position := 0;
  DialAttack.DialBitmap.LoadFromStream(RS);    RS.Position := 0;
  DialRelease.DialBitmap.LoadFromStream(RS);   RS.Position := 0;
 finally
  RS.Free;
 end;
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
 TSoftKneeFeedbackCompressorDataModule(Owner).Parameter[0] := DialThreshold.Position;
 LbThresholdValue.Caption := FloatToStrF(DialThreshold.Position, ffFixed, 3, 1) + ' dB';
end;

procedure TEditorForm.DialRatioChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[1] := Power(10, 0.01 * DialRatio.Position);
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[1], ffFixed, 3, 1);
  end;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[2] := Power(10, 0.01 * DialAttack.Position);
   LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TEditorForm.DialReleaseChange(Sender: TObject);
begin
 with TSoftKneeFeedbackCompressorDataModule(Owner) do
  begin
   Parameter[3] := Power(10, 0.001 * DialRelease.Position);
   LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 5) + ' ms';
  end;
end;

end.
