unit EditorFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, PNGImage,
  DAV_Common, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TEditorForm = class(TForm)
    DialAttack: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    DialSoftKnee: TGuiDial;
    DialThreshold: TGuiDial;
    LbThreshold: TGuiLabel;
    LbRatio: TGuiLabel;
    LbAttack: TGuiLabel;
    LbRelease: TGuiLabel;
    LbSoftKnee: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbSoftKneeValue: TGuiLabel;
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialSoftKneeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateRatio;
    procedure UpdateRelease;
    procedure UpdateSoftKnee;
    procedure UpdateThreshold;
  end;

implementation

{$R *.DFM}

uses
  Math, Graphics, SKLDM;

procedure TEditorForm.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
  PNG : TPNGObject;
begin
 RS := TResourceStream.Create(hInstance, 'NuNop', 'PNG');
 try
  PNG := TPNGObject.Create;
  with PNG do
   try
    LoadFromStream(RS);
    with DialThreshold.DialBitmap do
     begin
(*
      PixelFormat := pf32bit;
      Transparent := True;
      TransparentMode := tmFixed;
      TransparentColor := clGray;
      DialThreshold.DialBitmap.Assign(Png);
*)
      SetSize(PNG.Width, PNG.Height);
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Draw(0, 0, PNG);
     end;

    with DialRatio.DialBitmap do
     begin
      SetSize(PNG.Width, PNG.Height);
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Draw(0, 0, PNG);
     end;

    with DialAttack.DialBitmap do
     begin
      SetSize(PNG.Width, PNG.Height);
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Draw(0, 0, PNG);
     end;

    with DialRelease.DialBitmap do
     begin
      SetSize(PNG.Width, PNG.Height);
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Draw(0, 0, PNG);
     end;

    with DialSoftKnee.DialBitmap do
     begin
      SetSize(PNG.Width, PNG.Height);
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Draw(0, 0, PNG);
     end;
   finally
    FreeAndNil(PNG);
   end;
 finally
  RS.Free;
 end;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateRatio;
 UpdateRelease;
 UpdateSoftKnee;
 UpdateThreshold;
end;

procedure TEditorForm.UpdateRatio;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[1], ffGeneral, 3, 2);
  end;
end;

procedure TEditorForm.UpdateAttack;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if Parameter[2] < 1
    then LbAttackValue.Caption := FloatToStrF(Parameter[2] * 1E3, ffGeneral, 4, 2) + ' μs'
    else LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateRelease;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if Parameter[3] >= 1000
    then LbReleaseValue.Caption := FloatToStrF(Parameter[3] * 1E-3, ffGeneral, 3, 2) + ' s'
    else LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 3, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateSoftKnee;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   LbSoftKneeValue.Caption := FloatToStrF(Parameter[4], ffGeneral, 4, 5);
  end;
end;

procedure TEditorForm.UpdateThreshold;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   LbThresholdValue.Caption := FloatToStrF(DialThreshold.Position, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[0] := DialThreshold.Position;
   UpdateThreshold;
  end;
end;

procedure TEditorForm.DialRatioChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[1] := Power(10, DialRatio.Position);
   UpdateRatio;
  end;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[2] := Power(10, DialAttack.Position);
   UpdateAttack;
  end;
end;

procedure TEditorForm.DialReleaseChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[3] := Power(10, DialRelease.Position);
   UpdateRelease;
  end;
end;

procedure TEditorForm.DialSoftKneeChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[4] := DialSoftKnee.Position;
   UpdateSoftKnee;
  end;
end;

end.
