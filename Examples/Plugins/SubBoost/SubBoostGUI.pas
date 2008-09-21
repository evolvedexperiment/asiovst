unit SubBoostGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel, DAV_GuiSelectBox, ExtCtrls,
  DAV_GuiPanel;

type
  TFmSubBoost = class(TForm)
    DialDryMix: TGuiDial;
    DialLevel: TGuiDial;
    DialRelease: TGuiDial;
    DialThreshold: TGuiDial;
    DialTune: TGuiDial;
    GuiPanel1: TGuiPanel;
    LbDryMix: TGuiLabel;
    LbLevel: TGuiLabel;
    LbRelease: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbTitle: TGuiLabel;
    LbTune: TGuiLabel;
    LbType: TGuiLabel;
    SBType: TGuiSelectBox;
    LbTitleShadow: TGuiLabel;
    GuiPanel2: TGuiPanel;
    DialInputFilter: TGuiDial;
    DialFilterOrder: TGuiDial;
    GuiLabel2: TGuiLabel;
    LbFilterOrder: TGuiLabel;
    procedure DialDryMixChange(Sender: TObject);
    procedure DialLevelChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialTuneChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialFilterOrderChange(Sender: TObject);
  private
    fBackgrounBitmap : TBitmap;
  public
    procedure UpdateType;
    procedure UpdateLevel;
    procedure UpdateTune;
    procedure UpdateDryMix;
    procedure UpdateThreshold;
    procedure UpdateRelease;
  end;

var
  FmSubBoost: TFmSubBoost;

implementation

{$R *.dfm}

uses
  PngImage, SubBoostDM;

procedure TFmSubBoost.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  Line   : PRGB32Array;
  PngBmp : TPngObject;

begin
 // Create Background Image
 fBackgrounBitmap := TBitmap.Create;
 with fBackgrounBitmap do
  begin
   PixelFormat := pf32bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.9 * s[0] + 0.1 * random;
       b := round($3F * s[1]);
       s[0] := s[1];
       Line[x].B := b;
       Line[x].G := b;
       Line[x].R := b;
       Line[x].A := 0;
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'SubBoostKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialLevel.DialBitmap.Assign(PngBmp);
   DialTune.DialBitmap.Assign(PngBmp);
   DialDryMix.DialBitmap.Assign(PngBmp);
   DialThreshold.DialBitmap.Assign(PngBmp);
   DialRelease.DialBitmap.Assign(PngBmp);
  finally
   FreeAndNil(RS);
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmSubBoost.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, fBackgrounBitmap);
end;

procedure TFmSubBoost.FormShow(Sender: TObject);
begin
 UpdateType;
 UpdateLevel;
 UpdateTune;
 UpdateDryMix;
 UpdateThreshold;
 UpdateRelease;
 LbTitleShadow.Transparent := True;
 LbTitle.Transparent := True;
 LbType.Transparent := True;
end;

procedure TFmSubBoost.SBTypeChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[0] <> SBType.ItemIndex
    then Parameter[0] := SBType.ItemIndex;
  end;
end;

procedure TFmSubBoost.DialFilterOrderChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[6] <> DialFilterOrder.Position
    then Parameter[6] := DialFilterOrder.Position;
   LbFilterOrder.Caption := IntToStr(round(DialFilterOrder.Position));
  end;
end;

procedure TFmSubBoost.DialLevelChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[1] <> DialLevel.Position
    then Parameter[1] := DialLevel.Position;
  end;
end;

procedure TFmSubBoost.DialTuneChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[2] <> DialTune.Position
    then Parameter[2] := DialTune.Position;
   LbTune.Caption := FloatToStrF(Parameter[2], ffGeneral, 3, 3);
  end;
end;

procedure TFmSubBoost.DialDryMixChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[3] <> DialDryMix.Position
    then Parameter[3] := DialDryMix.Position;
  end;
end;

procedure TFmSubBoost.DialThresholdChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[4] <> DialThreshold.Position
    then Parameter[4] := DialThreshold.Position;
  end;
end;

procedure TFmSubBoost.DialReleaseChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[5] <> DialRelease.Position
    then Parameter[5] := DialRelease.Position;
  end;
end;

procedure TFmSubBoost.UpdateType;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[0] <> round(SBType.ItemIndex)
    then SBType.ItemIndex := round(Parameter[0]);
  end;
end;

procedure TFmSubBoost.UpdateLevel;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialLevel.Position <> Parameter[1] 
    then DialLevel.Position := Parameter[1];
  end;
end;

procedure TFmSubBoost.UpdateTune;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialTune.Position <> Parameter[2]
    then DialTune.Position := Parameter[2];
  end;
end;

procedure TFmSubBoost.UpdateDryMix;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialDryMix.Position <> Parameter[3] 
    then DialDryMix.Position := Parameter[3];
  end;
end;

procedure TFmSubBoost.UpdateRelease;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialRelease.Position <> Parameter[4]
    then DialRelease.Position := Parameter[4];
  end;
end;

procedure TFmSubBoost.UpdateThreshold;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialThreshold.Position <> Parameter[5] 
    then DialThreshold.Position := Parameter[5];
  end;
end;

end.
