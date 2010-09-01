unit ButtonTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_GuiBaseControl, DAV_GuiButton, ComCtrls;

type
  TFmButton = class(TForm)
    ButtonA: TGuiButton;
    ButtonB: TGuiButton;
    ButtonC: TGuiButton;
    ButtonD: TGuiButton;
    CbTransparent: TCheckBox;
    TbLineWidth: TTrackBar;
    LbLineWidth: TLabel;
    TbRadius: TTrackBar;
    LbRadius: TLabel;
    procedure CbTransparentClick(Sender: TObject);
    procedure TbLineWidthChange(Sender: TObject);
    procedure TbRadiusChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  end;

var
  FmButton: TFmButton;

implementation

uses
  DAV_GuiCommon;

{$R *.dfm}

procedure TFmButton.FormCreate(Sender: TObject);
begin
 FBackgrounBitmap := TBitmap.Create;
end;

procedure TFmButton.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmButton.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmButton.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
begin
 with FBackgrounBitmap do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.1 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       Line[x].B := Round($70 - $34 * (s[1] - h));
       Line[x].G := Round($84 - $48 * (s[1] - h));
       Line[x].R := Round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmButton.TbLineWidthChange(Sender: TObject);
begin
 ButtonA.LineWidth := TbLineWidth.Position;
 ButtonB.LineWidth := TbLineWidth.Position;
 ButtonC.LineWidth := TbLineWidth.Position;
 ButtonD.LineWidth := TbLineWidth.Position;
end;

procedure TFmButton.CbTransparentClick(Sender: TObject);
begin
 ButtonA.Transparent := CbTransparent.Checked;
 ButtonB.Transparent := CbTransparent.Checked;
 ButtonC.Transparent := CbTransparent.Checked;
 ButtonD.Transparent := CbTransparent.Checked;
end;

procedure TFmButton.TbRadiusChange(Sender: TObject);
begin
 ButtonA.Radius := TbRadius.Position;
 ButtonB.Radius := TbRadius.Position;
 ButtonC.Radius := TbRadius.Position;
 ButtonD.Radius := TbRadius.Position;
end;

end.
