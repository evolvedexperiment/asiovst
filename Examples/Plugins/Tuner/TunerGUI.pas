unit TunerGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Graphics, DAV_Common,
  DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel, Controls, ExtCtrls;

type
  TFmTuner = class(TForm)
    PBDisplay: TPaintBox;
    LbLowE: TGuiLabel;
    LbA: TGuiLabel;
    LbD: TGuiLabel;
    LbG: TGuiLabel;
    LbH: TGuiLabel;
    LbE: TGuiLabel;
    LbGuitarTuning: TGuiLabel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbNoteClick(Sender: TObject);
    procedure PBDisplayPaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FBackgrounBitmap   : TBitmap;
    FNeedlePosition    : Single;
    FOldNeedlePosition : Integer;
  end;

implementation

{$R *.DFM}

uses
  DAV_GuiCommon;

procedure TFmTuner.FormCreate(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  Line   : PRGB24Array;
//  RS     : TResourceStream;
//  PngBmp : TPngObject;

begin
 // Create Background Image
 FBackgrounBitmap := TBitmap.Create;
 with FBackgrounBitmap do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
       b := round($9F + $1A * s[1]);
       s[0] := s[1];
       Line[x].B := b;
       Line[x].G := b;
       Line[x].R := b;
      end;
    end;
  end;

(*
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'TwoBandKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with DIL.DialImages.Add do
    begin
     DialBitmap.Canvas.Brush.Color := $696969;
     DialBitmap.Assign(PngBmp);
     NumGlyphs := 65;
    end;
   DialFreq.DialImageIndex := 0;
   DialOrder.DialImageIndex := 0;
   DialHighDist.DialImageIndex := 0;
   DialLowDist.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
*)
// PBDisplay.ControlStyle := PBDisplay.ControlStyle + [csOpaque];
end;

procedure TFmTuner.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmTuner.FormShow(Sender: TObject);
begin
 LbLowE.Transparent := True;
 LbA.Transparent := True;
 LbD.Transparent := True;
 LbG.Transparent := True;
 LbH.Transparent := True;
 LbE.Transparent := True;
 LbGuitarTuning.Transparent := True;
end;

procedure TFmTuner.LbNoteClick(Sender: TObject);
begin
 if Sender <> LbLowE then LbLowE.Font.Color := $4F4F4F else LbLowE.Font.Color := clBlack;
 if Sender <> LbA then LbA.Font.Color := $4F4F4F else LbA.Font.Color := clBlack;
 if Sender <> LbD then LbD.Font.Color := $4F4F4F else LbD.Font.Color := clBlack;
 if Sender <> LbG then LbG.Font.Color := $4F4F4F else LbG.Font.Color := clBlack;
 if Sender <> LbH then LbH.Font.Color := $4F4F4F else LbH.Font.Color := clBlack;
 if Sender <> LbE then LbE.Font.Color := $4F4F4F else LbE.Font.Color := clBlack;
end;

procedure TFmTuner.PBDisplayPaint(Sender: TObject);
var
  NeedlePosition : Integer;
begin
 FNeedlePosition := 0.9 * FNeedlePosition + 0.1 * ((2 * random) - 1);

 NeedlePosition := round( (PBDisplay.Width div 2) * FNeedlePosition);
 FOldNeedlePosition := NeedlePosition;

 with PBDisplay.Canvas do
  begin
   Lock;

   // main line
   Pen.Color := clBlack;
(*
   Pen.Mode := pmNot;
   MoveTo((PBDisplay.Width div 2) + FOldNeedlePosition, 0);
   LineTo((PBDisplay.Width div 2) + FOldNeedlePosition, PBDisplay.Height);
*)

   MoveTo((PBDisplay.Width div 2) + NeedlePosition, 0);
   LineTo((PBDisplay.Width div 2) + NeedlePosition, PBDisplay.Height);

   // side lines
   Pen.Color := $4F4F4F;
   MoveTo((PBDisplay.Width div 2) + NeedlePosition - 1, 0);
   LineTo((PBDisplay.Width div 2) + NeedlePosition - 1, PBDisplay.Height);
   MoveTo((PBDisplay.Width div 2) + NeedlePosition + 1, 0);
   LineTo((PBDisplay.Width div 2) + NeedlePosition + 1, PBDisplay.Height);

   Unlock;
  end;
end;

procedure TFmTuner.TimerTimer(Sender: TObject);
begin
 PBDisplay.Invalidate;
end;

end.