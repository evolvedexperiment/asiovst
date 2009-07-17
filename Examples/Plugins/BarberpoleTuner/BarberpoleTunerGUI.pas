unit BarberpoleTunerGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, Graphics,
  DAV_Common, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel, ExtCtrls;

type
  TFmBarberpoleTuner = class(TForm)
    LbDisplay: TGuiLabel;
    Barberpole: TPaintBox;
    Timer: TTimer;
    LbGuitarTuning: TGuiLabel;
    LbLowE: TGuiLabel;
    LbA: TGuiLabel;
    LbD: TGuiLabel;
    LbG: TGuiLabel;
    LbH: TGuiLabel;
    LbE: TGuiLabel;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure BarberpolePaint(Sender: TObject);
    procedure LbNoteClick(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  end;

implementation

uses
  DAV_GuiCommon, BarberpoleTunerDM;

{$R *.DFM}

procedure TFmBarberpoleTuner.FormCreate(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
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
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.1 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       Line[x].B := round($70 - $34 * (s[1] - h));
       Line[x].G := round($84 - $48 * (s[1] - h));
       Line[x].R := round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
 Barberpole.ControlStyle := Barberpole.ControlStyle + [csOpaque];  
end;

procedure TFmBarberpoleTuner.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmBarberpoleTuner.FormShow(Sender: TObject);
begin
 // workaround, please remove these lines if fixed!!!
 LbDisplay.Width := LbDisplay.Width + 1;
 LbGuitarTuning.Width := LbGuitarTuning.Width + 1;
 LbGuitarTuning.Width := LbGuitarTuning.Width + 1;
 LbLowE.Height := LbLowE.Height - 1;
 LbA.Height := LbA.Height - 1;
 LbD.Height := LbD.Height - 1;
 LbG.Height := LbG.Height - 1;
 LbH.Height := LbH.Height - 1;
 LbE.Height := LbE.Height - 1;
end;

procedure TFmBarberpoleTuner.LbNoteClick(Sender: TObject);
begin
 if Sender <> LbLowE then LbLowE.Font.Color := $4F4F4F else begin LbLowE.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 1; end;
 if Sender <> LbA then LbA.Font.Color := $4F4F4F else begin LbA.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 2; end;
 if Sender <> LbD then LbD.Font.Color := $4F4F4F else begin LbD.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 3; end;
 if Sender <> LbG then LbG.Font.Color := $4F4F4F else begin LbG.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 4; end;
 if Sender <> LbH then LbH.Font.Color := $4F4F4F else begin LbH.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 5; end;
 if Sender <> LbE then LbE.Font.Color := $4F4F4F else begin LbE.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 6; end;
end;

procedure TFmBarberpoleTuner.TimerTimer(Sender: TObject);
begin
 Barberpole.Invalidate;
end;

procedure TFmBarberpoleTuner.BarberpolePaint(Sender: TObject);
var
  Column : Integer;
begin
 with Barberpole.Canvas do
  begin
   Pen.Color := clBlack;
   Pen.Style := psSolid;
   Brush.Color := clBlack;
   Brush.Style := bsSolid;
   FrameRect(Barberpole.ClientRect);
  end;

 with TBarberpoleTunerDataModule(Owner) do
  for Column := 0 to Barberpole.Width - 3 do
   begin
    Barberpole.Canvas.Pen.Color := round($70 - $34 * BufferPointer^[Column]) shl 16 +
                                   round($84 - $48 * BufferPointer^[Column]) shl  8 +
                                   round($8D - $50 * BufferPointer^[Column]);
//    Barberpole.Canvas.Pen.Color := TColor(round($64 + BufferPointer^[Column] * $60));
    Barberpole.Canvas.MoveTo(Column + 1, 1);
    Barberpole.Canvas.LineTo(Column + 1, Barberpole.Height - 1);
   end;
end;

end.