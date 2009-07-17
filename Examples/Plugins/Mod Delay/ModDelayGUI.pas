unit ModDelayGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Graphics, DAV_Common,
  DAV_VSTModule, Controls, DAV_GuiBaseControl, DAV_GuiLabel, StdCtrls;

type
  TFmModDelay = class(TForm)
    LbGain: TGuiLabel;
    LbMix: TGuiLabel;
    LbLpf: TGuiLabel;
    LbDelay: TGuiLabel;
    LbDepth: TGuiLabel;
    LbRate: TGuiLabel;
    LbFeedback: TGuiLabel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    ScrollBar4: TScrollBar;
    ScrollBar5: TScrollBar;
    ScrollBar6: TScrollBar;
    ScrollBar7: TScrollBar;
    LbGainValue: TGuiLabel;
    LbMixValue: TGuiLabel;
    LbLpfValue: TGuiLabel;
    LbDelayValue: TGuiLabel;
    LbDepthValue: TGuiLabel;
    LbRateValue: TGuiLabel;
    LbFeedbackValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  end;

implementation

uses
  DAV_GuiCommon, ModDelayDM;

{$R *.DFM}

procedure TFmModDelay.FormCreate(Sender: TObject);
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
end;

procedure TFmModDelay.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

end.
