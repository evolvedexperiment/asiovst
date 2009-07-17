unit ModDelay2GUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Graphics, Controls, StdCtrls,
  DAV_Common, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel;

type
  TFmModDelay2 = class(TForm)
    LbCurrentValue: TGuiLabel;
    LbDelay: TGuiLabel;
    LbDepth: TGuiLabel;
    LbFeedback: TGuiLabel;
    LbGain: TGuiLabel;
    LbLeft: TGuiLabel;
    LbLpf: TGuiLabel;
    LbMix: TGuiLabel;
    LbRate: TGuiLabel;
    LbRight: TGuiLabel;
    SBDelayLeft: TScrollBar;
    SBdelayRight: TScrollBar;
    SbDepthLeft: TScrollBar;
    SBDepthRight: TScrollBar;
    SBFeedbackLeft: TScrollBar;
    SBFeedbackRight: TScrollBar;
    SBGainLeft: TScrollBar;
    SBGainRight: TScrollBar;
    SBLPFLeft: TScrollBar;
    SBLpfRight: TScrollBar;
    SBMixLeft: TScrollBar;
    SBMixRight: TScrollBar;
    SBRateLeft: TScrollBar;
    SBRateRight: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  end;

implementation

uses
  DAV_GuiCommon, ModDelay2DM;

{$R *.DFM}

procedure TFmModDelay2.FormCreate(Sender: TObject);
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

procedure TFmModDelay2.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

end.