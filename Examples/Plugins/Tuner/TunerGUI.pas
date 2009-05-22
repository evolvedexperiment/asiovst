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
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbNoteClick(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
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
 if Sender <> LbLowE then LbLowE.Font.Color := $003F3F3F else LbLowE.Font.Color := clBlack;
 if Sender <> LbA then LbA.Font.Color := $003F3F3F else LbA.Font.Color := clBlack;
 if Sender <> LbD then LbD.Font.Color := $003F3F3F else LbD.Font.Color := clBlack;
 if Sender <> LbG then LbG.Font.Color := $003F3F3F else LbG.Font.Color := clBlack;
 if Sender <> LbH then LbH.Font.Color := $003F3F3F else LbH.Font.Color := clBlack;
 if Sender <> LbE then LbE.Font.Color := $003F3F3F else LbE.Font.Color := clBlack;
end;

end.