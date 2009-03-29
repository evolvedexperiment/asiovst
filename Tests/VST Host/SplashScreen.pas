unit SplashScreen;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, DAV_GuiBaseControl, DAV_GuiLabel, pngimage;

type
  TFmSplashScreen = class(TForm)
    IVST: TImage;
    IDUnit: TImage;
    LbTitle: TGuiLabel;
    LbVstAbout: TLabel;
    LbDUnitAbout: TLabel;
    LbScanning: TLabel;
    LbScannedPlugin: TLabel;
    Border: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBitmap : TBitmap;
  public
    { Public-Deklarationen }
  end;

implementation

uses
  DAV_GuiCommon;

{$R *.dfm}

procedure TFmSplashScreen.FormCreate(Sender: TObject);
var
  s     : array [0..1] of Single;
  hr, h : Single;
  x, y  : Integer;
  Line  : PRGB24Array;
begin
 FBitmap := TBitmap.Create;
 with FBitmap do
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
     h    := 0.3 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       Line[x].B := round(255 - $1F * (s[1] - h));
       Line[x].G := Line[x].B;
       Line[x].R := Line[x].B;
      end;
    end;
  end;
end;

procedure TFmSplashScreen.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBitmap);
end;

procedure TFmSplashScreen.FormResize(Sender: TObject);
begin
 with FBitmap do
  begin
   SetSize(Width, Height);
  end;
end;

procedure TFmSplashScreen.FormShow(Sender: TObject);
begin
 LbTitle.Width := LbTitle.Width + 1;
end;

end.
