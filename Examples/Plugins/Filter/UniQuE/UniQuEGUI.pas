unit UniQuEGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, StdCtrls,
  DAV_Common, DAV_VSTModule, DAV_GuiLED, DAV_GuiLabel, DAV_GuiGroup,
  DAV_GuiDial, DAV_GuiBaseControl;

type
  TFmUniQuE = class(TForm)
    GpUnique: TGuiGroup;
    DialLow: TGuiDial;
    DialMid: TGuiDial;
    DialPresence: TGuiDial;
    DialHigh: TGuiDial;
    LEDOnOff: TGuiLED;
    LbOnOff: TGuiLabel;
    LbPad: TGuiLabel;
    LEDPad: TGuiLED;
    LbInvert: TGuiLabel;
    LEDInvert: TGuiLED;
    LbLow: TGuiLabel;
    LbMid: TGuiLabel;
    LbPRes: TGuiLabel;
    LbHigh: TGuiLabel;
    procedure OnOffClick(Sender: TObject);
    procedure DialLowChange(Sender: TObject);
    procedure DialMidChange(Sender: TObject);
    procedure DialPresenceChange(Sender: TObject);
    procedure DialHighChange(Sender: TObject);
    procedure PadClick(Sender: TObject);
    procedure InvertClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateOnOff;
    procedure UpdatePad;
    procedure UpdateInvert;
    procedure UpdateLow;
    procedure UpdateMid;
    procedure UpdatePres;
    procedure UpdateHigh;
  end;

implementation

{$R *.DFM}

uses
  UniQuEDM;

procedure TFmUniQuE.FormCreate(Sender: TObject);
var
  RS   : TResourceStream;
  x, y : Integer;
  s    : array[0..1] of Single;
  b    : ShortInt;
  Line : PRGB24Array;
begin
 RS := TResourceStream.Create(hInstance, 'UniQuEKnob', 'BMP');
 try
  DialLow.DialBitmap.LoadFromStream(RS);      RS.Position := 0;
  DialMid.DialBitmap.LoadFromStream(RS);      RS.Position := 0;
  DialPresence.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DialHigh.DialBitmap.LoadFromStream(RS);     RS.Position := 0;
 finally
  RS.Free;
 end;

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
       s[1] := 0.9 * s[0] + 0.1 * random;
       b := round($1F + $32 * s[1]);
       s[0] := s[1];
       Line[x].B := b;
       Line[x].G := b;
       Line[x].R := b;
      end;
    end;
  end;
end;

procedure TFmUniQuE.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmUniQuE.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmUniQuE.FormShow(Sender: TObject);
begin
 UpdateOnOff;
 UpdatePad;
 UpdateInvert;
 UpdateLow;
 UpdateMid;
 UpdatePres;
 UpdateHigh;
end;

procedure TFmUniQuE.UpdateInvert;
begin
 with TUniQuEDataModule(Owner) do
  begin
   LEDInvert.Brightness_Percent := 20 + 80 * (Parameter[2]);
  end;
end;

procedure TFmUniQuE.UpdateOnOff;
begin
 with TUniQuEDataModule(Owner) do
  begin
   LEDOnOff.Brightness_Percent := 20 + 80 * Parameter[0];
  end;
end;

procedure TFmUniQuE.UpdatePad;
begin
 with TUniQuEDataModule(Owner) do
  begin
   LEDPad.Brightness_Percent := 20 + 6 * Parameter[1];
  end;
end;

procedure TFmUniQuE.UpdateHigh;
begin
 with TUniQuEDataModule(Owner) do
  begin
   if DialHigh.Position <> Parameter[6]
    then DialHigh.Position := Parameter[6];
  end;
end;

procedure TFmUniQuE.UpdateLow;
begin
 with TUniQuEDataModule(Owner) do
  begin
   if DialLow.Position <> Parameter[3]
    then DialLow.Position := Parameter[3];
  end;
end;

procedure TFmUniQuE.UpdateMid;
begin
 with TUniQuEDataModule(Owner) do
  begin
   if DialMid.Position <> Parameter[4]
    then DialMid.Position := Parameter[4];
  end;
end;

procedure TFmUniQuE.UpdatePres;
begin
 with TUniQuEDataModule(Owner) do
  begin
   if DialPresence.Position <> Parameter[5]
    then DialPresence.Position := Parameter[5];
  end;
end;

procedure TFmUniQuE.DialLowChange(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[3] := DialLow.Position;
  end;
end;

procedure TFmUniQuE.DialMidChange(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[4] := DialMid.Position;
  end;
end;

procedure TFmUniQuE.DialPresenceChange(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[5] := DialPresence.Position;
  end;
end;

procedure TFmUniQuE.DialHighChange(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[6] := DialHigh.Position;
  end;
end;

procedure TFmUniQuE.OnOffClick(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[0] := round(1 - Parameter[0]);
   UpdateOnOff;
  end;
end;

procedure TFmUniQuE.InvertClick(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   Parameter[2] := round(1 - Parameter[2]);
   UpdateInvert;
  end;
end;

procedure TFmUniQuE.PadClick(Sender: TObject);
begin
 with TUniQuEDataModule(Owner) do
  begin
   if Parameter[1] < 6
    then Parameter[1] := 12
    else Parameter[1] := 0;
   UpdatePad;
  end;
end;

end.
