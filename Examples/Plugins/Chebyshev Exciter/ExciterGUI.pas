unit ExciterGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, ExtCtrls,
  DAV_Common, DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiPanel;

type
  TFmExciter = class(TForm)
    DialMix: TGuiDial;
    DialOrder: TGuiDial;
    DialShape: TGuiDial;
    DialTune: TGuiDial;
    LbFreq: TGuiLabel;
    LbFreqValue: TGuiLabel;
    LbMix: TGuiLabel;
    LbMixValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    LbShape: TGuiLabel;
    LbShapeValue: TGuiLabel;
    PnControl: TGuiPanel;
    procedure DialTuneChange(Sender: TObject);
    procedure DialShapeChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateTune;
    procedure UpdateOrder;
    procedure UpdateShape;
    procedure UpdateMix;
  end;

implementation

{$R *.DFM}

uses
  PNGImage, DAV_GUICommon, ExciterDM;

procedure TFmExciter.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  Line   : PRGB24Array;
  h, hr  : Single;
  PngBmp : TPngObject;

begin
 // Create Background Image
 FBackgrounBitmap := TBitmap.Create;
 with FBackgrounBitmap do
  begin
   PixelFormat := pf24bit;
   Width       := Self.Width;
   Height      := Self.Height;
   hr          := 1 / Height;
   s[0]        := 0;
   s[1]        := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.6 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
       b := round($3F + $1A * (h + s[1]));
       s[0] := s[1];
       Line[x].B := b;
       Line[x].G := b;
       Line[x].R := b;
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ExciterKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   PngBmp.AssignTo(DialTune.DialBitmap);
   PngBmp.AssignTo(DialOrder.DialBitmap);
   DialShape.DialBitmap.Assign(PngBmp);
   DialMix.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmExciter.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmExciter.DialTuneChange(Sender: TObject);
begin
 with Owner as TExciterDataModule do
  begin
   ParameterByName['Tune'] := DialTune.Position;
  end;
end;

procedure TFmExciter.DialMixChange(Sender: TObject);
begin
 with Owner as TExciterDataModule do
  begin
   ParameterByName['Mix'] := DialMix.Position;
  end; 
end;

procedure TFmExciter.DialShapeChange(Sender: TObject);
begin
 with Owner as TExciterDataModule do
  begin
   ParameterByName['Shape'] := DialShape.Position;
  end;
end;

procedure TFmExciter.DialOrderChange(Sender: TObject);
var
  CurrentOrder : Single;
  DesiredOrder : Integer;
begin
 with Owner as TExciterDataModule do
  begin
   DesiredOrder := round(DialOrder.Position);
   CurrentOrder := ParameterByName['Order'];
   if round(CurrentOrder) = DesiredOrder then
    if DialOrder.Position < CurrentOrder
     then ParameterByName['Order'] := DesiredOrder - 1 else
    if DialOrder.Position > CurrentOrder
     then ParameterByName['Order'] := DesiredOrder + 1 else
   else ParameterByName['Order'] := DesiredOrder;
  end;
end;

procedure TFmExciter.FormShow(Sender: TObject);
begin
 UpdateTune;
 UpdateOrder;
 UpdateShape;
 UpdateMix;
end;

procedure TFmExciter.UpdateTune;
var
  Freq : Single;
begin
 with Owner as TExciterDataModule do
  begin
   Freq := ParameterByName['Tune'];
   if Freq < 1000
    then LbFreqValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + 'Hz'
    else LbFreqValue.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + 'kHz';
   if DialTune.Position <> Freq
    then DialTune.Position := Freq;
  end;
end;

procedure TFmExciter.UpdateMix;
var
  Mix : Single;
begin
 with Owner as TExciterDataModule do
  begin
   Mix := ParameterByName['Mix'];
   LbMixValue.Caption := FloatToStrF(Mix, ffGeneral, 3, 1) + '%';
   if DialMix.Position <> Mix
    then DialMix.Position := Mix;
  end;
end;

procedure TFmExciter.UpdateShape;
var
  Shape : Single;
begin
 with Owner as TExciterDataModule do
  begin
   Shape := ParameterByName['Shape'];
   LbShapeValue.Caption := FloatToStrF(Shape, ffGeneral, 3, 1) + '%';
   if DialShape.Position <> Shape
    then DialShape.Position := Shape;
  end;
end;

procedure TFmExciter.UpdateOrder;
var
  Order : Integer;
begin
 with Owner as TExciterDataModule do
  begin
   Order := round(ParameterByName['Order']);
   LbOrderValue.Caption := IntToStr(Order);
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
  end;
end;

end.