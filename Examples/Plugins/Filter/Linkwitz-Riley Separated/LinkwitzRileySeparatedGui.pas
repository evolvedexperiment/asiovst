unit LinkwitzRileySeparatedGui;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DAV_GuiPanel, DAV_GuiLabel, DAV_GuiBaseControl,
  DAV_GuiDial, StdCtrls, DAV_GuiGroup, DAV_GuiEQGraph;

type
  TFmLinkwitzRiley = class(TForm)
    GpStage1: TGuiGroup;
    DialFrequency: TGuiDial;
    DialOrder: TGuiDial;
    DialType: TGuiDial;
    LbIFrequency: TGuiLabel;
    LbOrder: TGuiLabel;
    LbType: TGuiLabel;
    PnDisplay: TGuiPanel;
    LbDisplay: TGuiLabel;
    GbFrequencyResponse: TGuiGroup;
    GuiEQGraph: TGuiEQGraph;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DialTypeChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GuiEQGraphGetFilterGain(Sender: TObject;
      const Frequency: Single): Single;
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateFrequency;
    procedure UpdateOrder;
    procedure UpdateType;
  end;

implementation

{$R *.dfm}

uses
  DAV_GuiCommon, PNGImage, LinkwitzRileySeparatedDM, DAV_VSTModuleWithPrograms;

procedure TFmLinkwitzRiley.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
  PngBmp : TPngObject;

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

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ClipperKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialFrequency.DialBitmap.Assign(PngBmp);
   DialOrder.DialBitmap.Assign(PngBmp);
   DialType.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmLinkwitzRiley.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmLinkwitzRiley.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateOrder;
 UpdateType;
 LbDisplay.Caption := 'Linkwitz-Riley';
end;

function TFmLinkwitzRiley.GuiEQGraphGetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   Result := -5;
  end;
end;

procedure TFmLinkwitzRiley.DialFrequencyChange(Sender: TObject);
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if Parameter[0] <> DialFrequency.Position
    then Parameter[0] := DialFrequency.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialOrderChange(Sender: TObject);
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if Parameter[1] <> DialOrder.Position
    then Parameter[1] := DialOrder.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialTypeChange(Sender: TObject);
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if Parameter[2] <> DialType.Position
    then Parameter[2] := DialType.Position;
  end;
end;

procedure TFmLinkwitzRiley.UpdateFrequency;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if DialFrequency.Position <> Parameter[0]
    then DialFrequency.Position := Parameter[0];
   LbDisplay.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLinkwitzRiley.UpdateOrder;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if DialOrder.Position <> Parameter[1]
    then DialOrder.Position := Parameter[1];
   LbDisplay.Caption := 'Order: ' + ParameterDisplay[1];
  end;
end;

procedure TFmLinkwitzRiley.UpdateType;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if DialType.Position <> Parameter[2]
    then DialType.Position := Parameter[2];
   LbDisplay.Caption := ParameterDisplay[2];
  end;
end;

end.
