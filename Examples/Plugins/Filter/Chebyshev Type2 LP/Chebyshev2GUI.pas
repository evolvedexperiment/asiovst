unit Chebyshev2GUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, ExtCtrls, Controls, DAV_Common,
  DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiDial, DAV_GuiPanel;

type
  TFmChebyshev = class(TForm)
    DialFrequency: TGuiDial;
    DialOrder: TGuiDial;
    DialStopband: TGuiDial;
    LbChebyshevFilterDemo: TGuiLabel;
    LbChebyshevFilterDemoShaddow: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    LbStopband: TGuiLabel;
    LbStopbandValue: TGuiLabel;
    PnControls: TGuiPanel;
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialStopbandChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure UpdateFrequency;
    procedure UpdateStopband;
    procedure UpdateOrder;
  end;

implementation

{$R *.DFM}

uses
  DAV_VSTModuleWithPrograms, Chebyshev2DM;

procedure TFmChebyshev.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'WineKnob', 'BMP');
 try
  DialFrequency.DialBitmap.LoadFromStream(RS);
  DialOrder.DialBitmap.Assign(DialFrequency.DialBitmap);
  DialStopband.DialBitmap.Assign(DialFrequency.DialBitmap);
 finally
  RS.Free;
 end;
end;

procedure TFmChebyshev.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateStopband;
 UpdateOrder;
end;

procedure TFmChebyshev.DialFrequencyChange(Sender: TObject);
begin
 with TChebyshev2LPModule(Owner) do
  begin
   if ParameterByName['Frequency'] <> DialFrequency.Position
    then ParameterByName['Frequency'] := DialFrequency.Position;
  end;
end;

procedure TFmChebyshev.DialOrderChange(Sender: TObject);
begin
 with TChebyshev2LPModule(Owner) do
  begin
   if ParameterByName['Order'] <> DialOrder.Position
    then ParameterByName['Order'] := DialOrder.Position;
  end;
end;

procedure TFmChebyshev.DialStopbandChange(Sender: TObject);
begin
 with TChebyshev2LPModule(Owner) do
  begin
   if ParameterByName['Stopband'] <> DialStopband.Position
    then ParameterByName['Stopband'] := DialStopband.Position;
  end;
end;

procedure TFmChebyshev.UpdateFrequency;
var
  Freq : Single;
begin
 with TChebyshev2LPModule(Owner) do
  begin
   Freq := ParameterByName['Frequency'];
   if DialFrequency.Position <> Freq
    then DialFrequency.Position := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 5, 5) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(Freq * 1E-3, ffGeneral, 5, 5) + ' kHz'
  end;
end;

procedure TFmChebyshev.UpdateOrder;
var
  Order : Single;
begin
 with TChebyshev2LPModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
   LbOrderValue.Caption := IntToStr(round(Order));
  end;
end;

procedure TFmChebyshev.UpdateStopband;
var
  Stopband : Single;
begin
 with TChebyshev2LPModule(Owner) do
  begin
   Stopband := ParameterByName['Stopband'];
   if DialStopband.Position <> Stopband
    then DialStopband.Position := Stopband;
   LbStopbandValue.Caption := FloatToStrF(Stopband, ffGeneral, 3, 3) + ' dB';
  end;
end;

end.
