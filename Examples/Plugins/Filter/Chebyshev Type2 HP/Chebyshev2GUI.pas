unit Chebyshev2GUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, ExtCtrls, Controls, DAV_Types,
  DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiDial, DAV_GuiPanel;

type
  TFmChebyshev2 = class(TForm)
    DialFrequency: TGuiDial;
    DialOrder: TGuiDial;
    DialStopband: TGuiDial;
    LbChebyshev2FilterDemo: TGuiLabel;
    LbChebyshev2FilterDemoShaddow: TGuiLabel;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TFmChebyshev2.FormCreate(Sender: TObject);
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

procedure TFmChebyshev2.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateStopband;
 UpdateOrder;
(*
 with TChebyshev2HPModule(Owner) do
  begin
   Resizer.SetEditorHwnd(Self.Handle);
  end;
*)
end;

procedure TFmChebyshev2.UpdateFrequency;
var
  Freq : Single;
begin
 with TChebyshev2HPModule(Owner) do
  begin
   Freq := ParameterByName['Frequency'];
   if DialFrequency.Position <> Freq
    then DialFrequency.Position := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(Freq * 1E-3, ffGeneral, 3, 3) + ' kHz'
  end;
end;

procedure TFmChebyshev2.DialFrequencyChange(Sender: TObject);
begin
 with TChebyshev2HPModule(Owner) do
  begin
   if ParameterByName['Frequency'] <> DialFrequency.Position
    then ParameterByName['Frequency'] := DialFrequency.Position;
  end;
end;

procedure TFmChebyshev2.DialOrderChange(Sender: TObject);
begin
 with TChebyshev2HPModule(Owner) do
  begin
   if ParameterByName['Order'] <> DialOrder.Position
    then ParameterByName['Order'] := DialOrder.Position;
  end;
end;

procedure TFmChebyshev2.DialStopbandChange(Sender: TObject);
begin
 with TChebyshev2HPModule(Owner) do
  begin
   if ParameterByName['Stopband'] <> DialStopband.Position
    then ParameterByName['Stopband'] := DialStopband.Position;
  end;
end;

procedure TFmChebyshev2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TChebyshev2HPModule(Owner) do
  begin
   Resizer.SetEditorHwnd(0);
  end;
end;

procedure TFmChebyshev2.UpdateOrder;
var
  Order : Single;
begin
 with TChebyshev2HPModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
   LbOrderValue.Caption := IntToStr(round(Order));
  end;
end;

procedure TFmChebyshev2.UpdateStopband;
var
  Stopband : Single;
begin
 with TChebyshev2HPModule(Owner) do
  begin
   Stopband := ParameterByName['Stopband'];
   if DialStopband.Position <> Stopband
    then DialStopband.Position := Stopband;
   LbStopbandValue.Caption := FloatToStrF(Stopband, ffGeneral, 3, 3) + ' dB';
  end;
end;

end.
