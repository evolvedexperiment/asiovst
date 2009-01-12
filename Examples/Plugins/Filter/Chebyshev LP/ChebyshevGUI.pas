unit ChebyshevGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, ExtCtrls, Controls, DAV_Common,
  DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiDial, DAV_GuiPanel;

type
  TFmChebyshev = class(TForm)
    DialFrequency: TGuiDial;
    DialOrder: TGuiDial;
    DialRipple: TGuiDial;
    LbChebyshevFilterDemo: TGuiLabel;
    LbChebyshevFilterDemoShaddow: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    LbRipple: TGuiLabel;
    LbRippleValue: TGuiLabel;
    PnControls: TGuiPanel;
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialRippleChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  public
    procedure UpdateFrequency;
    procedure UpdateRipple;
    procedure UpdateOrder;
  end;

implementation

{$R *.DFM}

uses
  DAV_VSTModuleWithPrograms, ChebyshevDM;

procedure TFmChebyshev.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'WineKnob', 'BMP');
 try
  DialFrequency.DialBitmap.LoadFromStream(RS);
  DialOrder.DialBitmap.Assign(DialFrequency.DialBitmap);
  DialRipple.DialBitmap.Assign(DialFrequency.DialBitmap);
 finally
  RS.Free;
 end;
end;

procedure TFmChebyshev.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateRipple;
 UpdateOrder;
(*
 with TChebyshevLPModule(Owner) do
  begin
   Resizer.SetEditorHwnd(Self.Handle);
  end;
*)
end;

procedure TFmChebyshev.DialFrequencyChange(Sender: TObject);
begin
 with TChebyshevLPModule(Owner) do
  begin
   if ParameterByName['Frequency'] <> DialFrequency.Position
    then ParameterByName['Frequency'] := DialFrequency.Position;
  end;
end;

procedure TFmChebyshev.DialOrderChange(Sender: TObject);
begin
 with TChebyshevLPModule(Owner) do
  begin
   if ParameterByName['Order'] <> DialOrder.Position
    then ParameterByName['Order'] := DialOrder.Position;
  end;
end;

procedure TFmChebyshev.DialRippleChange(Sender: TObject);
begin
 with TChebyshevLPModule(Owner) do
  begin
   if ParameterByName['Ripple'] <> DialRipple.Position
    then ParameterByName['Ripple'] := DialRipple.Position;
  end;
end;

procedure TFmChebyshev.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TChebyshevLPModule(Owner) do
  begin
   Resizer.SetEditorHwnd(0);
  end;
end;

procedure TFmChebyshev.UpdateFrequency;
var
  Freq : Single;
begin
 with TChebyshevLPModule(Owner) do
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
 with TChebyshevLPModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
   LbOrderValue.Caption := IntToStr(round(Order));
  end;
end;

procedure TFmChebyshev.UpdateRipple;
var
  Ripple : Single;
begin
 with TChebyshevLPModule(Owner) do
  begin
   Ripple := ParameterByName['Ripple'];
   if DialRipple.Position <> Ripple
    then DialRipple.Position := Ripple;
   LbRippleValue.Caption := FloatToStrF(Ripple, ffGeneral, 3, 3) + ' dB';
  end;
end;

end.
