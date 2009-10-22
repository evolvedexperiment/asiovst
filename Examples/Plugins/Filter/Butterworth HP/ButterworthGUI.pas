unit ButterworthGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_GuiLabel, Controls, DAV_GuiBaseControl, DAV_GuiDial, ExtCtrls,
  DAV_GuiPanel;

type
  TFmButterworth = class(TForm)
    DialFrequency: TGuiDial;
    DialOrder: TGuiDial;
    LbButterworthFilterDemo: TGuiLabel;
    LbButterworthFilterDemoShaddow: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    PnControls: TGuiPanel;
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure UpdateFrequency;
    procedure UpdateOrder;
  end;

implementation

{$R *.DFM}

uses
  ButterworthDM, DAV_VSTModuleWithPrograms;

procedure TFmButterworth.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'WineKnob', 'BMP');
 try
  DialFrequency.DialBitmap.LoadFromStream(RS);
  DialOrder.DialBitmap.Assign(DialFrequency.DialBitmap);
 finally
  RS.Free;
 end;
end;

procedure TFmButterworth.DialFrequencyChange(Sender: TObject);
begin
 with TButterworthHPModule(Owner) do
  begin
   ParameterByName['Frequency'] := DialFrequency.Position;
  end;
end;

procedure TFmButterworth.DialOrderChange(Sender: TObject);
begin
 with TButterworthHPModule(Owner) do
  begin
   ParameterByName['Order'] := DialOrder.Position;
  end;
end;

procedure TFmButterworth.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateOrder;
end;

procedure TFmButterworth.UpdateFrequency;
var
  Freq : Single;
begin
 with TButterworthHPModule(Owner) do
  begin
   Freq := ParameterByName['Frequency'];
   if DialFrequency.Position <> Freq
    then DialFrequency.Position := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 4, 4) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(Freq * 1E-3, ffGeneral, 4, 4) + ' kHz';
  end;
end;

procedure TFmButterworth.UpdateOrder;
var
  Order : Single;
begin
 with TButterworthHPModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
   LbOrderValue.Caption := IntToStr(round(Order));
  end;
end;

end.
