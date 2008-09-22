unit FrequencySplitTemplateGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DAV_Common, DAV_GuiBaseControl, DAV_GuiButton, DAV_GuiLabel,
  DAV_GuiLED, DAV_GuiDial, DAV_GuiPanel;

type
  TFmFrequencySplitter = class(TForm)
    BtHigh: TGuiButton;
    BtLow: TGuiButton;
    DialOversampling: TGuiDial;
    DialSplitFrequency: TGuiDial;
    DialSplitOrder: TGuiDial;
    GuiLEDOversampling: TGuiLED;
    GuiLEDSplit: TGuiLED;
    PnControl: TGuiPanel;
    LbOversampling: TGuiLabel;
    LbOversamplingFactor: TGuiLabel;
    LbSplit: TGuiLabel;
    LbSplitFrequency: TGuiLabel;
    LbSplitOrder: TGuiLabel;
    PnGui: TPanel;
    LbLeftRight: TGuiLabel;
    ShBorder: TShape;
    procedure BtLowClick(Sender: TObject);
    procedure BtHighClick(Sender: TObject);
    procedure GuiLEDSplitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialSplitFrequencyChange(Sender: TObject);
    procedure DialSplitOrderChange(Sender: TObject);
    procedure DialOversamplingChange(Sender: TObject);
    procedure GuiLEDOversamplingClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LbLeftRightClick(Sender: TObject);
  private
    fBackground : TBitmap;  
  public
    procedure UpdateMode;
    procedure UpdateFrequency;
    procedure UpdateOrder;
    procedure UpdateOSFactor;
    procedure UpdateOverSampling;
    procedure ShowPlugin(Index: Integer);
  end;

implementation

{$R *.dfm}

uses
  FrequencySplitTemplateDM, DAV_VSTModuleWithPrograms;

procedure TFmFrequencySplitter.BtLowClick(Sender: TObject);
begin
 BtLow.ButtonColor  := $0018CF1D;
 BtHigh.ButtonColor := $00626C71;
 ShowPlugin(0);
end;

procedure TFmFrequencySplitter.BtHighClick(Sender: TObject);
begin
 BtLow.ButtonColor  := $00626C71;
 BtHigh.ButtonColor := $0018CF1D;
 ShowPlugin(1);
end;

procedure TFmFrequencySplitter.ShowPlugin(Index: Integer);
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   if VstHost[Index].Active and not VstHost[Index].EditVisible
    then VstHost[Index].ShowEdit(TForm(PnGui));
   if VstHost[1 - Index].EditVisible then VstHost[1 - Index].CloseEdit;
  end;
end;

procedure TFmFrequencySplitter.DialOversamplingChange(Sender: TObject);
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   ParameterByName['OS Factor'] := DialOversampling.Position;
  end;
end;

procedure TFmFrequencySplitter.DialSplitFrequencyChange(Sender: TObject);
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Frequency'] := DialSplitFrequency.Position;
  end;
end;

procedure TFmFrequencySplitter.DialSplitOrderChange(Sender: TObject);
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Order'] := DialSplitOrder.Position;
  end;
end;

procedure TFmFrequencySplitter.FormPaint(Sender: TObject);
begin
 if assigned(fBackground)
  then Canvas.Draw(0, PnControl.Height, fBackground);
end;

procedure TFmFrequencySplitter.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  Line   : PRGB32Array;

begin
 // Create Background Image (if not already done
 if not assigned(fBackground)
  then fBackground := TBitmap.Create;
 with fBackground do
  begin
   PixelFormat := pf32bit;
   Width := Self.Width;
   Height := Self.Height - PnControl.Height;
   if Height < 2 then exit;
   s[0] := 0;
   s[1] := 0;

   // start with separator
   Line := Scanline[0];
   for x := 0 to Width - 1 do
    begin
     s[1] := 0.97 * s[0] + 0.03 * random;
     s[0] := s[1];
     Line[x].B := round($2B - $2A * s[1]);
     Line[x].G := round($31 - $2A * s[1]);
     Line[x].R := round($33 - $2A * s[1]);
     Line[x].A := 0;
    end;

   for y := 1 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];
       Line[x].B := round($2B + $2A * s[1]);
       Line[x].G := round($31 + $2A * s[1]);
       Line[x].R := round($33 + $2A * s[1]);
       Line[x].A := 0;
      end;
    end;
  end;
end;

procedure TFmFrequencySplitter.FormShow(Sender: TObject);
begin
 UpdateMode;
 UpdateFrequency;
 UpdateOrder;
 UpdateOSFactor;
 ShowPlugin(0);
end;

procedure TFmFrequencySplitter.GuiLEDOversamplingClick(Sender: TObject);
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Oversampling'] := f_Limit(1 - ParameterByName['Oversampling'], 0, 1);
  end;
end;

procedure TFmFrequencySplitter.GuiLEDSplitClick(Sender: TObject);
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Mode'] := f_Limit(2 - ParameterByName['Mode'], 0, 2);
  end;
end;

procedure TFmFrequencySplitter.LbLeftRightClick(Sender: TObject);
begin
 with TFrequencySplitTemplateDataModule(Owner), LbLeftRight do
  begin
   Tag := 1 - Tag;
   if Tag = 0 then
    begin
     Caption := 'Left / Right';
     ParameterByName['Mode'] := 2;
    end
   else
    begin
     Caption := 'Mid / Side';
     ParameterByName['Mode'] := 3;
    end;
   UpdateMode;
  end;
end;

procedure TFmFrequencySplitter.UpdateMode;
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   GuiLEDSplit.Brightness_Percent := 20 + 60 * (1 - f_Limit(0.5 * ParameterByName['Mode'], 0, 1));
   DialSplitFrequency.Visible := round(ParameterByName['Mode']) = 0;
   LbSplitFrequency.Visible   := DialSplitFrequency.Visible;
   DialSplitOrder.Visible     := DialSplitFrequency.Visible;
   LbSplitOrder.Visible       := DialSplitFrequency.Visible;
   LbLeftRight.Visible        := not DialSplitFrequency.Visible;
   case SplitType of
    stSimple,
    stLinkwitzRiley : begin
                       BtLow.Caption := 'Low';
                       BtHigh.Caption := 'High';
                      end;
        stLeftRight : begin
                       BtLow.Caption  := 'Left';
                       BtHigh.Caption := 'Right';
                      end;
               stMS : begin
                       BtLow.Caption  := 'Mid';
                       BtHigh.Caption := 'Side';
                      end;
   end;
  end;
end;

procedure TFmFrequencySplitter.UpdateFrequency;
var
  Freq : Single;
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   Freq := ParameterByName['Frequency'];
   if DialSplitFrequency.Position <> Freq
    then DialSplitFrequency.Position := Freq;
   if Freq < 1000
    then LbSplitFrequency.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + 'Hz'
    else LbSplitFrequency.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + 'kHz';
  end;
end;

procedure TFmFrequencySplitter.UpdateOrder;
var
  Order : Single;
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialSplitOrder.Position <> Order
    then DialSplitOrder.Position := Order;
   LbSplitOrder.Caption := ConvertOrderToString(2 * round(Order));
  end;
end;

procedure TFmFrequencySplitter.UpdateOverSampling;
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   GuiLEDOversampling.Brightness_Percent := 20 + 60 * (f_Limit(ParameterByName['Oversampling'], 0, 1));

   DialOversampling.Visible     := round(ParameterByName['Oversampling']) = 1;
   LbOversamplingFactor.Visible := DialOversampling.Visible;
   LbOversampling.Width         := 97 + 5 * round(ParameterByName['Oversampling']);
  end;
end;

procedure TFmFrequencySplitter.UpdateOSFactor;
var
  OSFactor : Single;
begin
 with TFrequencySplitTemplateDataModule(Owner) do
  begin
   OSFactor := ParameterByName['OS Factor'];
   if DialOversampling.Position <> OSFactor
    then DialOversampling.Position := OSFactor;
   LbOversamplingFactor.Caption := IntToStr(round(OSFactor)) + 'x';
  end;
end;

end.
