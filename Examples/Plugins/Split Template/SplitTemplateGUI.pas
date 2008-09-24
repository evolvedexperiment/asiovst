unit SplitTemplateGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DAV_Common, DAV_GuiBaseControl, DAV_GuiButton, DAV_GuiLabel,
  DAV_GuiLED, DAV_GuiDial, DAV_GuiPanel, DAV_GuiSelectBox;

type
  TFmSplitter = class(TForm)
    BtHigh: TGuiButton;
    BtLow: TGuiButton;
    DialOversampling: TGuiDial;
    DialSplitFrequency: TGuiDial;
    DialSplitOrder: TGuiDial;
    GuiLEDOversampling: TGuiLED;
    PnControl: TGuiPanel;
    LbOversampling: TGuiLabel;
    LbOversamplingFactor: TGuiLabel;
    LbSplitFrequency: TGuiLabel;
    LbSplitOrder: TGuiLabel;
    PnGui: TPanel;
    ShBorder: TShape;
    SBMode: TGuiSelectBox;
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure BtHighClick(Sender: TObject);
    procedure BtLowClick(Sender: TObject);
    procedure DialOversamplingChange(Sender: TObject);
    procedure DialSplitFrequencyChange(Sender: TObject);
    procedure DialSplitOrderChange(Sender: TObject);
    procedure GuiLEDOversamplingClick(Sender: TObject);
    procedure SBModeChange(Sender: TObject);
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
  SplitTemplateDM, DAV_VSTModuleWithPrograms;

procedure TFmSplitter.BtLowClick(Sender: TObject);
begin
 BtLow.ButtonColor  := $0018CF1D;
 BtHigh.ButtonColor := $00626C71;
 ShowPlugin(0);
end;

procedure TFmSplitter.BtHighClick(Sender: TObject);
begin
 BtLow.ButtonColor  := $00626C71;
 BtHigh.ButtonColor := $0018CF1D;
 ShowPlugin(1);
end;

procedure TFmSplitter.SBModeChange(Sender: TObject);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Mode'] := SBMode.ItemIndex;
  end;
end;

procedure TFmSplitter.ShowPlugin(Index: Integer);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   if VstHost[Index].Active and not VstHost[Index].EditVisible
    then VstHost[Index].ShowEdit(PnGui);
   if VstHost[1 - Index].EditVisible then VstHost[1 - Index].CloseEdit;
  end;
end;

procedure TFmSplitter.DialOversamplingChange(Sender: TObject);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   ParameterByName['OS Factor'] := DialOversampling.Position;
  end;
end;

procedure TFmSplitter.DialSplitFrequencyChange(Sender: TObject);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Frequency'] := DialSplitFrequency.Position;
  end;
end;

procedure TFmSplitter.DialSplitOrderChange(Sender: TObject);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Order'] := DialSplitOrder.Position;
  end;
end;

procedure TFmSplitter.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TSplitTemplateDataModule(Owner) do
  try
   if VstHost[0].EditVisible then VstHost[0].CloseEdit;
   if VstHost[1].EditVisible then VstHost[1].CloseEdit;
  except 
  end;
end;

procedure TFmSplitter.FormDestroy(Sender: TObject);
begin
 if assigned(fBackground)
  then FreeAndNil(fBackground); 
end;

procedure TFmSplitter.FormPaint(Sender: TObject);
begin
 if assigned(fBackground)
  then Canvas.Draw(0, PnControl.Height, fBackground);
end;

procedure TFmSplitter.FormResize(Sender: TObject);
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

procedure TFmSplitter.FormShow(Sender: TObject);
begin
 UpdateMode;
 UpdateFrequency;
 UpdateOrder;
 UpdateOSFactor;
 ShowPlugin(0);
end;

procedure TFmSplitter.GuiLEDOversamplingClick(Sender: TObject);
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   ParameterByName['Oversampling'] := f_Limit(1 - ParameterByName['Oversampling'], 0, 1);
  end;
end;

procedure TFmSplitter.UpdateMode;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   SBMode.ItemIndex := round(ParameterByName['Mode']);
//   GuiLEDSplit.Brightness_Percent := 20 + 60 * (1 - f_Limit(0.5 * ParameterByName['Mode'], 0, 1));
   DialSplitFrequency.Visible := round(ParameterByName['Mode']) < 3;
   LbSplitFrequency.Visible   := DialSplitFrequency.Visible;
   DialSplitOrder.Visible     := DialSplitFrequency.Visible;
   LbSplitOrder.Visible       := DialSplitFrequency.Visible;
   case SplitType of
      stSimple,
         stLiRi : begin
                   BtLow.Caption := 'Low';
                   BtHigh.Caption := 'High';
                  end;
          stDyn : begin
                   BtLow.Caption  := 'Quiet';
                   BtHigh.Caption := 'Loud';
                  end;
    stLeftRight : begin
                   BtLow.Caption  := 'Left';
                   BtHigh.Caption := 'Right';
                  end;
           stMS : begin
                   BtLow.Caption  := 'Mid';
                   BtHigh.Caption := 'Side';
                  end;
       stSerial : begin
                   BtLow.Caption  := 'Stage 1';
                   BtHigh.Caption := 'Stage 2';
                  end;
   end;
  end;
end;

procedure TFmSplitter.UpdateFrequency;
var
  Freq : Single;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   Freq := ParameterByName['Frequency'];
   if DialSplitFrequency.Position <> Freq
    then DialSplitFrequency.Position := Freq;
   if Freq < 1000
    then LbSplitFrequency.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + 'Hz'
    else LbSplitFrequency.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + 'kHz';
  end;
end;

procedure TFmSplitter.UpdateOrder;
var
  Order : Single;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialSplitOrder.Position <> Order
    then DialSplitOrder.Position := Order;
   if SplitType = stLiRi
    then LbSplitOrder.Caption := ConvertOrderToString(2 * round(Order))
    else LbSplitOrder.Caption := ConvertOrderToString(round(Order));
  end;
end;

procedure TFmSplitter.UpdateOverSampling;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   GuiLEDOversampling.Brightness_Percent := 20 + 60 * (f_Limit(ParameterByName['Oversampling'], 0, 1));

   DialOversampling.Visible     := round(ParameterByName['Oversampling']) = 1;
   LbOversamplingFactor.Visible := DialOversampling.Visible;
   LbOversampling.Width         := 97 + 5 * round(ParameterByName['Oversampling']);
  end;
end;

procedure TFmSplitter.UpdateOSFactor;
var
  OSFactor : Single;
begin
 with TSplitTemplateDataModule(Owner) do
  begin
   OSFactor := ParameterByName['OS Factor'];
   if DialOversampling.Position <> OSFactor
    then DialOversampling.Position := OSFactor;
   LbOversamplingFactor.Caption := IntToStr(round(OSFactor)) + 'x';
  end;
end;

end.
