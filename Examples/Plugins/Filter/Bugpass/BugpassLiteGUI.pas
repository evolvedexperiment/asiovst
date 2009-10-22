unit BugpassLiteGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, ExtCtrls,
  DAV_Types, DAV_VSTModule, DAV_GuiLabel, DAV_GuiCommon, DAV_GuiBaseControl,
  DAV_GuiDial;

type
  TMouseEdit = (meNone, meLow, meHigh);
  TFmBugpassLite = class(TForm)
    LbTitle: TGuiLabel;
    LbFreqLowValue: TGuiLabel;
    FrequencyBar: TPaintBox;
    LbTitleShadow: TGuiLabel;
    LbSubTitle: TGuiLabel;
    LbSubtitleShadow: TGuiLabel;
    LbFreqHighValue: TGuiLabel;
    GuiLabel1: TGuiLabel;
    GuiLabel2: TGuiLabel;
    procedure DialFrequencyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FrequencyBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FrequencyBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FrequencyBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
    FMouseEdit       : TMouseEdit;
  public
    procedure UpdateFrequencyBar;
  end;

implementation

{$R *.DFM}

uses
  BugpassLiteDM, DAV_VSTModuleWithPrograms;

procedure TFmBugpassLite.FormCreate(Sender: TObject);
var
  x, y : Integer;
  s    : array[0..1] of Single;
  amt  : Shortint;
  Line : PRGB24Array;
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
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.9 * s[0] + 0.1 * random;
       amt  := round($E * (2 * s[1] - 1));
       Line[x].B := $E0 + amt;
       Line[x].G := $D0 + amt;
       Line[x].R := $B6 + amt;
       s[0] := s[1];
      end;
    end;
  end;
 FrequencyBar.ControlStyle := FrequencyBar.ControlStyle + [csOpaque];
end;

procedure TFmBugpassLite.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmBugpassLite.FormShow(Sender: TObject);
begin
 UpdateFrequencyBar;
end;

procedure TFmBugpassLite.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmBugpassLite.FrequencyBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 with TBugpassLiteDataModule(Owner), FrequencyBar do
  if abs(X - FreqLogToLinear(Parameter[0]) * Width) < 5
   then FMouseEdit := meLow else
  if abs(X - FreqLogToLinear(Parameter[1]) * Width) < 5
   then FMouseEdit := meHigh
   else FMouseEdit := meNone;
end;

procedure TFmBugpassLite.FrequencyBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if ssLeft in Shift then
  with TBugpassLiteDataModule(Owner), FrequencyBar do
   case FMouseEdit of
    meLow  : Parameter[0] := Limit(FreqLinearToLog(Limit(X / Width, 0, Width)), 20, 20000);
    meHigh : Parameter[1] := Limit(FreqLinearToLog(Limit(X / Width, 0, Width)), 20, 20000);
   end;
end;

procedure TFmBugpassLite.FrequencyBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FMouseEdit := meNone;
end;

procedure TFmBugpassLite.PaintBoxPaint(Sender: TObject);
begin
 with TBugpassLiteDataModule(Owner), FrequencyBar, Canvas do
  begin
   Brush.Color := $00C1AE9B;
   FillRect(Rect(0, 0, round(FreqLogToLinear(Parameter[0]) * Width), Height));
   FillRect(Rect(round(FreqLogToLinear(Parameter[1]) * Width), 0, Width, Height));

   Brush.Color := $00372D22;
   FillRect(Rect(round(FreqLogToLinear(Parameter[0]) * Width), 0,
                 round(FreqLogToLinear(Parameter[1]) * Width), Height));
  end;
end;

procedure TFmBugpassLite.DialFrequencyChange(Sender: TObject);
begin
 with TBugpassLiteDataModule(Owner) do
  begin

  end;
end;

procedure TFmBugpassLite.UpdateFrequencyBar;
var
  Freq : array [0..1] of Single;
begin
 with TBugpassLiteDataModule(Owner) do
  begin
   Freq[0] := Parameter[0];
   Freq[1] := Parameter[1];

   FrequencyBar.Invalidate;

   if Freq[0] < 1000
    then LbFreqLowValue.Caption := FloatToStrF(Freq[0], ffGeneral, 3, 3) + ' Hz'
    else LbFreqLowValue.Caption := FloatToStrF(1E-3 * Freq[0], ffGeneral, 3, 3) + ' kHz';
   if Freq[1] < 1000
    then LbFreqHighValue.Caption := FloatToStrF(Freq[1], ffGeneral, 3, 3) + ' Hz'
    else LbFreqHighValue.Caption := FloatToStrF(1E-3 * Freq[1], ffGeneral, 3, 3) + ' kHz';
  end;
end;

end.
