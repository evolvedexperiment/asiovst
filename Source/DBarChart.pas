unit DBarChart;

interface

uses {$IFDEF FPC} LCLIntf, LResources, LMessages, Windows,
     {$ELSE} Windows, RTLConsts, {$ENDIF}
     Classes, Graphics, Forms, Messages, SysUtils, Controls;

{$R DBarChart.res}
{$R-}
{$DEFINE x87}
{$I JEDI.INC}

const
  cNumFrequencies = 32;
  cThirdOctaveFrequencies : Array [0..cNumFrequencies-1] of Single =
      (16,20,25,31,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,
       1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500,16000,20000);

type
  TPaintEvent = procedure(Sender: TObject; const Buffer: TBitmap) of object;

  TFrequencyBarChart = class(TGraphicControl)
  private
    fOnPaint             : TPaintEvent;
    fFontAlpha           : Byte;
    fFontQuality         : Byte;
    fMargin              : TRect;
    fOldMouse            : TPoint;
    fDoubleClick         : Boolean;
    fTransparent         : Boolean;
    fMagnitudeArray      : array [0..cNumFrequencies-1] of Double;
    fMagnitudeUpper      : Double;
    fMagnitudeLower      : Double;
    fRange,fRangeReci    : Double;
    fGranular            : Double;
    fZeroPosition        : Double;
    procedure SetFontAlpha(const Value: Byte);
    procedure SetFontQuality(const Value: Byte);
    procedure DrawFast;
    procedure DrawAxis;
    procedure SetTransparent(const Value: Boolean);
    function GetMagnitudedB(index: Integer): Double;
    procedure SetMagnitudedB(index: Integer; const Value: Double);
    procedure SetMagnitudeLower(const Value: Double);
    procedure SetMagnitudeUpper(const Value: Double);
    procedure SetRange(const Value: Double);
    procedure CalcGranularity;
    procedure SetMargin(const Value: TRect);
  protected
    fChartRect    : TRect;
    fDoubleBuffer : TBitmap;
    procedure CalcChartRect;
    {$IFDEF FPC}
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
    {$ELSE}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    {$ENDIF}
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure DblClick; override;
    property Range : Double read fRange write SetRange;
    procedure Paint; override;
    procedure Resize; override;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetAxis;
    procedure SetMagnitudeLimits(const Upper, Lower : Double);
    property MagnitudedB[index : Integer] : Double read GetMagnitudedB write SetMagnitudedB;
  published
    property Align;
    property Anchors;
    {$IFNDEF FPC}
    property BiDiMode;
    property OnCanResize;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property Margin: TRect read fMargin write SetMargin;
    {$ENDIF}
    property Constraints;
    property Color;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property PopupMenu;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Font;
    property FontAlpha: Byte read fFontAlpha write SetFontAlpha default 255;
    property FontQuality: Byte read fFontQuality write SetFontQuality default 4;
    property Transparent : Boolean read fTransparent write SetTransparent default false;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    property MagnitudeLower : Double read fMagnitudeLower write SetMagnitudeLower;
    property MagnitudeUpper : Double read fMagnitudeUpper write SetMagnitudeUpper;
  end;

var ln2,ln2Rez : Double;

const crUp=1;
      crDown=2;
      crMid=3;
      cr5dB=4;

procedure Register;

implementation

uses Math, Dialogs, Types;

procedure Register;
begin
 RegisterComponents('Audio', [TFrequencyBarChart]);
end;

{ TFrequencyBarChart }

constructor TFrequencyBarChart.Create(AOwner: TComponent);
var i : Integer;
begin
 inherited Create(AOwner);
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                  csDoubleClicks, csReplicatable, csOpaque];
 Canvas.Font.Assign(Font);
 fDoubleBuffer:=TBitmap.Create;
 fDoubleBuffer.Width:=Width;
 fDoubleBuffer.Height:=Height;
 fTransparent:=false;
 fFontAlpha:=255;
 fFontQuality:=2;
 fOldMouse.Y:=-1;
 fOldMouse.X:=-1;
 SetMagnitudeLimits(90,0);
 for i:=0 to Length(fMagnitudeArray)-1
  do fMagnitudeArray[i]:=0;
end;

destructor TFrequencyBarChart.Destroy;
begin
 FreeAndNil(fDoubleBuffer);
 inherited Destroy;
end;

procedure TFrequencyBarChart.CalcChartRect;
begin
 fChartRect.Top:=max(ClientRect.Top,ClientRect.Top+fMargin.Top);
 fChartRect.Right:=min(ClientRect.Right,ClientRect.Right-fMargin.Right);
 fChartRect.Bottom:=min(ClientRect.Bottom,ClientRect.Bottom-fMargin.Bottom)-round(1.1*(Canvas.TextHeight('1k')+Canvas.TextHeight('Frequency [Hz]')));
 fChartRect.Left:=max(ClientRect.Left,ClientRect.Left+fMargin.Left)+Canvas.TextWidth('100')+4+Canvas.TextHeight('dB');
end;

procedure TFrequencyBarChart.CMColorChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TFrequencyBarChart.DrawFast;
var i           : Integer;
    r           : TRect;
    Offset      : Integer;
    BarWidth    : Integer;
    ChartWidth  : Integer;
    ChartHeight : Integer;
    MagCount    : Integer;
begin
 with fDoubleBuffer.Canvas do
  begin
   ChartWidth:=fChartRect.Right-fChartRect.Left-3;
   ChartHeight:=fChartRect.Bottom-fChartRect.Top;
   BarWidth:=ChartWidth div Length(fMagnitudeArray);
   MagCount:=Length(fMagnitudeArray);
   if BarWidth>2
    then begin BarWidth:=BarWidth-1; Offset:=1; end
    else Offset:=0;
   for i:=0 to MagCount-1 do
    begin
     r:=Rect(3+fChartRect.Left+Round(ChartWidth*i/(MagCount)), max(fChartRect.Bottom-Round((fMagnitudeArray[i]-fMagnitudeLower)/fRange*ChartHeight),0), 3+fChartRect.Left+Round(ChartWidth*i/(MagCount) +BarWidth)-Offset,fChartRect.Bottom);
     if r.Top>r.Bottom then continue;
     case (i mod 10) of
      3: Brush.Color:=$0A0A8A;
      8: Brush.Color:=$0C0CCA;
      else Brush.Color:=$0A0A8A;
     end;
     Pen.Color:=$0A0A0A;
     FillRect(r);
    end;
  end;
end;

procedure DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: Integer;
  Position: TPoint;
begin
 with Control do
  begin
   if Parent = nil then Exit;
   DC := Dest.Handle;
   SaveIndex := SaveDC(DC);
   GetViewportOrgEx(DC, Position);
   SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
   IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
   Parent.Perform(WM_ERASEBKGND, DC, 0);
   Parent.Perform(WM_PAINT, DC, 0);
   RestoreDC(DC, SaveIndex);
  end;
end;

procedure TFrequencyBarChart.DrawAxis;
var s,c          : Double;
    txt          : string;
    ChartHeight  : Integer;
    ChartWidth   : Integer;
    i,x,y        : Integer;
    BarWidth     : Integer;
    MagCount     : Integer;
    BitMaps      : Array [0..1] of TBitmap;
    OldFontStyle : TFontStyles;

 function FloatToStrX(Value: Double): string;
 begin
  if abs(Value)<1E-3 then result:='0' else
  if Value>=1000000
   then result:=FloatToStrF(Value*1E-6,ffGeneral,4,4)+'M'
   else
  if Value>=1000
   then result:=FloatToStrF(Value*1E-3,ffGeneral,4,4)+'k'
   else
  if Abs(Value)<=0.001
   then result:=FloatToStrF(Value*1E+3,ffGeneral,4,4)+'m'
   else result:=FloatToStrF(Value     ,ffGeneral,4,4)
 end;

begin
 with fDoubleBuffer.Canvas do
  begin
   if fTransparent
    then DrawParentImage(Self, fDoubleBuffer.Canvas)
    else
     begin
      Brush.Color:=Self.Color;
      FillRect(ClientRect);
     end;

   Pen.Color:=clBlack;
   FrameRect(fChartRect);
   ChartHeight:=(fChartRect.Bottom-fChartRect.Top);

   if fRange<>0 then
    begin
     s:=fGranular*fRangeReci;
     c:=fZeroPosition+Ceil((-fZeroPosition/s))*s;
     if c<0 then c:=c+2*s;
     if c>=0 then
      while c<1 do
       begin
        MoveTo(fChartRect.Left-2,fChartRect.Top+round((1-c)*ChartHeight));
        if Abs(c-fZeroPosition)<0.1*s
         then Canvas.Pen.Color:=clSilver
         else Canvas.Pen.Color:=clGray;
        LineTo(fChartRect.Right,fChartRect.Top+round((1-c)*ChartHeight));
        txt:=FloatToStrX((-fZeroPosition+c)*fRange);
        TextOut(fChartRect.Left-Canvas.TextWidth(txt)-4,fChartRect.Top+round((1-c)*ChartHeight)-Canvas.TextHeight(txt) div 2,txt);
        c:=c+s;
       end;
    end;

   BitMaps[0]:=TBitmap.Create;
   with BitMaps[0] do
    try
     Canvas.Brush.Color:=Self.Color;
     Canvas.Pen.Color:=Self.Color;
     Canvas.Font:=Self.Font;
     Canvas.Font.Style:=Font.Style+[fsBold];
     Canvas.Font.Height:=Font.Height;
     txt:='Magnitude [dB]';
     Width:=Canvas.TextWidth(txt);
     Height:=Canvas.TextHeight(txt);
     Canvas.Textout(0,0,txt);
     BitMaps[1]:=TBitmap.Create;
     BitMaps[1].Height:=Width;
     BitMaps[1].Width:=Height;
     with BitMaps[1] do
      try
       Brush.Color:=Self.Color;
       Pen.Color:=Self.Color;
       for x:=0 to Width-1 do
        for y:=0 to Height-1
         do BitMaps[1].Canvas.Pixels[X,Height-Y-1]:=BitMaps[0].Canvas.Pixels[Y,X];
       fDoubleBuffer.Canvas.Draw(0,(fChartRect.Bottom-fChartRect.Top-Height) div 2,BitMaps[1]);
      finally
       Free;
      end;
    finally
     Free;
    end;

   OldFontStyle:=Font.Style;
   i:=Font.Size; Font.Size:=Font.Size-1;
   Font.Style:=Font.Style+[fsBold];
   txt:='Frequency [Hz]';
   Textout(fChartRect.Left+(fChartRect.Right-fChartRect.Left-Canvas.TextWidth(txt)) div 2,Round(fChartRect.Bottom+1.05*Canvas.TextHeight(txt)),txt);
   Font.Size:=i;
   Font.Style:=OldFontStyle;

   ChartWidth:=fChartRect.Right-fChartRect.Left-3;
   BarWidth:=ChartWidth div Length(fMagnitudeArray);
   MagCount:=Length(fMagnitudeArray);
   if BarWidth>2 then BarWidth:=BarWidth-1;

   txt:='31.5';
   s:=3+fChartRect.Left+ChartWidth* 3/(MagCount)+(BarWidth-TextWidth(txt))*0.5;
   Textout(Round(s),fChartRect.Bottom+1,txt);

   txt:='100';
   s:=3+fChartRect.Left+ChartWidth* 8/(MagCount)+(BarWidth-TextWidth(txt))*0.5;
   Textout(Round(s),fChartRect.Bottom+1,txt);

   txt:='315';
   s:=3+fChartRect.Left+ChartWidth*13/(MagCount)+(BarWidth-TextWidth(txt))*0.5;
   Textout(Round(s),fChartRect.Bottom+1,txt);

   txt:='1k';
   s:=3+fChartRect.Left+ChartWidth*18/(MagCount)+(BarWidth-TextWidth(txt))*0.5;
   Textout(Round(s),fChartRect.Bottom+1,txt);

   txt:='3150';
   s:=3+fChartRect.Left+ChartWidth*23/(MagCount)+(BarWidth-TextWidth(txt))*0.5;
   Textout(Round(s),fChartRect.Bottom+1,txt);

   txt:='10k';
   s:=3+fChartRect.Left+ChartWidth*28/(MagCount)+(BarWidth-TextWidth(txt))*0.5;
   Textout(Round(s),fChartRect.Bottom+1,txt);
  end;
end;

procedure TFrequencyBarChart.SetFontAlpha(const Value: Byte);
begin
 if fFontAlpha<>Value then
  begin
   fFontAlpha := Value;
   Invalidate;
  end;
end;

procedure TFrequencyBarChart.SetFontQuality(const Value: Byte);
begin
 if fFontQuality<>Value then
  begin
   fFontQuality := Value;
   Invalidate;
  end;
end;

procedure TFrequencyBarChart.MouseMove(Shift: TShiftState; X, Y: Integer);
var dt : Double;
begin
 if X<fChartRect.Left
  then Cursor:=crSizeNS
  else Cursor:=crDefault;

 if fOldMouse.Y>-1 then
  begin
   dt:=(Y-fOldMouse.Y)*(fRange/(fChartRect.Bottom-fChartRect.Top));
   if ssCtrl in Shift
    then MagnitudeLower:=fMagnitudeLower+dt
    else SetMagnitudeLimits(fMagnitudeUpper+dt,fMagnitudeLower+dt);
   fOldMouse.Y:=Y;
  end;
 inherited;
end;

procedure TFrequencyBarChart.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 try
  if (Y>fChartRect.Bottom) then fOldMouse.X:=X else
  if (X<fChartRect.Left) then fOldMouse.Y:=Y;
 finally
  inherited;
 end;
end;

procedure TFrequencyBarChart.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fOldMouse.X:=-1;
 fOldMouse.Y:=-1;
 inherited;
end;

procedure TFrequencyBarChart.ResetAxis;
begin
end;

procedure TFrequencyBarChart.SetMagnitudeLower(const Value: Double);
begin
 if fMagnitudeLower<>Value then
  begin
   fMagnitudeLower := Value;
   Range := fMagnitudeUpper - fMagnitudeLower;
   Invalidate;
  end;
end;

procedure TFrequencyBarChart.SetMagnitudeUpper(const Value: Double);
begin
 if fMagnitudeUpper<>Value then
  begin
   fMagnitudeUpper := Value;
   Range := fMagnitudeUpper - fMagnitudeLower;
   Invalidate;
  end;
end;

procedure TFrequencyBarChart.SetMagnitudeLimits(const Upper, Lower: Double);
begin
 if (fMagnitudeLower<>Lower) or (fMagnitudeUpper<>Upper) then
  begin
   fMagnitudeLower := Lower;
   fMagnitudeUpper := Upper;
   Range := fMagnitudeUpper - fMagnitudeLower;
   Invalidate;
  end;
end;

procedure TFrequencyBarChart.CalcGranularity;
var
  FontHeight : Integer;
  Ep1,Ep2    : Integer;
  MaxLabDist : Double;
  MaxLabNo   : Integer;

 function CeiL10(Value: Double):Integer;
 {$IFNDEF FPC}
 const half : Double = 0.5;
 asm
  fldlg2
  fld Value
  fabs
  fyl2x
  fsub half
  fistp result.Integer
 end;
 {$ELSE}
 begin
  result:=Ceil(Log2(abs(Value)));
 end;
 {$ENDIF}

begin
 try
  if Parent<>nil
   then FontHeight := 2*Canvas.TextHeight('dB')
   else FontHeight := 20;
 except
  FontHeight := 20;
 end;

 if fChartRect.Bottom=fChartRect.Top
  then MaxLabNo:=10
  else MaxLabNo:=round((fChartRect.Bottom-fChartRect.Top) / (2*FontHeight)-0.5);
 MaxLabDist:=(fRange/MaxLabNo);
 Ep1:=CeiL10(MaxLabDist);
 Ep2:=Ceil(MaxLabDist*IntPower(10,-Ep1));
 case Ep2 of
  1       : fGranular:=IntPower(10,Ep1);
  2,3     : fGranular:=2*IntPower(10,Ep1);
  4,5,6,7 : fGranular:=5*IntPower(10,Ep1);
  8,9,10  : fGranular:=IntPower(10,Ep1+1);
 end;
 if fGranular<1 then fGranular:=1;
end;

procedure TFrequencyBarChart.SetRange(const Value: Double);
begin
 if fRange <> Value then
  begin
   fRange := Value;
   if fRange<>0
    then fRangeReci:=abs(1/fRange)
    else begin fRangeReci:=1; fRange:=1; exit; end;
   fZeroPosition:=-min(fMagnitudeUpper,fMagnitudeLower)*fRangeReci;
   CalcGranularity;
  end;
end;

procedure TFrequencyBarChart.DblClick;
begin
 inherited;
 fDoubleClick:=True;
end;

procedure TFrequencyBarChart.SetTransparent(const Value: Boolean);
begin
 if fTransparent<>Value then
  begin
   fTransparent:=Value;
   Invalidate;
  end;
end;

function TFrequencyBarChart.GetMagnitudedB(index: Integer): Double;
begin
 if (index<0) or (index>=cNumFrequencies)
  then raise Exception.Create('Index out of bounds');
 Result:=fMagnitudeArray[index];
end;

procedure TFrequencyBarChart.SetMagnitudedB(index: Integer; const Value: Double);
begin
 if (index<0) or (index>=cNumFrequencies)
  then raise Exception.Create('Index out of bounds');
 fMagnitudeArray[index]:=Value;
end;

procedure TFrequencyBarChart.SetMargin(const Value: TRect);
begin
 fMargin := Value;
 CalcChartRect;
end;

procedure TFrequencyBarChart.Paint;
begin
 inherited;
 DrawAxis;
 DrawFast;
 Canvas.Draw(0,0,fDoubleBuffer);
end;

procedure TFrequencyBarChart.Resize;
begin
 inherited;
 CalcChartRect;
 fDoubleBuffer.Width:=Width;
 fDoubleBuffer.Height:=Height;
end;

procedure TFrequencyBarChart.ReadState(Reader: TReader);
begin
 inherited;
 CalcChartRect;
 fDoubleBuffer.Width:=Width;
 fDoubleBuffer.Height:=Height;
end;

initialization

end.
