unit DAV_GuiGraphXY;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, Types, {$ELSE} Windows, {$ENDIF}
  Classes, Graphics, Forms, Messages, SysUtils, RTLConsts, Controls,
  DAV_Classes, DAV_GuiBaseControl;

type
  TCustomAxisFlag = (cafAutoGranularity, cafAutoExtendBounds);
  TCustomAxisFlags = set of TCustomAxisFlag;
  TCustomGuiGraphXY = class;

  TCustomAxis = class(TPersistent)
  private
    FFlags         : TCustomAxisFlags;
    FGranularity   : Double;
    FLower         : Double;
    FUpper         : Double;
    FMaximum       : Double;
    FMinimum       : Double;
    FMinGranDist   : Integer;
    FOnChanged     : TNotifyEvent;
    FGranBase      : Integer;
    FPixelPerValue : Double;
    FPixelSize     : Integer;
    FRange         : Double;
    FRangeReci     : Double;
    FValuePerPixel : Double;
    FZeroPosition  : Double;
    function CalculateAutoGranularity: Boolean;
    procedure CalculatePixelValueRelation;
    procedure CalculateRange;
    procedure CalculateZeroPosition;
    procedure Changed;
    procedure GranularityBaseChanged;
    procedure MinimumGranularityDistanceChanged;
    procedure SetFlags(const Value: TCustomAxisFlags);
    procedure SetGranularity(const Value: Double);
    procedure SetGranularityBase(const Value: Integer);
    procedure SetLower(Value: Double);
    procedure SetMaximum(Value: Double);
    procedure SetMinGranDist(Value: Integer);
    procedure SetMinimum(Value: Double);
    procedure SetPixelSize(Value: Integer);
    procedure SetUpper(Value: Double);
  protected
    procedure AutoExtendBoundsFlagChanged; virtual;
    procedure AutoGranularityFlagChanged; virtual;
    procedure GranularityChanged; virtual;
    procedure RangeChanged; virtual;
    procedure LowerChanged; virtual;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure UpperChanged; virtual;
    procedure Resized; virtual;
    procedure AssignTo(Dest: TPersistent); override;

    property ZeroPosition: Double read FZeroPosition;
    property ValuePerPixel: Double read FValuePerPixel;
    property PixelPerValue: Double read FPixelPerValue;
  public
    constructor Create; virtual;
    procedure SetBounds(Lower, Upper: Double);

    property Range: Double read fRange;
    property PixelSize: Integer read FPixelSize write SetPixelSize nodefault;
  published
    property Flags: TCustomAxisFlags read FFlags write SetFlags default [cafAutoGranularity];
    property Granularity: Double read FGranularity write SetGranularity;
    property GranularityBase: Integer read FGranBase write SetGranularityBase default 10; 
    property MinimumGranularityDistance: Integer read FMinGranDist write SetMinGranDist default 30; 
    property Minimum: Double read FMinimum write SetMinimum;
    property Maximum: Double read FMaximum write SetMaximum;
    property Lower: Double read FLower write SetLower;
    property Upper: Double read FUpper write SetUpper;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TCustomGuiGraphXYSeriesClass = class of TCustomGuiGraphXYSeries;
  TCustomGuiGraphXYSeries = class(TPersistent)
  private
    FColor    : TColor;
    FTag      : Integer;
    FVisible  : Boolean;
    FOnChange : TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed;
    procedure PaintToGraph(const GraphXY: TCustomGuiGraphXY; const Bitmap: TBitmap); virtual; abstract;
  public
    constructor Create; virtual;

    property Color: TColor read FColor write SetColor default clRed;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Tag: Longint read FTag write FTag default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFunctionEvaluateEvent = function (Sender: TObject; X: Double): Double of object;

  TCustomGuiGraphXYFunctionSeries = class(TCustomGuiGraphXYSeries)
  private
    FOnEvaluate : TFunctionEvaluateEvent;
    procedure SetOnEvaluate(const Value: TFunctionEvaluateEvent);
  protected
    procedure PaintToGraph(const GraphXY: TCustomGuiGraphXY; const Bitmap: TBitmap); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property OnEvaluate: TFunctionEvaluateEvent read FOnEvaluate write SetOnEvaluate;
  end;

  TGuiGraphXYFunctionSeries = class(TCustomGuiGraphXYFunctionSeries)
  published
    property Color;
    property Tag;
    property Visible;
    property OnEvaluate;
  end;

  TDAVPointSingle = record
    x, y : Single;
  end;
  TDAVPointSingleFixedArray = array of TDAVPointSingle;
  PDAVPointSingleFixedArray = ^TDAVPointSingleFixedArray;

  TCustomGuiGraphXYDataSeries = class(TCustomGuiGraphXYSeries)
  private
    FData  : PDAVPointSingleFixedArray;
    FCount : Integer;
  protected
    function Get(Index: Integer): TDAVPointSingle;
    procedure Put(Index: Integer; Item: TDAVPointSingle);
  public
    constructor Create; override;
    destructor Destroy; override;

    function AddPoint(X, Y : Single): Integer; overload; virtual;
    function AddPoint(Item: TDAVPointSingle): Integer; overload; virtual; abstract;
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Clear; virtual;
    function First: TDAVPointSingle;
    function Last: TDAVPointSingle;

    property Count: Integer read FCount;
    property Items[Index: Integer]: TDAVPointSingle read Get write Put; default;
  end;

  TGuiGraphXYDataSeries = class(TCustomGuiGraphXYDataSeries)
  public
    function AddPoint(Item: TDAVPointSingle): Integer; overload; override;
    function Extract(Item: TDAVPointSingle): TDAVPointSingle;
    function IndexOf(Item: TDAVPointSingle): Integer;
    function Remove(Item: TDAVPointSingle): Integer;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Sort;
  published
    property Color;
    property Visible;
  end;

  TGuiGraphXYSortedDataSeries = class(TGuiGraphXYDataSeries)
  public
    function AddPoint(Item: TDAVPointSingle): Integer; overload; override;
  published
    property Color;
    property Visible;
  end;

  TGuiGraphXYSeriesCollectionItem = class(TCollectionItem)
  private
    FSeries             : TCustomGuiGraphXYSeries;
    FDisplayName        : string;
    FSeriesClassChanged : TNotifyEvent;
    function GetSeriesClassName: string;
    procedure SetSeries(const Value: TCustomGuiGraphXYSeries);
    procedure SetSeriesClassName(const Value: string);
    procedure Changed;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property DisplayName;
    property SeriesClassName: string read GetSeriesClassName write SetSeriesClassName;
    property Series: TCustomGuiGraphXYSeries read FSeries write SetSeries;
    property SeriesClassChanged: TNotifyEvent read FSeriesClassChanged write FSeriesClassChanged;
  end;

  TGuiGraphXYSeriesCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiGraphXYSeriesCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiGraphXYSeriesCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiGraphXYSeriesCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGuiGraphXYSeriesCollectionItem;
    function Insert(Index: Integer): TGuiGraphXYSeriesCollectionItem;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TGraphXYFlag = (gfShowLabels);
  TGraphXYFlags = set of TGraphXYFlag;
  TCustomGuiGraphXY = class(TCustomGuiBaseAntialiasedControl)
  private
    FOnChange         : TNotifyEvent;
    FBorderColor      : TColor;
    FFrameColor       : TColor;
    FXAxis            : TCustomAxis;
    FYAxis            : TCustomAxis;
    FSeriesCollection : TGuiGraphXYSeriesCollection;
    FFlags            : TGraphXYFlags;
    FBorderRadius     : Integer;
    function GetSeriesCollectionItem(Index: Integer): TGuiGraphXYSeriesCollectionItem;
    procedure SetSeriesCollectionItem(Index: Integer; const Value: TGuiGraphXYSeriesCollectionItem);
    procedure SetFlags(const Value: TGraphXYFlags);
    procedure ShowLabelsChanged;
    procedure SetFrameColor(const Value: TColor);
    procedure SetSeriesCollection(const Value: TGuiGraphXYSeriesCollection);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BorderColorChanged; virtual;
    procedure BorderRadiusChanged; virtual;
    procedure FrameColorChanged; virtual;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RenderGraphXYToBitmap(const Bitmap: TBitmap); virtual;
    procedure Resize; override;
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure UpdateBuffer; override;
    property SeriesCollectionItem[Index: Integer]: TGuiGraphXYSeriesCollectionItem read GetSeriesCollectionItem write SetSeriesCollectionItem; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateGraph;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clRed;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius default 0;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clRed;
    property Flags: TGraphXYFlags read FFlags write SetFlags default [gfShowLabels];
    property SeriesCollection: TGuiGraphXYSeriesCollection read FSeriesCollection write SetSeriesCollection;
    property XAxis: TCustomAxis read FXAxis write FXAxis;
    property YAxis: TCustomAxis read FYAxis write FYAxis;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiGraphXY = class(TCustomGuiGraphXY)
  published
    property Font;
    property Align;
    property Anchors;
    property AntiAlias;
    property BorderColor;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FrameColor;
    property LineColor;
    property LineWidth;
    property OnChange;
    property PopupMenu;
    property SeriesCollection;
    property ShowHint;
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property Visible;
  end;

var
  SeriesClassList: TClassList;

implementation

uses
  ExtCtrls, Math, DAV_Common, DAV_Complex;

resourcestring
  RCStrCalculateRange = 'The upper value must not be equal to the lower value!' + #10#13 +
                        'If you need to set both values at the same time use' +  #10#13 +
                        'SetBounds(Lower, Upper: Double)';

{ TCustomAxis }

constructor TCustomAxis.Create;
begin
 inherited;

 // set some initial values manually
 FLower       := -5;
 FUpper       :=  5;
 FMinimum     := -5;
 FMaximum     :=  5;
 FGranularity :=  1;
 FPixelSize   :=  1;
 FMinGranDist := 30;
 FGranBase    := 10;
 FFlags       := [cafAutoGranularity];

 // set missing initial values automatically
 CalculateRange;
 CalculateZeroPosition;
 CalculatePixelValueRelation;
end;

procedure TCustomAxis.SetBounds(Lower, Upper: Double);
begin
 if not (cafAutoExtendBounds in Flags) then
  begin
   if (Lower < Minimum) then Lower := Minimum;
   if (Upper > Maximum) then Upper := Maximum;
  end;

 if FLower <> Lower then
  begin
   FLower := Lower;
   LowerChanged;
  end;

 if FUpper <> Upper then
  begin
   FUpper := Upper;
   UpperChanged;
  end;
end;

procedure TCustomAxis.SetFlags(const Value: TCustomAxisFlags);
var
  OldFlags: TCustomAxisFlags;
begin
 if FFlags <> Value then
  begin
   OldFlags := FFlags;
   FFlags   := Value;
   if (cafAutoGranularity in FFlags) xor
      (cafAutoGranularity in OldFlags)
    then AutoGranularityFlagChanged;
   if (cafAutoExtendBounds in FFlags) xor
      (cafAutoExtendBounds in OldFlags)
    then AutoExtendBoundsFlagChanged;
  end;
end;

procedure TCustomAxis.AutoGranularityFlagChanged;
begin
 if cafAutoGranularity in Flags then
  if CalculateAutoGranularity then Changed;
end;

procedure TCustomAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomAxis then
  with TCustomAxis(Dest) do
   begin
    FFlags         := Self.FFlags;
    FGranularity   := Self.FGranularity;
    FLower         := Self.FLower;
    FUpper         := Self.FUpper;
    FMaximum       := Self.FMaximum;
    FMinimum       := Self.FMinimum;
    FMinGranDist   := Self.FMinGranDist;
    FOnChanged     := Self.FOnChanged;
    FGranBase      := Self.FGranBase;
    FPixelPerValue := Self.FPixelPerValue;
    FPixelSize     := Self.FPixelSize;
    FRange         := Self.FRange;
    FRangeReci     := Self.FRangeReci;
    FValuePerPixel := Self.FValuePerPixel;
    FZeroPosition  := Self.FZeroPosition;
   end else inherited;
end;

procedure TCustomAxis.AutoExtendBoundsFlagChanged;
begin
 if not (cafAutoExtendBounds in Flags) then
  begin
   if FLower < FMinimum then FLower := FMinimum;
   if FUpper > FMaximum then FLower := FMaximum;
  end;
end;

function TCustomAxis.CalculateAutoGranularity: Boolean;
var
  OldGranularity : Double;
  MinGran        : Double;
  MinCount       : Integer;
  GranRange      : Double;
  IntExp         : Integer;
  BaseGran       : Integer;
begin
 OldGranularity := FGranularity;
 MinGran   := MinimumGranularityDistance * ValuePerPixel;
 MinCount  := Round(PixelSize / MinimumGranularityDistance - 0.5);
 if MinCount > 0 then
  begin
   GranRange := Range / MinCount;
   IntExp := Round(Log2(GranRange) / Log2(FGranBase) - 0.5);
   BaseGran := Round(GranRange * IntPower(FGranBase, -IntExp) + 0.5);
   case BaseGran of
    3, 4 : BaseGran := 5;
    6..9 : BaseGran := 10;
   end;
   FGranularity := BaseGran * IntPower(FGranBase, IntExp);
//   Assert(FGranularity > MinGran);
   Result := OldGranularity <> FGranularity;
  end
 else Result := False;
end;

procedure TCustomAxis.GranularityChanged;
begin
 Changed; 
end;

procedure TCustomAxis.MinimumChanged;
begin
 // check, whether lower is outside new minimum and limit if necessary
 if not (cafAutoExtendBounds in Flags) and (Lower < Minimum)
  then Lower := Minimum;
end;

procedure TCustomAxis.CalculateRange;
begin
 FRange := FUpper - FLower;
 if FRange <> 0
  then FRangeReci := 1 / FRange
  else raise Exception.Create(RCStrCalculateRange);
end;

procedure TCustomAxis.RangeChanged;
begin
 CalculateRange;
 CalculatePixelValueRelation;
 CalculateZeroPosition;
 if cafAutoGranularity in Flags
  then CalculateAutoGranularity;
 Changed;
end;

procedure TCustomAxis.CalculateZeroPosition;
begin
 FZeroPosition := -fLower * fRangeReci;
end;

procedure TCustomAxis.MaximumChanged;
begin
 // check, whether lower is outside new minimum and limit if necessary
 if not (cafAutoExtendBounds in Flags) and (Upper > Maximum)
  then Upper := Maximum;
end;

procedure TCustomAxis.LowerChanged;
begin
 // check, whether new lower exceed minimum and extend if necessary
 if (cafAutoExtendBounds in Flags) and (Lower < Minimum)
  then Minimum := Lower;

 // calculate new range
 RangeChanged;
end;

procedure TCustomAxis.UpperChanged;
begin
 // check, whether new upper exceed maximum and extend if necessary
 if (cafAutoExtendBounds in Flags) and (Upper < Maximum)
  then Maximum := Upper;

 // calculate new range
 RangeChanged;
end;

procedure TCustomAxis.SetGranularity(const Value: Double);
begin
 if (FGranularity <> Value) and not (cafAutoGranularity in Flags) then
  begin
   FGranularity := Value;
   GranularityChanged;
  end;
end;

procedure TCustomAxis.SetGranularityBase(const Value: Integer);
begin
 if (Value <= 0) or (Value > 1E10) then exit;
 if FGranBase <> Value then
  begin
   FGranBase := Value;
   GranularityBaseChanged;
  end;
end;

procedure TCustomAxis.SetMinGranDist(Value: Integer);
begin
 if Value < 1 then Value := 1;
 if FMinGranDist <> Value then
  begin
   FMinGranDist := Value;
   MinimumGranularityDistanceChanged;
  end;
end;

procedure TCustomAxis.GranularityBaseChanged;
begin
 if cafAutoGranularity in Flags then
  if CalculateAutoGranularity then Changed;
end;

procedure TCustomAxis.MinimumGranularityDistanceChanged;
begin
 if cafAutoGranularity in Flags then
  if CalculateAutoGranularity then Changed;
end;

procedure TCustomAxis.SetMinimum(Value: Double);
begin
 // check, whether lower exceed new minimum and limit if necessary
 if (cafAutoExtendBounds in Flags) and (Value > Lower)
  then Value := Lower;

 if FMinimum <> Value then
  begin
   FMinimum := Value;
   MinimumChanged;
  end;
end;

procedure TCustomAxis.SetPixelSize(Value: Integer);
begin
 if Value < 1 then Value := 1; 
 if FPixelSize <> Value then
  begin
   FPixelSize := Value;
   Resized;
  end;
end;

procedure TCustomAxis.CalculatePixelValueRelation;
begin
 FPixelPerValue := FRangeReci * FPixelSize;
 FValuePerPixel := 1 / FPixelPerValue;
end;

procedure TCustomAxis.Resized;
begin
 CalculatePixelValueRelation;
 if cafAutoGranularity in Flags
  then CalculateAutoGranularity;
 Changed;
end;

procedure TCustomAxis.Changed;
begin
 // something changed, send notify event
 if Assigned(FOnChanged)
  then FOnChanged(Self)
end;

procedure TCustomAxis.SetMaximum(Value: Double);
begin
 // check, whether lower exceed new minimum and limit if necessary
 if (cafAutoExtendBounds in Flags) and (Value < Upper)
  then Value := Upper;

 if FMaximum <> Value then
  begin
   FMaximum := Value;
   MaximumChanged;
  end;
end;

procedure TCustomAxis.SetLower(Value: Double);
begin
 // check, whether new lower exceed minimum and limit if necessary
 if not (cafAutoExtendBounds in Flags) and (Value < Minimum)
  then Value := Minimum;

 if FLower <> Value then
  begin
   FLower := Value;
   LowerChanged;
  end;
end;

procedure TCustomAxis.SetUpper(Value: Double);
begin
 // check, whether new upper exceed maximum and extend if necessary
 if not (cafAutoExtendBounds in Flags) and (Value > Maximum)
  then Value := Maximum;

 if FUpper <> Value then
  begin
   FUpper := Value;
   UpperChanged;
  end;
end;


{ TCustomGuiGraphXYSeries }

constructor TCustomGuiGraphXYSeries.Create;
begin
 inherited;
 FVisible := True;
 FColor   := clRed;
end;

procedure TCustomGuiGraphXYSeries.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiGraphXYSeries then
  with TCustomGuiGraphXYSeries(Dest) do
   begin
    FColor    := Self.FColor;
    FVisible  := Self.FVisible;
    FOnChange := Self.FOnChange;
   end else inherited;
end;

procedure TCustomGuiGraphXYSeries.Changed;
begin
 if Assigned(FOnChange) then FOnChange(Self)
end;

procedure TCustomGuiGraphXYSeries.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   Changed;
  end;
end;

procedure TCustomGuiGraphXYSeries.SetVisible(const Value: Boolean);
begin
 if FVisible <> Value then
  begin
   FVisible := Value;
   Changed;
  end;
end;

{ TCustomGuiFunctionSeries }

procedure TCustomGuiGraphXYFunctionSeries.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiGraphXYFunctionSeries then
  with TCustomGuiGraphXYSeries(Dest) do
   begin
    inherited;
    FOnEvaluate := Self.FOnEvaluate;
   end else inherited;
end;

procedure TCustomGuiGraphXYFunctionSeries.PaintToGraph(
  const GraphXY: TCustomGuiGraphXY; const Bitmap: TBitmap);
var
  x        : Integer;
  Offset   : TDAVPointSingle;
  Scale    : TDAVPointSingle;
  Value    : Single;
begin
 if Visible and Assigned(FOnEvaluate) then
  with GraphXY, Bitmap do
   begin
    Scale.X   := FXAxis.ValuePerPixel / OversamplingFactor;
    Offset.X  := FXAxis.Lower;
    Scale.Y   := OversamplingFactor * FYAxis.PixelPerValue;
    Offset.Y  := {Top} + OversamplingFactor * FYAxis.PixelSize + fYAxis.Lower * Scale.Y;

    Canvas.Pen.Color := fColor;
    Canvas.Pen.Width := LineWidth * OversamplingFactor;
    Value := FOnEvaluate(Self, Offset.X);
    Canvas.MoveTo(0 {+ Left}, Round(Offset.Y - Scale.Y * Value));
    for x := 1 to OversamplingFactor * FXAxis.PixelSize do
     begin
      Value := FOnEvaluate(Self, Offset.X + x * Scale.X);
      Canvas.LineTo(0 {+ Left} + x, Round(Offset.Y - Scale.Y * Value));
     end;
   end;
end;

procedure TCustomGuiGraphXYFunctionSeries.SetOnEvaluate(const Value: TFunctionEvaluateEvent);
begin
 if @FOnEvaluate <> @Value then
  begin
   FOnEvaluate := Value;
   Changed;
  end;
end;

{ TCustomGuiGraphXYDataSeries }

constructor TCustomGuiGraphXYDataSeries.Create;
begin
 inherited;
 FData := nil;
end;

destructor TCustomGuiGraphXYDataSeries.Destroy;
begin
 Dispose(FData);
 inherited;
end;

function TCustomGuiGraphXYDataSeries.AddPoint(X, Y: Single): Integer;
var
  Item: TDAVPointSingle;
begin
 Item.X := X;
 Item.Y := Y;
 Result := AddPoint(Item);
end;

procedure TCustomGuiGraphXYDataSeries.Clear;
begin
 FCount := 0;
 FillChar(FData^, FCount * SizeOf(Single), 0);
end;

class procedure TCustomGuiGraphXYDataSeries.Error(const Msg: string; Data: Integer);
{$IFDEF FPC}
begin
{$ELSE}
  function ReturnAddr: Pointer;
  asm
    MOV EAX, [EBP + 4]
  end;

begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
{$ENDIF}
end;

class procedure TCustomGuiGraphXYDataSeries.Error(Msg: PResStringRec;
  Data: Integer);
begin
  TCustomGuiGraphXYDataSeries.Error(LoadResString(Msg), Data);
end;

function TCustomGuiGraphXYDataSeries.First: TDAVPointSingle;
begin
 Result := FData^[0];
end;

function TCustomGuiGraphXYDataSeries.Last: TDAVPointSingle;
begin
 Result := FData^[FCount - 1];
end;

function TCustomGuiGraphXYDataSeries.Get(Index: Integer): TDAVPointSingle;
begin
 if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
 Result := FData^[Index];
end;

procedure TCustomGuiGraphXYDataSeries.Put(Index: Integer; Item: TDAVPointSingle);
var
  Temp: TDAVPointSingle;
begin
 if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
 if (Item.X <> FData^[Index].X) and (Item.Y <> FData^[Index].Y) then
  begin
   Temp := FData^[Index];
   FData^[Index] := Item;
  end;
end;


{ TGuiGraphXYDataSeries }

function TGuiGraphXYDataSeries.IndexOf(Item: TDAVPointSingle): Integer;
begin
 Result := 0;
 while (Result < FCount) and (Item.X <> FData^[Result].X) and (Item.Y <> FData^[Result].Y)
  do Inc(Result);
 if Result = FCount then Result := -1;
end;

function TGuiGraphXYDataSeries.Remove(Item: TDAVPointSingle): Integer;
begin
 Result := IndexOf(Item);
 if Result >= 0 then Delete(Result);
end;

procedure QuickSort(SortList: PDAVPointSingleFixedArray; L, R: Integer);
var
  I, J: Integer;
  P, T: TDAVPointSingle;
begin
 repeat
  I := L;
  J := R;
  P := SortList^[(L + R) shr 1];
  repeat
    while SortList^[I].X < P.X do Inc(I);
    while SortList^[J].X > P.X do Dec(J);
     if I <= J then
      begin
       T := SortList^[I];
       SortList^[I] := SortList^[J];
       SortList^[J] := T;
       Inc(I);
       Dec(J);
      end;
    until I > J;
   if L < J then QuickSort(SortList, L, J);
   L := I;
  until I >= R;
end;

procedure TGuiGraphXYDataSeries.Sort;
begin
 if (FData <> nil) and (Count > 0)
  then QuickSort(FData, 0, Count - 1);
end;

function TGuiGraphXYDataSeries.AddPoint(Item: TDAVPointSingle): Integer;
begin
 ReallocMem(FData, (FCount + 1) * SizeOf(TDAVPointSingle));
 FData^[FCount] := Item;
 Result := FCount;
 Inc(FCount);
end;

procedure TGuiGraphXYDataSeries.Delete(Index: Integer);
begin
 Move(FData^[Index + 1], FData^[Index], (FCount - Index - 1) * SizeOf(TDAVPointSingle));
 ReallocMem(FData, FCount);
end;

procedure TGuiGraphXYDataSeries.Exchange(Index1, Index2: Integer);
var
  Temp : TDAVPointSingle;
begin
 if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
 if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
 Temp := FData^[Index1];
 FData^[Index1] := FData^[Index2];
 FData^[Index2] := Temp;
end;

function TGuiGraphXYDataSeries.Extract(Item: TDAVPointSingle): TDAVPointSingle;
var
  I: Integer;
begin
 I := IndexOf(Item);
 if I >= 0 then
  begin
   Result := Item;
   Delete(I);
  end;
end;

{ TGuiGraphXYSortedDataSeries }

function TGuiGraphXYSortedDataSeries.AddPoint(
  Item: TDAVPointSingle): Integer;
var
  i : Integer;
begin
 Result := -1;
 if FCount = 0 then
  begin
   ReallocMem(FData, SizeOf(TDavPointSingle));
   FData^[0] := Item;
   FCount := 1;
   Result := 0;
  end
 else
  begin
   i := 0; while i < FCount do
    begin
     if FData^[i].X > Item.X then Break;
     Inc(i);
    end;
   ReallocMem(FData, (FCount + 1) * SizeOf(TDAVPointSingle));
   if i < FCount
    then System.Move(FData^[i], FData^[i + 1], (FCount - i) * SizeOf(TDAVPointSingle));
   FData^[i] := Item;
   Inc(FCount);
  end;
end;

{ TGuiGraphXYSeriesCollectionItem }

constructor TGuiGraphXYSeriesCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FSeries := TGuiGraphXYFunctionSeries.Create;
 FDisplayName := ClassName;
end;

destructor TGuiGraphXYSeriesCollectionItem.Destroy;
begin
 if Assigned(FSeries) then FreeAndNil(FSeries);
 inherited;
end;

function TGuiGraphXYSeriesCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TGuiGraphXYSeriesCollectionItem.GetSeriesClassName: string;
begin
 if Assigned(FSeries)
  then Result := FSeries.ClassName
  else Result := '';
end;

procedure TGuiGraphXYSeriesCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiGraphXYSeriesCollectionItem.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TGuiGraphXYSeriesCollectionItem then
  with TGuiGraphXYSeriesCollectionItem(Dest) do
   begin
    FSeries.Assign(Self.FSeries);
    FDisplayName := Self.FDisplayName;
    FSeriesClassChanged := Self.FSeriesClassChanged;
   end;
end;

procedure TGuiGraphXYSeriesCollectionItem.Changed;
begin
 if Assigned(FSeriesClassChanged)
  then FSeriesClassChanged(Self);
end;

procedure TGuiGraphXYSeriesCollectionItem.SetSeries(
  const Value: TCustomGuiGraphXYSeries);
begin
 FSeries.Assign(Value);
end;

procedure TGuiGraphXYSeriesCollectionItem.SetSeriesClassName(const Value: string);
var
  SeriesClass: TCustomGuiGraphXYSeriesClass;
begin
 if (Value <> '') and (FSeries.ClassName <> Value) and Assigned(SeriesClassList) then
  begin
   SeriesClass := TCustomGuiGraphXYSeriesClass(SeriesClassList.Find(Value));
   if Assigned(SeriesClass) then
    begin
     FSeries.Free;
     FSeries := SeriesClass.Create;
     Changed;
    end;
  end;
end;


{ TGuiGraphXYSeriesCollection }

constructor TGuiGraphXYSeriesCollection.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TGuiGraphXYSeriesCollectionItem);
end;

function TGuiGraphXYSeriesCollection.Add: TGuiGraphXYSeriesCollectionItem;
begin
 Result := TGuiGraphXYSeriesCollectionItem(inherited Add);
end;

procedure TGuiGraphXYSeriesCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiGraphXYSeriesCollection.GetItem(Index: Integer): TGuiGraphXYSeriesCollectionItem;
begin
 Result := TGuiGraphXYSeriesCollectionItem(inherited GetItem(Index));
end;

function TGuiGraphXYSeriesCollection.Insert(
  Index: Integer): TGuiGraphXYSeriesCollectionItem;
begin
 Result:= TGuiGraphXYSeriesCollectionItem(inherited Insert(Index));
end;

procedure TGuiGraphXYSeriesCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 if UpdateCount = 0 then
  begin
   Assert(Owner is TCustomGuiGraphXY);
   TCustomGuiGraphXY(Owner).UpdateBuffer;
  end;
end;

procedure TGuiGraphXYSeriesCollection.SetItem(Index: Integer;
  const Value: TGuiGraphXYSeriesCollectionItem);
begin
 inherited SetItem(Index, Value);
end;


{ TCustomGuiGraphXY }

constructor TCustomGuiGraphXY.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderColor      := clRed;
  FBorderRadius     := 0;
  FLineColor        := clBlack;
  FLineColor        := clMaroon;
  FFrameColor       := clRed;
  FLineWidth        := 2;
  FFlags            := [gfShowLabels];
  FSeriesCollection := TGuiGraphXYSeriesCollection.Create(Self);
  FXAxis            := TCustomAxis.Create;
  FYAxis            := TCustomAxis.Create;
  FXAxis.OnChanged  := SettingsChanged;
  FYAxis.OnChanged  := SettingsChanged;
end;

destructor TCustomGuiGraphXY.Destroy;
begin
 FreeAndNil(FXAxis);
 FreeAndNil(FYAxis);
 FreeAndNil(FSeriesCollection);
 inherited Destroy;
end;

function TCustomGuiGraphXY.GetSeriesCollectionItem(
  Index: Integer): TGuiGraphXYSeriesCollectionItem;
begin
 if (Index >= 0) and (Index < FSeriesCollection.Count)
  then Result := FSeriesCollection[Index]
  else Result := nil;
end;

procedure TCustomGuiGraphXY.Loaded;
begin
 inherited;
 Resize;
end;

procedure TCustomGuiGraphXY.SettingsChanged(Sender: TObject);
begin
 Invalidate;
end;

procedure TCustomGuiGraphXY.RenderGraphXYToBitmap(const Bitmap: TBitmap);
var
  Rct        : TRect;
  SeriesNr   : Integer;
  NormGran   : Double;
  DispValue  : Double;
  c          : Double;
  PixelRange : Integer;
  TextSize   : TSize;
  ZeroPos    : TPoint;
  str        : string;
const
  TextXMargin = 2;
begin
 with Bitmap, Canvas do
  begin
   Pen.Color := FBorderColor;
   Brush.Style := bsClear;
   Rct := ClipRect;
   RoundRect(Rct.Left, Rct.Top, Rct.Right, Rct.Bottom,
     OversamplingFactor * FBorderRadius, OversamplingFactor * FBorderRadius);
   InflateRect(Rct, -1, -1);
   ZeroPos   := Point(Round(XAxis.ZeroPosition * OversamplingFactor * XAxis.PixelSize),
                      Round((1 - YAxis.ZeroPosition) * OversamplingFactor * YAxis.PixelSize));

   Pen.Color   := FLineColor;
   Pen.Width   := OversamplingFactor;
   Brush.Color := Color;
   Font.Assign(Self.Font);
   Font.Height := Self.Font.Height * OversamplingFactor;

   with XAxis do
    begin
     PixelRange := OversamplingFactor * (PixelSize - 2);
     NormGran := FGranularity * FRangeReci;
     c := FZeroPosition + Round(0.5 - FZeroPosition / NormGran) * NormGran;
     while c < 0 do c := c + NormGran;
     while c < 1 do
      begin
       if Abs(c - FZeroPosition) < 0.1 * NormGran
        then Pen.Color := FFrameColor
        else Pen.Color := FLineColor;

       MoveTo(Rct.Left + Round(c * PixelRange), Rct.Top);
       LineTo(Rct.Left + Round(c * PixelRange), Rct.Bottom);

       if (gfShowLabels in Self.Flags) then
        begin
         DispValue := fLower + FRange * c;
         if abs(DispValue) < 0.01 * FGranularity then
          begin
           str := '0';
           TextSize := TextExtent(str);
           TextOut(Rct.Left + Round(c * PixelRange - TextSize.cx) - OversamplingFactor * TextXMargin,
                   ZeroPos.Y, str);
          end
         else
          begin
           str := FloatToStrF(fLower + FRange * c, ffGeneral, 2, 2);
           TextSize := TextExtent(str);

           TextOut(Rct.Left + Round(c * PixelRange - 0.5 * TextSize.cx),
                   ZeroPos.Y, str);
          end;
        end;

       c := c + NormGran;
      end;
    end;

   with YAxis do
    if fRange <> 0 then
     begin
      PixelRange := OversamplingFactor * (PixelSize - 2);
      NormGran := FGranularity * fRangeReci;
      c := FZeroPosition + Round( -FZeroPosition / NormGran + 0.5) * NormGran;
      while c < 0 do c := c + NormGran;
      while c < 1 do
       begin
       if Abs(c - FZeroPosition) < 0.1 * NormGran
        then Pen.Color := FFrameColor
        else Pen.Color := FLineColor;

        MoveTo(Rct.Left, Rct.Bottom - Round(c * PixelRange));
        LineTo(Rct.Right, Rct.Bottom - Round(c * PixelRange));

       if (gfShowLabels in Self.Flags) then
        begin
         DispValue := fLower + FRange * c;
         if abs(DispValue) > 0.01 * FGranularity then
          begin
           str := FloatToStrF(fLower + FRange * c, ffGeneral, 2, 2);
           TextSize := TextExtent(str);

           TextOut(ZeroPos.X - TextSize.cx - OversamplingFactor * TextXMargin,
                   Rct.Top + Round((1 - c) * PixelRange - 0.5 * TextSize.cy),
                   str);
          end;
        end;

        c := c + NormGran;
       end;
     end;

   for SeriesNr := 0 to FSeriesCollection.Count - 1 do
    if Assigned(FSeriesCollection[SeriesNr].FSeries)
     then FSeriesCollection[SeriesNr].FSeries.PaintToGraph(Self, Bitmap);
  end;
end;

procedure TCustomGuiGraphXY.Resize;
begin
 inherited;
 FXAxis.PixelSize := Width;
 FYAxis.PixelSize := Height;
 Invalidate;
end;

procedure TCustomGuiGraphXY.UpdateBuffer;
var
  Bmp : TBitmap;
begin
 if (Width > 0) and (Height > 0) then with FBuffer.Canvas do
  begin
   Lock;
   if AntiAlias = gaaNone then
    begin
     // draw background
     {$IFNDEF FPC}
     if FTransparent
      then CopyParentImage(Self, FBuffer.Canvas)
      else
     {$ENDIF}
      begin
       Brush.Color := Self.Color;
       FillRect(ClipRect);
      end;
     RenderGraphXYToBitmap(FBuffer);
    end
   else
    begin
     Bmp := TBitmap.Create;
     with Bmp do
      try
       PixelFormat := pf32bit;
       Width       := OversamplingFactor * FBuffer.Width;
       Height      := OversamplingFactor * FBuffer.Height;
       {$IFNDEF FPC}
       if FTransparent then
        begin
         CopyParentImage(Self, Bmp.Canvas);
         UpsampleBitmap(Bmp);
        end
       else
       {$ENDIF}
        with Bmp.Canvas do
         begin
          Brush.Color := Self.Color;
          FillRect(ClipRect);
         end;
       RenderGraphXYToBitmap(Bmp);
       DownsampleBitmap(Bmp);
       FBuffer.Canvas.Draw(0, 0, Bmp);
      finally
       Free;
      end;
    end;
   Unlock;
  end;
end;

procedure TCustomGuiGraphXY.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TCustomGuiGraphXY.FrameColorChanged;
begin
 UpdateGraph;
end;

procedure TCustomGuiGraphXY.BorderRadiusChanged;
begin
 UpdateGraph;
end;

procedure TCustomGuiGraphXY.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiGraphXY then
  with TCustomGuiGraphXY(Dest) do
   begin
    FOnChange    := Self.FOnChange;
    FBorderColor := Self.FBorderColor;
    FFrameColor  := Self.FFrameColor;
    FFlags       := Self.FFlags;

    FXAxis.Assign(Self.FXAxis);
    FYAxis.Assign(Self.FYAxis);
    FSeriesCollection.Assign(Self.FSeriesCollection);
   end;
end;

procedure TCustomGuiGraphXY.BorderColorChanged;
begin
 UpdateGraph;
end;

procedure TCustomGuiGraphXY.ShowLabelsChanged;
begin
 Changed;
end;

procedure TCustomGuiGraphXY.UpdateGraph;
begin
 Invalidate;
end;

procedure TCustomGuiGraphXY.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiGraphXY.SetBorderRadius(const Value: Integer);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiGraphXY.SetFlags(const Value: TGraphXYFlags);
var
  OldFlags : TGraphXYFlags;
begin
 if FFlags <> Value then
  begin
   OldFlags := FFlags;
   FFlags := Value;
   if (gfShowLabels in FFlags) xor
      (gfShowLabels in OldFlags)
    then ShowLabelsChanged;
  end;
end;

procedure TCustomGuiGraphXY.SetFrameColor(const Value: TColor);
begin
 if FFrameColor <> Value then
  begin
   FFrameColor := Value;
   FrameColorChanged;
  end;
end;

procedure TCustomGuiGraphXY.SetSeriesCollection(
  const Value: TGuiGraphXYSeriesCollection);
begin
 FSeriesCollection.Assign(Value);
end;

procedure TCustomGuiGraphXY.SetSeriesCollectionItem(Index: Integer;
  const Value: TGuiGraphXYSeriesCollectionItem);
begin
 if (Index >= 0) and (Index < FSeriesCollection.Count)
  then FSeriesCollection[Index] := Value else
 if (Index = FSeriesCollection.Count) then
  begin
   FSeriesCollection.Add;
   FSeriesCollection[Index] := Value;
  end
 else raise Exception.Create('Index out of bounds (' + IntToStr(Index) + ')');
end;

procedure RegisterSeriesClass(SeriesClass: TCustomGuiGraphXYSeriesClass);
begin
 if not Assigned(SeriesClassList) then SeriesClassList := TClassList.Create;
 SeriesClassList.Add(SeriesClass);
end;

initialization
  // register series classes
  RegisterSeriesClass(TGuiGraphXYFunctionSeries);
  RegisterSeriesClass(TGuiGraphXYDataSeries);

finalization
  FreeAndNil(SeriesClassList);

end.
