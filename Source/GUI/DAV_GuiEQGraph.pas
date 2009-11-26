unit DAV_GuiEQGraph;

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

{$I ..\DAV_Compiler.Inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, Types, SysUtils, Controls, DAV_GuiCommon,
  DAV_GuiBaseControl;

type
  TGuiEQGraph = class;
  TXAxisLabelStyle = (xlsNone, xlsTop, xlsBottom);
  TYAxisLabelStyle = (ylsNone, ylsLeft, ylsRight);

  TGetFilterGainEvent = function(Sender: TObject; const Frequency: Single): Single of object;

  TCustomGuiEQGraph = class;

  TCustomGuiEQGraphAxis = class(TPersistent)
  private
  protected
    FOwner : TCustomGuiEQGraph;
    FUpper : Single;
    FLower : Single;
    FRange : Single;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; virtual;
    procedure CalculateRange;
    procedure RangeChanged; virtual;
  public
    constructor Create(AOwner: TCustomGuiEQGraph); virtual;
    property Range: Single read FRange;
  end;

  // X-Axis

  TCustomGuiEQGraphXAxis = class(TCustomGuiEQGraphAxis)
  private
    FLabelStyle   : TXAxisLabelStyle;
    FInvUpper     : Single;
    FInvLower     : Single;
    FLog2Ratio    : Single;
    FInvLog2Ratio : Single;
    procedure SetLabelStyle(const Value: TXAxisLabelStyle);
    procedure SetLowerFrequency(const Value: Single);
    procedure SetUpperFrequency(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure LabelStyleChanged; virtual;
    procedure LowerFrequencyChanged; virtual;
    procedure UpperFrequencyChanged; virtual;

    procedure CalculateLowerFrequencyReciprocal;
    procedure CalculateUpperFrequencyReciprocal;
    procedure CalculateFrequencyRangeRatios;
  public
    constructor Create(AOwner: TCustomGuiEQGraph); override;

    // conversion between logarithmic frequency and linear
    function LinearToLogarithmicFrequency(Value: Double): Double;
    function LogarithmicFrequencyToLinear(Value: Double): Double;

    // conversion between linear and logarithmic frequency
    function FastLinearToLogarithmicFrequency(Value: Single): Single;
    function FastLogarithmicFrequencyToLinear(Value: Single): Single;

    property UpperFrequency: Single read FUpper write SetUpperFrequency;
    property LowerFrequency: Single read FLower write SetLowerFrequency;
    property LabelStyle: TXAxisLabelStyle read FLabelStyle write SetLabelStyle default xlsNone;
  end;

  TGuiEQGraphXAxis = class(TCustomGuiEQGraphXAxis)
  published
    property LabelStyle;
    property UpperFrequency;
    property LowerFrequency;
  end;

  // Y-Axis

  TCustomGuiEQGraphYAxis = class(TCustomGuiEQGraphAxis)
  private
    FLabelStyle       : TYAxisLabelStyle;
    FMaximumGridLines : Integer;
    procedure SetUpperLevel(const Value: Single);
    procedure SetLowerLevel(const Value: Single);
    procedure SetLabelStyle(const Value: TYAxisLabelStyle);
    procedure SetMaximumGridLines(const Value: Integer);
    function GetLowerGridLine: Single;
    function GetUpperGridLine: Single;
  protected
    FGranularity : Single;
    procedure AssignTo(Dest: TPersistent); override;
    procedure LabelStyleChanged; virtual;
    procedure UpperLevelChanged; virtual;
    procedure LowerLevelChanged; virtual;
    procedure MaximumGridLinesChanged; virtual;
    procedure CalculateGranularity;
    procedure RangeChanged; override;
  public
    constructor Create(AOwner: TCustomGuiEQGraph); override;

    property UpperLevel: Single read FUpper write SetUpperLevel;
    property LowerLevel: Single read FLower write SetLowerLevel;

    property UpperGridline: Single read GetUpperGridLine;
    property LowerGridline: Single read GetLowerGridLine;
    property Granularity: Single read FGranularity;

    property LabelStyle: TYAxisLabelStyle read FLabelStyle write SetLabelStyle default ylsNone;
    property MaximumGridLines: Integer read FMaximumGridLines write SetMaximumGridLines default 10;
  end;

  TGuiEQGraphYAxis = class(TCustomGuiEQGraphYAxis)
  published
    property LabelStyle;
    property LowerLevel;
    property UpperLevel;
  end;


  // EQ Series

  TGuiEQGraphSeriesCollectionItem = class(TCollectionItem)
  private
    FDisplayName     : string;
    FOnGetFilterGain : TGetFilterGainEvent;
    FColor           : TColor;
    FLineWidth       : Integer;
    procedure SetColor(const Value: TColor);
    procedure SetLineWidth(const Value: Integer);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ColorChanged; virtual;
    procedure LineWidthChanged; virtual;
    procedure Changed; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property DisplayName;
    property Color: TColor read FColor write SetColor default clRed;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 2;
    property OnGetFilterGain: TGetFilterGainEvent read FOnGetFilterGain write FOnGetFilterGain;
  end;

  TGuiEQGraphSeriesCollection = class(TOwnedCollection)
  protected
    procedure Changed; virtual;
    function GetItem(Index: Integer): TGuiEQGraphSeriesCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiEQGraphSeriesCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiEQGraphSeriesCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGuiEQGraphSeriesCollectionItem;
    function Insert(Index: Integer): TGuiEQGraphSeriesCollectionItem;
    procedure Delete(Index: Integer);
    property Count;
  end;


  // EQ-Graph

  TCustomGuiEQGraph = class(TCustomControl)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGuiEQGraph = class(TCustomGuiEQGraph)
  private
    FAutoColor        : Boolean;       
    FBuffer           : TBitmap;       
    FChartColor       : TColor;       
    FBorderRadius     : Integer;       
    FBorderWidth      : Integer;       
    FGraphColorDark   : TColor;       
    FGraphColorLight  : TColor;       
    FAntiAlias        : TGuiAntiAlias;       
    FOSFactor         : Integer;       
    FTransparent      : Boolean;       

    FYAxis            : TGuiEQGraphYAxis;       
    FXAxis            : TGuiEQGraphXAxis;       

    FOnPaint          : TNotifyEvent;       
    FFilterSeries     : TGuiEQGraphSeriesCollection;

    procedure SetAutoColor(const Value: Boolean);
    procedure SetChartColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetGraphColorDark(const Value: TColor);
    procedure SetGraphColorLight(const Value: TColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetYAxis(const Value: TGuiEQGraphYAxis);
    procedure SetXAxis(const Value: TGuiEQGraphXAxis);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetTransparent(const Value: Boolean);
    procedure RenderGridToBitmap(Bitmap: TBitmap);
    procedure SetFilterSeries(const Value: TGuiEQGraphSeriesCollection);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;

    procedure AntiAliasChanged; virtual;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure ChartColorChanged; virtual;
    procedure GraphColorDarkChanged; virtual;
    procedure GraphColorLightChanged; virtual;
    procedure RenderBuffer; virtual;
    procedure TransparentChanged; virtual;
    procedure DownsampleBitmap(Bitmap: TBitmap);
    procedure UpsampleBitmap(Bitmap: TBitmap);

    procedure RenderToBitmap(Bitmap: TBitmap); virtual;
    {$IFDEF FPC}
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}

    {$IFNDEF FPC}
    procedure DrawParentImage(Dest: TCanvas); virtual;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property AutoColor: Boolean read FAutoColor write SetAutoColor default False;
    property GraphColorDark: TColor read FGraphColorDark write SetGraphColorDark default $303030;
    property GraphColorLight: TColor read FGraphColorLight write SetGraphColorLight default $606060;
    property ColorChart: TColor read FChartColor write SetChartColor;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius default 0;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property Transparent: Boolean read FTransparent write SetTransparent default False;

    property FilterSeries: TGuiEQGraphSeriesCollection read FFilterSeries write SetFilterSeries;
    property YAxis: TGuiEQGraphYAxis read FYAxis write SetYAxis;
    property XAxis: TGuiEQGraphXAxis read FXAxis write SetXAxis;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property BiDiMode;
    property OnCanResize;
    {$ENDIF}
  end;

implementation

uses
  Math, DAV_Common, DAV_Approximations;

{ TCustomGuiEQGraphAxis }

constructor TCustomGuiEQGraphAxis.Create(AOwner: TCustomGuiEQGraph);
begin
 FOwner := AOwner;
end;

procedure TCustomGuiEQGraphAxis.Changed;
begin
 FOwner.Invalidate;
end;

procedure TCustomGuiEQGraphAxis.RangeChanged;
begin
 CalculateRange;
end;

procedure TCustomGuiEQGraphAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQGraphAxis then
  with TCustomGuiEQGraphAxis(Dest) do
   begin
    FOwner := Self.FOwner;
    FUpper := Self.FUpper;
    FLower := Self.FLower;
    FRange := Self.FRange;
   end
 else inherited;
end;

procedure TCustomGuiEQGraphAxis.CalculateRange;
begin
 FRange := FUpper - FLower;
end;


{ TCustomGuiEQGraphXAxis }

constructor TCustomGuiEQGraphXAxis.Create(AOwner: TCustomGuiEQGraph);
begin
 inherited;
 FLabelStyle  := xlsNone;
 FLower := 20;
 FUpper := 20000;
 CalculateUpperFrequencyReciprocal;
 CalculateLowerFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
end;

procedure TCustomGuiEQGraphXAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQGraphXAxis then
  with TCustomGuiEQGraphXAxis(Dest) do
   begin
    inherited;
    FLabelStyle   := Self.FLabelStyle;
    FInvUpper     := Self.FInvUpper;
    FInvLower     := Self.FInvLower;
    FLog2Ratio    := Self.FLog2Ratio;
    FInvLog2Ratio := Self.FInvLog2Ratio;
   end
 else inherited;
end;

function TCustomGuiEQGraphXAxis.LogarithmicFrequencyToLinear(Value: Double): Double;
begin
 Result := Log2(Value * FInvLower) * FInvLog2Ratio;
end;

function TCustomGuiEQGraphXAxis.LinearToLogarithmicFrequency(Value: Double): Double;
begin
 Result := Power(2, Value * FLog2Ratio) * FLower;
end;

function TCustomGuiEQGraphXAxis.FastLogarithmicFrequencyToLinear(Value: Single): Single;
begin
 Result := FastLog2MinError3(Value * FInvLower) * FInvLog2Ratio;
end;

function TCustomGuiEQGraphXAxis.FastLinearToLogarithmicFrequency(Value: Single): Single;
begin
 Result := FastPower2MinError3(Value * FLog2Ratio) * FLower;
end;

procedure TCustomGuiEQGraphXAxis.SetLabelStyle(const Value: TXAxisLabelStyle);
begin
 if FLabelStyle <> Value then
  begin
   FLabelStyle := Value;
   LabelStyleChanged;
  end;
end;

procedure TCustomGuiEQGraphXAxis.SetLowerFrequency(const Value: Single);
begin
 if FLower <> Value then
  begin
   FLower := Value;
   LowerFrequencyChanged;
  end;
end;

procedure TCustomGuiEQGraphXAxis.SetUpperFrequency(const Value: Single);
begin
 if FUpper <> Value then
  begin
   FUpper := Value;
   UpperFrequencyChanged;
  end;
end;

procedure TCustomGuiEQGraphXAxis.UpperFrequencyChanged;
begin
 RangeChanged;
 CalculateUpperFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
 Changed;
end;

procedure TCustomGuiEQGraphXAxis.LowerFrequencyChanged;
begin
 RangeChanged;
 CalculateLowerFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
 Changed;
end;

procedure TCustomGuiEQGraphXAxis.CalculateUpperFrequencyReciprocal;
begin
 Assert(FUpper <> 0);

 // calculate reciprocal of upper frequency
 FInvUpper := 1 / FUpper;
end;

procedure TCustomGuiEQGraphXAxis.CalculateLowerFrequencyReciprocal;
begin
 Assert(FLower <> 0);

 // calculate reciprocal of lower frequency
 FInvLower := 1 / FLower;
end;

procedure TCustomGuiEQGraphXAxis.CalculateFrequencyRangeRatios;
begin
 Assert(FUpper <> 0);
 Assert(FInvLower <> 0);

 // calculate lograithmic frequency ratio (as new logarithm base)
 FLog2Ratio := Log2(FUpper * FInvLower);
 FInvLog2Ratio := 1 / FLog2Ratio;
end;

procedure TCustomGuiEQGraphXAxis.LabelStyleChanged;
begin
 Changed;
end;


{ TCustomGuiEQGraphYAxis }

constructor TCustomGuiEQGraphYAxis.Create(AOwner: TCustomGuiEQGraph);
begin
 inherited Create(AOwner);
 FUpper :=  15;
 FLower := -15;
 FLabelStyle := ylsNone;
 FMaximumGridLines := 10;

 CalculateRange;
 CalculateGranularity;
end;

procedure TCustomGuiEQGraphYAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQGraphYAxis then
  with TCustomGuiEQGraphYAxis(Dest) do
   begin
    FLabelStyle := Self.FLabelStyle;
    FGranularity := Self.FGranularity;
    FMaximumGridLines := Self.FMaximumGridLines;
   end
 else inherited;
end;

procedure TCustomGuiEQGraphYAxis.SetLowerLevel(const Value: Single);
begin
 if FLower <> Value then
  begin
   FLower := Value;
   LowerLevelChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.SetMaximumGridLines(const Value: Integer);
begin
 if Value < 1
  then raise Exception.Create('Value must be larger than 0!');
 
 if FMaximumGridLines <> Value then
  begin
   FMaximumGridLines := Value;
   MaximumGridLinesChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.SetUpperLevel(const Value: Single);
begin
 if FUpper <> Value then
  begin
   FUpper := Value;
   UpperLevelChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.SetLabelStyle(const Value: TYAxisLabelStyle);
begin
 if FLabelStyle <> Value then
  begin
   FLabelStyle := Value;
   LabelStyleChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.LabelStyleChanged;
begin
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.UpperLevelChanged;
begin
 RangeChanged;
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.LowerLevelChanged;
begin
 RangeChanged;
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.MaximumGridLinesChanged;
begin
 CalculateGranularity;
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.RangeChanged;
begin
 inherited;
 CalculateGranularity;
end;

function TCustomGuiEQGraphYAxis.GetLowerGridLine: Single;
begin
 Result := FGranularity * Trunc(FLower / FGranularity);
end;

function TCustomGuiEQGraphYAxis.GetUpperGridLine: Single;
begin
 Result := FGranularity * Trunc(FUpper / FGranularity);
end;

procedure TCustomGuiEQGraphYAxis.CalculateGranularity;
var
  RoughGranularity : Single;
  GranularityBase  : Integer;
  GranularityScale : Single;
begin
 RoughGranularity := Range / FMaximumGridLines;
 GranularityBase  := Trunc(Log10(abs(RoughGranularity)));
 GranularityScale := IntPower(10, GranularityBase);

 FGranularity := GranularityScale * (Trunc(RoughGranularity / GranularityScale) + 1);

 Assert(FGranularity >= RoughGranularity);
 Assert(FGranularity < Range);
end;


{ TGuiEQGraphSeriesCollectionItem }

constructor TGuiEQGraphSeriesCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FDisplayName := ClassName;
 FColor := clRed;
 FLineWidth := 2;
end;

destructor TGuiEQGraphSeriesCollectionItem.Destroy;
begin
 inherited;
end;

function TGuiEQGraphSeriesCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

procedure TGuiEQGraphSeriesCollectionItem.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   ColorChanged;
  end;
end;

procedure TGuiEQGraphSeriesCollectionItem.ColorChanged;
begin
 Changed;
end;

procedure TGuiEQGraphSeriesCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiEQGraphSeriesCollectionItem.SetLineWidth(const Value: Integer);
begin
 if FLineWidth <> Value then
  begin
   FLineWidth := Value;
   LineWidthChanged;
  end;
end;

procedure TGuiEQGraphSeriesCollectionItem.LineWidthChanged;
begin
 Changed;
end;

procedure TGuiEQGraphSeriesCollectionItem.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiEQGraphSeriesCollectionItem then
  with TGuiEQGraphSeriesCollectionItem(Dest) do
   begin
    FColor           := Self.FColor;
    FLineWidth       := Self.LineWidth;
    FDisplayName     := Self.FDisplayName;
    FOnGetFilterGain := Self.FOnGetFilterGain;
   end
 else inherited;
end;

procedure TGuiEQGraphSeriesCollectionItem.Changed;
begin
 if Collection is TGuiEQGraphSeriesCollection
  then TGuiEQGraphSeriesCollection(Collection).Changed;
end;


{ TGuiEQGraphSeriesCollection }

constructor TGuiEQGraphSeriesCollection.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TGuiEQGraphSeriesCollectionItem);
end;

function TGuiEQGraphSeriesCollection.Add: TGuiEQGraphSeriesCollectionItem;
begin
 Result := TGuiEQGraphSeriesCollectionItem(inherited Add);
end;

procedure TGuiEQGraphSeriesCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiEQGraphSeriesCollection.GetItem(Index: Integer): TGuiEQGraphSeriesCollectionItem;
begin
 Result := TGuiEQGraphSeriesCollectionItem(inherited GetItem(Index));
end;

function TGuiEQGraphSeriesCollection.Insert(
  Index: Integer): TGuiEQGraphSeriesCollectionItem;
begin
 Result:= TGuiEQGraphSeriesCollectionItem(inherited Insert(Index));
end;

procedure TGuiEQGraphSeriesCollection.Changed;
begin
 if Owner is TCustomGuiEQGraph
  then TCustomGuiEQGraph(Owner).Invalidate;
end;

procedure TGuiEQGraphSeriesCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 Changed;
end;

procedure TGuiEQGraphSeriesCollection.SetItem(Index: Integer;
  const Value: TGuiEQGraphSeriesCollectionItem);
begin
 inherited SetItem(Index, Value);
end;


{ TCustomGuiEQGraph }

constructor TCustomGuiEQGraph.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
   csDoubleClicks, csReplicatable, csOpaque];
 TabStop      := False; // Ensure we're not a tab-stop
 Color        := clBtnFace;
end;


{ TGuiEQGraph }

constructor TGuiEQGraph.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 FAutoColor       := False;
 FXAxis           := TGuiEQGraphXAxis.Create(Self);
 FYAxis           := TGuiEQGraphYAxis.Create(Self);
 FFilterSeries    := TGuiEQGraphSeriesCollection.Create(Self);
 FBuffer          := TBitmap.Create;
 FGraphColorLight := $606060;
 FGraphColorDark  := $303030;
 FBorderWidth     := 1;
 FOSFactor        := 1;
 FChartColor      := Color;
end;

destructor TGuiEQGraph.Destroy;
begin
 FreeAndNil(FBuffer);
 FreeAndNil(FXAxis);
 FreeAndNil(FYAxis);
 FreeAndNil(FFilterSeries);
 inherited Destroy;
end;

procedure TGuiEQGraph.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TGuiEQGraph then
  with TGuiEQGraph(Dest) do
   begin
    FAutoColor               := Self.FAutoColor;
    FChartColor              := Self.FChartColor;
    FBorderRadius            := Self.FBorderRadius;
    FBorderWidth             := Self.FBorderWidth;
    FGraphColorDark          := Self.FGraphColorDark;
    FGraphColorLight         := Self.FGraphColorLight;
    FOnPaint                 := Self.FOnPaint;

    FYAxis.Assign(Self.FYAxis);
    FXAxis.Assign(Self.FXAxis);
    FBuffer.Assign(Self.FBuffer);
   end;
end;

{$IFNDEF FPC}
procedure TGuiEQGraph.CMFontChanged(var Message: TMessage);
{$ELSE}
procedure TGuiEQGraph.CMFontChanged(var Message: TLMessage);
{$ENDIF}
begin
 inherited;
 FBuffer.Canvas.Font.Assign(Font);
end;

// Drawing stuff

procedure TGuiEQGraph.UpsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Upsample2xBitmap32(Bitmap);
   gaaLinear3x: Upsample3xBitmap32(Bitmap);
   gaaLinear4x: Upsample4xBitmap32(Bitmap);
   gaaLinear8x: begin
                 Upsample4xBitmap32(Bitmap);
                 Upsample2xBitmap32(Bitmap);
                end;
  gaaLinear16x: begin
                 Upsample4xBitmap32(Bitmap);
                 Upsample4xBitmap32(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

procedure TGuiEQGraph.DownsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Downsample2xBitmap32(Bitmap);
   gaaLinear3x: Downsample3xBitmap32(Bitmap);
   gaaLinear4x: Downsample4xBitmap32(Bitmap);
   gaaLinear8x: begin
                 Downsample4xBitmap32(Bitmap);
                 Downsample2xBitmap32(Bitmap);
                end;
  gaaLinear16x: begin
                 Downsample4xBitmap32(Bitmap);
                 Downsample4xBitmap32(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

{$IFNDEF FPC}
procedure TGuiEQGraph.DrawParentImage(Dest: TCanvas);
var
  SaveIndex : Integer;
  DC        : THandle;
  Position  : TPoint;
begin
  if Parent = nil then Exit;
  DC := Dest.Handle;
  SaveIndex := SaveDC(DC);
  GetViewportOrgEx(DC, Position);
  SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
  IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
  Parent.Perform(WM_ERASEBKGND, Longint(DC), 0);
  Parent.Perform(WM_PAINT, Longint(DC), 0);
  RestoreDC(DC, SaveIndex);
end;
{$ENDIF}

procedure TGuiEQGraph.Paint;
begin
 RenderBuffer;
 Canvas.Draw(0, 0, FBuffer);

 inherited;

 if Assigned(FOnPaint)
  then FOnPaint(Self);
end;

procedure TGuiEQGraph.SetChartColor(const Value: TColor);
begin
 if not FAutoColor and (FChartColor <> Value) then
  begin
   FChartColor := Value;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetFilterSeries(const Value: TGuiEQGraphSeriesCollection);
begin
 FFilterSeries.Assign(Value);
end;

procedure TGuiEQGraph.ChartColorChanged;
begin
 Invalidate;
end;

procedure TGuiEQGraph.SetBorderRadius(const Value: Integer);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TGuiEQGraph.BorderRadiusChanged;
begin
 Invalidate;
end;

procedure TGuiEQGraph.SetBorderWidth(const Value: Integer);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TGuiEQGraph.BorderWidthChanged;
begin
 Invalidate;
end;

procedure TGuiEQGraph.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   AntiAliasChanged;
  end;
end;

procedure TGuiEQGraph.AntiAliasChanged;
begin
 case FAntiAlias of
       gaaNone : FOSFactor :=  1;
   gaaLinear2x : FOSFactor :=  2;
   gaaLinear3x : FOSFactor :=  3;
   gaaLinear4x : FOSFactor :=  4;
   gaaLinear8x : FOSFactor :=  8;
  gaaLinear16x : FOSFactor := 16;
 end;
 Invalidate;
end;

procedure TGuiEQGraph.SetAutoColor(const Value: Boolean);
begin
 if FAutoColor <> Value then
  begin
   FAutoColor := Value;
   if FAutoColor then
    begin
(*
     FChartColor32 := Lighten(Color32(Color),60);
     FChartColor := WinColor(FChartColor32);
     RepaintChartBuffer;
*)
    end;
  end;
end;

procedure TGuiEQGraph.RenderBuffer;
var
  Bmp: TBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with FBuffer.Canvas do
   begin
    Lock;
    Brush.Assign(Canvas.Brush);

    case FAntiAlias of
     gaaNone:
      begin
       // draw background
       {$IFNDEF FPC}
       if FTransparent
        then DrawParentImage(FBuffer.Canvas)
        else
       {$ENDIF}
        begin
         Brush.Color := Self.Color;
         FillRect(ClipRect);
        end;
       RenderGridToBitmap(FBuffer);
       RenderToBitmap(FBuffer);
      end;
     else
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := FOSFactor * FBuffer.Width;
         Height      := FOSFactor * FBuffer.Height;
         Canvas.Font.Assign(Font);
         Canvas.Font.Size := FOSFactor * Font.Size;
         {$IFNDEF FPC}
         if FTransparent then
          begin
           CopyParentImage(Self, Bmp.Canvas);
//           DrawParentImage(Bmp.Canvas);
           UpsampleBitmap(Bmp);
          end
         else
         {$ENDIF}
          with Bmp.Canvas do
           begin
            Brush.Color := Self.Color;
            FillRect(ClipRect);
           end;
         RenderGridToBitmap(Bmp);
         RenderToBitmap(Bmp);
         DownsampleBitmap(Bmp);
         FBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         Free;
        end;
      end;
    end;
    Unlock;
   end;
end;

procedure TGuiEQGraph.RenderGridToBitmap(Bitmap: TBitmap);
var
  i, j, w, h  : Integer;
  Rct         : TRect;
  Txt         : string;
  Temp        : Single;
  Wdth        : Integer;
begin
 with Bitmap, Canvas do
  begin
   Lock;

   Rct := Rect(0, 0, Width, Height);

   Brush.Color := Color;
   Brush.Style := bsSolid;
   FillRect(ClientRect);
   Brush.Color := FChartColor;

   Pen.Width := FOSFactor;
   InflateRect(Rct, -FOSFactor, -FOSFactor);

   // add top margin to half height as offset
   Wdth := Round(Rct.Right - Rct.Left);

   (*
   // draw middle line
   Pen.Color := FGraphColorDark;
   HHOffs := Round(Rct.Top + HalfHeight);
   MoveTo(Rct.Left, HHOffs);
   LineTo(Rct.Right, HHOffs);
   *)

   Pen.Color := FGraphColorLight;
(*
   if FYAxis.UpperLevel <=   5 then w := 1 else
   if FYAxis.UpperLevel <=  10 then w := 5 else
   if FYAxis.UpperLevel >= 180 then w := 45 else w := 10;
*)

   // draw y-axis grid lines
   with FYAxis do
    begin
     Temp := GetLowerGridLine;

     while Temp < UpperLevel do
      begin
       i := Round(((Temp - LowerLevel) / Range) *  (Rct.Bottom - Rct.Top));
       MoveTo(Rct.Left,  Round(Rct.Bottom - i));
       LineTo(Rct.Right, Round(Rct.Bottom - i));
       Temp := Temp + Granularity;
      end;
    end;


   // draw x-axis grid lines
   i := Round(IntPower(10, Trunc(Log10(abs(FXAxis.LowerFrequency)))));
   j := Round(FXAxis.LowerFrequency / i);
   if j = FXAxis.LowerFrequency / i then
    begin
     Inc(j);
     if j >= 10 then
      begin
       i := i * 10;
       j := 1;
      end;
    end;

   while j * i < FXAxis.UpperFrequency do
    begin
     w := Round(Rct.Left + FXAxis.LogarithmicFrequencyToLinear(j * i) * Wdth);
     MoveTo(w, Rct.Top);
     LineTo(w, Rct.Bottom);
     Inc(j);
     if j >= 10 then
      begin
       i := i * 10;
       j := 1;
      end;
    end;

   // draw text
   i := Round(IntPower(10, Trunc(Log10(abs(FXAxis.LowerFrequency)))));
   j := Round(FXAxis.LowerFrequency / i);
   if j = FXAxis.LowerFrequency / i then
    begin
     Inc(j);
     if j >= 10 then
      begin
       i := i * 10;
       j := 1;
      end;
    end;

   case FXAxis.LabelStyle of
    xlsBottom :
     begin
      while j * i < FXAxis.UpperFrequency do
       begin
        if j in [1, 2, 5] then
         begin
          h := Rct.Bottom + Font.Height - 2 * FOSFactor - FBorderWidth div 2;
          Txt := FloatToStrF(j * i, ffGeneral, 5, 5) + ' Hz';
          TextOut(Round(Rct.Left + FXAxis.LogarithmicFrequencyToLinear(j * i) * Wdth) - TextWidth(Txt) div 2, h, Txt);
         end;
        Inc(j);
        if j >= 10 then
         begin
          i := i * 10;
          j := 1;
         end;
       end;
     end;
    xlsTop:
     begin
      while j * i < FXAxis.UpperFrequency do
       begin
        if j in [1, 2, 5] then
         begin
          h := Rct.Bottom + Font.Height - 2 * FOSFactor - FBorderWidth div 2;
          Txt := FloatToStrF(j * i, ffGeneral, 5, 5) + ' Hz';
          TextOut(Round(Rct.Left + FXAxis.LogarithmicFrequencyToLinear(j * i) * Wdth) - TextWidth(Txt) div 2, h, Txt);
         end;
        Inc(j);
        if j >= 10 then
         begin
          i := i * 10;
          j := 1;
         end;
       end;
     end;
   end;

   case FYAxis.LabelStyle of
    ylsLeft:
     with FYAxis do
      begin
       Temp := GetLowerGridLine;

       while Temp < UpperLevel do
        begin
         i := Round(((Temp - LowerLevel) / Range) *  (Rct.Bottom - Rct.Top));
         Txt := IntToStr(i) + 'dB';
         TextOut(Rct.Left,  Round(Rct.Bottom - i) + Font.Height div 2 + 2, Txt);
         Temp := Temp + Granularity;
        end;
      end;
    ylsRight:
     with FYAxis do
      begin
       Temp := GetLowerGridLine;

       while Temp < UpperLevel do
        begin
         i := Round(((Temp - LowerLevel) / Range) *  (Rct.Bottom - Rct.Top));
         Txt := IntToStr(i) + 'dB';
         TextOut(Round(Rct.Right - TextWidth(Txt)), Round(Rct.Bottom - i) + Font.Height div 2 + 2, Txt);
         Temp := Temp + Granularity;
        end;
      end;
   end;

   Unlock;
  end;
end;

procedure TGuiEQGraph.RenderToBitmap(Bitmap: TBitmap);
var
  FilterIndex : Integer;
  PixelIndex  : Integer;
  Temp        : Single;
  YValue      : Single;
begin
 with Bitmap, Canvas do
  begin
   Lock;

   for FilterIndex := 0 to FFilterSeries.Count - 1 do
    with FFilterSeries[FilterIndex] do
     begin
      Pen.Color := Color;
      Pen.Width := LineWidth * FOSFactor;
      if Assigned(FOnGetFilterGain) then
       begin
        YValue := FOnGetFilterGain(Self, FXAxis.LowerFrequency);
        MoveTo(FOSFactor, Round((Height - FOSFactor) * (1 - (YValue - FYAxis.LowerLevel) / FYAxis.Range) - FOSFactor));
        Temp := 1 / (Width - 2 * FOSFactor);
        for PixelIndex := FOSFactor + 1 to Width - FOSFactor - 1 do
         begin
          YValue := FOnGetFilterGain(Self, FXAxis.LinearToLogarithmicFrequency((PixelIndex - FOSFactor) * Temp));
          LineTo(PixelIndex, Round((Height - FOSFactor) * (1 - (YValue - FYAxis.LowerLevel) / FYAxis.Range) - FOSFactor));
         end;
       end;
     end;

   if FBorderWidth > 0 then
    begin
     Pen.Color := Font.Color;
     Pen.Width := FOSFactor * FBorderWidth;
     Brush.Style := bsClear;
     RoundRect((FOSFactor * FBorderWidth) div 2,
       (FOSFactor * FBorderWidth) div 2,
       Width - (FOSFactor * FBorderWidth) div 2,
       Height - (FOSFactor * FBorderWidth) div 2,
       FOSFactor * FBorderRadius, FOSFactor * FBorderRadius);
    end;

   Unlock;
  end;
end;

procedure TGuiEQGraph.Resize;
begin
 inherited;
 FBuffer.Canvas.Brush.Color := Self.Color;
 FBuffer.Width := Self.Width;
 FBuffer.Height := Self.Height;
 Invalidate;
end;

procedure TGuiEQGraph.SetGraphColorDark(const Value: TColor);
begin
 if FGraphColorDark <> Value then
  begin
   FGraphColorDark := Value;
   GraphColorDarkChanged;
  end;
end;

procedure TGuiEQGraph.GraphColorDarkChanged;
begin
 Invalidate;
end;

procedure TGuiEQGraph.SetGraphColorLight(const Value: TColor);
begin
 if FGraphColorLight <> Value then
  begin
   FGraphColorLight := Value;
   GraphColorLightChanged;
  end;
end;

procedure TGuiEQGraph.GraphColorLightChanged;
begin
 Invalidate;
end;

procedure TGuiEQGraph.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TGuiEQGraph.TransparentChanged;
begin
 Invalidate;
end;

procedure TGuiEQGraph.SetXAxis(const Value: TGuiEQGraphXAxis);
begin
 FXAxis.Assign(Value);
end;

procedure TGuiEQGraph.SetYAxis(const Value: TGuiEQGraphYAxis);
begin
 FYAxis.Assign(Value);
end;

end.
