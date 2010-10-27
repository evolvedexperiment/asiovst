unit DAV_GuiStitchedControls;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, SysUtils, Controls, Contnrs,
  DAV_GuiCommon, DAV_GuiPixelMap;

type
  TGuiStitchKind = (skHorizontal, skVertical);

  // forward declarations
  TGuiCustomStitchedCollectionItem = class;
  TGuiCustomStitchedControl = class;

  TGuiStitchedImageCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiCustomStitchedCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiCustomStitchedCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiCustomStitchedCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    function Add: TGuiCustomStitchedCollectionItem;
    function Insert(Index: Integer): TGuiCustomStitchedCollectionItem;
    procedure Delete(Index: Integer);

    property Count;
  end;

  TGuiCustomStitchedCollectionItem = class(TCollectionItem)
  private
    FGlyphCount     : Integer;
    FOnChange       : TNotifyEvent;
    FStitchKind     : TGuiStitchKind;
    FLinkedControls : TObjectList;
    FDisplayName    : string;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetStitchedPixelMap(const Value: TGuiCustomPixelMap);
    procedure SetGlyphCount(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SettingsChanged(Sender: TObject);
  protected
    FStitchedPixelMap : TGuiCustomPixelMap;
    procedure GlyphCountChanged; virtual;
    procedure StitchKindChanged; virtual;

    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;

    procedure LinkStitchedControl(Stitched: TGuiCustomStitchedControl);
    procedure UnLinkStitchedControl(Stitched: TGuiCustomStitchedControl);
    procedure UnLinkStitchedControls;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property DisplayName: string read GetDisplayName write SetDisplayName;
    property StitchedPixelMap: TGuiCustomPixelMap read FStitchedPixelMap write SetStitchedPixelMap;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TGuiCustomStitchedList = class(TComponent)
  private
    function GetItems(Index: Integer): TGuiCustomStitchedCollectionItem;
  protected
    FStitchedCollection : TGuiStitchedImageCollection;
    FLinkedControls     : TObjectList;
    function GetCount: Integer; virtual; abstract;

    procedure LinkStitchedControl(Stitched: TGuiCustomStitchedControl);
    procedure UnLinkStitchedControl(Stitched: TGuiCustomStitchedControl);
    procedure UnLinkStitchedControls;

    property Items[Index: Integer]: TGuiCustomStitchedCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Count: Integer read GetCount;
  end;

  TGuiCustomStitchedControl = class(TCustomControl)
  private
    function GetGlyphCount: Integer;
    function GetStitchedImageIndex: Integer;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetStitchedIndex(Value: Integer);
    procedure SetStitchedList(const Value: TGuiCustomStitchedList);
    procedure SetTransparent(const Value: Boolean);
    procedure SetGlyphIndex(Value: Integer);
    procedure SetDefaultGlyphIndex(Value: Integer);
    procedure SetStitchedItem(Value: TGuiCustomStitchedCollectionItem);
  protected
    FAutoSize          : Boolean;
    FTransparent       : Boolean;
    FUpdateBuffer      : Boolean;
    FUpdateBackBuffer  : Boolean;
    FBackBuffer        : TGuiCustomPixelMap;
    FBuffer            : TGuiCustomPixelMap;
    FOnChange          : TNotifyEvent;
    FStitchedList      : TGuiCustomStitchedList;
    FStitchedItem      : TGuiCustomStitchedCollectionItem;
    FStitchedImageIndex : Integer;

    FGlyphIndex        : Integer;
    FDefaultGlyphIndex : Integer;

    {$IFDEF FPC}
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
    {$ELSE}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    {$ENDIF}

    procedure BackBufferChanged; virtual;
    procedure BufferChanged; virtual;
    procedure ColorChanged; virtual;
    procedure Changed; reintroduce; virtual;
    procedure DefaultGlyphIndexChanged; virtual;
    procedure DoAutoSize; virtual;
    procedure GlyphIndexChanged; virtual;
    procedure StitchedIndexChanged; virtual;
    procedure StitchedListChanged; virtual;
    procedure StitchedItemChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;
    procedure TransparentChanged; virtual;

    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure CopyParentImage(PixelMap: TGuiCustomPixelMap); virtual;

    property Buffer: TGuiCustomPixelMap read FBuffer;
    property DefaultGlyphIndex: Integer read FDefaultGlyphIndex write SetDefaultGlyphIndex;
    property GlyphIndex: Integer read FGlyphIndex write SetGlyphIndex;
    property StitchedItem: TGuiCustomStitchedCollectionItem read FStitchedItem write SetStitchedItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property GlyphCount: Integer read GetGlyphCount;
    property StitchedImageList: TGuiCustomStitchedList read FStitchedList write SetStitchedList;
    property StitchedImageIndex: Integer read GetStitchedImageIndex write SetStitchedIndex default -1;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  DAV_Common, DAV_GuiBlend;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrGlyphCountMustBePositive = 'The glyph count must be positive';

{ TGuiStitchedImageCollection }

constructor TGuiStitchedImageCollection.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
 inherited Create(AOwner, ItemClass);
end;

function TGuiStitchedImageCollection.Add: TGuiCustomStitchedCollectionItem;
begin
 Result := TGuiCustomStitchedCollectionItem(inherited Add);
end;

procedure TGuiStitchedImageCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiStitchedImageCollection.GetItem(
  Index: Integer): TGuiCustomStitchedCollectionItem;
begin
 Result := TGuiCustomStitchedCollectionItem(inherited GetItem(Index));
end;

function TGuiStitchedImageCollection.Insert(
  Index: Integer): TGuiCustomStitchedCollectionItem;
begin
 Result:= TGuiCustomStitchedCollectionItem(inherited Insert(Index));
end;

procedure TGuiStitchedImageCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;

 if Item is TGuiCustomStitchedCollectionItem then
  with TGuiCustomStitchedCollectionItem(Item) do
   begin
    if Action in [cnDeleting, cnExtracting]
     then UnLinkStitchedControls;
   end;
end;

procedure TGuiStitchedImageCollection.SetItem(Index: Integer;
  const Value: TGuiCustomStitchedCollectionItem);
begin
 inherited SetItem(Index, Value);
end;


{ TGuiCustomStitchedCollectionItem }

constructor TGuiCustomStitchedCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FGlyphCount                := 1;
 FStitchedPixelMap          := TGuiPixelMapMemory.Create;
 FStitchedPixelMap.OnResize := SettingsChanged;
 FLinkedControls            := TObjectList.Create(False);
end;

destructor TGuiCustomStitchedCollectionItem.Destroy;
begin
 FreeAndNil(FLinkedControls);
 FreeAndNil(FStitchedPixelMap);
 inherited;
end;

function TGuiCustomStitchedCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TGuiCustomStitchedCollectionItem.GetHeight: Integer;
begin
 Result := FStitchedPixelMap.Height;
end;

function TGuiCustomStitchedCollectionItem.GetWidth: Integer;
begin
 Result := FStitchedPixelMap.Width;
end;

procedure TGuiCustomStitchedCollectionItem.LinkStitchedControl(Stitched: TGuiCustomStitchedControl);
begin
 Assert(Assigned(FLinkedControls));
 if FLinkedControls.IndexOf(Stitched) < 0
  then FLinkedControls.Add(Stitched);
end;

procedure TGuiCustomStitchedCollectionItem.UnLinkStitchedControl(Stitched: TGuiCustomStitchedControl);
begin
 Assert(Assigned(FLinkedControls));
 if Assigned(Stitched) then
  begin
   FLinkedControls.Remove(Stitched);
   Stitched.StitchedImageIndex := -1;
  end;
end;

procedure TGuiCustomStitchedCollectionItem.UnLinkStitchedControls;
begin
 if Assigned(FLinkedControls) then
  while FLinkedControls.Count > 0
   do UnLinkStitchedControl(TGuiCustomStitchedControl(FLinkedControls[0]));
end;

procedure TGuiCustomStitchedCollectionItem.SettingsChanged(Sender: TObject);
var
  Index : Integer;
begin
 Assert(Assigned(FLinkedControls));
 for Index := 0 to FLinkedControls.Count - 1 do
  with TGuiCustomStitchedControl(FLinkedControls[Index])
   do BufferChanged;
end;

procedure TGuiCustomStitchedCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiCustomStitchedCollectionItem.SetWidth(const Value: Integer);
begin
 if Value < 0 then Exit;
 FStitchedPixelMap.Width := Value;
end;

procedure TGuiCustomStitchedCollectionItem.SetHeight(const Value: Integer);
begin
 if Value < 0 then Exit;
 FStitchedPixelMap.Height := Value;
end;

procedure TGuiCustomStitchedCollectionItem.GlyphCountChanged;
var
  Index : Integer;
begin
 for Index := 0 to FLinkedControls.Count - 1 do
  with TGuiCustomStitchedControl(FLinkedControls[Index]) do
   begin
    if FDefaultGlyphIndex >= FGlyphCount then DefaultGlyphIndex := FGlyphCount - 1;
    if FGlyphIndex >= FGlyphCount then GlyphIndex := FGlyphCount - 1;

    if FAutoSize then DoAutoSize;
   end;
end;

procedure TGuiCustomStitchedCollectionItem.StitchKindChanged;
var
  Index : Integer;
begin
 for Index := 0 to FLinkedControls.Count - 1 do
  with TGuiCustomStitchedControl(FLinkedControls[Index]) do
   if FAutoSize then DoAutoSize;
end;

procedure TGuiCustomStitchedCollectionItem.SetGlyphCount(const Value: Integer);
begin
 if Value <= 0
  then raise Exception.Create(RCStrGlyphCountMustBePositive);

 if FGlyphCount <> Value then
  begin
   FGlyphCount := Value;
   GlyphCountChanged;
  end;
end;

procedure TGuiCustomStitchedCollectionItem.SetStitchedPixelMap(
  const Value: TGuiCustomPixelMap);
begin
 FStitchedPixelMap.Assign(Value);
end;

procedure TGuiCustomStitchedCollectionItem.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   StitchKindChanged;
  end;
end;


{ TGuiCustomStitchedList }

constructor TGuiCustomStitchedList.Create(AOwner: TComponent);
begin
 inherited;
 FLinkedControls := TObjectList.Create(False);
end;

destructor TGuiCustomStitchedList.Destroy;
begin
 FreeAndNil(FLinkedControls);
 inherited;
end;

function TGuiCustomStitchedList.GetItems(
  Index: Integer): TGuiCustomStitchedCollectionItem;
begin
 Assert(Assigned(FStitchedCollection));
 if (Index >= 0) and (Index < FStitchedCollection.Count)
  then Result := TGuiCustomStitchedCollectionItem(FStitchedCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TGuiCustomStitchedList.LinkStitchedControl(
  Stitched: TGuiCustomStitchedControl);
begin
 Assert(Assigned(FLinkedControls));
 if FLinkedControls.IndexOf(Stitched) < 0
  then FLinkedControls.Add(Stitched);
end;

procedure TGuiCustomStitchedList.UnLinkStitchedControl(
  Stitched: TGuiCustomStitchedControl);
begin
 Assert(Assigned(FLinkedControls));
 if Assigned(Stitched)
  then FLinkedControls.Remove(Stitched);
end;

procedure TGuiCustomStitchedList.UnLinkStitchedControls;
begin
 Assert(Assigned(FLinkedControls));
 while FLinkedControls.Count > 0 do
  with TGuiCustomStitchedControl(FLinkedControls[0])
   do SetStitchedList(nil);
end;


{ TGuiCustomStitchedControl }

constructor TGuiCustomStitchedControl.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FBuffer            := TGuiPixelMapMemory.Create;
 FBackBuffer        := TGuiPixelMapMemory.Create;
 FUpdateBuffer      := False;
 FGlyphIndex        := 0;
 FDefaultGlyphIndex := 0;
 {$IFDEF FPC}
 DoubleBuffered     := True;
 {$ENDIF}

 ControlStyle       := ControlStyle + [csOpaque];
end;

destructor TGuiCustomStitchedControl.Destroy;
begin
 // unlink any stitched item
 StitchedItem := nil;

 // free buffers
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);

 inherited;
end;

type
  TParentControl = class(TWinControl);

procedure TGuiCustomStitchedControl.CopyParentImage(
  PixelMap: TGuiCustomPixelMap);
var
  I         : Integer;
  SubCount  : Integer;
  SaveIndex : Integer;
  Pnt       : TPoint;
  R, SelfR  : TRect;
  CtlR      : TRect;
  Bmp       : TBitmap;
begin
 if (Parent = nil) then Exit;
 SubCount := Parent.ControlCount;
 // set
 {$IFDEF WIN32}
 with Parent
  do ControlState := ControlState + [csPaintCopy];
 try
 {$ENDIF}

  SelfR := Bounds(Left, Top, Width, Height);
  Pnt.X := -Left;
  Pnt.Y := -Top;

  {$IFNDEF FPC}
  Bmp := TBitmap.Create;
  with Bmp do
   try
    Width := Self.Width;
    Height := Self.Height;
    PixelFormat := pf32bit;

    // Copy parent control image
    SaveIndex := SaveDC(Canvas.Handle);
    try
     SetViewportOrgEx(Canvas.Handle, Pnt.X, Pnt.Y, nil);
     IntersectClipRect(Canvas.Handle, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
     with TParentControl(Parent) do
      begin
       Perform(WM_ERASEBKGND, Canvas.Handle, 0);
       PaintWindow(Canvas.Handle);
      end;
    finally
     RestoreDC(Canvas.Handle, SaveIndex);
    end;

    // Copy images of graphic controls
    for I := 0 to SubCount - 1 do
     begin
      if Parent.Controls[I] = Self then Break else
       if (Parent.Controls[I] <> nil) and
          (Parent.Controls[I] is TGraphicControl)
        then
         with TGraphicControl(Parent.Controls[I]) do
          begin
           CtlR := Bounds(Left, Top, Width, Height);
           if Boolean(IntersectRect(R, SelfR, CtlR)) and Visible then
            begin
             {$IFDEF WIN32}
             ControlState := ControlState + [csPaintCopy];
             {$ENDIF}
             SaveIndex := SaveDC(Canvas.Handle);
             try
              SaveIndex := SaveDC(Canvas.Handle);
              SetViewportOrgEx(Canvas.Handle, Left + Pnt.X, Top + Pnt.Y, nil);
              IntersectClipRect(Canvas.Handle, 0, 0, Width, Height);
              Perform(WM_PAINT, Canvas.Handle, 0);
             finally
              RestoreDC(Handle, SaveIndex);
              {$IFDEF WIN32}
              ControlState := ControlState - [csPaintCopy];
              {$ENDIF}
             end;
            end;
          end;
     end;
    PixelMap.Draw(Bmp);
   finally
    Free;
   end;
  {$ENDIF}

 {$IFDEF WIN32}
 finally
   with Parent do ControlState := ControlState - [csPaintCopy];
 end;
 {$ENDIF}
end;

procedure TGuiCustomStitchedControl.DoAutoSize;
begin
 if Assigned(FStitchedItem) then
  with FStitchedItem do
   begin
    if (GlyphCount = 0) then Exit;

    if StitchKind = skVertical then
     begin
      Self.Width  := FStitchedPixelMap.Width;
      Self.Height := FStitchedPixelMap.Height div GlyphCount;
     end
    else
     begin
      Self.Width  := FStitchedPixelMap.Width div GlyphCount;
      Self.Height := FStitchedPixelMap.Height;
     end;
   end;
end;

procedure TGuiCustomStitchedControl.Paint;
begin
 inherited;

 if FUpdateBackBuffer
  then UpdateBackBuffer;

 if FUpdateBuffer
  then UpdateBuffer;

 if Assigned(FBuffer)
  then FBuffer.PaintTo(Canvas);
end;

procedure TGuiCustomStitchedControl.Resize;
begin
 if Assigned(FBuffer)
  then FBuffer.SetSize(Width, Height);

 if Assigned(FBackBuffer) then
  begin
   FBackBuffer.SetSize(Width, Height);
   UpdateBackBuffer;
  end;

 inherited;
end;

procedure TGuiCustomStitchedControl.Loaded;
begin
 inherited;
 Resize;

 if Assigned(FStitchedList) then
  begin
   if FStitchedImageIndex >= FStitchedList.Count then
    begin
     StitchedImageIndex := -1;
     FStitchedItem := nil;
     Exit;
    end;

   if FStitchedImageIndex >= 0
    then StitchedItem := FStitchedList[FStitchedImageIndex];
   StitchedIndexChanged;
  end;

 // check and set glyph index
 if FGlyphIndex > -1 then
  begin
   if Assigned(FStitchedItem) and (FGlyphIndex >= FStitchedItem.FGlyphCount)
    then FGlyphIndex := -1;
  end;

 // check and set default glyph index
 if FDefaultGlyphIndex > -1 then
  begin
   if Assigned(FStitchedItem) and (FDefaultGlyphIndex >= FStitchedItem.FGlyphCount)
    then FDefaultGlyphIndex := -1;
  end;
end;

procedure TGuiCustomStitchedControl.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Autosize then DoAutoSize;
  end;
end;

procedure TGuiCustomStitchedControl.SetStitchedIndex(Value: Integer);
begin
 if csLoading in ComponentState then
  begin
   FStitchedImageIndex := Value;
   Exit;
  end;

 // check if stitched image list is available
 if Assigned(FStitchedList) then
  begin
   // limit range to existing stitched images (or -1 for nothing)
   if Value < 0 then Value := -1 else
   if Value >= FStitchedList.Count then Value := FStitchedList.Count - 1;

   if StitchedImageIndex <> Value then
    begin
     FStitchedImageIndex := Value;

     if Value > -1
      then StitchedItem := FStitchedList[Value]
      else StitchedItem := nil;

     StitchedIndexChanged;
    end;
  end;
end;

procedure TGuiCustomStitchedControl.SetStitchedItem(
  Value: TGuiCustomStitchedCollectionItem);
begin
 if FStitchedItem <> Value then
  begin
   // check whether a item is linked at all
   if not Assigned(Value) then
    begin
     Assert(Assigned(FStitchedItem));
     Value := FStitchedItem;
     FStitchedItem := nil;
     Value.UnLinkStitchedControl(Self);
    end
   else
    begin
     Assert(Assigned(Value));
     if Assigned(FStitchedItem)
      then FStitchedItem.UnLinkStitchedControl(Self);
     FStitchedItem := Value;
     FStitchedItem.LinkStitchedControl(Self);
    end;
   StitchedItemChanged;
  end;
end;

procedure TGuiCustomStitchedControl.SetStitchedList(const Value: TGuiCustomStitchedList);
begin
 if FStitchedList <> Value then
  begin
   // check whether a list is linked at all
   if not Assigned(Value) then
    begin
     Assert(Assigned(FStitchedList));
     StitchedItem := nil;
     FStitchedList.UnLinkStitchedControl(Self);
     FStitchedList := nil;
    end
   else
    begin
     Assert(Assigned(Value));
     FStitchedList := Value;
     FStitchedList.LinkStitchedControl(Self);
    end;
   StitchedListChanged;
  end;
end;

procedure TGuiCustomStitchedControl.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TGuiCustomStitchedControl.SetDefaultGlyphIndex(Value: Integer);
begin
 if csLoading in ComponentState then
  begin
   FDefaultGlyphIndex := Value;
   Exit;
  end;

 if Assigned(FStitchedItem)
  then Value := Limit(Value, 0, FStitchedItem.GlyphCount - 1)
  else Value := -1;

 if Value <> FDefaultGlyphIndex then
  begin
   FDefaultGlyphIndex := Value;
   DefaultGlyphIndexChanged;
  end;
end;

procedure TGuiCustomStitchedControl.SetGlyphIndex(Value: Integer);
begin
 if csLoading in ComponentState then
  begin
   FGlyphIndex := Value;
   Exit;
  end;

 if Assigned(FStitchedItem)
  then Value := Limit(Value, 0, FStitchedItem.GlyphCount - 1)
  else Value := -1;

 if Value <> FGlyphIndex then
  begin
   FGlyphIndex := Value;
   GlyphIndexChanged;
  end;
end;

procedure TGuiCustomStitchedControl.Changed;
begin
 inherited Changed;
 if Assigned(FOnChange) and ([csLoading, csDestroying] * ComponentState = [])
  then FOnChange(Self);
end;

procedure TGuiCustomStitchedControl.CMColorChanged(var Message: {$IFDEF FPC}TLMessage {$ELSE} TMessage{$ENDIF});
begin
 ColorChanged;
end;

procedure TGuiCustomStitchedControl.ColorChanged;
begin
 if not FTransparent
  then BackBufferChanged;
end;

procedure TGuiCustomStitchedControl.DefaultGlyphIndexChanged;
begin
 //
end;

procedure TGuiCustomStitchedControl.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TGuiCustomStitchedControl.BufferChanged;
begin
 FUpdateBuffer := True;
 Invalidate;
end;

function TGuiCustomStitchedControl.GetGlyphCount: Integer;
begin
 if Assigned(FStitchedItem)
  then Result := FStitchedItem.GlyphCount
  else Result := 0;
end;

function TGuiCustomStitchedControl.GetStitchedImageIndex: Integer;
begin
 if Assigned(FStitchedItem)
  then Result := FStitchedItem.Index
  else Result := -1;
end;

procedure TGuiCustomStitchedControl.GlyphIndexChanged;
begin
 Changed;
 BufferChanged;
end;

procedure TGuiCustomStitchedControl.StitchedIndexChanged;
begin
 BufferChanged;
end;

procedure TGuiCustomStitchedControl.StitchedItemChanged;
begin
 if Assigned(FStitchedItem) and FAutoSize then
  with FStitchedItem do
   case StitchKind of
    skHorizontal :
     begin
      Self.Width  := Width div GlyphCount;
      Self.Height := Height;
     end;
    skVertical :
     begin
      Self.Width  := Width;
      Self.Height := Height div GlyphCount;
     end;
   end;
 GlyphIndex := -1;
end;

procedure TGuiCustomStitchedControl.StitchedListChanged;
begin
 StitchedItem := nil;
 BufferChanged;
end;

procedure TGuiCustomStitchedControl.TransparentChanged;
begin
 BackBufferChanged;
end;

procedure TGuiCustomStitchedControl.UpdateBackBuffer;
var
  PixelColor32 : TPixel32;
begin
 if FTransparent then CopyParentImage(FBackBuffer) else
  begin
   PixelColor32 := ConvertColor(Color);
   FBackBuffer.FillRect(ClientRect, PixelColor32);
  end;

 FUpdateBuffer := True;
end;

procedure TGuiCustomStitchedControl.UpdateBuffer;
var
  DataPointer : PPixel32Array;
  LineIndex   : Integer;
begin
 FUpdateBuffer := False;

 inherited;

 // check whether a buffer or a back buffer is assigned
 if not Assigned(FBuffer) or not Assigned(FBackBuffer)
  then Exit;

 Assert((FBackBuffer.Width = FBuffer.Width) and (FBackBuffer.Height = FBuffer.Height));

 // copy back buffer to buffer
 Move(FBackBuffer.DataPointer^, FBuffer.DataPointer^, FBuffer.Height *
   FBuffer.Width * SizeOf(TPixel32));

 if Assigned(FStitchedItem) and (FGlyphIndex >= 0) then
  with FStitchedItem do
   begin
    // check whether the stitched item contains at least one glyph
    if GlyphCount = 0 then Exit;

    case StitchKind of
     skHorizontal :
      if FGlyphIndex * Self.Width < FStitchedItem.StitchedPixelMap.Width then
       begin
        DataPointer := @StitchedPixelMap.DataPointer[FGlyphIndex * Self.Width];
        for LineIndex := 0 to Self.Height - 1 do
         begin
          BlendLine(PPixel32(DataPointer), PPixel32(FBuffer.ScanLine[LineIndex]), Self.Width);
          DataPointer := @DataPointer[StitchedPixelMap.Width];
         end;
        EMMS;
       end;
     skVertical   :
      if FGlyphIndex * Self.Height < StitchedPixelMap.Height then
       begin
        DataPointer := StitchedPixelMap.ScanLine[FGlyphIndex * Self.Height];
        BlendLine(PPixel32(DataPointer), PPixel32(FBuffer.DataPointer),
          Self.Height * Self.Width);
        EMMS;
       end;
    end;
   end;
end;

end.
