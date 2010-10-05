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
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, Messages,
  {$ENDIF} Classes, Graphics, Forms, SysUtils, Controls, Contnrs,
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
    FStitchedPixelMap : TGuiCustomPixelMap;
    FGlyphCount       : Integer;
    FOnChange         : TNotifyEvent;
    FStitchKind       : TGuiStitchKind;
    FLinkedStitcheds  : TObjectList;
    FDisplayName      : string;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetStitchedPixelMap(const Value: TGuiCustomPixelMap);
    procedure SetGlyphCount(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SettingsChanged(Sender: TObject);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    procedure GlyphCountChanged; virtual;
    procedure StitchKindChanged; virtual;

    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure LinkStitchedControl(Stitched: TGuiCustomStitchedControl);
    procedure UnLinkStitchedControl(Stitched: TGuiCustomStitchedControl);

    property DisplayName: string read GetDisplayName write SetDisplayName;
    property StitchedPixelMap: TGuiCustomPixelMap read FStitchedPixelMap write SetStitchedPixelMap;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TGuiStitchedImageCollectionItem = class(TGuiCustomStitchedCollectionItem)
  published
    property DisplayName;
    property StitchedPixelMap;
    property GlyphCount;
    property StitchKind;
    property OnChange;
    property Height;
    property Width;
  end;

  TGuiCustomStitchedList = class(TComponent)
  protected
    function GetCount: Integer; virtual; abstract;
  public
    property Count: Integer read GetCount;
  end;

  TGuiStitchedImageList = class(TGuiCustomStitchedList)
  private
    FStitchedImageCollection : TGuiStitchedImageCollection;
    function GetItems(Index: Integer): TGuiStitchedImageCollectionItem;
  protected
    function GetCount: Integer; override;
    property Items[Index: Integer]: TGuiStitchedImageCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property StitchedImages: TGuiStitchedImageCollection read FStitchedImageCollection write FStitchedImageCollection;
  end;

  TGuiCustomStitchedControl = class(TCustomControl)
  private
    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetStitchedIndex(Value: Integer);
    procedure SetStitchedList(const Value: TGuiStitchedImageList);
    procedure SetColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetGlyphIndex(Value: Integer);
    procedure SetDefaultGlyphIndex(Value: Integer);
  protected
    FAutoSize          : Boolean;
    FColor             : TColor;
    FTransparent       : Boolean;
    FUpdateBuffer      : Boolean;
    FUpdateBackBuffer  : Boolean;
    FBackBuffer        : TGuiCustomPixelMap;
    FBuffer            : TGuiCustomPixelMap;
    FOnChange          : TNotifyEvent;
    FStitchedItemIndex : Integer;
    FStitchedList      : TGuiStitchedImageList;
    FStitchedItem      : TGuiCustomStitchedCollectionItem;

    FGlyphIndex        : Integer;
    FDefaultGlyphIndex : Integer;

    procedure BackBufferChanged; virtual;
    procedure BufferChanged; virtual;
    procedure ColorChanged; virtual;
    procedure Changed; reintroduce; virtual;
    procedure DefaultGlyphIndexChanged; virtual;
    procedure GlyphIndexChanged; virtual;
    procedure StitchedIndexChanged; virtual;
    procedure StitchedListChanged; virtual;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property StitchedImageList: TGuiStitchedImageList read FStitchedList write SetStitchedList;
    property StitchedImageIndex: Integer read FStitchedItemIndex write SetStitchedIndex default -1;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  DAV_Common, DAV_GuiBlend;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

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
 // add things that depend on the order here!
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
 FLinkedStitcheds           := TObjectList.Create(False);
end;

destructor TGuiCustomStitchedCollectionItem.Destroy;
begin
 FreeAndNil(FStitchedPixelMap);
 FreeAndNil(FLinkedStitcheds);
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
 if FLinkedStitcheds.IndexOf(Stitched) < 0 then
  begin
   FLinkedStitcheds.Add(Stitched);
   case StitchKind of
    skHorizontal :
     begin
      Stitched.Width  := Width div GlyphCount;
      Stitched.Height := Height;
     end;
    skVertical :
     begin
      Stitched.Width  := Width;
      Stitched.Height := Height div GlyphCount;
     end;
   end;
  end;
end;

procedure TGuiCustomStitchedCollectionItem.UnLinkStitchedControl(Stitched: TGuiCustomStitchedControl);
begin
 FLinkedStitcheds.Remove(Stitched);
end;

procedure TGuiCustomStitchedCollectionItem.SettingsChanged(Sender: TObject);
var
  Index : Integer;
begin
 for Index := 0 to FLinkedStitcheds.Count - 1 do
  with TGuiCustomStitchedControl(FLinkedStitcheds[Index]) do
   begin
    GlyphCount := Self.GlyphCount;
    StitchKind := Self.StitchKind;
    BufferChanged;
   end;
end;

procedure TGuiCustomStitchedCollectionItem.SetWidth(const Value: Integer);
begin
 if Value < 0 then Exit;
 FStitchedPixelMap.Width := Value;
end;

procedure TGuiCustomStitchedCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
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
 for Index := 0 to FLinkedStitcheds.Count - 1 do
  with TGuiCustomStitchedControl(FLinkedStitcheds[Index]) do
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
 for Index := 0 to FLinkedStitcheds.Count - 1 do
  with TGuiCustomStitchedControl(FLinkedStitcheds[Index]) do
   if FAutoSize then DoAutoSize;
end;

procedure TGuiCustomStitchedCollectionItem.SetGlyphCount(const Value: Integer);
begin
// if Value <= 0
//  then raise Exception.Create(RCStrGlyphCountMustBePositive);

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


{ TGuiStitchedImageList }

constructor TGuiStitchedImageList.Create(AOwner: TComponent);
begin
  inherited;
  FStitchedImageCollection := TGuiStitchedImageCollection.Create(Self, TGuiStitchedImageCollectionItem);
end;

destructor TGuiStitchedImageList.Destroy;
begin
  FreeAndNil(FStitchedImageCollection);
  inherited;
end;

function TGuiStitchedImageList.GetCount: Integer;
begin
  Result := FStitchedImageCollection.Count;
end;

function TGuiStitchedImageList.GetItems(Index: Integer): TGuiStitchedImageCollectionItem;
begin
 if (Index >= 0) and (Index < FStitchedImageCollection.Count)
  then Result := TGuiStitchedImageCollectionItem(FStitchedImageCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;


{ TGuiCustomStitchedControl }

constructor TGuiCustomStitchedControl.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FBuffer            := TGuiPixelMapMemory.Create;
 FBackBuffer        := TGuiPixelMapMemory.Create;
 FUpdateBuffer      := False;
 FColor             := clBtnFace;
 FGlyphIndex        := 0;
 FDefaultGlyphIndex := 0;
 FStitchedItemIndex := -1;

 ControlStyle       := ControlStyle + [csOpaque];
end;

destructor TGuiCustomStitchedControl.Destroy;
begin
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);
 inherited Destroy;
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
      Width  := FBuffer.Width;
      Height := FBuffer.Height div GlyphCount;
     end
    else
     begin
      Width  := FBuffer.Width div GlyphCount;
      Height := FBuffer.Height;
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
   if FStitchedItemIndex >= FStitchedList.Count then
    begin
     FStitchedItemIndex := -1;
     FStitchedItem := nil;
     Exit;
    end;

   if FStitchedItemIndex >= 0 then
    begin
     FStitchedList[FStitchedItemIndex].LinkStitchedControl(Self);
     FStitchedItem := FStitchedList[FStitchedItemIndex];
    end;
   StitchedIndexChanged;
  end;

 // check and set glyph index
 if FGlyphIndex > -1 then
  begin
   if Assigned(FStitchedItem) and (FGlyphIndex >= FStitchedItem.FGlyphCount)
    then FGlyphIndex := -1;

   GlyphIndexChanged;
  end;

 // check and set default glyph index
 if FDefaultGlyphIndex > -1 then
  begin
   if Assigned(FStitchedItem) and (FDefaultGlyphIndex >= FStitchedItem.FGlyphCount)
    then FGlyphIndex := -1;

   DefaultGlyphIndexChanged;
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

procedure TGuiCustomStitchedControl.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   ColorChanged;
  end;
end;

procedure TGuiCustomStitchedControl.SetStitchedIndex(Value: Integer);
begin
 if csLoading in ComponentState then
  begin
   FStitchedItemIndex := Value;
   FStitchedItem := nil;
   Exit;
  end;

 // check if Stitched image list is available
 if not Assigned(FStitchedList) then Exit;

 // limit range to existing Stitched images
 if Value < 0 then Value := 0 else
 if Value >= FStitchedList.Count then Value := FStitchedList.Count - 1;

 if StitchedImageIndex <> Value then
  begin
   FStitchedItemIndex := Value;

   if FStitchedItemIndex >= 0 then
    begin
     FStitchedList[FStitchedItemIndex].LinkStitchedControl(Self);
     FStitchedItem := FStitchedList[FStitchedItemIndex];
    end
   else
    begin
     FStitchedItem.UnLinkStitchedControl(Self);
     FStitchedItem := nil;
    end;

   StitchedIndexChanged;
  end;
end;

procedure TGuiCustomStitchedControl.SetStitchedList(const Value: TGuiStitchedImageList);
begin
 if FStitchedList <> Value then
  begin
   FStitchedList := Value;
   if not Assigned(FStitchedList) then
    begin
     FStitchedItem := nil;
     FStitchedItemIndex := -1;
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

procedure TGuiCustomStitchedControl.GlyphIndexChanged;
begin
 Changed;
 BufferChanged;
end;

procedure TGuiCustomStitchedControl.StitchedIndexChanged;
begin
 BufferChanged;
end;

procedure TGuiCustomStitchedControl.StitchedListChanged;
begin
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
   PixelColor32 := ConvertColor(FColor);
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
      begin
       for LineIndex := 0 to Self.Height - 1 do
        begin
         DataPointer := StitchedPixelMap.ScanLine[
           LineIndex * StitchedPixelMap.Width + FGlyphIndex * Self.Width];
         BlendLine(PPixel32(DataPointer), PPixel32(FBuffer.ScanLine[LineIndex]), Self.Width);
        end;
       EMMS;
      end;
     skVertical   :
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
