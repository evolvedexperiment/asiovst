unit DAV_GuiMidiKeys;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Graphics, Forms, Controls, ExtCtrls, Messages, DAV_GuiBaseControl,
  DAV_GuiMidiKeyZones;

const
  GUI_KB_MAXOCTAVES = 11;
  GUI_KB_HIGHESTKEY = GUI_KB_MAXOCTAVES*12+1;  // Octaves + Highest C
  
type
  TGuiZoneMousePosType = set of (mptOutside, mptInZone, mptOnLowestKey, mptOnHighestKey, mptOnLowestBorder, mptOnHighestBorder);
  TGuiKbMouseAction = (kmaMove, kmaDown, kmaUp, kmaStartDrag);
  
  TGuiKbMouseDragInfo = record
    Button     : TMouseButton;
    isDragging : Boolean;       
    StartKey   : Integer; // -1 = Out of visible range
    LastKey    : Integer; // -1 = Out of visible range
  end;

  TGuiZbMouseDragInfo = record
    Button              : TMouseButton;
    isDragging          : Boolean;
    Zone                : TGuiKeyZoneItem;
    InZonePos           : TGuiZoneMousePosType;
    StartKey            : Integer; // -1 = Out of visible range
    LastKey             : Integer; // -1 = Out of visible range
    StartLowestZoneKey  : Integer;
    StartHighestZoneKey : Integer;
  end;

  TGuiKeyFlags = set of (kfBlackKey, kfVisible, kfPressed, kfMouseOver, kfByMouse, kfMousePinned);
  TGuiKeyDownMode = (kdmUp, kdmFlat, kdmDown);
  TGuiColorRect = record
    Top, Left, Right, Bottom: TColor;
  end;

  TGuiSingleKey = record
    KeyNr        : Byte;
    Flags        : TGuiKeyFlags;
    Area         : TRect;
    BaseColor    : TColor;
    PressedColor : TColor;
    OverColor    : TColor;
    Velocity     : Single;
  end;

  TGuiKeyArray = array[0..GUI_KB_HIGHESTKEY] of TGuiSingleKey;

  TGuiOnMouseUpOnMidiKey       = procedure (Sender: TObject; KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer) of Object;
  TGuiOnMouseDownOnMidiKey     = procedure (Sender: TObject; KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer) of Object;

  TGuiOnMouseUpOnKeyZoneBar    = procedure (Sender: TObject; KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer) of Object;
  TGuiOnMouseDownOnKeyZoneBar  = procedure (Sender: TObject; KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer) of Object;

  TGuiOnZoneBarMouseEnter      = procedure (Sender: TObject; KeyNr: Byte; Shift: TShiftState; X, Y: Integer) of Object;
  TGuiOnZoneBarMouseLeave      = TNotifyEvent;

  TGuiOnZoneMouseEnter         = procedure (Sender: TObject; Zone: TGuiKeyZoneItem; Shift: TShiftState; X, Y: Integer) of Object;
  TGuiOnZoneMouseLeave         = procedure (Sender: TObject; Zone: TGuiKeyZoneItem) of Object;
  TGuiOnZoneMouseOverChanged   = procedure (Sender: TObject; KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Shift: TShiftState; X, Y: Integer) of Object;
  TGuiOnZoneSelectionChanged   = procedure (Sender: TObject; Zone: TGuiKeyZoneItem) of Object;

  TGuiOnKeyMouseEnter          = procedure (Sender: TObject; KeyNr: Byte; Shift: TShiftState; X, Y: Integer) of Object;
  TGuiOnKeyMouseLeave          = procedure (Sender: TObject; KeyNr: Byte) of Object;

  TGuiOnStartZoneBarDragging   = procedure (Sender: TObject; KeyNr: Integer; var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of Object;
  TGuiOnMoveZoneBarDragging    = procedure (Sender: TObject; KeyNr: Integer; var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of Object;
  TGuiOnEndZoneBarDragging     = procedure (Sender: TObject; var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of Object;

  TGuiOnStartKeyDragging       = procedure (Sender: TObject; KeyNr: Integer; var DragInfo: TGuiKbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of Object;
  TGuiOnMoveKeyDragging        = procedure (Sender: TObject; KeyNr: Integer; var DragInfo: TGuiKbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of Object;
  TGuiOnEndKeyDragging         = procedure (Sender: TObject; var DragInfo: TGuiKbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of Object;

  TGuiOnNoteOn                 = procedure (Sender: TObject; KeyNr: Byte; Velocity: Single) of Object;
  TGuiOnNoteOff                = procedure (Sender: TObject; KeyNr: Byte) of Object;

  TGuiMidiKeys = class(TGuiBaseControl)
  protected
    FRedrawEnabled           : Boolean;
    FKeys                    : TGuiKeyArray;
    FBaseOctave              : Byte;
    FNumOctaves              : Byte;
    FIncludeLastOctave       : Boolean;
    FShowKeyZones            : Boolean;
    FAllowKeyDragging        : Boolean;

    FKeyZoneHeight           : Integer;
    FKeyZones                : TGuiKeyZoneCollection;
    FBlackKeyHeight          : Single;
    FHeight3d                : Single;
    FBorderColor             : TColor;
    FKeySeparatorColor       : TColor;
    FZoneSeparatorColor      : TColor;
    FZoneBarColor            : TColor;
    FZoneBarHoverColor       : TColor;
    FBlackKeyColor           : TColor;
    FBlackKeyHoverColor      : TColor;
    FBlackKeyPressedColor    : TColor;
    FWhiteKeyColor           : TColor;
    FWhiteKeyHoverColor      : TColor;
    FWhiteKeyPressedColor    : TColor;
    FKeyDownMode             : TGuiKeyDownMode;

    FCursorKeys              : TCursor;
    FCursorZoneBar           : TCursor;
    FCursorZone              : TCursor;
    FCursorZoneBorder        : TCursor;

    FZoneMouseOverType       : TGuiZoneMousePosType;

    FOnMouseUpOnMidiKey      : TGuiOnMouseUpOnMidiKey;
    FOnMouseUpOnKeyZoneBar   : TGuiOnMouseUpOnKeyZoneBar;
    FOnMouseDownOnMidiKey    : TGuiOnMouseDownOnMidiKey;
    FOnMouseDownOnKeyZoneBar : TGuiOnMouseDownOnKeyZoneBar;

    FOnZoneBarMouseEnter     : TGuiOnZoneBarMouseEnter;
    FOnZoneBarMouseLeave     : TGuiOnZoneBarMouseLeave;
    FOnZoneMouseEnter        : TGuiOnZoneMouseEnter;
    FOnZoneMouseLeave        : TGuiOnZoneMouseLeave;
    FOnZoneMouseOverChanged  : TGuiOnZoneMouseOverChanged;
    FOnZoneSelectionChanged  : TGuiOnZoneSelectionChanged;

    FOnKeyMouseEnter         : TGuiOnKeyMouseEnter;
    FOnKeyMouseLeave         : TGuiOnKeyMouseLeave;

    FOnStartZoneBarDragging  : TGuiOnStartZoneBarDragging;
    FOnMoveZoneBarDragging   : TGuiOnMoveZoneBarDragging;
    FOnEndZoneBarDragging    : TGuiOnEndZoneBarDragging;

    FOnStartKeyDragging      : TGuiOnStartKeyDragging;
    FOnMoveKeyDragging       : TGuiOnMoveKeyDragging;
    FOnEndKeyDragging        : TGuiOnEndKeyDragging;

    FOnNoteOn                : TGuiOnNoteOn;
    FOnNoteOff               : TGuiOnNoteOff;

    FMidiKeyDragging         : TGuiKbMouseDragInfo;
    FMidiZoneBarDragging     : TGuiZbMouseDragInfo;
    FMidiZoneBarMouseOver    : Boolean;

    procedure SetBorderColor(Value: TColor);
    procedure SetKeySeparatorColor(Value: TColor);
    procedure SetZoneSeparatorColor(Value: TColor);
    procedure SetZoneBarColor(Value: TColor);
    procedure SetZoneBarHoverColor(Value: TColor);
    procedure SetBlackKeyColor(Value: TColor);
    procedure SetBlackKeyHoverColor(Value: TColor);
    procedure SetBlackKeyPressedColor(Value: TColor);
    procedure SetWhiteKeyColor(Value: TColor);
    procedure SetWhiteKeyHoverColor(Value: TColor);
    procedure SetWhiteKeyPressedColor(Value: TColor);

    procedure SetKeyDownMode(Value: TGuiKeyDownMode);
    procedure SetHeight3d(Value: Single);
    procedure SetBlackKeyHeight(Value: Single);
    procedure SetBaseOctave(Value: Byte);
    procedure SetNumOctaves(Value: Byte);
    procedure SetIncludeLastOctave(Value: Boolean);
    procedure SetKeyZones(Value: TGuiKeyZoneCollection);
    procedure SetShowKeyZones(Value: Boolean);
    procedure SetKeyZoneHeight(Value: Integer);

    function  ZoneKeyArea(KeyNr: Byte): TRect;

    function CalculateLights(Color : TColor; DoubledHeight: boolean = False; KeyPressed: boolean = False): TGuiColorRect; virtual;
    procedure CalculateVisibleKeys; virtual;
    procedure InitKeys; virtual;
    procedure DrawKeyZones; virtual;
    procedure DrawSingleKey(CurKey: TGuiSingleKey); virtual;
    procedure DrawKeys; virtual;
    procedure MouseLeaveAllKeys(exceptkey: Integer = -1); virtual;
    procedure MouseLeaveAllZones(ExceptZone: TGuiKeyZoneItem = nil); virtual;
    procedure UnSelectAllZones(ExceptZone: TGuiKeyZoneItem = nil); virtual;

    procedure ResizeBuffer; override;
    
    procedure FireNoteOn(KeyNr: Byte; Flags: TGuiKeyFlags); dynamic;
    procedure FireNoteOff(KeyNr: Byte); dynamic;

    procedure MouseLeave; override;
    procedure KeyMouseEnter(KeyNr: Byte; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure KeyMouseLeave(KeyNr: Byte); dynamic;

    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure StartZoneBarDragging(KeyNr: Integer; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; DragButton: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure MoveZoneBarDragging(KeyNr: Integer; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure EndZoneBarDragging(Shift: TShiftState; X,Y: Integer); dynamic;

    procedure StartKeyDragging(KeyNr: Integer; DragButton: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure MoveKeyDragging(KeyNr: Integer; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure EndKeyDragging(Shift: TShiftState; X,Y: Integer); dynamic;

    procedure ZoneMouseActivity(Action: TGuiKbMouseAction; KeyNr: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure ZoneMouseEnter(Zone: TGuiKeyZoneItem; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure ZoneMouseLeave(Zone: TGuiKeyZoneItem); dynamic;
    procedure ZoneMouseOverChanged(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure ZoneBarMouseEnter(KeyNr: Byte; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure ZoneBarMouseLeave; dynamic;
    procedure ZoneSelectionChanged(Zone: TGuiKeyZoneItem = nil); dynamic;

    procedure MouseUpOnMidiKey(KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure MouseDownOnMidiKey(KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;

    procedure MouseUpOnKeyZoneBar(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure MouseDownOnKeyZoneBar(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetKeyColor(FromKey, ToKey: Byte; BaseColor: TColor = clNone; Over: TColor = clNone; Pressed: TColor = clNone);
    procedure   SetKeyVelocity(FromKey, ToKey: Byte; Amount: Single);
    procedure   SetKeyPressed(KeyNr: Byte; KeyDown: Boolean = True);
    procedure   ReleaseKey(KeyNr: Byte; ReleaseMouseTriggered: boolean = False);
    procedure   ReleaseAllKeys(ReleaseMouseTriggered: boolean = False);
    procedure   RemoveKeyColor(FromKey, ToKey: Byte);
    procedure   AllNotesOff(MouseTriggeredOnly: Boolean = True; ExceptKey: Integer = -1);
    procedure   RedrawBuffer(doBufferFlip: Boolean); override;
    function    MousePosToKey(X, Y: Integer; CheckYPos: Boolean = True): Integer; dynamic;
    function    GetZoneMouseOverType(X: Integer; KeyNr: Integer = -1; Zone: TGuiKeyZoneItem = nil): TGuiZoneMousePosType;
    function    ScreenCoordOnKey(X, Y: Integer; KeyNr: Integer; CheckYPos: Boolean = True): Boolean; dynamic;
    property    Keys: TGuiKeyArray read FKeys;
  published
    property ReleaseMouseBtnOnLeave;
    
    property KeyDownMode: TGuiKeyDownMode read FKeyDownMode write SetKeyDownMode;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property KeySeparatorColor: TColor read FKeySeparatorColor write SetKeySeparatorColor default clBlack;
    property ZoneSeparatorColor: TColor read FZoneSeparatorColor write SetZoneSeparatorColor default clBlack;
    property ZoneBarColor: TColor read FZoneBarColor write SetZoneBarColor default clWhite;
    property ZoneBarHoverColor: TColor read FZoneBarHoverColor write SetZoneBarHoverColor default $00CCEEFF;
    property BlackKeyColor: TColor read FBlackKeyColor write SetBlackKeyColor default $00333333;
    property BlackKeyHoverColor: TColor read FBlackKeyHoverColor write SetBlackKeyHoverColor default $00666666;
    property BlackKeyPressedColor: TColor read FBlackKeyPressedColor write SetBlackKeyPressedColor default clSilver;
    property WhiteKeyColor: TColor read FWhiteKeyColor write SetWhiteKeyColor default $00F8F8F8;
    property WhiteKeyHoverColor: TColor read FWhiteKeyHoverColor write SetWhiteKeyHoverColor default $00DDDDDD;
    property WhiteKeyPressedColor: TColor read FWhiteKeyPressedColor write SetWhiteKeyPressedColor default clSilver;

    property CursorKeys: TCursor read FCursorKeys write FCursorKeys default crHandPoint;
    property CursorZoneBar: TCursor read FCursorZoneBar write FCursorZoneBar default crDefault;
    property CursorZone: TCursor read FCursorZone write FCursorZone default crSize;
    property CursorZoneBorder: TCursor read FCursorZoneBorder write FCursorZoneBorder default crSizeWE;

    property Height3d: Single read FHeight3d write SetHeight3d;
    property BlackKeyHeight: Single read FBlackKeyHeight write SetBlackKeyHeight;
    property BaseOctave: Byte read FBaseOctave write SetBaseOctave default 3;
    property NumOctaves: Byte read FNumOctaves write SetNumOctaves default 3;
    property IncludeLastOctave: Boolean read FIncludeLastOctave write SetIncludeLastOctave default False;
    property KeyZones: TGuiKeyZoneCollection read FKeyZones write SetKeyZones;

    property ShowKeyZones: Boolean read FShowKeyZones write SetShowKeyZones default True;
    property KeyZoneHeight: Integer read FKeyZoneHeight write SetKeyZoneHeight default 10;
    property AllowKeyDragging: Boolean read FAllowKeyDragging write FAllowKeyDragging default True;

    property OnMouseUpOnMidiKey: TGuiOnMouseUpOnMidiKey read FOnMouseUpOnMidiKey write FOnMouseUpOnMidiKey;
    property OnMouseUpOnKeyZoneBar: TGuiOnMouseUpOnKeyZoneBar read FOnMouseUpOnKeyZoneBar write FOnMouseUpOnKeyZoneBar;
    property OnMouseDownOnMidiKey: TGuiOnMouseDownOnMidiKey read FOnMouseDownOnMidiKey write FOnMouseDownOnMidiKey;
    property OnMouseDownKeyZoneBar: TGuiOnMouseDownOnKeyZoneBar read FOnMouseDownOnKeyZoneBar write FOnMouseDownOnKeyZoneBar;

    property OnZoneBarMouseEnter: TGuiOnZoneBarMouseEnter read FOnZoneBarMouseEnter write FOnZoneBarMouseEnter;
    property OnZoneBarMouseLeave: TGuiOnZoneBarMouseLeave read FOnZoneBarMouseLeave write FOnZoneBarMouseLeave;
    property OnZoneMouseEnter: TGuiOnZoneMouseEnter read FOnZoneMouseEnter write FOnZoneMouseEnter;
    property OnZoneMouseLeave: TGuiOnZoneMouseLeave read FOnZoneMouseLeave write FOnZoneMouseLeave;
    property OnZoneMouseOverChanged: TGuiOnZoneMouseOverChanged read FOnZoneMouseOverChanged write FOnZoneMouseOverChanged;
    property OnZoneSelectionChanged: TGuiOnZoneSelectionChanged read FOnZoneSelectionChanged write FOnZoneSelectionChanged;

    property OnKeyMouseEnter: TGuiOnKeyMouseEnter read FOnKeyMouseEnter write FOnKeyMouseEnter;
    property OnKeyMouseLeave: TGuiOnKeyMouseLeave read FOnKeyMouseLeave write FOnKeyMouseLeave;

    property OnStartZoneBarDragging: TGuiOnStartZoneBarDragging read FOnStartZoneBarDragging write FOnStartZoneBarDragging;
    property OnMoveZoneBarDragging: TGuiOnMoveZoneBarDragging read FOnMoveZoneBarDragging write FOnMoveZoneBarDragging;
    property OnEndZoneBarDragging: TGuiOnEndZoneBarDragging read FOnEndZoneBarDragging write FOnEndZoneBarDragging;

    property OnStartKeyDragging: TGuiOnStartKeyDragging read FOnStartKeyDragging write FOnStartKeyDragging;
    property OnMoveKeyDragging: TGuiOnMoveKeyDragging read FOnMoveKeyDragging write FOnMoveKeyDragging;
    property OnEndKeyDragging: TGuiOnEndKeyDragging read FOnEndKeyDragging write FOnEndKeyDragging;

    property OnNoteOn: TGuiOnNoteOn read FOnNoteOn write FOnNoteOn;
    property OnNoteOff: TGuiOnNoteOff read FOnNoteOff write FOnNoteOff;
  end;

implementation

uses
  SysUtils, Math;

constructor TGuiMidiKeys.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyZones := TGuiKeyZoneCollection.Create(Self);
   
  FShowKeyZones         := True;
  FKeyZoneHeight        := 10;

  FAllowKeyDragging        := True;

  FBorderColor          := clBlack;
  ZoneBarColor          := clWhite;
  ZoneBarHoverColor     := $00CCEEFF;
  FKeySeparatorColor    := clBlack;
  FBlackKeyColor        := $00333333;
  FBlackKeyHoverColor   := $00666666;
  FBlackKeyPressedColor := clSilver;
  FWhiteKeyColor        := $00F8F8F8;
  FWhiteKeyHoverColor   := $00DDDDDD;
  FWhiteKeyPressedColor := clSilver;
  FKeyDownMode          := kdmFlat;
  FZoneMouseOverType    := [];
  FRedrawEnabled         := True;

  
  FCursorKeys           := crHandPoint;
  FCursorZoneBar        := crDefault;
  FCursorZone           := crSize;
  FCursorZoneBorder     := crSizeWE;

  FMidiKeyDragging.isDragging     := False;
  FMidiKeyDragging.LastKey        := -1;
  FMidiKeyDragging.StartKey       := -1;
  FMidiZoneBarDragging.isDragging := False;
  FMidiZoneBarDragging.LastKey    := -1;
  FMidiZoneBarDragging.StartKey   := -1;
  FMidiZoneBarDragging.Zone       := nil;
  FMidiZoneBarDragging.InZonePos  := [mptOutside];
  
  FMidiZoneBarMouseOver    := False;
  
  FBaseOctave           := 3;
  FNumOctaves           := 3;
  FIncludeLastOctave    := False;
  FHeight3d             := 0.2;
  FBlackKeyHeight       := 0.63;

  InitKeys;
  RedrawInterval := 50;
end;

destructor TGuiMidiKeys.Destroy;
begin
 FreeAndNil(FKeyZones);
 inherited;
end;

procedure TGuiMidiKeys.InitKeys;
var
  i: Byte;
begin
 for i := 0 to GUI_KB_HIGHESTKEY do with FKeys[i] do
  begin
   KeyNr          := i;
   Flags          := [];
   Area           := Rect(-1, -1, -1, -1);
   PressedColor   := clNone;
   BaseColor      := clNone;
   OverColor      := clNone;
   Velocity       := 1;

   if (i mod 12) in [1, 3, 6, 8, 10]
    then Include(Flags, kfBlackKey);
  end;
end;

procedure TGuiMidiKeys.ResizeBuffer;
begin
  CalculateVisibleKeys;
  inherited;
end;

procedure TGuiMidiKeys.CalculateVisibleKeys;
var
  i, cnt, KeyCount        : Integer;
  KeyWidth                : Single;
  LastRightBorder         : Integer;
  FirstKey, LastKey       : Integer;
  TopMargin, BottomMargin : Integer;
begin
  FirstKey := FBaseOctave*12;
  LastKey  := (FBaseOctave+FNumOctaves)*12+Integer(FIncludeLastOctave)-1;

  for i := 0 to GUI_KB_HIGHESTKEY do with FKeys[i] do
  begin
    if i>LastKey then Area := Rect(width-1,0,width-1,0) else Area := Rect(0,0,0,0);
    exclude(Flags, kfVisible);
  end;

  if FShowKeyZones then TopMargin := FKeyZoneHeight+2 else TopMargin := 1;
  BottomMargin := height - 1;
  

  // set white keys  
  // -------------------------------------------
  if FIncludeLastOctave then KeyCount := 1 else KeyCount := 0;
  KeyWidth := (Width-1) / (KeyCount+FNumOctaves*7);

  cnt := 0;
  LastRightBorder := -1;
  for i := FirstKey to LastKey do
  begin
    if (i mod 12) in [0,2,4,5,7,9,11] then
    begin
      FKeys[i].Area := Rect(LastRightBorder+2,TopMargin,round((cnt+1)*KeyWidth)-1,BottomMargin);
      LastRightBorder := FKeys[i].Area.Right;
      include(FKeys[i].Flags, kfVisible);
      inc(cnt);
    end;
  end;

  // set black keys
  // -------------------------------------------
  if not FIncludeLastOctave then KeyWidth := 0;
  KeyWidth := (Width-2-KeyWidth) / (FNumOctaves*12);

  BottomMargin := Round((height-TopMargin) * FBlackKeyHeight + TopMargin);

  cnt := 0;
  for i := FirstKey to LastKey do
  begin
    if (i mod 12) in [1,3,6,8,10] then
    begin
      FKeys[i].Area := Rect(
        round(cnt*KeyWidth+2),TopMargin,
        round((cnt+1)*KeyWidth+2),BottomMargin);
      include(FKeys[i].Flags, kfVisible);
    end;
    inc(cnt);
  end;
end;

function TGuiMidiKeys.CalculateLights(Color : TColor; DoubledHeight, KeyPressed: boolean): TGuiColorRect;
var r1,g1,b1,r2,g2,b2: Integer; tmpheight: Single;
begin
  if DoubledHeight then tmpheight := FHeight3d*2 else tmpheight := FHeight3d;
  if KeyPressed then tmpheight := -tmpheight;

  r1 := (Color shr 16) and $FF;
  g1 := (Color shr 8)  and $FF;
  b1 :=  Color         and $FF;

  r2 := round(r1+tmpheight*$FF);
  g2 := round(g1+tmpheight*$FF);
  b2 := round(b1+tmpheight*$FF);

  Result.Left := max(min(r2,$FF),0) shl 16
               + max(min(g2,$FF),0) shl 8
               + max(min(b2,$FF),0);
  
  r2 := round(r1-tmpheight*$FF);
  g2 := round(g1-tmpheight*$FF);
  b2 := round(b1-tmpheight*$FF);

  Result.Right := max(min(r2,$FF),0) shl 16
                + max(min(g2,$FF),0) shl 8
                + max(min(b2,$FF),0);

  Result.Bottom := Result.Right;
  Result.Top    := Result.Left;
end;

procedure TGuiMidiKeys.SetKeyDownMode(Value: TGuiKeyDownMode);
begin
  if FKeyDownMode <> Value then
  begin
    FKeyDownMode := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetKeySeparatorColor(Value: TColor);
begin
  if FKeySeparatorColor <> Value then
  begin
    FKeySeparatorColor := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetZoneSeparatorColor(Value: TColor);
begin
  if FZoneSeparatorColor <> Value then
  begin
    FZoneSeparatorColor := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetZoneBarColor(Value: TColor);
begin
  if FZoneBarColor <> Value then
  begin
    FZoneBarColor := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetZoneBarHoverColor(Value: TColor);
begin
  if FZoneBarHoverColor <> Value then
  begin
    FZoneBarHoverColor := Value;
    if FMidiZoneBarMouseOver or (FMidiZoneBarDragging.isDragging) then RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetBlackKeyColor(Value: TColor);
begin
  if FBlackKeyColor <> Value then
  begin
    FBlackKeyColor := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetBlackKeyHoverColor(Value: TColor);
begin
  if FBlackKeyHoverColor <> Value then
  begin
    FBlackKeyHoverColor := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetBlackKeyPressedColor(Value: TColor);
begin
  if FBlackKeyPressedColor <> Value then
  begin
    FBlackKeyPressedColor := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetWhiteKeyColor(Value: TColor);
begin
  if FWhiteKeyColor <> Value then
  begin
    FWhiteKeyColor := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetWhiteKeyHoverColor(Value: TColor);
begin
  if FWhiteKeyHoverColor <> Value then
  begin
    FWhiteKeyHoverColor := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetWhiteKeyPressedColor(Value: TColor);
begin
  if FWhiteKeyPressedColor <> Value then
  begin
    FWhiteKeyPressedColor := Value;
    RedrawBuffer(True);
  end;
end;


procedure TGuiMidiKeys.SetShowKeyZones(Value: Boolean);
begin
  if FShowKeyZones <> Value then
  begin
    FShowKeyZones := Value;
    if ShowKeyZones and (FKeyZoneHeight<1) then KeyZoneHeight := 10 else
    begin
      CalculateVisibleKeys;
      RedrawBuffer(True);
    end;
  end;
end;

procedure TGuiMidiKeys.SetKeyZoneHeight(Value: Integer);
begin
  if FKeyZoneHeight <> Value then
  begin
    FKeyZoneHeight := Value;
    if not (csLoading in  ComponentState) and not ShowKeyZones and (FKeyZoneHeight>0) then FShowKeyZones := True;
    CalculateVisibleKeys;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetHeight3d(Value: Single);
begin
  if FHeight3d <> Value then
  begin
    FHeight3d := Value;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetBlackKeyHeight(Value: Single);
begin
  if FBlackKeyHeight <> Value then
  begin
    FBlackKeyHeight := Value;
    CalculateVisibleKeys;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetBaseOctave(Value: Byte);
begin
  if (Value+FNumOctaves < GUI_KB_MAXOCTAVES) and (Value<>FBaseOctave) then
  begin
    FBaseOctave := Value;
    CalculateVisibleKeys;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetNumOctaves(Value: Byte);
begin
  if (Value+FBaseOctave < GUI_KB_MAXOCTAVES) and (Value<>FNumOctaves) then
  begin
    FNumOctaves := Value;
    CalculateVisibleKeys;
    RedrawBuffer(True);
  end;
end;

procedure TGuiMidiKeys.SetIncludeLastOctave(Value: Boolean);
begin
  FIncludeLastOctave := Value;
  CalculateVisibleKeys;
  RedrawBuffer(True);
end;

procedure TGuiMidiKeys.SetKeyVelocity(FromKey, ToKey: Byte; Amount: Single);
var i, tmp: Byte;
begin
  if ToKey<FromKey then begin tmp := ToKey; ToKey := FromKey; FromKey := tmp; end; // Flip!!!

  for i := FromKey to min(ToKey, GUI_KB_HIGHESTKEY) do
    FKeys[i].Velocity := Amount;
end;

procedure TGuiMidiKeys.SetKeyColor(FromKey, ToKey: Byte;BaseColor, Over, Pressed: TColor);
var i, tmp: Byte;
begin
  if ToKey<FromKey then begin tmp := ToKey; ToKey := FromKey; FromKey := tmp; end; // Flip!!!

  for i := FromKey to min(ToKey, GUI_KB_HIGHESTKEY) do
  begin
    FKeys[i].BaseColor    := BaseColor;
    FKeys[i].PressedColor := Pressed;
    FKeys[i].OverColor    := Over;
  end;

  RedrawBuffer(True);
end;

procedure TGuiMidiKeys.RemoveKeyColor(FromKey, ToKey: Byte);
begin
  SetKeyColor(FromKey, ToKey);
end;

procedure TGuiMidiKeys.SetKeyPressed(KeyNr: Byte; KeyDown: Boolean);
begin
  if not KeyDown then ReleaseKey(KeyNr);
  if KeyNr<GUI_KB_HIGHESTKEY then include(FKeys[KeyNr].Flags, kfPressed);

  if fRedrawTimer.Interval<1 then RedrawBuffer(True) else fTimerMustRedraw := True;
end;

procedure TGuiMidiKeys.ReleaseKey(KeyNr: Byte; ReleaseMouseTriggered: boolean);
begin
  if KeyNr<GUI_KB_HIGHESTKEY then
    if ReleaseMouseTriggered or not (kfByMouse in FKeys[KeyNr].Flags) then
    begin
      exclude(FKeys[KeyNr].Flags, kfPressed);
      exclude(FKeys[KeyNr].Flags, kfByMouse);
      exclude(FKeys[KeyNr].Flags, kfMousePinned);
    end;

  if fRedrawTimer.Interval<1 then RedrawBuffer(True) else fTimerMustRedraw := True;
end;

procedure TGuiMidiKeys.ReleaseAllKeys(ReleaseMouseTriggered: boolean);
var i: Byte;
begin
  for i := 0 to GUI_KB_HIGHESTKEY do
    if ReleaseMouseTriggered or not (kfByMouse in FKeys[i].Flags) then
    begin
      exclude(FKeys[i].Flags, kfPressed);
      exclude(FKeys[i].Flags, kfByMouse);
      exclude(FKeys[i].Flags, kfMousePinned);
    end;

  if fRedrawTimer.Interval<1 then RedrawBuffer(True) else fTimerMustRedraw := True;
end;

procedure TGuiMidiKeys.SetKeyZones(Value: TGuiKeyZoneCollection);
begin
  FKeyZones.Assign(Value);
end;

function TGuiMidiKeys.ZoneKeyArea(KeyNr: Byte): TRect;
begin
  Result := FKeys[KeyNr].Area;
  if kfBlackKey in FKeys[KeyNr].Flags then exit;
  if KeyNr>0 then Result.Left := max(Result.Left, FKeys[KeyNr-1].Area.Right+1);
  if KeyNr<GUI_KB_HIGHESTKEY then Result.Right := min(Result.Right, FKeys[KeyNr+1].Area.Left-1);
end;

procedure TGuiMidiKeys.DrawKeyZones;
var i: Integer; lowbound, highbound: Integer;
begin
  with fBuffer.Canvas do
  begin
    Brush.Style := bsSolid;
    if FMidiZoneBarMouseOver or (FMidiZoneBarDragging.isDragging) then
      Brush.Color := FZoneBarHoverColor
    else
      Brush.Color := FZoneBarColor;
    FillRect(Rect(1,1,width-1,FKeyZoneHeight+1));

    if FKeyZones.Count<1 then exit;
    for i := 0 to FKeyZones.Count-1 do
      if FKeyZones.Items[i].Visible then with FKeyZones.Items[i] do
      begin
        lowbound := ZoneKeyArea(LowestZoneKey).Left;
        highbound := ZoneKeyArea(HighestZoneKey).Right;
        
        if highbound<>lowbound then
        begin
          if Selected then
          begin
            Brush.Color := SelectedBrushColor;
            Brush.Style := SelectedBrushStyle;
          end else if IsMouseOver then
          begin
            Brush.Color := HoverBrushColor;
            Brush.Style := HoverBrushStyle;
          end else begin
            Brush.Color := DefaultBrushColor;
            Brush.Style := DefaultBrushStyle;
          end;

          Pen.Style := psClear;
          Rectangle(lowbound,1,highbound+2,FKeyZoneHeight+3);

          if Selected then
          begin
            Pen.Color   := SelectedBorderColor;
            Pen.Width   := SelectedBorderWidth;
            Pen.Style   := SelectedBorderStyle;
          end else if IsMouseOver then
          begin
            Pen.Color   := HoverBorderColor;
            Pen.Width   := HoverBorderWidth;
            Pen.Style   := HoverBorderStyle;
          end else begin
            Pen.Color   := DefaultBorderColor;
            Pen.Width   := DefaultBorderWidth;
            Pen.Style   := DefaultBorderStyle;
          end;

          dec(lowbound);
          inc(highbound);
          MoveTo(lowbound,1);  LineTo(lowbound,FKeyZoneHeight+1);
          MoveTo(highbound,1); LineTo(highbound,FKeyZoneHeight+1);
        end;
      end;
  end;
end;

procedure TGuiMidiKeys.DrawSingleKey(CurKey: TGuiSingleKey);
var kcolor: TColor; LightAndShadow: TGuiColorRect;
begin
  if kfPressed in CurKey.Flags then kcolor := CurKey.PressedColor
  else if kfMouseOver in CurKey.Flags then kcolor := CurKey.OverColor
  else kcolor := CurKey.BaseColor;

  if kfBlackKey in CurKey.Flags then
  begin
    if kcolor = clNone then
    begin
      if kfPressed in CurKey.Flags then kcolor := FBlackKeyPressedColor
      else if kfMouseOver in CurKey.Flags then kcolor := FBlackKeyHoverColor
      else kcolor := FBlackKeyColor;
    end;
  end else begin
   if kcolor = clNone then
    begin
      if kfPressed in CurKey.Flags then kcolor := FWhiteKeyPressedColor
      else if kfMouseOver in CurKey.Flags then kcolor := FWhiteKeyHoverColor
      else kcolor := FWhiteKeyColor;
    end;
  end;  

  if (FKeyDownMode=kdmFlat) and (kfPressed in CurKey.Flags) then
  begin
    LightAndShadow.Top := kcolor;
    LightAndShadow.Left := kcolor;
    LightAndShadow.Bottom := kcolor;
    LightAndShadow.Right := kcolor;
  end else
    LightAndShadow := CalculateLights(kcolor, kfBlackKey in CurKey.Flags, (kfPressed in CurKey.Flags) and (FKeyDownMode=kdmDown));

  with fBuffer.Canvas do
  begin
    brush.color := kcolor;
    FillRect(CurKey.Area);
    MoveTo(CurKey.Area.Left,CurKey.Area.Top);
    Pen.Color := LightAndShadow.Top;    LineTo(CurKey.Area.Right, CurKey.Area.Top);
    Pen.Color := LightAndShadow.Right;  LineTo(CurKey.Area.Right, CurKey.Area.Bottom);
    Pen.Color := LightAndShadow.Bottom; LineTo(CurKey.Area.Left, CurKey.Area.Bottom);
    Pen.Color := LightAndShadow.Left;   LineTo(CurKey.Area.Left, CurKey.Area.Top); 
  end;
end;

procedure TGuiMidiKeys.DrawKeys;
var i: byte;
begin
  with fBuffer.Canvas do
  begin
    Brush.Style := bsSolid;
    Pen.Width   := 1;
    Pen.Style   := psSolid;

    for i := 0 to GUI_KB_HIGHESTKEY do
    begin
      if (kfVisible in FKeys[i].Flags) and not (kfBlackKey in FKeys[i].Flags) then
      begin
        DrawSingleKey(FKeys[i]);
      end;
      if (i > 0) and (kfVisible in FKeys[i - 1].Flags) and
                     (kfBlackKey in FKeys[i-1].Flags)
       then DrawSingleKey(FKeys[i-1]);
    end;
  end;
end;

procedure TGuiMidiKeys.RedrawBuffer(doBufferFlip: Boolean);
begin
  if not FRedrawEnabled then exit;
  if (Width > 0) and (Height > 0) then with fBuffer.Canvas do
   begin
    Lock;
    Brush.Color := FKeySeparatorColor;
    FillRect(clientrect);

    if FShowKeyZones then
     begin
      DrawKeyZones;

      pen.Width := 1;
      pen.Style := psSolid;
      pen.Color := FZoneSeparatorColor;

      MoveTo(1,         FKeyZoneHeight + 1);
      LineTo(Width - 1 ,FKeyZoneHeight + 1);
     end;

    DrawKeys;
    pen.Width := 1;
    pen.Style := psSolid;
    pen.Color := FBorderColor;

    MoveTo(0       ,0);
    LineTo(Width-1 ,0);
    LineTo(Width-1 ,Height-1);
    LineTo(0       ,Height-1);
    LineTo(0       ,0);

    UnLock;
  end;
  if doBufferFlip then Invalidate;
end;

function TGuiMidiKeys.ScreenCoordOnKey(X, Y: Integer; KeyNr: Integer; CheckYPos: Boolean): Boolean;
begin
  Result := False;
  with FKeys[KeyNr] do if kfVisible in Flags then
  begin
    if (X <= Area.Right) And (X>=Area.Left) then
    begin
      // check for overlaying black key
      if not (kfBlackKey in Flags) and (KeyNr<GUI_KB_HIGHESTKEY) then
      begin
        if ScreenCoordOnKey(X, Y, KeyNr+1, CheckYPos) then
        begin
          Result := False;
          exit;
        end;
      end;

      // so X is on the key, and there is nothing overlaying, now check Y if required
      if CheckYPos then Result := (Y<=Area.Bottom) And (Y>=Area.Top)
      else Result := True;

    end;
  end;
end;

// returns -1 if it's not a visible key
function TGuiMidiKeys.MousePosToKey(X, Y: Integer; CheckYPos: Boolean): Integer;
var i: byte;
begin
  Result := -1;
  if (X<1) or (X>Width-2) then exit;
  
  for i := 0 to GUI_KB_HIGHESTKEY do
  begin
    if ScreenCoordOnKey(X, Y, i, CheckYPos) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

procedure TGuiMidiKeys.FireNoteOn(KeyNr: Byte; Flags: TGuiKeyFlags);
begin
  if kfPressed in FKeys[KeyNr].Flags then
  begin
    FKeys[KeyNr].Flags := FKeys[KeyNr].Flags + Flags;
    exit;
  end;

  FKeys[KeyNr].Flags := FKeys[KeyNr].Flags + Flags;
  
  if Assigned(FOnNoteOn) then FOnNoteOn(Self, KeyNr, FKeys[KeyNr].Velocity);
end;

procedure TGuiMidiKeys.FireNoteOff(KeyNr: Byte);
begin
  if not (kfPressed in FKeys[KeyNr].Flags) then exit;

  exclude(FKeys[KeyNr].Flags, kfPressed);
  exclude(FKeys[KeyNr].Flags, kfByMouse);
  exclude(FKeys[KeyNr].Flags, kfMousePinned);
  
  if Assigned(FOnNoteOff) then FOnNoteOff(Self, KeyNr);
end;

procedure TGuiMidiKeys.AllNotesOff(MouseTriggeredOnly: Boolean; ExceptKey: Integer);
var i: Integer;
begin
  for i := 0 to GUI_KB_HIGHESTKEY do
    if (i<>exceptkey) and (kfPressed in FKeys[i].Flags) then
      if (MouseTriggeredOnly and (kfByMouse in FKeys[i].Flags) and not (kfMousePinned in FKeys[i].Flags)) or not MouseTriggeredOnly then
        FireNoteOff(i);
end;

procedure TGuiMidiKeys.MouseLeaveAllKeys(exceptkey: Integer);
var i: Integer;
begin
  for i := 0 to GUI_KB_HIGHESTKEY do
    if (i<>exceptkey) and (kfMouseOver in FKeys[i].Flags) then
    begin
      KeyMouseLeave(i);
      exclude(FKeys[i].Flags, kfMouseOver);
    end;
end;

procedure TGuiMidiKeys.MouseLeave;
begin
  inherited;
  if FMidiZoneBarMouseOver then
    ZoneBarMouseLeave
  else
    MouseLeaveAllKeys;
end;

procedure TGuiMidiKeys.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var mkey: Integer;
begin
  if Enabled and (X>0) and (X<Width-1) and (y>0) and (y<Height-1) then
  begin
    FRedrawEnabled := False;

    if FShowKeyZones and (Y<=FKeyZoneHeight) then
    begin
      mkey := MousePosToKey(x,y,False);
      if mkey>=0 then
      begin
        ZoneMouseActivity(kmaDown, mkey, Button, Shift, X, Y);
        if not FMidiZoneBarDragging.isDragging then ZoneMouseActivity(kmaStartDrag, mkey, Button, Shift, X, Y);
      end;
    end else begin
      mkey := MousePosToKey(x,y);
      if mkey>=0 then
      begin
        MouseDownOnMidiKey(mkey, Button, Shift, X, Y);
        if not FMidiKeyDragging.isDragging and FAllowKeyDragging then StartKeyDragging(mkey, Button, Shift, X, Y);
      end;
    end;

    FRedrawEnabled := True;
    RedrawBuffer(True);
  end;

  inherited;
end;

procedure TGuiMidiKeys.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var mkey: Integer;
begin
  if Enabled then
  begin
    FRedrawEnabled := False;
    if FMidiZoneBarDragging.isDragging and (Button = FMidiZoneBarDragging.Button)then
      EndZoneBarDragging(Shift, X,Y);

    if FMidiKeyDragging.isDragging and (Button = FMidiKeyDragging.Button)then
      EndKeyDragging(Shift, X,Y);

    if (X>0) and (X<Width-1) and (y>0) and (y<Height-1) then
    begin
      if FShowKeyZones and (Y<=FKeyZoneHeight) then
      begin
        mkey := MousePosToKey(x,y,False);
        if mkey>=0 then ZoneMouseActivity(kmaUp, mkey, Button, Shift, X, Y);
      end else begin
        mkey := MousePosToKey(x,y);
        if mkey>=0 then MouseUpOnMidiKey(mkey, Button, Shift, X, Y);
      end;
    end;

    FRedrawEnabled := True;
    RedrawBuffer(True);
  end;
  inherited;
end;

procedure TGuiMidiKeys.MouseMove(Shift: TShiftState; X, Y: Integer);
var mkey: Integer;
begin
  if Enabled then
  begin
    FRedrawEnabled := False;

    if (FMidiZoneBarDragging.isDragging) then
    begin
      mkey := MousePosToKey(x,y, False);
      MoveZoneBarDragging(mkey, Shift, X, Y);
    end else if FMidiKeyDragging.isDragging then
    begin
      if ( ( not FShowKeyZones and (y>0) ) or ( FShowKeyZones and (y>FKeyZoneHeight) ) ) and (y<Height-1) then
        mkey := MousePosToKey(x,y)
      else
        mkey := MousePosToKey(x,y, False);

      MoveKeyDragging(mkey, Shift, X, Y);
    end else begin
      if FShowKeyZones and (Y<=FKeyZoneHeight) then
      begin
        mkey := MousePosToKey(x,y,False);
        
        if not FMidiZoneBarMouseOver then
        begin
          if mkey>-1 then ZoneBarMouseEnter(mkey, Shift, X, Y);
          MouseLeaveAllKeys;
        end;

        ZoneMouseActivity(kmaMove, mkey, mbMiddle, Shift, X, Y);
      end else begin
        if FMidiZoneBarMouseOver then ZoneBarMouseLeave;

        mkey := MousePosToKey(x,y);
        MouseLeaveAllKeys(mkey);

        if (mkey>-1) and not (kfMouseOver in FKeys[mkey].Flags) then
        begin
          include(FKeys[mkey].Flags, kfMouseOver);
          KeyMouseEnter(mkey, Shift, X, Y);
        end;
      end;
    end;

    FRedrawEnabled := True;
    fTimerMustRedraw := True;
  end;
  
  inherited;
end;




{ midi key mouse tracking
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.KeyMouseEnter(KeyNr: Byte; Shift: TShiftState; X, Y: Integer);
begin
  Cursor := CursorKeys;
  if Assigned(FOnKeyMouseEnter) then FOnKeyMouseEnter(Self, KeyNr, Shift, X, Y);
end;

procedure TGuiMidiKeys.KeyMouseLeave(KeyNr: Byte);
begin
  if Assigned(FOnKeyMouseLeave) then FOnKeyMouseLeave(Self, KeyNr);
end;


{ midi key mouse up/down
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.MouseUpOnMidiKey(KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if Assigned(FOnMouseUpOnMidiKey) then FOnMouseUpOnMidiKey(Self,KeyNr, Button, Shift, X,Y);
end;

procedure TGuiMidiKeys.MouseDownOnMidiKey(KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if (Button = mbRight) then
  begin
    if (kfPressed in FKeys[KeyNr].Flags) and (kfMousePinned in FKeys[KeyNr].Flags) then
      FireNoteOff(KeyNr)
    else
      FireNoteOn(KeyNr, [kfPressed, kfByMouse, kfMousePinned]);
  end else FireNoteOn(KeyNr, [kfPressed, kfByMouse]);

  if Assigned(FOnMouseDownOnMidiKey) then FOnMouseDownOnMidiKey(Self,KeyNr, Button, Shift, X,Y);
end;


{ midi key mouse dragging
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.StartKeyDragging(KeyNr: Integer; DragButton: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  MouseLeaveAllKeys;

  FMidiKeyDragging.Button := DragButton;
  FMidiKeyDragging.isDragging := True;
  FMidiKeyDragging.LastKey := KeyNr;
  FMidiKeyDragging.StartKey := KeyNr;

  if Assigned(FOnStartKeyDragging) then FOnStartKeyDragging(Self, KeyNr, FMidiKeyDragging, Shift, X, Y);
end;

procedure TGuiMidiKeys.MoveKeyDragging(KeyNr: Integer; Shift: TShiftState; X,Y: Integer);
begin
  if (FMidiKeyDragging.Button <> mbRight) then
  begin
    AllNotesOff(True, KeyNr);
    if KeyNr>=0 then FireNoteOn(KeyNr, [kfPressed, kfByMouse]);
  end;

  if Assigned(FOnMoveKeyDragging) then FOnMoveKeyDragging(Self, KeyNr, FMidiKeyDragging, Shift, X, Y);
  FMidiKeyDragging.LastKey := KeyNr;
end;

procedure TGuiMidiKeys.EndKeyDragging(Shift: TShiftState; X,Y: Integer);
begin
  AllNotesOff;
  if Assigned(FOnEndKeyDragging) then FOnEndKeyDragging(Self, FMidiKeyDragging, Shift, X,Y);
  FMidiKeyDragging.isDragging := False;
end;



{ zone bar mouse tracking
--------------------------------------------------------------------------------}
function TGuiMidiKeys.GetZoneMouseOverType(X: Integer; KeyNr: Integer; Zone: TGuiKeyZoneItem): TGuiZoneMousePosType;
var ZKeyArea: TRect; temp: Integer;
begin
  Result := [mptOutside];
  if KeyNr < 0 then KeyNr := MousePosToKey(X,0,False)
  else if not ScreenCoordOnKey(X,0,KeyNr,False) then exit;

  if KeyNr < 0 then exit;
  if Zone = nil then Zone := KeyZones.ZoneByKey(KeyNr);
  if Zone = nil then exit;

  Result := [mptInZone];

  
  ZKeyArea := ZoneKeyArea(KeyNr);
  temp := ZKeyArea.Right - ZKeyArea.Left;
  if KeyNr = Zone.LowestZoneKey then
  begin
    include(Result,mptOnLowestKey);
    if X<ZKeyArea.Left+temp/4 then include(Result,mptOnLowestBorder);
  end;
  if KeyNr = Zone.HighestZoneKey then
  begin
    include(Result,mptOnHighestKey);
    if X>ZKeyArea.Right-temp/4 then include(Result,mptOnHighestBorder);
  end;
end;

procedure TGuiMidiKeys.ZoneMouseActivity(Action: TGuiKbMouseAction; KeyNr: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Zone: TGuiKeyZoneItem; tmpMouseOverType: TGuiZoneMousePosType;
begin
  if KeyNr<0 then exit;
  Zone := KeyZones.ZoneByKey(KeyNr);
  if Zone = nil then
  begin
    MouseLeaveAllZones;

    case Action of
      kmaDown:
            begin
              ZoneSelectionChanged;
              MouseDownOnKeyZoneBar(KeyNr, nil, [mptOutside], Button, Shift, X, Y);
            end;

      kmaUp:  MouseUpOnKeyZoneBar(KeyNr, nil, [mptOutside], Button, Shift, X, Y);

      kmaStartDrag: StartZoneBarDragging(KeyNr, nil, [mptOutside], Button, Shift, X, Y);
    end;
    exit;
  end;

  tmpMouseOverType := GetZoneMouseOverType(X, KeyNr, Zone);
  case Action of
    kmaMove:
          begin
            // new hover
            if not Zone.IsMouseOver then
            begin
              MouseLeaveAllZones(Zone);
              ZoneMouseEnter(Zone, Shift, X, Y);
              ZoneMouseOverChanged(KeyNr, Zone, tmpMouseOverType, Shift, X, Y);
            end else if FZoneMouseOverType <> tmpMouseOverType then
              ZoneMouseOverChanged(KeyNr, Zone, tmpMouseOverType, Shift, X, Y);
          end;

    kmaDown:
          begin
            // new select
            if not Zone.Selected then ZoneSelectionChanged(Zone);
            MouseDownOnKeyZoneBar(KeyNr, Zone, tmpMouseOverType, Button, Shift, X, Y);
          end;

    kmaUp:  MouseUpOnKeyZoneBar(KeyNr, Zone, tmpMouseOverType, Button, Shift, X, Y);

    kmaStartDrag: StartZoneBarDragging(KeyNr, Zone, tmpMouseOverType, Button, Shift, X, Y);
  end;
end; 



{ midi zone mouse selection
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.UnSelectAllZones(ExceptZone: TGuiKeyZoneItem);
var i: Integer;
begin
  for i := KeyZones.Count-1 downto 0 do
    if (ExceptZone<>KeyZones.Items[i]) and KeyZones.Items[i].Selected then KeyZones.Items[i].Unselect(False);
end;

procedure TGuiMidiKeys.ZoneSelectionChanged(Zone: TGuiKeyZoneItem);
begin
  if Zone<>nil then Zone.Select(False);
  UnSelectAllZones(Zone);
  if Assigned(FOnZoneSelectionChanged) then FOnZoneSelectionChanged(Self, Zone);
end;

{ zone bar mouse enter/leave
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.MouseLeaveAllZones(ExceptZone: TGuiKeyZoneItem);
var i: Integer;
begin
  for i := KeyZones.Count-1 downto 0 do
    if (ExceptZone<>KeyZones.Items[i]) and KeyZones.Items[i].IsMouseOver then ZoneMouseLeave(KeyZones.Items[i]);
end;

procedure TGuiMidiKeys.ZoneMouseEnter(Zone: TGuiKeyZoneItem; Shift: TShiftState; X, Y: Integer);
begin
  Cursor := CursorZone;
  Zone.SetMouseOver(True, False);
  if Assigned(FOnZoneMouseEnter) then FOnZoneMouseEnter(Self, Zone, Shift, X, Y);
end;

procedure TGuiMidiKeys.ZoneMouseLeave(Zone: TGuiKeyZoneItem);
begin
  FZoneMouseOverType := [];
  Cursor := CursorZoneBar;
  Zone.SetMouseOver(False, False);
  if Assigned(FOnZoneMouseLeave) then FOnZoneMouseLeave(Self, Zone);
end;

procedure TGuiMidiKeys.ZoneMouseOverChanged(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Shift: TShiftState; X, Y: Integer);
begin
  FZoneMouseOverType := MouseOverType;
  if (mptOnLowestBorder in MouseOverType) or (mptOnHighestBorder in MouseOverType) then Cursor := CursorZoneBorder
  else if mptInZone in MouseOverType then Cursor := CursorZone
  else Cursor := CursorZoneBar;
  if Assigned(FOnZoneMouseOverChanged) then FOnZoneMouseOverChanged(Self, KeyNr, Zone, MouseOverType, Shift, X, Y);
end;


procedure TGuiMidiKeys.ZoneBarMouseEnter(KeyNr: Byte; Shift: TShiftState; X, Y: Integer);
begin
  Cursor := CursorZoneBar;
  FMidiZoneBarMouseOver := True;
  if Assigned(FOnZoneBarMouseEnter) then FOnZoneBarMouseEnter(Self, KeyNr, Shift, X, Y);
end;

procedure TGuiMidiKeys.ZoneBarMouseLeave;
begin
  Cursor := CursorKeys;
  FMidiZoneBarMouseOver := False;
  MouseLeaveAllZones;
  if Assigned(FOnZoneBarMouseLeave) then FOnZoneBarMouseLeave(Self);
end;

{ zone bar mouse up/down
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.MouseUpOnKeyZoneBar(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
 if Assigned(FOnMouseUpOnKeyZoneBar)
  then FOnMouseUpOnKeyZoneBar(Self, KeyNr, Zone, MouseOverType, Button, Shift, X,Y);
end;

procedure TGuiMidiKeys.MouseDownOnKeyZoneBar(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
 if Assigned(FOnMouseDownOnKeyZoneBar)
  then FOnMouseDownOnKeyZoneBar(Self, KeyNr, Zone, MouseOverType, Button, Shift, X,Y);
end;

{ zone bar dragging
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.StartZoneBarDragging(KeyNr: Integer; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; DragButton: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  with FMidiZoneBarDragging do
   begin
    Button     := DragButton;
    isDragging := True;
    Zone       := Zone;
    InZonePos  := MouseOverType;
    LastKey    := KeyNr;
    StartKey   := KeyNr;
    if Zone <> nil then
     begin
      StartLowestZoneKey := Zone.LowestZoneKey;
      StartHighestZoneKey := Zone.HighestZoneKey;
     end;
   end;

  if Assigned(FOnStartZoneBarDragging)
   then FOnStartZoneBarDragging(Self, KeyNr, FMidiZoneBarDragging, Shift, X, Y);
end;

procedure TGuiMidiKeys.MoveZoneBarDragging(KeyNr: Integer; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMoveZoneBarDragging) then FOnMoveZoneBarDragging(Self, KeyNr, FMidiZoneBarDragging, Shift, X, Y);
  FMidiZoneBarDragging.LastKey := KeyNr;
end;

procedure TGuiMidiKeys.EndZoneBarDragging(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnEndZoneBarDragging) then FOnEndZoneBarDragging(Self, FMidiZoneBarDragging, Shift, X, Y);
  FMidiZoneBarDragging.isDragging := False;
end;

end.
