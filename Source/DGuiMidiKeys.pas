unit DGuiMidiKeys;

{$I ASIOVST.INC}

interface

uses {$IFDEF FPC} LCLIntf, LResources, LMessages,
     {$ELSE} Windows, {$ENDIF}
     Classes, Graphics, Forms, Controls, ExtCtrls, Messages, DGuiBaseControl, DGuiMidiKeyZones;

const
  GUI_KB_MAXOCTAVES = 11;
  GUI_KB_HIGHESTKEY = GUI_KB_MAXOCTAVES*12+1;  // Octaves + Highest C
  
type
  TGuiZoneMousePosType = set of (mptOutside, mptInZone, mptOnLowestKey, mptOnHighestKey, mptOnLowestBorder, mptOnHighestBorder);
  TGuiKbMouseAction = (kmaMove, kmaDown, kmaUp, kmaStartDrag);
  
  TGuiKbMouseDragInfo = record
    Button: TMouseButton;
    isDragging: Boolean;       
    StartKey: Integer; // -1 = Out of visible range
    LastKey: Integer; // -1 = Out of visible range
  end;

  TGuiZbMouseDragInfo = record
    Button: TMouseButton;
    isDragging: Boolean;
    Zone: TGuiKeyZoneItem;
    InZonePos: TGuiZoneMousePosType;
    StartKey: Integer; // -1 = Out of visible range
    LastKey: Integer; // -1 = Out of visible range
    StartLowestZoneKey: Integer;
    StartHighestZoneKey: Integer;
  end;

  TGuiKeyFlags = set of (kfBlackKey, kfVisible, kfPressed, kfMouseOver, kfByMouse, kfMousePinned);
  TGuiKeyDownMode = (kdmUp, kdmFlat, kdmDown);
  TGuiColorRect = record
    Top, Left, Right, Bottom: TColor;
  end;

  TGuiSingleKey = record
    KeyNr: Byte;
    Flags: TGuiKeyFlags;
    Area: TRect;
    BaseColor: TColor;
    PressedColor: TColor;
    OverColor: TColor;
    Velocity: single;
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
    RedrawEnabled:         Boolean;
    fKeys:                 TGuiKeyArray;
    fBaseOctave:           Byte;
    fNumOctaves:           Byte;
    fIncludeLastOctave:    Boolean;
    fShowKeyZones:         Boolean;
    fAllowKeyDragging:     Boolean;

    fKeyZoneHeight:        Integer;
    fKeyZones:             TGuiKeyZoneCollection;
    fBlackKeyHeight:       Single;
    fHeight3d:             Single;
    fBorderColor:          TColor;
    fKeySeparatorColor:    TColor;
    fZoneSeparatorColor:   TColor;
    fZoneBarColor:         TColor;
    fZoneBarHoverColor:    TColor;
    fBlackKeyColor:        TColor;
    fBlackKeyHoverColor:   TColor;
    fBlackKeyPressedColor: TColor;
    fWhiteKeyColor:        TColor;
    fWhiteKeyHoverColor:   TColor;
    fWhiteKeyPressedColor: TColor;
    fKeyDownMode:          TGuiKeyDownMode;

    fCursorKeys:           TCursor;
    fCursorZoneBar:        TCursor;
    fCursorZone:           TCursor;
    fCursorZoneBorder:     TCursor;

    fZoneMouseOverType:    TGuiZoneMousePosType;

    fOnMouseUpOnMidiKey: TGuiOnMouseUpOnMidiKey;
    fOnMouseUpOnKeyZoneBar: TGuiOnMouseUpOnKeyZoneBar;
    fOnMouseDownOnMidiKey: TGuiOnMouseDownOnMidiKey;
    fOnMouseDownOnKeyZoneBar: TGuiOnMouseDownOnKeyZoneBar;

    fOnZoneBarMouseEnter: TGuiOnZoneBarMouseEnter;
    fOnZoneBarMouseLeave: TGuiOnZoneBarMouseLeave;
    fOnZoneMouseEnter: TGuiOnZoneMouseEnter;
    fOnZoneMouseLeave: TGuiOnZoneMouseLeave;
    fOnZoneMouseOverChanged: TGuiOnZoneMouseOverChanged;
    fOnZoneSelectionChanged: TGuiOnZoneSelectionChanged;

    fOnKeyMouseEnter: TGuiOnKeyMouseEnter;
    fOnKeyMouseLeave: TGuiOnKeyMouseLeave;

    fOnStartZoneBarDragging: TGuiOnStartZoneBarDragging;
    fOnMoveZoneBarDragging: TGuiOnMoveZoneBarDragging;
    fOnEndZoneBarDragging: TGuiOnEndZoneBarDragging;

    fOnStartKeyDragging: TGuiOnStartKeyDragging;
    fOnMoveKeyDragging: TGuiOnMoveKeyDragging;
    fOnEndKeyDragging: TGuiOnEndKeyDragging;

    fOnNoteOn: TGuiOnNoteOn;
    fOnNoteOff: TGuiOnNoteOff;

    fMidiKeyDragging:      TGuiKbMouseDragInfo;
    fMidiZoneBarDragging:  TGuiZbMouseDragInfo;
    fMidiZoneBarMouseOver:    Boolean;

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

    function CalculateLights(Color : TColor; DoubledHeight: boolean = false; KeyPressed: boolean = false): TGuiColorRect; virtual;
    procedure CalculateVisibleKeys; virtual;
    procedure InitKeys; virtual;
    procedure DrawKeyZones; virtual;
    procedure DrawSingleKey(CurKey: TGuiSingleKey); virtual;
    procedure DrawKeys; virtual;
    procedure MouseLeaveAllKeys(exceptkey: integer = -1); virtual;
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
    procedure   SetKeyVelocity(FromKey, ToKey: Byte; Amount: single);
    procedure   SetKeyPressed(KeyNr: Byte; KeyDown: Boolean = true);
    procedure   ReleaseKey(KeyNr: Byte; ReleaseMouseTriggered: boolean = false);
    procedure   ReleaseAllKeys(ReleaseMouseTriggered: boolean = false);
    procedure   RemoveKeyColor(FromKey, ToKey: Byte);
    procedure   AllNotesOff(MouseTriggeredOnly: Boolean = true; ExceptKey: Integer = -1);
    procedure   RedrawBuffer(doBufferFlip: Boolean); override;
    function    MousePosToKey(X, Y: Integer; CheckYPos: Boolean = true): Integer; dynamic;
    function    GetZoneMouseOverType(X: Integer; KeyNr: Integer = -1; Zone: TGuiKeyZoneItem = nil): TGuiZoneMousePosType;
    function    ScreenCoordOnKey(X, Y: Integer; KeyNr: Integer; CheckYPos: Boolean = true): Boolean; dynamic;
    property    Keys: TGuiKeyArray read fKeys;
  published
    property ReleaseMouseBtnOnLeave;
    
    property KeyDownMode: TGuiKeyDownMode read fKeyDownMode write SetKeyDownMode;
    property BorderColor: TColor read fBorderColor write SetBorderColor default clBlack;
    property KeySeparatorColor: TColor read fKeySeparatorColor write SetKeySeparatorColor default clBlack;
    property ZoneSeparatorColor: TColor read fZoneSeparatorColor write SetZoneSeparatorColor default clBlack;
    property ZoneBarColor: TColor read fZoneBarColor write SetZoneBarColor default clWhite;
    property ZoneBarHoverColor: TColor read fZoneBarHoverColor write SetZoneBarHoverColor default $00CCEEFF;
    property BlackKeyColor: TColor read fBlackKeyColor write SetBlackKeyColor default $00333333;
    property BlackKeyHoverColor: TColor read fBlackKeyHoverColor write SetBlackKeyHoverColor default $00666666;
    property BlackKeyPressedColor: TColor read fBlackKeyPressedColor write SetBlackKeyPressedColor default clSilver;
    property WhiteKeyColor: TColor read fWhiteKeyColor write SetWhiteKeyColor default $00F8F8F8;
    property WhiteKeyHoverColor: TColor read fWhiteKeyHoverColor write SetWhiteKeyHoverColor default $00DDDDDD;
    property WhiteKeyPressedColor: TColor read fWhiteKeyPressedColor write SetWhiteKeyPressedColor default clSilver;


    property CursorKeys: TCursor read fCursorKeys write fCursorKeys default crHandPoint;
    property CursorZoneBar: TCursor read fCursorZoneBar write fCursorZoneBar default crDefault;
    property CursorZone: TCursor read fCursorZone write fCursorZone default crSize;
    property CursorZoneBorder: TCursor read fCursorZoneBorder write fCursorZoneBorder default crSizeWE;

    property Height3d: Single read fHeight3d write SetHeight3d;
    property BlackKeyHeight: Single read fBlackKeyHeight write SetBlackKeyHeight;
    property BaseOctave: Byte read fBaseOctave write SetBaseOctave default 3;
    property NumOctaves: Byte read fNumOctaves write SetNumOctaves default 3;
    property IncludeLastOctave: Boolean read fIncludeLastOctave write SetIncludeLastOctave default false;
    property KeyZones: TGuiKeyZoneCollection read fKeyZones write SetKeyZones;

    property ShowKeyZones: Boolean read fShowKeyZones write SetShowKeyZones default True;
    property KeyZoneHeight: Integer read fKeyZoneHeight write SetKeyZoneHeight default 10;
    property AllowKeyDragging: Boolean read fAllowKeyDragging write fAllowKeyDragging default true;

    property OnMouseUpOnMidiKey: TGuiOnMouseUpOnMidiKey read fOnMouseUpOnMidiKey write fOnMouseUpOnMidiKey;
    property OnMouseUpOnKeyZoneBar: TGuiOnMouseUpOnKeyZoneBar read fOnMouseUpOnKeyZoneBar write fOnMouseUpOnKeyZoneBar;
    property OnMouseDownOnMidiKey: TGuiOnMouseDownOnMidiKey read fOnMouseDownOnMidiKey write fOnMouseDownOnMidiKey;
    property OnMouseDownKeyZoneBar: TGuiOnMouseDownOnKeyZoneBar read fOnMouseDownOnKeyZoneBar write fOnMouseDownOnKeyZoneBar;

    property OnZoneBarMouseEnter: TGuiOnZoneBarMouseEnter read fOnZoneBarMouseEnter write fOnZoneBarMouseEnter;
    property OnZoneBarMouseLeave: TGuiOnZoneBarMouseLeave read fOnZoneBarMouseLeave write fOnZoneBarMouseLeave;
    property OnZoneMouseEnter: TGuiOnZoneMouseEnter read fOnZoneMouseEnter write fOnZoneMouseEnter;
    property OnZoneMouseLeave: TGuiOnZoneMouseLeave read fOnZoneMouseLeave write fOnZoneMouseLeave;
    property OnZoneMouseOverChanged: TGuiOnZoneMouseOverChanged read fOnZoneMouseOverChanged write fOnZoneMouseOverChanged;
    property OnZoneSelectionChanged: TGuiOnZoneSelectionChanged read fOnZoneSelectionChanged write fOnZoneSelectionChanged;

    property OnKeyMouseEnter: TGuiOnKeyMouseEnter read fOnKeyMouseEnter write fOnKeyMouseEnter;
    property OnKeyMouseLeave: TGuiOnKeyMouseLeave read fOnKeyMouseLeave write fOnKeyMouseLeave;

    property OnStartZoneBarDragging: TGuiOnStartZoneBarDragging read fOnStartZoneBarDragging write fOnStartZoneBarDragging;
    property OnMoveZoneBarDragging: TGuiOnMoveZoneBarDragging read fOnMoveZoneBarDragging write fOnMoveZoneBarDragging;
    property OnEndZoneBarDragging: TGuiOnEndZoneBarDragging read fOnEndZoneBarDragging write fOnEndZoneBarDragging;

    property OnStartKeyDragging: TGuiOnStartKeyDragging read fOnStartKeyDragging write fOnStartKeyDragging;
    property OnMoveKeyDragging: TGuiOnMoveKeyDragging read fOnMoveKeyDragging write fOnMoveKeyDragging;
    property OnEndKeyDragging: TGuiOnEndKeyDragging read fOnEndKeyDragging write fOnEndKeyDragging;

    property OnNoteOn: TGuiOnNoteOn read fOnNoteOn write fOnNoteOn;
    property OnNoteOff: TGuiOnNoteOff read fOnNoteOff write fOnNoteOff;
  end;

implementation

uses Math;

constructor TGuiMidiKeys.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeyZones := TGuiKeyZoneCollection.Create(self);
   
  fShowKeyZones         := True;
  fKeyZoneHeight        := 10;

  fAllowKeyDragging        := true;

  fBorderColor          := clBlack;
  ZoneBarColor          := clWhite;
  ZoneBarHoverColor     := $00CCEEFF;
  fKeySeparatorColor    := clBlack;
  fBlackKeyColor        := $00333333;
  fBlackKeyHoverColor   := $00666666;
  fBlackKeyPressedColor := clSilver;
  fWhiteKeyColor        := $00F8F8F8;
  fWhiteKeyHoverColor   := $00DDDDDD;
  fWhiteKeyPressedColor := clSilver;
  fKeyDownMode          := kdmFlat;
  fZoneMouseOverType    := [];
  RedrawEnabled         := true;

  
  fCursorKeys           := crHandPoint;
  fCursorZoneBar        := crDefault;
  fCursorZone           := crSize;
  fCursorZoneBorder     := crSizeWE;

  fMidiKeyDragging.isDragging     := false;
  fMidiKeyDragging.LastKey        := -1;
  fMidiKeyDragging.StartKey       := -1;
  fMidiZoneBarDragging.isDragging := false;
  fMidiZoneBarDragging.LastKey    := -1;
  fMidiZoneBarDragging.StartKey   := -1;
  fMidiZoneBarDragging.Zone       := nil;
  fMidiZoneBarDragging.InZonePos  := [mptOutside];
  
  fMidiZoneBarMouseOver    := false;
  
  fBaseOctave           := 3;
  fNumOctaves           := 3;
  fIncludeLastOctave    := False;
  fHeight3d             := 0.2;
  fBlackKeyHeight       := 0.63;

  InitKeys;
  RedrawInterval := 50;
end;

destructor TGuiMidiKeys.Destroy;
begin
  fKeyZones.Free;
  inherited;
end;

procedure TGuiMidiKeys.InitKeys;
var i: Byte;
begin
  for i:=0 to GUI_KB_HIGHESTKEY do with fKeys[i] do
  begin
    KeyNr          := i;
    Flags          := [];
    Area           := Rect(-1,-1,-1,-1);
    PressedColor   := clNone;
    BaseColor      := clNone;
    OverColor      := clNone;
    Velocity       := 1;

    if (i mod 12) in [1,3,6,8,10] then
      include(Flags, kfBlackKey);
  end;
end;

procedure TGuiMidiKeys.ResizeBuffer;
begin
  CalculateVisibleKeys;
  inherited;
end;

procedure TGuiMidiKeys.CalculateVisibleKeys;
var i, cnt, KeyCount: integer;
    KeyWidth: single;
    LastRightBorder: integer;
    FirstKey, LastKey: integer;
    TopMargin, BottomMargin: integer;
begin
  FirstKey := fBaseOctave*12;
  LastKey  := (fBaseOctave+fNumOctaves)*12+integer(fIncludeLastOctave)-1;

  for i:=0 to GUI_KB_HIGHESTKEY do with fKeys[i] do
  begin
    if i>LastKey then Area := Rect(width-1,0,width-1,0) else Area := Rect(0,0,0,0);
    exclude(Flags, kfVisible);
  end;

  if fShowKeyZones then TopMargin := fKeyZoneHeight+2 else TopMargin := 1;
  BottomMargin := height - 1;
  

  // set white keys  
  // -------------------------------------------
  if fIncludeLastOctave then KeyCount:=1 else KeyCount:=0;
  KeyWidth := (Width-1) / (KeyCount+fNumOctaves*7);

  cnt := 0;
  LastRightBorder:=-1;
  for i:=FirstKey to LastKey do
  begin
    if (i mod 12) in [0,2,4,5,7,9,11] then
    begin
      fKeys[i].Area := Rect(LastRightBorder+2,TopMargin,round((cnt+1)*KeyWidth)-1,BottomMargin);
      LastRightBorder := fKeys[i].Area.Right;
      include(fKeys[i].Flags, kfVisible);
      inc(cnt);
    end;
  end;

  // set black keys
  // -------------------------------------------
  if not fIncludeLastOctave then KeyWidth:=0;
  KeyWidth := (Width-2-KeyWidth) / (fNumOctaves*12);

  BottomMargin:=Round((height-TopMargin) * fBlackKeyHeight + TopMargin);

  cnt := 0;
  for i:=FirstKey to LastKey do
  begin
    if (i mod 12) in [1,3,6,8,10] then
    begin
      fKeys[i].Area := Rect(
        round(cnt*KeyWidth+2),TopMargin,
        round((cnt+1)*KeyWidth+2),BottomMargin);
      include(fKeys[i].Flags, kfVisible);
    end;
    inc(cnt);
  end;
end;

function TGuiMidiKeys.CalculateLights(Color : TColor; DoubledHeight, KeyPressed: boolean): TGuiColorRect;
var r1,g1,b1,r2,g2,b2: Integer; tmpheight: single;
begin
  if DoubledHeight then tmpheight:=fHeight3d*2 else tmpheight:=fHeight3d;
  if KeyPressed then tmpheight:=-tmpheight;

  r1:=(Color shr 16) and $FF;
  g1:=(Color shr 8)  and $FF;
  b1:= Color         and $FF;

  r2:=round(r1+tmpheight*$FF);
  g2:=round(g1+tmpheight*$FF);
  b2:=round(b1+tmpheight*$FF);

  Result.Left := max(min(r2,$FF),0) shl 16
               + max(min(g2,$FF),0) shl 8
               + max(min(b2,$FF),0);
  
  r2:=round(r1-tmpheight*$FF);
  g2:=round(g1-tmpheight*$FF);
  b2:=round(b1-tmpheight*$FF);

  Result.Right := max(min(r2,$FF),0) shl 16
                + max(min(g2,$FF),0) shl 8
                + max(min(b2,$FF),0);

  Result.Bottom := Result.Right;
  Result.Top    := Result.Left;
end;

procedure TGuiMidiKeys.SetKeyDownMode(Value: TGuiKeyDownMode);
begin
  if fKeyDownMode <> Value then
  begin
    fKeyDownMode := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetBorderColor(Value: TColor);
begin
  if fBorderColor <> Value then
  begin
    fBorderColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetKeySeparatorColor(Value: TColor);
begin
  if fKeySeparatorColor <> Value then
  begin
    fKeySeparatorColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetZoneSeparatorColor(Value: TColor);
begin
  if fZoneSeparatorColor <> Value then
  begin
    fZoneSeparatorColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetZoneBarColor(Value: TColor);
begin
  if fZoneBarColor <> Value then
  begin
    fZoneBarColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetZoneBarHoverColor(Value: TColor);
begin
  if fZoneBarHoverColor <> Value then
  begin
    fZoneBarHoverColor := Value;
    if fMidiZoneBarMouseOver or (fMidiZoneBarDragging.isDragging) then RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetBlackKeyColor(Value: TColor);
begin
  if fBlackKeyColor <> Value then
  begin
    fBlackKeyColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetBlackKeyHoverColor(Value: TColor);
begin
  if fBlackKeyHoverColor <> Value then
  begin
    fBlackKeyHoverColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetBlackKeyPressedColor(Value: TColor);
begin
  if fBlackKeyPressedColor <> Value then
  begin
    fBlackKeyPressedColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetWhiteKeyColor(Value: TColor);
begin
  if fWhiteKeyColor <> Value then
  begin
    fWhiteKeyColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetWhiteKeyHoverColor(Value: TColor);
begin
  if fWhiteKeyHoverColor <> Value then
  begin
    fWhiteKeyHoverColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetWhiteKeyPressedColor(Value: TColor);
begin
  if fWhiteKeyPressedColor <> Value then
  begin
    fWhiteKeyPressedColor := Value;
    RedrawBuffer(true);
  end;
end;


procedure TGuiMidiKeys.SetShowKeyZones(Value: Boolean);
begin
  if fShowKeyZones <> Value then
  begin
    fShowKeyZones := Value;
    if ShowKeyZones and (fKeyZoneHeight<1) then KeyZoneHeight := 10 else
    begin
      CalculateVisibleKeys;
      RedrawBuffer(true);
    end;
  end;
end;

procedure TGuiMidiKeys.SetKeyZoneHeight(Value: Integer);
begin
  if fKeyZoneHeight <> Value then
  begin
    fKeyZoneHeight := Value;
    if not (csLoading in  ComponentState) and not ShowKeyZones and (fKeyZoneHeight>0) then fShowKeyZones := true;
    CalculateVisibleKeys;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetHeight3d(Value: Single);
begin
  if fHeight3d <> Value then
  begin
    fHeight3d := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetBlackKeyHeight(Value: Single);
begin
  if fBlackKeyHeight <> Value then
  begin
    fBlackKeyHeight := Value;
    CalculateVisibleKeys;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetBaseOctave(Value: Byte);
begin
  if (Value+fNumOctaves < GUI_KB_MAXOCTAVES) and (Value<>fBaseOctave) then
  begin
    fBaseOctave := Value;
    CalculateVisibleKeys;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetNumOctaves(Value: Byte);
begin
  if (Value+fBaseOctave < GUI_KB_MAXOCTAVES) and (Value<>fNumOctaves) then
  begin
    fNumOctaves := Value;
    CalculateVisibleKeys;
    RedrawBuffer(true);
  end;
end;

procedure TGuiMidiKeys.SetIncludeLastOctave(Value: Boolean);
begin
  fIncludeLastOctave := Value;
  CalculateVisibleKeys;
  RedrawBuffer(true);
end;

procedure TGuiMidiKeys.SetKeyVelocity(FromKey, ToKey: Byte; Amount: single);
var i, tmp: Byte;
begin
  if ToKey<FromKey then begin tmp:=ToKey; ToKey:=FromKey; FromKey:=tmp; end; // Flip!!!

  for i:=FromKey to min(ToKey, GUI_KB_HIGHESTKEY) do
    fKeys[i].Velocity := Amount;
end;

procedure TGuiMidiKeys.SetKeyColor(FromKey, ToKey: Byte;BaseColor, Over, Pressed: TColor);
var i, tmp: Byte;
begin
  if ToKey<FromKey then begin tmp:=ToKey; ToKey:=FromKey; FromKey:=tmp; end; // Flip!!!

  for i:=FromKey to min(ToKey, GUI_KB_HIGHESTKEY) do
  begin
    fKeys[i].BaseColor    := BaseColor;
    fKeys[i].PressedColor := Pressed;
    fKeys[i].OverColor    := Over;
  end;

  RedrawBuffer(true);
end;

procedure TGuiMidiKeys.RemoveKeyColor(FromKey, ToKey: Byte);
begin
  SetKeyColor(FromKey, ToKey);
end;

procedure TGuiMidiKeys.SetKeyPressed(KeyNr: Byte; KeyDown: Boolean);
begin
  if not KeyDown then ReleaseKey(KeyNr);
  if KeyNr<GUI_KB_HIGHESTKEY then include(fKeys[KeyNr].Flags, kfPressed);

  if fRedrawTimer.Interval<1 then RedrawBuffer(true) else fTimerMustRedraw:=true;
end;

procedure TGuiMidiKeys.ReleaseKey(KeyNr: Byte; ReleaseMouseTriggered: boolean);
begin
  if KeyNr<GUI_KB_HIGHESTKEY then
    if ReleaseMouseTriggered or not (kfByMouse in fKeys[KeyNr].Flags) then
    begin
      exclude(fKeys[KeyNr].Flags, kfPressed);
      exclude(fKeys[KeyNr].Flags, kfByMouse);
      exclude(fKeys[KeyNr].Flags, kfMousePinned);
    end;

  if fRedrawTimer.Interval<1 then RedrawBuffer(true) else fTimerMustRedraw:=true;
end;

procedure TGuiMidiKeys.ReleaseAllKeys(ReleaseMouseTriggered: boolean);
var i: Byte;
begin
  for i:=0 to GUI_KB_HIGHESTKEY do
    if ReleaseMouseTriggered or not (kfByMouse in fKeys[i].Flags) then
    begin
      exclude(fKeys[i].Flags, kfPressed);
      exclude(fKeys[i].Flags, kfByMouse);
      exclude(fKeys[i].Flags, kfMousePinned);
    end;

  if fRedrawTimer.Interval<1 then RedrawBuffer(true) else fTimerMustRedraw:=true;
end;

procedure TGuiMidiKeys.SetKeyZones(Value: TGuiKeyZoneCollection);
begin
  fKeyZones.Assign(Value);
end;

function TGuiMidiKeys.ZoneKeyArea(KeyNr: Byte): TRect;
begin
  Result:=fKeys[KeyNr].Area;
  if kfBlackKey in fKeys[KeyNr].Flags then exit;
  if KeyNr>0 then Result.Left:=max(Result.Left, fKeys[KeyNr-1].Area.Right+1);
  if KeyNr<GUI_KB_HIGHESTKEY then Result.Right:=min(Result.Right, fKeys[KeyNr+1].Area.Left-1);
end;

procedure TGuiMidiKeys.DrawKeyZones;
var i: integer; lowbound, highbound: integer;
begin
  with fBuffer.Canvas do
  begin
    Brush.Style := bsSolid;
    if fMidiZoneBarMouseOver or (fMidiZoneBarDragging.isDragging) then
      Brush.Color := fZoneBarHoverColor
    else
      Brush.Color := fZoneBarColor;
    FillRect(Rect(1,1,width-1,fKeyZoneHeight+1));

    if fKeyZones.Count<1 then exit;
    for i:=0 to fKeyZones.Count-1 do
      if fKeyZones.Items[i].Visible then with fKeyZones.Items[i] do
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
          Rectangle(lowbound,1,highbound+2,fKeyZoneHeight+3);

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
          MoveTo(lowbound,1);  LineTo(lowbound,fKeyZoneHeight+1);
          MoveTo(highbound,1); LineTo(highbound,fKeyZoneHeight+1);
        end;
      end;
  end;
end;

procedure TGuiMidiKeys.DrawSingleKey(CurKey: TGuiSingleKey);
var kcolor: TColor; LightAndShadow: TGuiColorRect;
begin
  if kfPressed in CurKey.Flags then kcolor:=CurKey.PressedColor
  else if kfMouseOver in CurKey.Flags then kcolor:=CurKey.OverColor
  else kcolor:=CurKey.BaseColor;

  if kfBlackKey in CurKey.Flags then
  begin
    if kcolor = clNone then
    begin
      if kfPressed in CurKey.Flags then kcolor:=fBlackKeyPressedColor
      else if kfMouseOver in CurKey.Flags then kcolor:=fBlackKeyHoverColor
      else kcolor:=fBlackKeyColor;
    end;
  end else begin
   if kcolor = clNone then
    begin
      if kfPressed in CurKey.Flags then kcolor:=fWhiteKeyPressedColor
      else if kfMouseOver in CurKey.Flags then kcolor:=fWhiteKeyHoverColor
      else kcolor:=fWhiteKeyColor;
    end;
  end;  

  if (fKeyDownMode=kdmFlat) and (kfPressed in CurKey.Flags) then
  begin
    LightAndShadow.Top:=kcolor;
    LightAndShadow.Left:=kcolor;
    LightAndShadow.Bottom:=kcolor;
    LightAndShadow.Right:=kcolor;
  end else
    LightAndShadow := CalculateLights(kcolor, kfBlackKey in CurKey.Flags, (kfPressed in CurKey.Flags) and (fKeyDownMode=kdmDown));

  with fBuffer.Canvas do
  begin
    brush.color := kcolor;
    FillRect(CurKey.Area);
    MoveTo(CurKey.Area.Left,CurKey.Area.Top);
    Pen.Color:=LightAndShadow.Top;    LineTo(CurKey.Area.Right, CurKey.Area.Top);
    Pen.Color:=LightAndShadow.Right;  LineTo(CurKey.Area.Right, CurKey.Area.Bottom);
    Pen.Color:=LightAndShadow.Bottom; LineTo(CurKey.Area.Left, CurKey.Area.Bottom);
    Pen.Color:=LightAndShadow.Left;   LineTo(CurKey.Area.Left, CurKey.Area.Top); 
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

    for i:=0 to GUI_KB_HIGHESTKEY do
    begin
      if (kfVisible in fKeys[i].Flags) and not (kfBlackKey in fKeys[i].Flags) then
      begin
        DrawSingleKey(fKeys[i]);
      end;
      if (i>0) and (kfVisible in fKeys[i-1].Flags) and (kfBlackKey in fKeys[i-1].Flags) then
        DrawSingleKey(fKeys[i-1]);
    end;
  end;
end;

procedure TGuiMidiKeys.RedrawBuffer(doBufferFlip: Boolean);
begin
  if not RedrawEnabled then exit;
  if (Width>0) and (Height>0) then with fBuffer.Canvas do
  begin
    Lock;
    Brush.Color := fKeySeparatorColor;
    FillRect(clientrect);

    if fShowKeyZones then
    begin
      DrawKeyZones;

      pen.Width := 1;
      pen.Style := psSolid;
      pen.Color := fZoneSeparatorColor;

      MoveTo(1,       fKeyZoneHeight+1);
      LineTo(Width-1 ,fKeyZoneHeight+1);
    end;

    DrawKeys;
    pen.Width := 1;
    pen.Style := psSolid;
    pen.Color := fBorderColor;

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
  Result:=false;
  with fKeys[KeyNr] do if kfVisible in Flags then
  begin
    if (X<=Area.Right) And (X>=Area.Left) then
    begin
      // check for overlaying black key
      if not (kfBlackKey in Flags) and (KeyNr<GUI_KB_HIGHESTKEY) then
      begin
        if ScreenCoordOnKey(X, Y, KeyNr+1, CheckYPos) then
        begin
          Result:=false;
          exit;
        end;
      end;

      // so X is on the key, and there is nothing overlaying, now check Y if required
      if CheckYPos then Result:=(Y<=Area.Bottom) And (Y>=Area.Top)
      else Result:=true;

    end;
  end;
end;

// returns -1 if it's not a visible key
function TGuiMidiKeys.MousePosToKey(X, Y: Integer; CheckYPos: Boolean): Integer;
var i: byte;
begin
  Result:=-1;
  if (X<1) or (X>Width-2) then exit;
  
  for i:=0 to GUI_KB_HIGHESTKEY do
  begin
    if ScreenCoordOnKey(X, Y, i, CheckYPos) then
    begin
      Result:=i;
      exit;
    end;
  end;
end;

procedure TGuiMidiKeys.FireNoteOn(KeyNr: Byte; Flags: TGuiKeyFlags);
begin
  if kfPressed in fKeys[KeyNr].Flags then
  begin
    fKeys[KeyNr].Flags := fKeys[KeyNr].Flags + Flags;
    exit;
  end;

  fKeys[KeyNr].Flags := fKeys[KeyNr].Flags + Flags;
  
  if Assigned(fOnNoteOn) then fOnNoteOn(self, KeyNr, fKeys[KeyNr].Velocity);
end;

procedure TGuiMidiKeys.FireNoteOff(KeyNr: Byte);
begin
  if not (kfPressed in fKeys[KeyNr].Flags) then exit;

  exclude(fKeys[KeyNr].Flags, kfPressed);
  exclude(fKeys[KeyNr].Flags, kfByMouse);
  exclude(fKeys[KeyNr].Flags, kfMousePinned);
  
  if Assigned(fOnNoteOff) then fOnNoteOff(self, KeyNr);
end;

procedure TGuiMidiKeys.AllNotesOff(MouseTriggeredOnly: Boolean; ExceptKey: Integer);
var i: integer;
begin
  for i:=0 to GUI_KB_HIGHESTKEY do
    if (i<>exceptkey) and (kfPressed in fKeys[i].Flags) then
      if (MouseTriggeredOnly and (kfByMouse in fKeys[i].Flags) and not (kfMousePinned in fKeys[i].Flags)) or not MouseTriggeredOnly then
        FireNoteOff(i);
end;

procedure TGuiMidiKeys.MouseLeaveAllKeys(exceptkey: integer);
var i: integer;
begin
  for i:=0 to GUI_KB_HIGHESTKEY do
    if (i<>exceptkey) and (kfMouseOver in fKeys[i].Flags) then
    begin
      KeyMouseLeave(i);
      exclude(fKeys[i].Flags, kfMouseOver);
    end;
end;

procedure TGuiMidiKeys.MouseLeave;
begin
  inherited;
  if fMidiZoneBarMouseOver then
    ZoneBarMouseLeave
  else
    MouseLeaveAllKeys;
end;

procedure TGuiMidiKeys.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var mkey: Integer;
begin
  if Enabled and (X>0) and (X<Width-1) and (y>0) and (y<Height-1) then
  begin
    RedrawEnabled := false;

    if fShowKeyZones and (Y<=fKeyZoneHeight) then
    begin
      mkey:=MousePosToKey(x,y,false);
      if mkey>=0 then
      begin
        ZoneMouseActivity(kmaDown, mkey, Button, Shift, X, Y);
        if not fMidiZoneBarDragging.isDragging then ZoneMouseActivity(kmaStartDrag, mkey, Button, Shift, X, Y);
      end;
    end else begin
      mkey:=MousePosToKey(x,y);
      if mkey>=0 then
      begin
        MouseDownOnMidiKey(mkey, Button, Shift, X, Y);
        if not fMidiKeyDragging.isDragging and fAllowKeyDragging then StartKeyDragging(mkey, Button, Shift, X, Y);
      end;
    end;

    RedrawEnabled := true;
    RedrawBuffer(true);
  end;

  inherited;
end;

procedure TGuiMidiKeys.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var mkey: Integer;
begin
  if Enabled then
  begin
    RedrawEnabled := false;
    if fMidiZoneBarDragging.isDragging and (Button = fMidiZoneBarDragging.Button)then
      EndZoneBarDragging(Shift, X,Y);

    if fMidiKeyDragging.isDragging and (Button = fMidiKeyDragging.Button)then
      EndKeyDragging(Shift, X,Y);

    if (X>0) and (X<Width-1) and (y>0) and (y<Height-1) then
    begin
      if fShowKeyZones and (Y<=fKeyZoneHeight) then
      begin
        mkey:=MousePosToKey(x,y,false);
        if mkey>=0 then ZoneMouseActivity(kmaUp, mkey, Button, Shift, X, Y);
      end else begin
        mkey:=MousePosToKey(x,y);
        if mkey>=0 then MouseUpOnMidiKey(mkey, Button, Shift, X, Y);
      end;
    end;

    RedrawEnabled := true;
    RedrawBuffer(true);
  end;
  inherited;
end;

procedure TGuiMidiKeys.MouseMove(Shift: TShiftState; X, Y: Integer);
var mkey: Integer;
begin
  if Enabled then
  begin
    RedrawEnabled := false;

    if (fMidiZoneBarDragging.isDragging) then
    begin
      mkey:=MousePosToKey(x,y, false);
      MoveZoneBarDragging(mkey, Shift, X, Y);
    end else if fMidiKeyDragging.isDragging then
    begin
      if ( ( not fShowKeyZones and (y>0) ) or ( fShowKeyZones and (y>fKeyZoneHeight) ) ) and (y<Height-1) then
        mkey:=MousePosToKey(x,y)
      else
        mkey:=MousePosToKey(x,y, false);

      MoveKeyDragging(mkey, Shift, X, Y);
    end else begin
      if fShowKeyZones and (Y<=fKeyZoneHeight) then
      begin
        mkey:=MousePosToKey(x,y,false);
        
        if not fMidiZoneBarMouseOver then
        begin
          if mkey>-1 then ZoneBarMouseEnter(mkey, Shift, X, Y);
          MouseLeaveAllKeys;
        end;

        ZoneMouseActivity(kmaMove, mkey, mbMiddle, Shift, X, Y);
      end else begin
        if fMidiZoneBarMouseOver then ZoneBarMouseLeave;

        mkey:=MousePosToKey(x,y);
        MouseLeaveAllKeys(mkey);

        if (mkey>-1) and not (kfMouseOver in fKeys[mkey].Flags) then
        begin
          include(fKeys[mkey].Flags, kfMouseOver);
          KeyMouseEnter(mkey, Shift, X, Y);
        end;
      end;
    end;

    RedrawEnabled := true;
    fTimerMustRedraw:=true;
  end;
  
  inherited;
end;




{ midi key mouse tracking
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.KeyMouseEnter(KeyNr: Byte; Shift: TShiftState; X, Y: Integer);
begin
  Cursor:=CursorKeys;
  if Assigned(fOnKeyMouseEnter) then fOnKeyMouseEnter(self, KeyNr, Shift, X, Y);
end;

procedure TGuiMidiKeys.KeyMouseLeave(KeyNr: Byte);
begin
  if Assigned(fOnKeyMouseLeave) then fOnKeyMouseLeave(self, KeyNr);
end;


{ midi key mouse up/down
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.MouseUpOnMidiKey(KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if Assigned(fOnMouseUpOnMidiKey) then fOnMouseUpOnMidiKey(self,KeyNr, Button, Shift, X,Y);
end;

procedure TGuiMidiKeys.MouseDownOnMidiKey(KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if (Button = mbRight) then
  begin
    if (kfPressed in fKeys[KeyNr].Flags) and (kfMousePinned in fKeys[KeyNr].Flags) then
      FireNoteOff(KeyNr)
    else
      FireNoteOn(KeyNr, [kfPressed, kfByMouse, kfMousePinned]);
  end else FireNoteOn(KeyNr, [kfPressed, kfByMouse]);

  if Assigned(fOnMouseDownOnMidiKey) then fOnMouseDownOnMidiKey(self,KeyNr, Button, Shift, X,Y);
end;


{ midi key mouse dragging
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.StartKeyDragging(KeyNr: Integer; DragButton: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  MouseLeaveAllKeys;

  fMidiKeyDragging.Button:=DragButton;
  fMidiKeyDragging.isDragging:=true;
  fMidiKeyDragging.LastKey:=KeyNr;
  fMidiKeyDragging.StartKey:=KeyNr;

  if Assigned(fOnStartKeyDragging) then fOnStartKeyDragging(self, KeyNr, fMidiKeyDragging, Shift, X, Y);
end;

procedure TGuiMidiKeys.MoveKeyDragging(KeyNr: Integer; Shift: TShiftState; X,Y: Integer);
begin
  if (fMidiKeyDragging.Button <> mbRight) then
  begin
    AllNotesOff(true, KeyNr);
    if KeyNr>=0 then FireNoteOn(KeyNr, [kfPressed, kfByMouse]);
  end;

  if Assigned(fOnMoveKeyDragging) then fOnMoveKeyDragging(self, KeyNr, fMidiKeyDragging, Shift, X, Y);
  fMidiKeyDragging.LastKey:=KeyNr;
end;

procedure TGuiMidiKeys.EndKeyDragging(Shift: TShiftState; X,Y: Integer);
begin
  AllNotesOff;
  if Assigned(fOnEndKeyDragging) then fOnEndKeyDragging(self, fMidiKeyDragging, Shift, X,Y);
  fMidiKeyDragging.isDragging := false;
end;



{ zone bar mouse tracking
--------------------------------------------------------------------------------}
function TGuiMidiKeys.GetZoneMouseOverType(X: Integer; KeyNr: Integer; Zone: TGuiKeyZoneItem): TGuiZoneMousePosType;
var ZKeyArea: TRect; temp: integer;
begin
  Result := [mptOutside];
  if KeyNr < 0 then KeyNr:=MousePosToKey(X,0,false)
  else if not ScreenCoordOnKey(X,0,KeyNr,false) then exit;

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
            end else if fZoneMouseOverType <> tmpMouseOverType then
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
var i: integer;
begin
  for i:=KeyZones.Count-1 downto 0 do
    if (ExceptZone<>KeyZones.Items[i]) and KeyZones.Items[i].Selected then KeyZones.Items[i].Unselect(false);
end;

procedure TGuiMidiKeys.ZoneSelectionChanged(Zone: TGuiKeyZoneItem);
begin
  if Zone<>nil then Zone.Select(false);
  UnSelectAllZones(Zone);
  if Assigned(fOnZoneSelectionChanged) then fOnZoneSelectionChanged(self, Zone);
end;

{ zone bar mouse enter/leave
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.MouseLeaveAllZones(ExceptZone: TGuiKeyZoneItem);
var i: integer;
begin
  for i:=KeyZones.Count-1 downto 0 do
    if (ExceptZone<>KeyZones.Items[i]) and KeyZones.Items[i].IsMouseOver then ZoneMouseLeave(KeyZones.Items[i]);
end;

procedure TGuiMidiKeys.ZoneMouseEnter(Zone: TGuiKeyZoneItem; Shift: TShiftState; X, Y: Integer);
begin
  Cursor:=CursorZone;
  Zone.SetMouseOver(true, false);
  if Assigned(fOnZoneMouseEnter) then fOnZoneMouseEnter(self, Zone, Shift, X, Y);
end;

procedure TGuiMidiKeys.ZoneMouseLeave(Zone: TGuiKeyZoneItem);
begin
  fZoneMouseOverType := [];
  Cursor:=CursorZoneBar;
  Zone.SetMouseOver(false, false);
  if Assigned(fOnZoneMouseLeave) then fOnZoneMouseLeave(self, Zone);
end;

procedure TGuiMidiKeys.ZoneMouseOverChanged(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Shift: TShiftState; X, Y: Integer);
begin
  fZoneMouseOverType := MouseOverType;
  if (mptOnLowestBorder in MouseOverType) or (mptOnHighestBorder in MouseOverType) then Cursor:=CursorZoneBorder
  else if mptInZone in MouseOverType then Cursor:=CursorZone
  else Cursor := CursorZoneBar;
  if Assigned(fOnZoneMouseOverChanged) then fOnZoneMouseOverChanged(self, KeyNr, Zone, MouseOverType, Shift, X, Y);
end;


procedure TGuiMidiKeys.ZoneBarMouseEnter(KeyNr: Byte; Shift: TShiftState; X, Y: Integer);
begin
  Cursor:=CursorZoneBar;
  fMidiZoneBarMouseOver:=true;
  if Assigned(fOnZoneBarMouseEnter) then fOnZoneBarMouseEnter(self, KeyNr, Shift, X, Y);
end;

procedure TGuiMidiKeys.ZoneBarMouseLeave;
begin
  Cursor:=CursorKeys;
  fMidiZoneBarMouseOver:=false;
  MouseLeaveAllZones;
  if Assigned(fOnZoneBarMouseLeave) then fOnZoneBarMouseLeave(self);
end;

{ zone bar mouse up/down
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.MouseUpOnKeyZoneBar(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if Assigned(fOnMouseUpOnKeyZoneBar) then fOnMouseUpOnKeyZoneBar(self, KeyNr, Zone, MouseOverType, Button, Shift, X,Y);
end;

procedure TGuiMidiKeys.MouseDownOnKeyZoneBar(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if Assigned(fOnMouseDownOnKeyZoneBar) then fOnMouseDownOnKeyZoneBar(self, KeyNr, Zone, MouseOverType, Button, Shift, X,Y);
end;

{ zone bar dragging
--------------------------------------------------------------------------------}
procedure TGuiMidiKeys.StartZoneBarDragging(KeyNr: Integer; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; DragButton: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fMidiZoneBarDragging.Button:=DragButton;
  fMidiZoneBarDragging.isDragging:=true;
  fMidiZoneBarDragging.Zone:=Zone;
  fMidiZoneBarDragging.InZonePos:=MouseOverType;
  fMidiZoneBarDragging.LastKey:=KeyNr;
  fMidiZoneBarDragging.StartKey:=KeyNr;
  if Zone<>nil then
  begin
    fMidiZoneBarDragging.StartLowestZoneKey:=Zone.LowestZoneKey;
    fMidiZoneBarDragging.StartHighestZoneKey:=Zone.HighestZoneKey;
  end;

  if Assigned(fOnStartZoneBarDragging) then fOnStartZoneBarDragging(self, KeyNr, fMidiZoneBarDragging, Shift, X,Y);
end;

procedure TGuiMidiKeys.MoveZoneBarDragging(KeyNr: Integer; Shift: TShiftState; X,Y: Integer);
begin
  if Assigned(fOnMoveZoneBarDragging) then fOnMoveZoneBarDragging(self, KeyNr, fMidiZoneBarDragging, Shift, X, Y);
  fMidiZoneBarDragging.LastKey:=KeyNr;
end;

procedure TGuiMidiKeys.EndZoneBarDragging(Shift: TShiftState; X,Y: Integer);
begin
  if Assigned(fOnEndZoneBarDragging) then fOnEndZoneBarDragging(self, fMidiZoneBarDragging, Shift, X, Y);
  fMidiZoneBarDragging.isDragging := false;
end;

end.
