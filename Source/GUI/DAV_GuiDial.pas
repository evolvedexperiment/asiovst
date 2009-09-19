unit DAV_GuiDial;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, SysUtils, Controls, Contnrs, DAV_GuiCommon,
  DAV_GuiBaseControl;

type
  TGuiDialRMBFunc = (rmbfReset, rmbfCircular);
  TGuiDialImageList = class;
  TGuiDialImageCollectionItem = class;

  {$IFDEF DELPHI10_UP} {$region 'Pointer Angles'} {$ENDIF}

  TGuiDialSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  protected
    procedure Changed;
  public
    constructor Create; virtual;
  end;

  TGuiDialPointerAngles = class(TGuiDialSettings)
  private
    FResolution : Extended;
    FStart      : Integer;
    FRange      : Integer;
    procedure SetRange(const Value: Integer);
    procedure SetResolution(const Value: Extended);
    procedure SetStart(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
  published
    property Start: Integer read FStart write SetStart default 0;
    property Range: Integer read FRange write SetRange default 360;
    property Resolution: Extended read FResolution write SetResolution;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Custom Dials'} {$ENDIF}

  TCustomGuiStitchedControl = class(TCustomGuiBaseAntialiasedControl)
  private
    function GetDialImageIndex: Integer;
    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetDialBitmap(const Value: TBitmap);
    procedure SetNumGlyphs(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SetImageList(const Value: TImageList);
    procedure SetDialImageIndex(Value: Integer);
    procedure SetDialImageList(const Value: TGuiDialImageList);
    procedure SetDialAlpha(const Value: TBitmap);
  protected
    FAutoSize      : Boolean;
    FDialBitmap    : TBitmap;
    FDialAlpha     : TBitmap;
    FImageList     : TImageList;
    FNumGlyphs     : Integer;
    FOnChange      : TNotifyEvent;
    FStitchKind    : TGuiStitchKind;
    FDialImageList : TGuiDialImageList;
    FDialImageItem : TGuiDialImageCollectionItem;

    function GetGlyphNr: Integer; virtual; abstract;
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure NumGlyphsChanged; virtual;
    procedure RedrawBuffer(doBufferFlip: Boolean); override;
    procedure RenderBitmap(const Bitmap: TBitmap); virtual; abstract;

    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs default 1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property DialBitmap: TBitmap read FDialBitmap write SetDialBitmap;
    property DialAlpha: TBitmap read FDialAlpha write SetDialAlpha;
    property DialImageIndex: Integer read GetDialImageIndex write SetDialImageIndex;
    property DialImageList: TGuiDialImageList read FDialImageList write SetDialImageList;
    property ImageList: TImageList read FImageList write SetImageList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
  end;

  TQuantizeValueEvent = procedure(Sender: TObject; var NewValue: Double) of object;

  TCustomGuiDial = class(TCustomGuiStitchedControl)
  private
    FAutoColor        : Boolean;
    FCircleColor      : TColor;
    FCurveMapping     : Single;
    FCurveMappingExp  : Single;
    FDefaultPosition  : Single;
    FInertia          : Single;
    FInertiaExp       : Single;
    FInertiaScale     : Single;
    FMin, FMax        : Single;
    FPointerAngles    : TGuiDialPointerAngles;
    FPosition         : Single;
    FRightMouseButton : TGuiDialRMBFunc;
    FScrollRange      : Single;
    FWheelStep        : Single;
    FOnQuantizeValue  : TQuantizeValueEvent;
    function CircularMouseToPosition(X, Y: Integer): Single;
    function GetNormalizedPosition: Single;
    function PositionToAngle: Single;
    function GetMappedPosition: Single;
    function MapValue(Value: Double): Double;
    function UnmapValue(Value: Double): Double;
    procedure SetAutoColor(const Value: Boolean);
    procedure SetCircleColor(const Value: TColor);
    procedure SetDefaultPosition(Value: Single);
    procedure SetInertia(Value: Single);
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetPointerAngles(const Value: TGuiDialPointerAngles);
    procedure SetPosition(Value: Single);
    procedure SetCurveMapping(const Value: Single);
    procedure SetNormalizedPosition(const Value: Single);
  protected
    function GetGlyphNr: Integer; override;

    procedure CalcColorCircle;
    procedure RenderBitmap(const Bitmap: TBitmap); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer); override;
    procedure DragMouseMoveRight(Shift: TShiftState; X, Y: Integer); override;
    procedure ReadState(Reader: TReader); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

    property NormalizedPosition: Single read GetNormalizedPosition write SetNormalizedPosition;
    property MappedPosition: Single read GetMappedPosition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AutoColor: Boolean read FAutoColor write SetAutoColor default False;
    property CircleColor : TColor read FCircleColor write SetCircleColor default clBlack;
    property CurveMapping: Single read FCurveMapping write SetCurveMapping;
    property DefaultPosition: Single read FDefaultPosition write SetDefaultPosition;
    property Inertia: Single read fInertia write SetInertia;
    property Max: Single read FMax write SetMax;
    property Min: Single read FMin write SetMin;
    property PointerAngles: TGuiDialPointerAngles read FPointerAngles write SetPointerAngles;
    property Position: Single read FPosition write SetPosition;
    property RightMouseButton: TGuiDialRMBFunc read FRightMouseButton write FRightMouseButton default rmbfCircular;
    property ScrollRange_Pixel: Single read FScrollRange write FScrollRange;
    property WheelStep: Single read FWheelStep write FWheelStep;
    property OnQuantizeValue: TQuantizeValueEvent read FOnQuantizeValue write FOnQuantizeValue;
  end;

  TCustomGuiSwitch = class(TCustomGuiStitchedControl)
  private
    FGlyphNr        : Integer;
    FDefaultGlyphNr : Integer;
    FStringList     : TStringList;
    FReadOnly       : Boolean;
    procedure SetGlyphNr(Value: Integer);
    procedure SetDefaultGlyphNr(Value: Integer);
    procedure SetStringList(const Value: TStringList);
  protected
    function GetGlyphNr: Integer; override;
    procedure RenderBitmap(const Bitmap: TBitmap); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure NumGlyphsChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DefaultGlyphNr: Integer read FDefaultGlyphNr write SetDefaultGlyphNr;
    property GlyphNr: Integer read FGlyphNr write SetGlyphNr;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property StringList: TStringList read FStringList write SetStringList; 
  end;

  TCustomGuiDialMetal = class(TCustomGuiDial)
  protected
    procedure RenderBitmap(const Bitmap: TBitmap); override;
  end;

  TCustomGuiDialEx = class(TCustomGuiDial)
  private
    FIndLineLength : Single;
    procedure SetIndLineLength(const Value: Single);
  protected
    procedure RenderBitmap(const Bitmap: TBitmap); override;
  public
    constructor Create(AOwner: TComponent); override;
    property IndicatorLineLength_Percent: Single read fIndLineLength write SetIndLineLength;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Dials'} {$ENDIF}

  TGuiDial = class(TCustomGuiDial)
  published
    property PopupMenu;
    property Anchors;
    property AntiAlias;
    property AutoColor;
    property AutoSize;
    property CircleColor;
    property Color;
    property CurveMapping;
    property DefaultPosition;
    property DialBitmap;
//    property DialAlpha;
    property DialImageList;
    property DialImageIndex;
    property ImageList;
    property Inertia;
    property LineColor;
    property LineWidth;
    property Max;
    property Min;
    property NumGlyphs;
    property OnChange;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnMouseWheel;
    property OnMouseMove;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnMouseUp;
    property OnMouseDown;
    property OnQuantizeValue;
    property ParentColor;
    property PointerAngles;
    property Position;
    property RightMouseButton;
    property ScrollRange_Pixel;
    property StitchKind;
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property Visible;
    property WheelStep;
  end;

  TGuiDialMetal = class(TCustomGuiDialMetal)
  published
    property Anchors;
    property AntiAlias;
    property AutoColor;
    property AutoSize;
    property CircleColor;
    property Color;
    property CurveMapping;
    property DefaultPosition;
    property DialBitmap;
    property DialImageList;
    property DialImageIndex;
    property LineColor;
    property LineWidth;
    property Max;
    property Min;
    property NumGlyphs;
    property OnChange;
    property ParentColor;
    property PointerAngles;
    property Position;
    property RightMouseButton;
    property ScrollRange_Pixel;
    property StitchKind;
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property Visible;
    property WheelStep;
  end;

  TGuiDialEx = class(TCustomGuiDialEx)
  published
    property Anchors;
    property AntiAlias;
    property AutoColor;
    property AutoSize;
    property CircleColor;
    property Color;
    property CurveMapping;
    property DefaultPosition;
    property DialBitmap;
    property DialImageList;
    property DialImageIndex;
    property IndicatorLineLength_Percent;
    property LineColor;
    property LineWidth;
    property Max;
    property Min;
    property NumGlyphs;
    property OnChange;
    property ParentColor;
    property PointerAngles;
    property Position;
    property RightMouseButton;
    property ScrollRange_Pixel;
    property StitchKind;
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property Visible;
    property WheelStep;
  end;

  TGuiSwitch = class(TCustomGuiSwitch)
  published
    property Anchors;
    property AntiAlias;
    property AutoSize;
    property Color;
    property DefaultGlyphNr;
    property DialBitmap;
    property DialImageList;
    property DialImageIndex;
    property GlyphNr;
    property ImageList;
    property LineColor;
    property LineWidth;
    property NumGlyphs;
    property OnChange;
    property ParentColor;
    property ReadOnly;
    property StitchKind;
    property StringList;
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property Visible;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Dial Image List'} {$ENDIF}

  TGuiDialImageCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiDialImageCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiDialImageCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiDialImageCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGuiDialImageCollectionItem;
    function Insert(Index: Integer): TGuiDialImageCollectionItem;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TGuiDialImageCollectionItem = class(TCollectionItem)
  private
    FDialBitmap  : TBitmap;
    FNumGlyphs   : Integer;
    FOnChange    : TNotifyEvent;
    FStitchKind  : TGuiStitchKind;
    FLinkedDials : TObjectList;
    FDisplayName : string;
    procedure SetDialBitmap(const Value: TBitmap);
    procedure SetNumGlyphs(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SettingsChanged(Sender: TObject);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    procedure NumGlyphsChanged; virtual;
    procedure StitchKindChanged; virtual;
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure LinkStitchedControl(Dial: TCustomGuiStitchedControl);
    procedure UnLinkStitchedControl(Dial: TCustomGuiStitchedControl);
  published
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property DialBitmap: TBitmap read FDialBitmap write SetDialBitmap;
    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs default 1;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TGuiDialImageList = class(TComponent)
  private
    FDialImageCollection : TGuiDialImageCollection;
    function GetCount: Integer;
    function GetItems(Index: Integer): TGuiDialImageCollectionItem;
  protected
    property Items[Index: Integer]: TGuiDialImageCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DialImages: TGuiDialImageCollection read FDialImageCollection write FDialImageCollection;
    property Count: Integer read GetCount;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Dial Renderer'} {$ENDIF}

  ////////////////////
  //  Dial Renderer //
  ////////////////////

  // Custom Primitives

  TCustomGuiDialPrimitiveClass = class of TCustomGuiDialPrimitive;
  TCustomGuiDialPrimitive = class(TPersistent)
  private
    FZoom     : Single;
    FTag      : Integer;
    FVisible  : Boolean;
    FOnChange : TNotifyEvent;
    procedure SetZoom(const Value: Single);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
//    procedure PaintToGraph(const GraphXY: TCustomGuiGraphXY; const Bitmap: TBitmap); virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    property Zoom: Single read FZoom write SetZoom;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Tag: Longint read FTag write FTag default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCustomGuiDialPrimitiveBasic = class(TCustomGuiDialPrimitive)
  private
    FColor    : TColor;
    FDiffuse  : Single;
    FSpecular : Single;
    procedure SetColor(const Value: TColor);
    procedure SetDiffuse(const Value: Single);
    procedure SetSpecular(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    property Color: TColor read FColor write SetColor;
    property Diffuse: Single read FDiffuse write SetDiffuse;
    property Specular: Single read FSpecular write SetSpecular;
  end;

  TCustomGuiDialPrimitiveLine = class(TCustomGuiDialPrimitiveBasic)
  private
    FLengh: Single;
    FWidth: Single;
    procedure SetLength(const Value: Single);
    procedure SetWidth(const Value: Single);
  public
    constructor Create; override;
  published
    property Width: Single read FWidth write SetWidth;
    property Length: Single read FLengh write SetLength;
  end;

  TCustomGuiDialPrimitiveAspectLine = class(TCustomGuiDialPrimitiveLine)
  private
    FAspect: Single;
    procedure SetAspect(const Value: Single);
  public
    constructor Create; override;
  published
    property Aspect: Single read FAspect write SetAspect;
  end;

  TCustomGuiDialPrimitiveFrame = class(TCustomGuiDialPrimitiveBasic)
  private
    FAspect     : Single;
    FFramwWidth : Single;
    procedure SetAspect(const Value: Single);
    procedure SetFramwWidth(const Value: Single);
  public
    constructor Create; override;
    property Aspect: Single read FAspect write SetAspect;
    property FramwWidth: Single read FFramwWidth write SetFramwWidth;
  end;

  TCustomGuiDialPrimitiveFill = class(TCustomGuiDialPrimitiveBasic)
  private
    FAspect        : Single;
    FTexture       : TBitmap;
    FTextureDepth  : Single;
    FTextureZoom   : Single;
    procedure SetAspect(const Value: Single);
    procedure SetTexture(const Value: TBitmap);
    procedure SetTextureDepth(const Value: Single);
    procedure SetTextureZoom(const Value: Single);
  public
    constructor Create; override;

    property Aspect: Single read FAspect write SetAspect;
    property Texture: TBitmap read FTexture write SetTexture;
    property TextureDepth: Single read FTextureDepth write SetTextureDepth;
    property TextureZoom: Single read FTextureZoom write SetTextureZoom;
  end;

  TCustomGuiDialPrimitiveEmbossFill = class(TCustomGuiDialPrimitiveFill)
  private
    FEmboss        : Single;
    FEmbossDiffuse : Single;
    procedure SetEmboss(const Value: Single);
    procedure SetEmbossDiffuse(const Value: Single);
  public
    constructor Create; override;

    property Emboss: Single read FEmboss write SetEmboss;
    property EmbossDiffuse: Single read FEmbossDiffuse write SetEmbossDiffuse;
  end;



  // Primitives

  TGuiDialPrimitiveNone = class(TCustomGuiDialPrimitive)
  published
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveImageTransparency = (itAlphaLayer, itOpaque, itFirstPixel);
  TGuiDialPrimitiveImage = class(TCustomGuiDialPrimitive)
  private
    FBitmap           : TBitmap;
    FTransparency     : TGuiDialPrimitiveImageTransparency;
    FIntelligentAlpha : Byte;
    FAutoFitToRect    : Boolean;
    FNumGlyphs        : Integer;
    FStitchKind       : TGuiStitchKind;
    procedure SetAutoFitToRect(const Value: Boolean);
    procedure SetBitmap(const Value: TBitmap);
    procedure SetIntelligentAlpha(const Value: Byte);
    procedure SetNumGlyphs(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SetTransparency(const Value: TGuiDialPrimitiveImageTransparency);
  public
    constructor Create; override;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Transparency: TGuiDialPrimitiveImageTransparency read FTransparency write SetTransparency default itAlphaLayer;
    property IntelligentAlpha: Byte read FIntelligentAlpha write SetIntelligentAlpha default 1;
    property AutoFitToRect: Boolean read FAutoFitToRect write SetAutoFitToRect default False;
    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs default 1;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind default skHorizontal;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFrameCircle = class(TCustomGuiDialPrimitiveFrame)
  published
    property Aspect;
    property Diffuse;
    property FramwWidth;
    property Specular;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFillCircle = class(TCustomGuiDialPrimitiveEmbossFill)
  published
    property Aspect;
    property Diffuse;
    property Emboss;
    property EmbossDiffuse;
    property Specular;
    property Texture;
    property TextureDepth;
    property TextureZoom;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveMetalCircle = class(TCustomGuiDialPrimitiveEmbossFill)
  private
    FAmbient : Single;
    procedure SetAmbient(const Value: Single);
  public
    constructor Create; override;
  published
    property Ambient: Single read FAmbient write SetAmbient;
    property Aspect;
    property Diffuse;
    property Emboss;
    property EmbossDiffuse;
    property Specular;
    property Texture;
    property TextureDepth;
    property TextureZoom;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFillWave = class(TCustomGuiDialPrimitiveFill)
  private
    FAngleStep : Single;
    FDepth     : Single;
    procedure SetDepth(const Value: Single);
    procedure SetAngleStep(const Value: Single);
  public
    constructor Create; override;
  published
    property AngleStep: Single read FAngleStep write SetAngleStep;
    property Depth: Single read FDepth write SetDepth;

    property Aspect;
    property Diffuse;
    property Specular;
    property Texture;
    property TextureDepth;
    property TextureZoom;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFillSphere = class(TCustomGuiDialPrimitiveEmbossFill)
  private
    FAmbient        : Single;
    FSpecularWidth  : Single;
    FLightDirection : Single;
    procedure SetAmbient(const Value: Single);
    procedure SetSpecularWidth(const Value: Single);
    procedure SetLightDirection(const Value: Single);
  public
    constructor Create; override;
  published
    property Ambient: Single read FAmbient write SetAmbient;
    property SpecularWidth: Single read FSpecularWidth write SetSpecularWidth;
    property LightDirection: Single read FLightDirection write SetLightDirection;

    property Aspect;
    property Diffuse;
    property Emboss;
    property EmbossDiffuse;
    property Specular;
    property Texture;
    property TextureDepth;
    property TextureZoom;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFrameRect = class(TCustomGuiDialPrimitiveFrame)
  private
    FRound : Single;
    procedure SetRound(const Value: Single);
  public
    constructor Create; override;
  published
    property Round: Single read FRound write SetRound;

    property Aspect;
    property Diffuse;
    property FramwWidth;
    property Specular;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFillRect = class(TCustomGuiDialPrimitiveEmbossFill)
  private
    FRound : Single;
    procedure SetRound(const Value: Single);
  public
    constructor Create; override;
  published
    property Round: Single read FRound write SetRound;

    property Aspect;
    property Diffuse;
    property Emboss;
    property EmbossDiffuse;
    property Specular;
    property Texture;
    property TextureDepth;
    property TextureZoom;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveTriangle = class(TCustomGuiDialPrimitiveLine)
  private
    FTexture       : TBitmap;
    FTextureDepth  : Single;
    FTextureZoom   : Single;
    procedure SetTexture(const Value: TBitmap);
    procedure SetTextureDepth(const Value: Single);
    procedure SetTextureZoom(const Value: Single);
  public
    constructor Create; override;
  published
    property Texture: TBitmap read FTexture write SetTexture;
    property TextureDepth: Single read FTextureDepth write SetTextureDepth;
    property TextureZoom: Single read FTextureZoom write SetTextureZoom;
    property Diffuse;
    property Length;
    property Specular;
    property Tag;
    property Visible;
    property Width;
    property Zoom;
  end;

  TGuiDialPrimitiveLine = class(TCustomGuiDialPrimitiveLine)
  published
    property Diffuse;
    property Length;
    property Specular;
    property Tag;
    property Visible;
    property Width;
    property Zoom;
  end;

  TGuiDialPrimitiveRadiateLine = class(TCustomGuiDialPrimitiveAspectLine)
  private
    FAngleStep: Single;
    procedure SetAngleStep(const Value: Single);
  public
    constructor Create; override;
  published
    property AngleStep: Single read FAngleStep write SetAngleStep;

    property Diffuse;
    property Length;
    property Specular;
    property Tag;
    property Visible;
    property Width;
    property Zoom;
  end;

  TGuiDialPrimitiveStrippedLines = class(TCustomGuiDialPrimitiveAspectLine)
  private
    FStep: Single;
    procedure SetStep(const Value: Single);
  public
    constructor Create; override;
  published
    property Step: Single read FStep write SetStep;

    property Diffuse;
    property Length;
    property Specular;
    property Tag;
    property Visible;
    property Width;
    property Zoom;
  end;

  TGuiDialPrimitiveText = class(TCustomGuiDialPrimitive)
  private
    FText             : string;
    FFont             : TFont;
    FAlignment        : TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Text: string read FText write SetText;
    property Font: TFont read FFont write SetFont;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveShape = class(TCustomGuiDialPrimitiveFill)
  private
    FWidth: Single;
    FShape: string;
    FFill: Boolean;
    procedure SetFill(const Value: Boolean);
    procedure SetShape(const Value: string);
    procedure SetWidth(const Value: Single);
  public
    constructor Create; override;

    property Shape: string read FShape write SetShape;
    property Width: Single read FWidth write SetWidth;
    property Fill: Boolean read FFill write SetFill;
  end;



  TGuiDialLayerCollectionItem = class;

  TGuiDialLayerCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiDialLayerCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiDialLayerCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiDialLayerCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGuiDialLayerCollectionItem;
    function Insert(Index: Integer): TGuiDialLayerCollectionItem;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TGuiDialLayerCollectionItem = class(TCollectionItem)
  private
    FOnChange              : TNotifyEvent;
    FDisplayName           : string;
    FPrimitive             : TCustomGuiDialPrimitive;
    FPrimitiveClassChanged : TNotifyEvent;
    function GetPrimitiveClassName: string;
    procedure SetPrimitive(const Value: TCustomGuiDialPrimitive);
    procedure SetPrimitiveClassName(const Value: string);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property DisplayName;
    property PrimitiveClassName: string read GetPrimitiveClassName write SetPrimitiveClassName;
    property Primitive: TCustomGuiDialPrimitive read FPrimitive write SetPrimitive;
    property PrimitiveClassChanged: TNotifyEvent read FPrimitiveClassChanged write FPrimitiveClassChanged;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TGuiDialImageRenderer = class(TComponent)
  private
    FDialLayerCollection : TGuiDialLayerCollection;
    function GetCount: Integer;
    function GetItems(Index: Integer): TGuiDialLayerCollectionItem;
  protected
    property Items[Index: Integer]: TGuiDialLayerCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Layers: TGuiDialLayerCollection read FDialLayerCollection write FDialLayerCollection;
    property Count: Integer read GetCount;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

var
  PrimitiveClassList: TClassList;

implementation

uses
  Dialogs, ExtCtrls, Math, {$IFNDEF FPC}Consts, {$ENDIF} DAV_Common,
  DAV_Complex, ImgList;

resourcestring
  RCStrNumGlyphsMustBePositive = 'NumGlyphs must be > 0!';
  RCStrPointerAngleRange = 'Range must be 1..360';
  RCStrPointerRangeResolution = 'Resolution must be above 0 and less than %d';
  RCStrPointerAngleStart = 'Start must be 0..359';

function RadToDeg(const Radians: Extended): Extended;  { Degrees := Radians * 180 / PI }
const
  DegPi : Double = (180 / PI);
begin
  Result := Radians * DegPi;
end;

function RelativeAngle(X1, Y1, X2, Y2: Integer): Single;
const
  MulFak = 180 / Pi;
begin
  Result := arctan2(X2 - X1, Y1 - Y2) * MulFak;
end;

function SafeAngle(Angle: Single): Single;
begin
  while Angle < 0 do Angle := Angle + 360;
  while Angle >= 360 do Angle := Angle - 360;
  Result := Angle;
end;

{ This function solves for x in the equation "x is y% of z". }
function SolveForX(Y, Z: Longint): Longint;
begin
  Result := round(Z * (Y * 0.01));//tt
end;

{ This function solves for y in the equation "x is y% of z". }
function SolveForY(X, Z: Longint): Longint;
begin
  if Z = 0 then Result := 0 else Result := round((X * 100.0) / Z); //t
end;


{$IFDEF DELPHI10_UP} {$region 'TGuiDialPointerAngles implementation'} {$ENDIF}

{ TGuiDialSettings }

procedure TGuiDialSettings.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TGuiDialSettings.Create;
begin
  inherited;
end;


{ TGuiDialPointerAngles }

procedure TGuiDialPointerAngles.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiDialPointerAngles then
  with TGuiDialPointerAngles(Dest) do
   begin
    FRange := Self.Range;
    FStart := Self.Start;
    FResolution := Self.Resolution;
    Changed;
   end else inherited;
end;

constructor TGuiDialPointerAngles.Create;
begin
  inherited;
  FStart := 0;
  FRange := 360;
  FResolution := 0;
end;

procedure TGuiDialPointerAngles.SetRange(const Value: Integer);
begin
  if (Value < 1) or (Value > 360)
   then raise Exception.Create(RCStrPointerAngleRange);

  FRange := Value;
  if FRange > Resolution then Resolution := FRange;
  Changed;
end;

procedure TGuiDialPointerAngles.SetResolution(const Value: Extended);
begin
  if (Value < 0) or (Value > Range)
   then raise Exception.CreateFmt(RCStrPointerRangeResolution, [Range + 1]);

  FResolution := Value;
  Changed;
end;

procedure TGuiDialPointerAngles.SetStart(const Value: Integer);
begin
  if (Value < 0) or (Value > 359)
   then raise Exception.Create(RCStrPointerAngleStart);

  FStart := Value;
  Changed;
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'Custom Dial implementations'} {$ENDIF}

{ TCustomGuiStitchedControl }

constructor TCustomGuiStitchedControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineColor              := clRed;
  FLineWidth              := 2;
  FNumGlyphs              := 1;
  FStitchKind             := skHorizontal;
  FDialBitmap             := TBitmap.Create;
  FDialBitmap.PixelFormat := pf24bit;
  FDialBitmap.OnChange    := SettingsChanged;
  FDialAlpha              := TBitmap.Create;
  FDialAlpha.PixelFormat  := pf8bit;
  FDialAlpha.OnChange     := SettingsChanged;
end;

destructor TCustomGuiStitchedControl.Destroy;
begin
  FreeAndNil(FDialBitmap);
  FreeAndNil(FDialAlpha);
  inherited Destroy;
end;

procedure TCustomGuiStitchedControl.SettingsChanged(Sender: TObject);
begin
  FDialBitmap.Canvas.Brush.Color := Self.Color;
  RedrawBuffer(True);
end;

procedure TCustomGuiStitchedControl.RedrawBuffer(doBufferFlip: Boolean);
var
  theRect   : TRect;
  GlyphNr   : Integer;
  Bmp       : TBitmap;
  OwnerDraw : Boolean;
begin
 if [csLoading..csDestroying] * ComponentState <> [] then exit;

 if (Width > 0) and (Height > 0) then with FBuffer.Canvas do
  begin
   Lock;
   Brush.Color := Self.Color;
   OwnerDraw := FDialBitmap.Empty and not assigned(FImageList);
   if OwnerDraw and assigned(FDialImageList) and assigned(FDialImageItem)
    then OwnerDraw := FDialImageItem.FDialBitmap.Empty;

   if OwnerDraw then
    if AntiAlias = gaaNone then
     begin
      // draw background
      {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
      FillRect(ClipRect);

      RenderBitmap(FBuffer);
     end
    else
     begin
      Bmp := TBitmap.Create;
      with Bmp do
       try
        PixelFormat := pf32bit;
        Width       := OversamplingFactor * FBuffer.Width;
        Height      := OversamplingFactor * FBuffer.Height;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := Self.Color;
        {$IFNDEF FPC}
        if FTransparent then
         begin
          CopyParentImage(Self, Bmp.Canvas);
          UpsampleBitmap(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderBitmap(Bmp);
        DownsampleBitmap(Bmp);
        FBuffer.Canvas.Draw(0, 0, Bmp);
       finally
        FreeAndNil(Bmp);
       end;
    end
   else
    begin
     // draw background
     Brush.Color := Self.Color;
     {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
     FillRect(ClipRect);

     GlyphNr := GetGlyphNr;
     if (GlyphNr >= FNumGlyphs) then GlyphNr := FNumGlyphs - 1 else
     if (GlyphNr < 0) then GlyphNr := 0;

     if Assigned(FDialImageItem)
      then Bmp := FDialImageItem.FDialBitmap
      else Bmp := DialBitmap;

     if not Bmp.Empty then
      begin

       theRect := ClientRect;
       if FStitchKind = skVertical then
        begin
         theRect.Top    := Bmp.Height * GlyphNr div FNumGlyphs;
         theRect.Bottom := Bmp.Height * (GlyphNr + 1) div FNumGlyphs;
        end
       else
        begin
         theRect.Left  := Bmp.Width * GlyphNr div FNumGlyphs;
         theRect.Right := Bmp.Width * (GlyphNr + 1) div FNumGlyphs;
        end;

       with ClientRect do
        begin
         BitBlt(Handle, Left, Top, Right - Left, Bottom - Top,
           Bmp.Canvas.Handle, theRect.Left, theRect.Top, CopyMode);
        end;
      end else

     if assigned(ImageList)
      then ImageList.Draw(FBuffer.Canvas, 0, 0, GlyphNr);
    end;
   Unlock;
  end;

 if doBufferFlip then Invalidate;
end;

function TCustomGuiStitchedControl.GetDialImageIndex: Integer;
begin
 if assigned(FDialImageItem)
  then result := FDialImageItem.Index
  else result := -1;
end;

procedure TCustomGuiStitchedControl.DoAutoSize;
begin
 if assigned(FImageList) then
  begin
   Width := FImageList.Width;
   Height := FImageList.Height;
   exit;
  end;
 if FDialBitmap.Empty or (FNumGlyphs = 0) then Exit;

 if FStitchKind = skVertical then
  begin
   Width  := FDialBitmap.Width;
   Height := FDialBitmap.Height div FNumGlyphs;
  end
 else
  begin
   Width  := FDialBitmap.Width div FNumGlyphs;
   Height := FDialBitmap.Height;
  end;
end;

procedure TCustomGuiStitchedControl.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Autosize then DoAutoSize;
  end;
end;

procedure TCustomGuiStitchedControl.SetNumGlyphs(const Value: Integer);
begin
 if assigned(FImageList) then exit;
 if FNumGlyphs <> Value then
  begin
   FNumGlyphs := Value;
   NumGlyphsChanged;
  end;
end;

procedure TCustomGuiStitchedControl.NumGlyphsChanged;
begin
 DoAutoSize;
end;

procedure TCustomGuiStitchedControl.SetDialAlpha(const Value: TBitmap);
begin
 FDialAlpha.Assign(Value);
end;

procedure TCustomGuiStitchedControl.SetDialBitmap(const Value: TBitmap);
begin
  FDialBitmap.Assign(Value);
  DoAutoSize;
end;

procedure TCustomGuiStitchedControl.SetDialImageIndex(Value: Integer);
begin
 // check if dial image list is available
 if not assigned(FDialImageList) then exit;

 // limit range to existing dial images
 if Value < 0 then Value := 0 else
 if Value >= FDialImageList.Count then Value := FDialImageList.Count - 1;

 if DialImageIndex <> Value then
  begin
   if Value >= 0
    then FDialImageList[Value].LinkStitchedControl(Self)
    else FDialImageItem.UnLinkStitchedControl(Self);
   FDialImageItem := FDialImageList[Value];
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiStitchedControl.SetDialImageList(const Value: TGuiDialImageList);
begin
 if FDialImageList <> Value then
  begin
   FDialImageList := Value;
   if not assigned(FDialImageList)
    then FDialImageItem := nil;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiStitchedControl.SetImageList(const Value: TImageList);
begin
 if FImageList <> Value then
  begin
   FImageList := Value;
   if assigned(FImageList) then
    begin
     Width := FImageList.Width;
     Height := FImageList.Height;
     FNumGlyphs := FImageList.Count;
    end;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiStitchedControl.SetStitchKind(const Value: TGuiStitchKind);
begin
  if FStitchKind <> Value then
  begin
    FStitchKind := Value;
    DoAutoSize;
  end;
end;

{ TCustomGuiDial }

constructor TCustomGuiDial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPointerAngles          := TGuiDialPointerAngles.Create;
  FPointerAngles.OnChange := SettingsChanged;
  FCircleColor            := clBlack;
  FLineColor              := clRed;
  FLineWidth              := 2;
  FRightMouseButton       := rmbfCircular;
  FCurveMapping           := 0;
  FCurveMappingExp        := 1;
  FPosition               := 0;
  FDefaultPosition        := 0;
  FScrollRange            := 400;
  FInertia                := 0;
  FInertiaExp             := 1;
  FInertiaScale           := 1;
  FMin                    := 0;
  FWheelStep              := 1; 
  if csDesigning in ComponentState
   then FMax := 100;
end;

destructor TCustomGuiDial.Destroy;
begin
  FreeAndNil(FPointerAngles);
  inherited Destroy;
end;

function TCustomGuiDial.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  Difference : Single;
begin
 Difference := FWheelStep * WheelDelta / ( 120 * FScrollRange );

// apply inertia function
 if Difference < 0
  then Difference := -Power(abs(Difference), FInertiaExp) * FInertiaScale
  else Difference :=  Power(abs(Difference), FInertiaExp) * FInertiaScale;
 NormalizedPosition := UnMapValue(MapValue(NormalizedPosition) + Difference);
 result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TCustomGuiDial.PositionToAngle: Single;
const
  Pi180 : Double = PI / 180;
begin
 Result := SafeAngle(PointerAngles.Start + (PointerAngles.Range * MapValue(NormalizedPosition))) * Pi180;
end;

procedure TCustomGuiDial.RenderBitmap(const Bitmap: TBitmap);
var
  Steps, i : Integer;
  Val, Off : TComplexDouble;
  Rad, tmp : Single;
  PtsArray : Array of TPoint;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   // draw circle
   Rad := 0.45 * Math.Min(Width, Height) - fLineWidth div 2;
   if Rad <= 0 then exit;
   Steps := Round(2 / arcsin(1 / Rad)) + 1;
   if Steps > 1 then
    begin
     SetLength(PtsArray, Steps);
     GetSinCos(PositionToAngle - (PI * 0.5), Val.Im, Val.Re);
     Val.Re := Val.Re * Rad; Val.Im := Val.Im * Rad;
     GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
     PtsArray[0] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));

     for i := 1 to Steps - 1 do
      begin
       tmp := Val.Re * Off.Re - Val.Im * Off.Im;
       Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
       Val.Re := tmp;
       PtsArray[i] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));
      end;

     Pen.Width := OversamplingFactor * fLineWidth;
     Pen.Color := fLineColor;
     Brush.Color := FCircleColor;
     Polygon(PtsArray);
    end;

   // draw position line
   MoveTo(PtsArray[0].X, PtsArray[0].Y);
   LineTo(Round(0.5 * Width), Round(0.5 * Height));
  end;
end;

procedure TCustomGuiDial.ReadState(Reader: TReader);
begin
 if csDesigning in ComponentState
  then FMax := 0;
 inherited;
end;

function TCustomGuiDial.GetNormalizedPosition: Single;
begin
 if Max = Min
  then Result := Min
  else Result := (FPosition - Min) / (Max - Min);
end;

function TCustomGuiDial.GetGlyphNr: Integer;
begin
 result := Trunc(MapValue(NormalizedPosition) * FNumGlyphs);
end;

function TCustomGuiDial.GetMappedPosition: Single;
begin
 result := MapValue(NormalizedPosition) * (Max - Min) + Min;
end;

function TCustomGuiDial.UnmapValue(Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(abs(Value), 1 / FCurveMappingExp)
  else Result :=  Power(abs(Value), 1 / FCurveMappingExp)
end;

procedure TCustomGuiDial.SetMax(const Value: Single);
begin
  if Value <> FMax then
  begin
   {$IFNDEF FPC}
   if (Value < FMin) and not (csLoading in ComponentState) then
     raise EInvalidOperation.CreateFmt(SOutOfRange, [FMin + 1, MaxInt]);
   {$ENDIF}

   FMax := Value;
   if FPosition > Value then FPosition := Value;
   if FDefaultPosition > Value then FDefaultPosition := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiDial.SetMin(const Value: Single);
begin
  if Value <> FMin then
  begin
   {$IFNDEF FPC}
   if (Value > FMax) and not (csLoading in ComponentState) then
    raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMax - 1]);
   {$ENDIF}

   FMin := Value;
   if FPosition < Value then FPosition := Value;
   if FDefaultPosition < Value then FDefaultPosition := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiDial.SetNormalizedPosition(const Value: Single);
var
  NewValue : Double;
begin
 NewValue := Min + Value * (Max - Min);

 if assigned(FOnQuantizeValue)
  then FOnQuantizeValue(Self, NewValue);

 Position := NewValue;
end;

procedure TCustomGuiDial.SetPosition(Value: Single);
begin
  if Value < FMin then Value := FMin else
  if Value > FMax then Value := FMax;

  if FPosition <> Value then
   begin
    FPosition := Value;
    if not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiDial.SetDefaultPosition(Value: Single);
begin
 if not (csLoading in ComponentState) then
  begin
   if Value < FMin then Value := FMin else
   if Value > FMax then Value := FMax;
  end;

  FDefaultPosition := Value;
end;

procedure TCustomGuiDial.SetInertia(Value: Single);
begin
 if Value < 0 then Value := 0;
 if FInertia <> Value then
  begin
   FInertia      := Value;
   FInertiaExp   := Power(2, -Value);
   FInertiaScale := 0.01 * Power(0.01, -FInertiaExp);
   RedrawBuffer(True);
  end;
end;

function TCustomGuiDial.CircularMouseToPosition(X, Y: Integer): Single;
var
  Range: Single;
  Angle: Single;
begin
  Range := Max - (Min - 1);
  Angle := SafeAngle(RelativeAngle(Width div 2, Height div 2, X, Y) - PointerAngles.Start);
  Result := Angle * Range / PointerAngles.Range;
  while Result > Max do Result := Result - Range;
  while Result < Min do Result := Result + Range;

  if Result > Max then Result := FPosition;
  if Result < Min then Result := FPosition;
end;

function TCustomGuiDial.MapValue(Value: Double): Double;
begin
 if Value < 0
  then result := -Power(abs(Value), FCurveMappingExp)
  else result :=  Power(abs(Value), FCurveMappingExp);
end;

procedure TCustomGuiDial.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if Enabled then
  begin
   if ssCtrl in Shift then Position := FDefaultPosition;
   if (Button = mbRight) and
      (FRightMouseButton = rmbfReset)
    then Position := FDefaultPosition;
  end;

 inherited;
end;

procedure TCustomGuiDial.DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer);
var
  Difference : Double;
  NewValue   : Double;   

begin
 Difference := (MouseState.LastEventY - Y) / FScrollRange;

 // apply inertia function
 if Difference < 0
  then Difference := -Power(abs(Difference), FInertiaExp) * FInertiaScale
  else Difference :=  Power(abs(Difference), FInertiaExp) * FInertiaScale;

 if ssShift in Shift
  then NewValue := MapValue(NormalizedPosition) + Difference * 0.1
  else NewValue := MapValue(NormalizedPosition) + Difference;

 NormalizedPosition := UnMapValue(NewValue);

 inherited;
end;

procedure TCustomGuiDial.DragMouseMoveRight(Shift: TShiftState; X, Y: Integer);
begin
  if FRightMouseButton = rmbfCircular
   then Position := CircularMouseToPosition(x, y);
  inherited;
end;

procedure TCustomGuiDial.SetPointerAngles(const Value: TGuiDialPointerAngles);
begin
  FPointerAngles.Assign(Value);
end;

procedure TCustomGuiDial.CalcColorCircle;
begin
  if (Color and $000000FF) < $80
   then if (((Color and $0000FF00) shr 8) <$80) or (((Color and $00FF0000) shr 16)<$80) then FCircleColor:=$FFFFFF
   else if (((Color and $0000FF00) shr 8) <$80) and (((Color and $00FF0000) shr 16)<$80) then FCircleColor:=$FFFFFF;

  RedrawBuffer(True);
end;

procedure TCustomGuiDial.SetAutoColor(const Value: Boolean);
begin
  CalcColorCircle;
end;

procedure TCustomGuiDial.SetCircleColor(const Value: TColor);
begin
  if not FAutoColor and (Value <> FCircleColor) then
  begin
    FCircleColor:=Value;
    RedrawBuffer(True);
  end;
end;

procedure TCustomGuiDial.SetCurveMapping(const Value: Single);
begin
 if FCurveMapping <> Value then
  begin
   FCurveMapping := Value;
   FCurveMappingExp := Power(2, Value);
   RedrawBuffer(True);
  end;
end;

{ TCustomGuiSwitch }

constructor TCustomGuiSwitch.Create(AOwner: TComponent);
begin
 inherited;
 FStringList := TStringList.Create;
 FGlyphNr := 0;
 FDefaultGlyphNr := 0;
end;

destructor TCustomGuiSwitch.Destroy;
begin
 FreeAndNil(FStringList);
 inherited;
end;

function TCustomGuiSwitch.GetGlyphNr: Integer;
begin
 result := FGlyphNr;
end;

procedure TCustomGuiSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
 if not FReadOnly then
  begin
   if (Button = mbLeft) then
    if FGlyphNr < FNumGlyphs - 1
     then GlyphNr := FGlyphNr + 1
     else GlyphNr := 0 else
   if (Button = mbRight) then
    if FGlyphNr > 0
     then GlyphNr := FGlyphNr - 1
     else GlyphNr := FNumGlyphs - 1;
  end;
 inherited;
end;

procedure TCustomGuiSwitch.NumGlyphsChanged;
begin
 inherited;
 if FDefaultGlyphNr >= FNumGlyphs then DefaultGlyphNr := FNumGlyphs - 1;
 if FGlyphNr >= FNumGlyphs then GlyphNr := FNumGlyphs - 1;
end;

procedure TCustomGuiSwitch.RenderBitmap(const Bitmap: TBitmap);
var
  txt : string; 
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;
   Font.Assign(Self.Font);
   Font.Size := Font.Size * OversamplingFactor;
   if FGlyphNr < FStringList.Count
    then txt := FStringList[FGlyphNr]
    else txt := IntToStr(FGlyphNr);
   TextOut((Width - TextWidth(txt)) div 2, 0, txt);
  end;
end;

procedure TCustomGuiSwitch.SetDefaultGlyphNr(Value: Integer);
begin
 if Value < 0 then Value := 0 else
 if Value >= FNumGlyphs then Value := FNumGlyphs - 1;
 if Value <> FDefaultGlyphNr then
  begin
   FDefaultGlyphNr := Value;
  end;
end;

procedure TCustomGuiSwitch.SetGlyphNr(Value: Integer);
begin
 if Value < 0 then Value := 0 else
 if Value >= FNumGlyphs then Value := FNumGlyphs - 1;
 if Value <> FGlyphNr then
  begin
   FGlyphNr := Value;
   if assigned(FOnChange) and ([csLoading, csDestroying] * ComponentState = []) 
    then FOnChange(Self);
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSwitch.SetStringList(const Value: TStringList);
begin
 FStringList.Assign(Value);
 if FDialBitmap.Empty
  then NumGlyphs := max(1, FStringList.Count);
 RedrawBuffer(True);
end;

{ TCustomGuiDialMetal }

procedure TCustomGuiDialMetal.RenderBitmap(const Bitmap: TBitmap);
var
  Steps, i : Integer;
  Val      : Single;
  Rad      : Single;
  Cmplx    : TComplexSingle;
  Pnt      : TPoint;
  XStart   : Single;
  LineFrac : Single;
  BW       : Single;
  Center   : TPointFloat;
  Line     : PRGB32Array;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   // draw background
   {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
   FillRect(ClipRect);

   // draw circle
   Rad := 0.45 * Math.Min(Width, Height) - fLineWidth div 2;
   BW := 1 - OversamplingFactor / Rad; // border width = 1 pixel
   if Rad < 0 then exit;

   Center.x := 0.5 * Width;
   Center.y := 0.5 * Height;
   Pen.Color := fLineColor;
   Brush.Color := FCircleColor;

   {$IFNDEF FPC}
   for i := 0 to round(2 * Rad) do
    begin
     XStart := sqrt(abs(sqr(rad) - sqr(Rad - i)));
     Line := Scanline[round(Center.y - (Rad - i))];
     for steps := round(Center.x - XStart) to round(Center.x + XStart) do
      begin
       val := 2.9999 * abs(ArcTan2(steps - Center.x, (Rad - i)) / Pi);
       if round(1.5 + val) mod 3 = 0
        then val := val * 50 - 99.5
        else val := -Round(99.5 + val * 50) mod 100;
       if sqr(steps - Center.x) + sqr(Rad - i) > sqr(BW * Rad) then val := -$90;

       Line[steps].B := round(Line[steps].B + val);
       Line[steps].G := round(Line[steps].G + val);
       Line[steps].R := round(Line[steps].R + val);
      end;

     GetSinCos(PositionToAngle - (PI * 0.5), Cmplx.Im, Cmplx.Re);
     Pnt := Point(Round(Center.x + Cmplx.Re * BW * Rad), Round(Center.y + Cmplx.Im * BW * Rad));

     //LineFrac := 0.01 * fIndLineLength;
     LineFrac := 0.5;
     Pen.Width := 3 * OversamplingFactor;
     MoveTo(Pnt.X, Pnt.Y);
     LineTo(Round((1 - LineFrac) * Pnt.X + LineFrac * Center.x),
            Round((1 - LineFrac) * Pnt.Y + LineFrac * Center.y));
    end;
   {$ENDIF}
  end;
end;

{ TCustomGuiDialEx }

constructor TCustomGuiDialEx.Create(AOwner: TComponent);
begin
 inherited;
 fIndLineLength := 100;
end;

procedure TCustomGuiDialEx.RenderBitmap(const Bitmap: TBitmap);
var
  Steps, i  : Integer;
  Val, Off  : TComplexDouble;
  Rad, tmp  : Single;
  PtsArray  : Array of TPoint;
  LineFrac  : Single;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
   FillRect(ClipRect);

   Rad := 0.45 * Math.Min(Width, Height) - fLineWidth div 2;
   if Rad < 0 then exit;
   Steps := Round(2 / arcsin(1 / Rad)) + 1;
   if Steps > 1 then
    begin
     SetLength(PtsArray, Steps);
     GetSinCos(PositionToAngle - (PI * 0.5), Val.Im, Val.Re);
     Val.Re := Val.Re * Rad; Val.Im := Val.Im * Rad;
     GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
     PtsArray[0] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));

     for i:=1 to Steps - 1 do
      begin
       tmp := Val.Re * Off.Re - Val.Im * Off.Im;
       Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
       Val.Re := tmp;
       PtsArray[i] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));
      end;

     Pen.Width := fLineWidth;
     Pen.Color := fLineColor;
     Brush.Color := FCircleColor;
     Polygon(PtsArray);
    end;

   LineFrac := 0.01 * fIndLineLength;
   MoveTo(PtsArray[0].X, PtsArray[0].Y);
   LineTo(Round((1 - LineFrac) * PtsArray[0].X + LineFrac * 0.5 * Width),
          Round((1 - LineFrac) * PtsArray[0].Y + LineFrac * 0.5 * Height));
  end;
end;

procedure TCustomGuiDialEx.SetIndLineLength(const Value: Single);
begin
 if fIndLineLength <> Value then
  begin
   fIndLineLength := Value;
   RedrawBuffer(True);
  end;
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'Dial Image List Implementation'} {$ENDIF}

{ TGuiDialImageCollection }

constructor TGuiDialImageCollection.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TGuiDialImageCollectionItem);
end;

function TGuiDialImageCollection.Add: TGuiDialImageCollectionItem;
begin
 result := TGuiDialImageCollectionItem(inherited Add);
end;

procedure TGuiDialImageCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiDialImageCollection.GetItem(
  Index: Integer): TGuiDialImageCollectionItem;
begin
 result := TGuiDialImageCollectionItem(inherited GetItem(Index));
end;

function TGuiDialImageCollection.Insert(
  Index: Integer): TGuiDialImageCollectionItem;
begin
 result:= TGuiDialImageCollectionItem(inherited Insert(Index));
end;

procedure TGuiDialImageCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 // add things that depend on the order here!
end;

procedure TGuiDialImageCollection.SetItem(Index: Integer;
  const Value: TGuiDialImageCollectionItem);
begin
 inherited SetItem(Index, Value);
end;

{ TGuiDialImageCollectionItem }

constructor TGuiDialImageCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FNumGlyphs           := 1;
 FDialBitmap          := TBitmap.Create;
 FDialBitmap.OnChange := SettingsChanged;
 FLinkedDials         := TObjectList.Create(False);
 if Collection.Owner is TGuiDialImageList then
  with FDialBitmap.Canvas, TGuiDialImageList(Collection.Owner) do
   if Owner is TForm
    then Brush.Color := TForm(Owner).Color;
end;

destructor TGuiDialImageCollectionItem.Destroy;
begin
 FreeAndNil(FDialBitmap);
 FreeAndNil(FLinkedDials);
 inherited;
end;

function TGuiDialImageCollectionItem.GetDisplayName: string;
begin
 result := FDisplayName;
end;

function TGuiDialImageCollectionItem.GetHeight: Integer;
begin
 result := FDialBitmap.Height;
end;

function TGuiDialImageCollectionItem.GetWidth: Integer;
begin
 result := FDialBitmap.Width;
end;

procedure TGuiDialImageCollectionItem.LinkStitchedControl(Dial: TCustomGuiStitchedControl);
begin
 if FLinkedDials.IndexOf(Dial) < 0 then
  begin
   FLinkedDials.Add(Dial);
   Dial.NumGlyphs := NumGlyphs;
   Dial.StitchKind := StitchKind;
   case StitchKind of
    skHorizontal :
     begin
      Dial.Width  := Width div NumGlyphs;
      Dial.Height := Height;
     end;
    skVertical :
     begin
      Dial.Width  := Width;
      Dial.Height := Height div NumGlyphs;
     end;
   end;
  end;
end;

procedure TGuiDialImageCollectionItem.UnLinkStitchedControl(Dial: TCustomGuiStitchedControl);
begin
 FLinkedDials.Remove(Dial);
end;

procedure TGuiDialImageCollectionItem.SettingsChanged(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to FLinkedDials.Count - 1 do
  with TCustomGuiDial(FLinkedDials[i]) do
   begin
    NumGlyphs := Self.NumGlyphs;
    StitchKind := Self.StitchKind;
    RedrawBuffer(True);
   end;
end;

procedure TGuiDialImageCollectionItem.SetWidth(const Value: Integer);
begin
 if Value < 0 then exit;
 FDialBitmap.Width := Value;
end;

procedure TGuiDialImageCollectionItem.SetDialBitmap(const Value: TBitmap);
begin
 FDialBitmap.Assign(Value);
end;

procedure TGuiDialImageCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiDialImageCollectionItem.SetHeight(const Value: Integer);
begin
 if Value < 0 then exit;
 FDialBitmap.Height := Value;
end;

procedure TGuiDialImageCollectionItem.NumGlyphsChanged;
var
  i : Integer;
begin
 for i := 0 to FLinkedDials.Count - 1
  do TCustomGuiDial(FLinkedDials[i]).NumGlyphs := NumGlyphs;
end;

procedure TGuiDialImageCollectionItem.StitchKindChanged;
var
  i : Integer;
begin
 for i := 0 to FLinkedDials.Count - 1
  do TCustomGuiDial(FLinkedDials[i]).StitchKind := StitchKind;
end;

procedure TGuiDialImageCollectionItem.SetNumGlyphs(const Value: Integer);
begin
 if Value <= 0
  then raise Exception.Create(RCStrNumGlyphsMustBePositive);

 if FNumGlyphs <> Value then
  begin
   FNumGlyphs := Value;
   NumGlyphsChanged;
  end;
end;

procedure TGuiDialImageCollectionItem.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   StitchKindChanged;
  end;
end;

{ TGuiDialImageList }

constructor TGuiDialImageList.Create(AOwner: TComponent);
begin
  inherited;
  FDialImageCollection := TGuiDialImageCollection.Create(Self);
end;

destructor TGuiDialImageList.Destroy;
begin
  FreeAndNil(FDialImageCollection);
  inherited;
end;

function TGuiDialImageList.GetCount: Integer;
begin
  result := FDialImageCollection.Count;
end;

function TGuiDialImageList.GetItems(Index: Integer): TGuiDialImageCollectionItem;
begin
 if (Index >= 0) and (Index < FDialImageCollection.Count)
  then result := FDialImageCollection[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

{$IFDEF DELPHI10_UP} {$endregion } {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'Dial Renderer'} {$ENDIF}

{ TGuiDialLayerCollection }

constructor TGuiDialLayerCollection.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TGuiDialLayerCollectionItem);
end;

function TGuiDialLayerCollection.Add: TGuiDialLayerCollectionItem;
begin
 result := TGuiDialLayerCollectionItem(inherited Add);
end;

procedure TGuiDialLayerCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiDialLayerCollection.GetItem(Index: Integer): TGuiDialLayerCollectionItem;
begin
 result := TGuiDialLayerCollectionItem(inherited GetItem(Index));
end;

function TGuiDialLayerCollection.Insert(Index: Integer): TGuiDialLayerCollectionItem;
begin
 result:= TGuiDialLayerCollectionItem(inherited Insert(Index));
end;

procedure TGuiDialLayerCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 // add things that depend on the order here!
end;

procedure TGuiDialLayerCollection.SetItem(Index: Integer;
  const Value: TGuiDialLayerCollectionItem);
begin
 inherited SetItem(Index, Value);
end;


{ TGuiDialLayerCollectionItem }

constructor TGuiDialLayerCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FPrimitive := TGuiDialPrimitiveNone.Create;
 FDisplayName := 'Layer ' + IntToStr(Index + 1);
end;

destructor TGuiDialLayerCollectionItem.Destroy;
begin
 if assigned(FPrimitive) then FreeAndNil(FPrimitive);
 inherited;
end;

function TGuiDialLayerCollectionItem.GetDisplayName: string;
begin
 result := FDisplayName;
end;

function TGuiDialLayerCollectionItem.GetPrimitiveClassName: string;
begin
 if assigned(FPrimitive)
  then result := FPrimitive.ClassName
  else result := '';
end;

procedure TGuiDialLayerCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiDialLayerCollectionItem.SetPrimitive(const Value: TCustomGuiDialPrimitive);
begin
 FPrimitive.Assign(Value);
end;

procedure TGuiDialLayerCollectionItem.SetPrimitiveClassName(const Value: string);
var
  PrimitiveClass: TCustomGuiDialPrimitiveClass;
begin
 if (Value <> '') and (FPrimitive.ClassName <> Value) and Assigned(PrimitiveClassList) then
  begin
   PrimitiveClass := TCustomGuiDialPrimitiveClass(PrimitiveClassList.Find(Value));
   if Assigned(PrimitiveClass) then
    begin
     FPrimitive.Free;
     FPrimitive := PrimitiveClass.Create;
//     Changed;
    end;
  end;
end;


{ TCustomGuiDialPrimitive }

constructor TCustomGuiDialPrimitive.Create;
begin
 FZoom := 100;
end;

procedure TCustomGuiDialPrimitive.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiDialPrimitive then
  with TCustomGuiDialPrimitive(Dest) do
   begin
    FZoom    := Self.FZoom;
    FVisible := Self.FVisible;
    FTag     := Self.FTag;
   end
 else inherited;
end;

procedure TCustomGuiDialPrimitive.Changed;
begin

end;

procedure TCustomGuiDialPrimitive.SetVisible(const Value: Boolean);
begin
 if FVisible <> Value then
  begin
   FVisible := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitive.SetZoom(const Value: Single);
begin
 if FZoom <> Value then
  begin
   FZoom := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveImage }

constructor TGuiDialPrimitiveImage.Create;
begin
 inherited;
 FTransparency     := itAlphaLayer;
 FIntelligentAlpha := 1;
 FAutoFitToRect    := False;
 FNumGlyphs        := 1;
 FStitchKind       := skHorizontal;
end;

procedure TGuiDialPrimitiveImage.SetAutoFitToRect(const Value: Boolean);
begin
 if FAutoFitToRect <> Value then
  begin
   FAutoFitToRect := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveImage.SetBitmap(const Value: TBitmap);
begin
 if FBitmap <> Value then
  begin
   FBitmap := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveImage.SetIntelligentAlpha(const Value: Byte);
begin
 if FIntelligentAlpha <> Value then
  begin
   FIntelligentAlpha := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveImage.SetNumGlyphs(const Value: Integer);
begin
 if FNumGlyphs <> Value then
  begin
   FNumGlyphs := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveImage.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveImage.SetTransparency(
  const Value: TGuiDialPrimitiveImageTransparency);
begin
 if FTransparency <> Value then
  begin
   FTransparency := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveText }

constructor TGuiDialPrimitiveText.Create;
begin
 inherited;
 FText := '(1:99)';
 FFont := TFont.Create;
 FAlignment := taCenter;
end;

destructor TGuiDialPrimitiveText.Destroy;
begin
 FreeAndNil(FFont);
 inherited;
end;

procedure TGuiDialPrimitiveText.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveText.SetFont(const Value: TFont);
begin
 if FFont <> Value then
  begin
   FFont.Assign(Value);
   Changed;
  end;
end;

procedure TGuiDialPrimitiveText.SetText(const Value: string);
begin
 if FText <> Value then
  begin
   FText := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveBasic }

constructor TCustomGuiDialPrimitiveBasic.Create;
begin
 FColor := clRed;
end;

procedure TCustomGuiDialPrimitiveBasic.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiDialPrimitiveBasic then
  with TCustomGuiDialPrimitiveBasic(Dest) do
   begin
    FColor := Self.Color;
   end;
end;

procedure TCustomGuiDialPrimitiveBasic.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveBasic.SetDiffuse(const Value: Single);
begin
 if FDiffuse <> Value then
  begin
   FDiffuse := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveBasic.SetSpecular(const Value: Single);
begin
 if FSpecular <> Value then
  begin
   FSpecular := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveLine }

constructor TCustomGuiDialPrimitiveLine.Create;
begin
 inherited;
 FLengh := 50;
 FWidth := 10;
end;

procedure TCustomGuiDialPrimitiveLine.SetLength(const Value: Single);
begin
 if FLengh <> Value then
  begin
   FLengh := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveLine.SetWidth(const Value: Single);
begin
 if FWidth <> Value then
  begin
   FWidth := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveTriangle }

constructor TGuiDialPrimitiveTriangle.Create;
begin
 inherited;
 FTextureDepth := 0;
 FTextureZoom  := 100;
end;

procedure TGuiDialPrimitiveTriangle.SetTexture(const Value: TBitmap);
begin
 if FTexture <> Value then
  begin
   FTexture := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveTriangle.SetTextureDepth(const Value: Single);
begin
 if FTextureDepth <> Value then
  begin
   FTextureDepth := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveTriangle.SetTextureZoom(const Value: Single);
begin
 if FTextureZoom <> Value then
  begin
   FTextureZoom := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveAspectLine }

constructor TCustomGuiDialPrimitiveAspectLine.Create;
begin
 inherited;
 FAspect := 0;
end;

procedure TCustomGuiDialPrimitiveAspectLine.SetAspect(const Value: Single);
begin
 if FAspect <> Value then
  begin
   FAspect := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveRadiateLine }

constructor TGuiDialPrimitiveRadiateLine.Create;
begin
 inherited;
 FAngleStep := 45;
end;

procedure TGuiDialPrimitiveRadiateLine.SetAngleStep(const Value: Single);
begin
 if FAngleStep <> Value then
  begin
   FAngleStep := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveStrippedLines }

constructor TGuiDialPrimitiveStrippedLines.Create;
begin
 inherited;
 FStep := 20;
end;

procedure TGuiDialPrimitiveStrippedLines.SetStep(const Value: Single);
begin
 if FStep <> Value then
  begin
   FStep := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveFrame }

constructor TCustomGuiDialPrimitiveFrame.Create;
begin
 inherited;
 FAspect     := 0;
 FDiffuse    := 0;
 FFramwWidth := 0;
 FSpecular   := 0;
end;

procedure TCustomGuiDialPrimitiveFrame.SetAspect(const Value: Single);
begin
 if FAspect <> Value then
  begin
   FAspect := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveFrame.SetFramwWidth(const Value: Single);
begin
 if FFramwWidth <> Value then
  begin
   FFramwWidth := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveFrameRect }

constructor TGuiDialPrimitiveFrameRect.Create;
begin
 inherited;
 FRound := 0;
end;

procedure TGuiDialPrimitiveFrameRect.SetRound(const Value: Single);
begin
 if FRound <> Value then
  begin
   FRound := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveFill }

constructor TCustomGuiDialPrimitiveFill.Create;
begin
 inherited;
 FAspect        := 0;
 FDiffuse       := 0;
 FSpecular      := 0;
 FTextureDepth  := 0;
 FTextureZoom   := 100;
end;

procedure TCustomGuiDialPrimitiveFill.SetAspect(const Value: Single);
begin
 if FAspect <> Value then
  begin
   FAspect := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveFill.SetTexture(const Value: TBitmap);
begin
 if FTexture <> Value then
  begin
   FTexture := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveFill.SetTextureDepth(const Value: Single);
begin
 if FTextureDepth <> Value then
  begin
   FTextureDepth := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveFill.SetTextureZoom(const Value: Single);
begin
 if FTextureZoom <> Value then
  begin
   FTextureZoom := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveEmbossFill }

constructor TCustomGuiDialPrimitiveEmbossFill.Create;
begin
 inherited;
 FEmboss        := 0;
 FEmbossDiffuse := 0;
end;

procedure TCustomGuiDialPrimitiveEmbossFill.SetEmboss(const Value: Single);
begin
 if FEmboss <> Value then
  begin
   FEmboss := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveEmbossFill.SetEmbossDiffuse(
  const Value: Single);
begin
 if FEmbossDiffuse <> Value then
  begin
   FEmbossDiffuse := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveFillSphere }

constructor TGuiDialPrimitiveFillSphere.Create;
begin
 inherited;
 FAmbient := 50; 
 FSpecularWidth := 50;
 FLightDirection := -50;
end;

procedure TGuiDialPrimitiveFillSphere.SetAmbient(const Value: Single);
begin
 if FAmbient <> Value then
  begin
   FAmbient := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveFillSphere.SetLightDirection(const Value: Single);
begin
 if FLightDirection <> Value then
  begin
   FLightDirection := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveFillSphere.SetSpecularWidth(const Value: Single);
begin
 if FSpecularWidth <> Value then
  begin
   FSpecularWidth := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveMetalCircle }

constructor TGuiDialPrimitiveMetalCircle.Create;
begin
 inherited;
 FAmbient := 50;
end;

procedure TGuiDialPrimitiveMetalCircle.SetAmbient(const Value: Single);
begin
 if FAmbient <> Value then
  begin
   FAmbient := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveFillRect }

constructor TGuiDialPrimitiveFillRect.Create;
begin
 inherited;
 FRound := 0;
end;

procedure TGuiDialPrimitiveFillRect.SetRound(const Value: Single);
begin
 if FRound <> Value then
  begin
   FRound := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveFillWave }

constructor TGuiDialPrimitiveFillWave.Create;
begin
 inherited;
 FAngleStep := 45;
 FDepth     := 10;
end;

procedure TGuiDialPrimitiveFillWave.SetAngleStep(const Value: Single);
begin
 if FAngleStep <> Value then
  begin
   FAngleStep := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveFillWave.SetDepth(const Value: Single);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveShape }

constructor TGuiDialPrimitiveShape.Create;
begin
 inherited;
 FFill := True;
end;

procedure TGuiDialPrimitiveShape.SetFill(const Value: Boolean);
begin
 if FFill <> Value then
  begin
   FFill := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveShape.SetShape(const Value: string);
begin
 if FShape <> Value then
  begin
   FShape := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveShape.SetWidth(const Value: Single);
begin
 if FWidth <> Value then
  begin
   FWidth := Value;
   Changed;
  end;
end;


{ TGuiDialImageRenderer }

constructor TGuiDialImageRenderer.Create(AOwner: TComponent);
begin
 inherited;
 FDialLayerCollection := TGuiDialLayerCollection.Create(Self);
end;

destructor TGuiDialImageRenderer.Destroy;
begin
 FreeAndNil(FDialLayerCollection);
 inherited;
end;

function TGuiDialImageRenderer.GetCount: Integer;
begin
 result := FDialLayerCollection.Count;
end;

function TGuiDialImageRenderer.GetItems(
  Index: Integer): TGuiDialLayerCollectionItem;
begin
 if (Index >= 0) and (Index < FDialLayerCollection.Count)
  then result := FDialLayerCollection[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure RegisterPrimitiveClass(PrimitiveClass: TCustomGuiDialPrimitiveClass);
begin
  if not Assigned(PrimitiveClassList) then PrimitiveClassList := TClassList.Create;
  PrimitiveClassList.Add(PrimitiveClass);
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

initialization
  // register primitive classes
  RegisterPrimitiveClass(TGuiDialPrimitiveNone);
  RegisterPrimitiveClass(TGuiDialPrimitiveImage);
  RegisterPrimitiveClass(TGuiDialPrimitiveFrameCircle);
  RegisterPrimitiveClass(TGuiDialPrimitiveFrameRect);
  RegisterPrimitiveClass(TGuiDialPrimitiveFillCircle);
  RegisterPrimitiveClass(TGuiDialPrimitiveMetalCircle);
  RegisterPrimitiveClass(TGuiDialPrimitiveFillSphere);
  RegisterPrimitiveClass(TGuiDialPrimitiveFillWave);
  RegisterPrimitiveClass(TGuiDialPrimitiveFillRect);
  RegisterPrimitiveClass(TGuiDialPrimitiveLine);
  RegisterPrimitiveClass(TGuiDialPrimitiveTriangle);
  RegisterPrimitiveClass(TGuiDialPrimitiveRadiateLine);
  RegisterPrimitiveClass(TGuiDialPrimitiveStrippedLines);
  RegisterPrimitiveClass(TGuiDialPrimitiveText);

finalization
  PrimitiveClassList.Free;

end.
