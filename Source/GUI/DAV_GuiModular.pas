unit DAV_GuiModular;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls,
  DAV_GuiBaseControl, DAV_ModularManager;

type
  TCustomGuiModular = class(TCustomControl)
  private
    FBuffer        : TBitmap;
    FPinSize       : Integer;
    FModuleManager : TModularManager;
    procedure SetModuleManager(const Value: TModularManager);
    procedure SetPinSize(const Value: Integer);
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure Paint; override;

    procedure RenderBuffer; virtual;
(*
    procedure RenderInputPins; virtual;
    procedure RenderOutputPins; virtual;
*)

    procedure PinSizeChanged; virtual;
    procedure ModuleManagerChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PinSize: Integer read FPinSize write SetPinSize;
    property ModuleManager: TModularManager read FModuleManager write SetModuleManager;
  end;

  TGuiModular = class(TCustomGuiModular)
  published
    property Align;
    property Anchors;
    property Color;
    property Enabled;
    property Font;
    property ModuleManager;
    property OnClick;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property Visible;
  end;

implementation

{ TCustomGuiModular }

constructor TCustomGuiModular.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 ControlStyle        := ControlStyle + [csOpaque];
 FBuffer             := TBitmap.Create;
 FBuffer.PixelFormat := pf24bit;
 FPinSize            := 8;
end;

destructor TCustomGuiModular.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited;
end;

procedure TCustomGuiModular.Loaded;
begin
 inherited;
 FBuffer.Width  := Width;
 FBuffer.Height := Height;
end;

procedure TCustomGuiModular.Paint;
begin
 RenderBuffer;
 Canvas.Draw(0, 0, FBuffer);
 inherited;
end;

procedure TCustomGuiModular.RenderBuffer;
var
  Channel : Integer;
begin
 with FBuffer, Canvas do
  if (Width > 0) and (Height > 0) then
   begin
    Brush.Color := Self.Color;
    FillRect(ClipRect);

    // check whether a module manager is assigned
    if not assigned(FModuleManager) then Exit;

//    RenderInputPins;
   end;
end;

procedure TCustomGuiModular.Resize;
begin
 inherited;
 FBuffer.Width  := Width;
 FBuffer.Height := Height;
end;

procedure TCustomGuiModular.SetModuleManager(const Value: TModularManager);
begin
 if FModuleManager <> Value then
  begin
   FModuleManager := Value;
   ModuleManagerChanged;
  end;
end;

procedure TCustomGuiModular.SetPinSize(const Value: Integer);
begin
 if FPinSize <> Value then
  begin
   FPinSize := Value;
   PinSizeChanged;
  end;
end;

procedure TCustomGuiModular.PinSizeChanged;
begin
 // nothing yet in here
end;

procedure TCustomGuiModular.ModuleManagerChanged;
begin
 Invalidate;
end;

end.
