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
    FModuleManager : TModularManager;
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure RenderBuffer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

  TGuiModular = class(TCustomGuiModular)
  published
    property Color;
  end;

implementation

{ TCustomGuiModular }

constructor TCustomGuiModular.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle        := ControlStyle + [csOpaque];
  FBuffer             := TBitmap.Create;
  FBuffer.PixelFormat := pf24bit;
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
begin
 with FBuffer, Canvas do
  if (Width > 0) and (Height > 0) then
   begin
    Brush.Color := Self.Color;
    FillRect(ClipRect);
   end;
end;

procedure TCustomGuiModular.Resize;
begin
 inherited;
 FBuffer.Width  := Width;
 FBuffer.Height := Height;
end;

end.
