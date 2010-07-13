unit DAV_TestGuiPngDisplay;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PngImage, DAV_GuiPng;

type
  TFmDisplay = class(TForm)
    BtNo: TButton;
    BtYes: TButton;
    Image: TImage;
    LbQuestion: TLabel;
    LbRenderer: TLabel;
    RbInternal: TRadioButton;
    RbPngImage: TRadioButton;
    procedure RbInternalClick(Sender: TObject);
    procedure RbPngImageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FReference : TBitmap;
    FInternal  : TBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Reference: TBitmap read FReference write FReference;
    property Internal: TBitmap read FInternal write FInternal;
  end;

var
  FmDisplay: TFmDisplay;

implementation

{$R *.dfm}

constructor TFmDisplay.Create(AOwner: TComponent);
begin
 inherited;
 FInternal := TBitmap.Create;
 FReference := TBitmap.Create;
end;

destructor TFmDisplay.Destroy;
begin
 FreeAndNil(FInternal);
 FreeAndNil(FReference);
 inherited;
end;

procedure TFmDisplay.FormShow(Sender: TObject);
begin
 Image.Canvas.Draw(0, 0, FInternal);
end;

procedure TFmDisplay.RbInternalClick(Sender: TObject);
begin
 Image.Canvas.Draw(0, 0, FInternal);
end;

procedure TFmDisplay.RbPngImageClick(Sender: TObject);
begin
 Image.Canvas.Draw(0, 0, FReference);
end;

end.
