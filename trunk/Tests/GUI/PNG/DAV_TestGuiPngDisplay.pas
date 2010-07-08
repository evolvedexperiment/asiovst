unit DAV_TestGuiPngDisplay;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PngImage, DAV_GuiPng;

type
  TFmDisplay = class(TForm)
    Image: TImage;
    BtNo: TButton;
    BtYes: TButton;
    LbQuestion: TLabel;
    RbPngImage: TRadioButton;
    LbRenderer: TLabel;
    RbInternal: TRadioButton;
    procedure RbInternalClick(Sender: TObject);
    procedure RbPngImageClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FReference : TBitmap;
    FInternal  : TBitmap;
  public
    constructor Create(AOwner: TComponent); override;
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

procedure TFmDisplay.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FInternal);
 FreeAndNil(FReference);
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
