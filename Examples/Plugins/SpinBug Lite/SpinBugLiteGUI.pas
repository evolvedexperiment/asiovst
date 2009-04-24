unit SpinBugLiteGUI;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils,
  Classes, Forms, Controls, ExtCtrls, StdCtrls, Graphics, DAV_Common,
  DAV_VSTModule, DAV_GuiSelectBox, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TFmSpinBugLite = class(TForm)
    DialLFO: TGuiDial;
    Label1: TLabel;
    LbLFOSpeed: TLabel;
    LbLFOSpeedValue: TLabel;
    LbTitle: TGuiLabel;
    LbTitleShadow: TGuiLabel;
    LbType: TLabel;
    SelectColour: TGuiSelectBox;
    SelectType: TGuiSelectBox;
    procedure FormCreate(Sender : TObject);
    procedure FormPaint(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure DialLFOChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SelectTypeChange(Sender: TObject);
    procedure SelectColourChange(Sender: TObject);
  private
    FBackground   : TBitmap;
  public
    procedure UpdateLFO;
    procedure UpdateType;
    procedure UpdateColour;
  end;

function GetProcessTypeCaption(Nr: Integer) : string;
function GetColourCaption(Nr: Integer) : string;

implementation

{$R *.dfm}

uses
  SpinBugLiteModule, Math, Types, DAV_VSTModuleWithPrograms;

function GetProcessTypeCaption(Nr : Integer) : string;
begin
 case Nr of
  0 : Result := 'illegal';
  1 : Result := 'stereo a';
  2 : Result := 'stereo b';
  3 : Result := 'stereo c';
  4 : Result := 'stereo d';
  5 : Result := 'mono';
  6 : Result := 'mono l';
  7 : Result := 'mono r';
  8 : Result := 'm+s';
  9 : Result := 'special';
  10 : Result := 'old one';
 end;
end;

function GetColourCaption(Nr : Integer) : string;
begin
  case Nr of
    1 : Result := 'rough';
    4 : Result := 'firm';
    8 : Result := 'medium';
   16 : Result := 'soft';
   32 : Result := 'smooth';
   else Result := 'untitled';
  end;
end;

procedure TFmSpinBugLite.FormCreate(Sender : TObject);
var
  c : Integer;
begin
 FBackground := TBitmap.Create;
 with FBackground do
  begin
   Width := Self.Width;
   Height := Self.Height;
   PixelFormat := pf24bit;
   Canvas.Brush.Color := Self.Color;
   Canvas.FillRect(ClientRect);
   Canvas.Font.Color := $00021076;
   for c := 0 to 128
    do Canvas.TextOut(Random(Width), Random(Height), 'Lite' );
  end;
end;

procedure TFmSpinBugLite.FormDestroy(Sender : TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmSpinBugLite.FormPaint(Sender : TObject);
begin
 Canvas.Draw(0, 0, FBackground);
end;

procedure TFmSpinBugLite.FormResize(Sender: TObject);
begin
 FBackground.Width := Width;
 FBackground.Height := Height;
end;

procedure TFmSpinBugLite.FormShow(Sender: TObject);
begin
 UpdateColour;
 UpdateType;
 UpdateLFO;
end;

procedure TFmSpinBugLite.SelectColourChange(Sender: TObject);
begin
 with Owner as TSpinBugLiteModule do
  begin
   case SelectColour.ItemIndex of
    0 : Parameter[0] :=  1;
    1 : Parameter[0] :=  4;
    2 : Parameter[0] :=  8;
    3 : Parameter[0] := 16;
    4 : Parameter[0] := 32;
   end;
  end;
end;

procedure TFmSpinBugLite.SelectTypeChange(Sender: TObject);
begin
 with Owner as TSpinBugLiteModule do
  begin
   Parameter[1] := SelectType.ItemIndex + 1;
  end;
end;

procedure TFmSpinBugLite.DialLFOChange(Sender: TObject);
begin
 with Owner as TSpinBugLiteModule do
  begin
   Parameter[2] := DialLFO.Position;
  end;
end;

procedure TFmSpinBugLite.UpdateColour;
begin
 with Owner as TSpinBugLiteModule do
  begin
   case round(Parameter[0]) of
     1 : SelectColour.ItemIndex := 0;
     4 : SelectColour.ItemIndex := 1;
     8 : SelectColour.ItemIndex := 2;
    16 : SelectColour.ItemIndex := 3;
    32 : SelectColour.ItemIndex := 4;
   end;
  end;
end;

procedure TFmSpinBugLite.UpdateLFO;
begin
 with Owner as TSpinBugLiteModule do
  begin
   LbLFOSpeedValue.Caption := FloatToStrF(Parameter[2], ffFixed, 5, 2) + ' hz';
  end;
end;

procedure TFmSpinBugLite.UpdateType;
begin
 with Owner as TSpinBugLiteModule do
  begin
   SelectType.ItemIndex := round(Parameter[1]) - 1;
  end;
end;

end.
