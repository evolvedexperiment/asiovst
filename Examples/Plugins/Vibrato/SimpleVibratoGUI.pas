unit SimpleVibratoGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TFmSimpleVibrato = class(TForm)
    DialDepth: TGuiDial;
    DialSpeed: TGuiDial;
    LbDepth: TGuiLabel;
    LbDepthValue: TGuiLabel;
    LbSpeed: TGuiLabel;
    LbSpeedValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialSpeedChange(Sender: TObject);
    procedure DialDepthChange(Sender: TObject);
  public
    procedure UpdateDepth;
    procedure UpdateSpeed;
  end;

implementation

{$R *.DFM}

uses
  Math, PngImage, DAV_VSTModuleWithPrograms, SimpleVibratoDM;

procedure TFmSimpleVibrato.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'VibratoKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialSpeed.DialBitmap.Assign(PngBmp);
   DialDepth.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmSimpleVibrato.FormShow(Sender: TObject);
begin
 UpdateDepth;
 UpdateSpeed;
end;

procedure TFmSimpleVibrato.DialDepthChange(Sender: TObject);
begin
 with TSimpleVibratoModule(Owner) do
  begin
   if Parameter[1] <> DialDepth.Position
    then Parameter[1] := DialDepth.Position;
  end;
end;

procedure TFmSimpleVibrato.DialSpeedChange(Sender: TObject);
begin
 with TSimpleVibratoModule(Owner) do
  begin
   if Parameter[0] <> DialSpeed.Position
    then Parameter[0] := DialSpeed.Position;
  end;
end;

procedure TFmSimpleVibrato.UpdateDepth;
var
  Depth : Single;
begin
 with TSimpleVibratoModule(Owner) do
  begin
   Depth := Parameter[1];
   if DialDepth.Position <> Depth
    then DialDepth.Position := Depth;
   LbDepthValue.Caption := FloatToStrF(RoundTo(Depth, -1), ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmSimpleVibrato.UpdateSpeed;
var
  Speed : Single;
begin
 with TSimpleVibratoModule(Owner) do
  begin
   Speed := Parameter[0];
   if DialSpeed.Position <> Speed
    then DialSpeed.Position := Speed;
   LbSpeedValue.Caption := FloatToStrF(RoundTo(Speed, -2), ffGeneral, 2, 2) + ' Hz';
  end;
end;

end.
