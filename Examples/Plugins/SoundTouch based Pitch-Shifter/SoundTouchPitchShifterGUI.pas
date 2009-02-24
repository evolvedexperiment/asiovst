unit SoundTouchPitchShifterGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, DAV_Common,
  DAV_VSTModule, DAV_GuiCommon, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmSoundTouchPitchShifter = class(TForm)
    DialSemitones: TGuiDial;
    LbSemitones: TGuiLabel;
    LbSemitoneValue: TGuiLabel;
    procedure DialSemitonesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateSemitones;
  end;

implementation

uses
  PngImage, SoundTouchPitchShifterDM, DAV_VSTModuleWithPrograms;

{$R *.DFM}

procedure TFmSoundTouchPitchShifter.DialSemitonesChange(Sender: TObject);
begin
 with TSoundTouchPitchShifterModule(Owner) do
  begin
   if Parameter[0] <> DialSemitones.Position
    then Parameter[0] := DialSemitones.Position
  end;
end;

procedure TFmSoundTouchPitchShifter.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  Line   : PRGB24Array;
  PngBmp : TPngObject;

begin
 // Create Background Image
 FBackgrounBitmap := TBitmap.Create;
 with FBackgrounBitmap do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
       s[0] := s[1];
       Line[x].B := round($0F + $0E * s[1]);;
       Line[x].G := round($12 + $0E * s[1]);;
       Line[x].R := round($13 + $0E * s[1]);;
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'SoundTouchKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialSemitones.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmSoundTouchPitchShifter.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmSoundTouchPitchShifter.UpdateSemitones;
var
  SemiTones : Integer;
begin
 with TSoundTouchPitchShifterModule(Owner) do
  begin
   if DialSemitones.Position <> Parameter[0]
    then DialSemitones.Position := Parameter[0];
   SemiTones := round(Parameter[0]);
   LbSemitoneValue.Caption := IntToStr(SemiTones) + ' : ' +
     IntToStr(round(100 * (Parameter[0] - SemiTones)));
  end;
end;

end.
