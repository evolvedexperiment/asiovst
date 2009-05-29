unit KGmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, DAV_VSTHost, ExtCtrls;

type
  TFmKnobGrabber = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    MIOpen: TMenuItem;
    VstHost: TVstHost;
    OpenDialog: TOpenDialog;
    MIGrabKnobs: TMenuItem;
    PnGUI: TPanel;
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MIGrabKnobsClick(Sender: TObject);
  private
    FFileName : TFileName;
    procedure LoadVstPlugin(FileName: TFileName);
    function FindKnobBounds(ParameterNo: Integer): TRect;
    procedure GrabKnob(ParameterNo: Integer; rct: TRect; FileName: string);
  public
  end;

var
  FmKnobGrabber: TFmKnobGrabber;

implementation

{$R *.dfm}

uses
  Types, FileCtrl, PngImage, DAV_GuiCommon;

procedure TFmKnobGrabber.FormCreate(Sender: TObject);
begin
 if FileExists(ParamStr(1))
  then LoadVstPlugin(ParamStr(1));
end;

procedure TFmKnobGrabber.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmKnobGrabber.MIGrabKnobsClick(Sender: TObject);
var
  Dir : string;
  rct : TRect;
  Prs : Integer;
  PNm : string;
begin
(*
 Dir := ExtractFileDir(FFileName);
 SelectDirectory('Select a directory', '', Dir);
*)
 PNm := ExtractFileName(FFileName);
 if Pos('.', PNm) > 0
  then PNm := Dir + Copy(PNm, 1, Pos('.', PNm) - 1);

 with VstHost[0] do
  for Prs := 0 to numParams - 1 do
   begin
    rct := FindKnobBounds(Prs);
    if (rct.Right - rct.Left > 0) and (rct.Bottom - rct.Top > 0)
     then GrabKnob(Prs, rct, PNm + IntToStr(Prs) + '.png');
   end;
end;

function TFmKnobGrabber.FindKnobBounds(ParameterNo: Integer): TRect;
var
  Bmp   : array [0..2] of TBitmap;
  Param : Single;
  rct   : TRect;
  x, y  : Integer;
  Scln  : PIntegerArray;
begin
 with VstHost[0] do
  if Active then
   begin
    Bmp[0] := TBitmap.Create;
    Bmp[1] := TBitmap.Create;
    Bmp[2] := TBitmap.Create;
    try
     // set dimensions
     rct := GetRect;
     Bmp[0].Width := rct.Right - rct.Left;
     Bmp[0].Height := rct.Bottom - rct.Top;
     Bmp[2].Assign(Bmp[0]);
     Bmp[2].Canvas.Brush.Color := clBlack;
     Bmp[2].Canvas.FillRect(ClientRect);
     Bmp[2].Canvas.CopyMode := cmSrcPaint;

     // render basic image
     Parameter[ParameterNo] := 0;
     Application.ProcessMessages;
     RenderEditorToBitmap(Bmp[0]);
     Application.ProcessMessages;

     Bmp[1].Assign(Bmp[0]);
     Bmp[1].Canvas.CopyMode := cmSrcInvert;

     Param := 0.001;
     while Param <= 1 do
      begin
       Parameter[ParameterNo] := Param;
       EditIdle;
       Idle;
       Application.ProcessMessages;
       RenderEditorToBitmap(Bmp[1]);
       Bmp[1].Canvas.Draw(0, 0, Bmp[0]);
       Bmp[2].Canvas.Draw(0, 0, Bmp[1]);
       Param := Param + 0.001;
      end;

     Bmp[2].PixelFormat := pf32bit;
     result := Rect(Bmp[2].Width, Bmp[2].Height, 0, 0);
     for y := 0 to Bmp[2].Height - 1 do
      begin
       Scln := PIntegerArray(Bmp[2].ScanLine[y]);
       for x := 0 to Bmp[2].Width - 1 do
        if Scln^[x] <> clBlack then
         begin
          if x < Result.Left then Result.Left := x;
          if x > Result.Right then Result.Right := x;
          if y < Result.Top then Result.Top := y;
          if y > Result.Bottom then Result.Bottom := y;
         end;
      end;

     Bmp[2].Canvas.Brush.Color := clWhite;
     Bmp[2].Canvas.FrameRect(result);
//     Image.Canvas.Draw(0, 0, Bmp[2]);
     result.Right := result.Right + 1;
     result.Bottom := result.Bottom + 1;
    finally
     FreeAndNil(Bmp[0]);
     FreeAndNil(Bmp[1]);
     FreeAndNil(Bmp[2]);
    end;
   end else result := Rect(0, 0, 0, 0);
end;

procedure TFmKnobGrabber.GrabKnob(ParameterNo: Integer; rct: TRect; FileName: string);
var
  Png   : TPNGObject;
  Bmp   : array [0..2] of TBitmap;
  Param : Single;
  vrct  : TRect;
  x, y  : Integer;
  Scln  : array [0..1] of PIntegerArray;

label
  next;
begin
 Png := TPNGObject.Create;
 try
  with VstHost[0] do
   if Active then
    begin
     Bmp[0] := TBitmap.Create;
     Bmp[1] := TBitmap.Create;
     Bmp[2] := TBitmap.Create;
     try
      // set dimensions
      vrct := GetRect;
      Bmp[0].Width := vrct.Right - vrct.Left;
      Bmp[0].Height := vrct.Bottom - vrct.Top;

      Bmp[1].Width := rct.Right - rct.Left;
      Bmp[1].Height := rct.Bottom - rct.Top;
      Bmp[1].PixelFormat := pf32bit;

      Bmp[2].Width := rct.Right - rct.Left;
      Bmp[2].Height := rct.Bottom - rct.Top;
      Bmp[2].PixelFormat := pf32bit;

//      Png.SetSize(rct.Right - rct.Left, rct.Bottom - rct.Top);

      // render basic image
      Parameter[ParameterNo] := 0;
      Application.ProcessMessages;
      RenderEditorToBitmap(Bmp[0]);

      Application.ProcessMessages;
      Bmp[1].Canvas.CopyRect(Rect(0, 0, rct.Right - rct.Left, rct.Bottom - rct.Top), Bmp[0].Canvas, rct);
      Png.Assign(Bmp[1]);

      Param := 0.001;
      while Param <= 1 do
       begin
        Parameter[ParameterNo] := Param;
        EditIdle;
        Idle;
        Application.ProcessMessages;
        RenderEditorToBitmap(Bmp[0]);
        Application.ProcessMessages;
        Bmp[2].Canvas.CopyRect(Rect(0, 0, rct.Right - rct.Left, rct.Bottom - rct.Top), Bmp[0].Canvas, rct);
        for y := 0 to Bmp[2].Height - 1 do
         begin
          Scln[0] := PIntegerArray(Bmp[2].ScanLine[y]);
          Scln[1] := PIntegerArray(Bmp[1].ScanLine[y]);
          for x := 0 to Bmp[2].Width - 1 do
           if Scln[0]^[x] <> Scln[1]^[x] then
            begin
             Png.SetSize(Png.Width + (rct.Right - rct.Left), Png.Height);
             Png.Canvas.CopyRect(rect(Png.Width - (rct.Right - rct.Left), 0, Png.Width, Png.Height),
               Bmp[2].Canvas, Rect(0, 0, rct.Right - rct.Left, rct.Bottom - rct.Top));
             Bmp[1].Assign(Bmp[2]);

             Application.ProcessMessages;
             goto next;
            end;
         end;

next:
        Param := Param + 0.001;
       end;

     finally
      FreeAndNil(Bmp[0]);
      FreeAndNil(Bmp[1]);
      FreeAndNil(Bmp[2]);
     end;
     Png.SaveToFile(FileName);
    end;
 finally
  FreeAndNil(Png);
 end;
end;

procedure TFmKnobGrabber.MIOpenClick(Sender: TObject);
begin
 with OpenDialog do
  if Execute
   then LoadVstPlugin(FileName);
end;

procedure TFmKnobGrabber.LoadVstPlugin(FileName: TFileName);
var
  rct : TRect;
begin
 with VstHost[0] do
  begin
   LoadFromFile(FileName);
   Active := True;
   ShowEdit(PnGUI);
   rct := GetRect;
   ClientWidth := rct.Right - rct.Left;
   ClientHeight := (rct.Bottom - rct.Top);
//   Image.Height := (rct.Bottom - rct.Top);
   FFileName := FileName;
  end;
end;

end.
