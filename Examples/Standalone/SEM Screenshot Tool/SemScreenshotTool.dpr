program SemScreenshotTool;

{$APPTYPE CONSOLE}

uses
  Windows, Classes, Controls, Forms, Graphics, SysUtils, FileCtrl, PngImage,
  DAV_SECommon, DAV_SEModule, DAV_SEHost;

procedure RenderScreenshot(FileName: TFileName);
var
  PartNr  : Integer;
  PinNr   : Integer;
  Pin     : TSEPinProperties;
  Pins    : array of TSEPinProperties;
  Bitmap  : TBitmap;
  Png     : TPNGObject;
  TpPos   : Integer;
begin
 with TSEHost.Create(nil) do
  try
   with HostedSEModules.Add do
    try
     // check VST plugin is a valid plugin
//     if not CheckValidPlugin(FileName) then exit;

     Writeln('Loading: ' + FileName);

     // load from file
     LoadFromFile(FileName);

     for PartNr := 0 to PartCount - 1 do
      with Part[PartNr] do
       begin
        Instantiation;
//        Open;
        try
         Writeln('Capturing: ' + Name);

         // create form for GUI rendering
         Bitmap := TBitmap.Create;
         with Bitmap, Canvas do
          try
           Canvas.Font.Name := 'Arial';
           Canvas.Font.Color := clWhite;
           Canvas.Font.Size := 8;
           Canvas.Brush.Color := clNavy;
           Canvas.Pen.Color := clNavy;

           Width := 4 + TextWidth(Properties.Name);
           Height := TextHeight(Properties.Name) + 4;

           FillRect(ClipRect);

           // calculate width and enumerate modules
           Canvas.Font.Size := 7;
           SetLength(Pins, 0);
           while GetPinProperties(Length(Pins), Pin) = True do
            begin
             SetLength(Pins, Length(Pins) + 1);
             Pins[Length(Pins) - 1] := Pin;
             if TextWidth(Pin.Name) + 6 > Width
              then Width := TextWidth(Pin.Name) + 6;

             Height := Height + TextHeight(Pin.Name) + 2;
            end;

           Canvas.Font.Size := 8;
           TextOut((Width - TextWidth(Properties.Name)) div 2, 1, Properties.Name);

           Canvas.Brush.Color := clSilver;
           FillRect(Rect(0, TextHeight(Properties.Name) + 2, Width, Height));
           Canvas.Font.Size := 7;

           for PinNr := 0 to Length(Pins) - 1 do
            begin
             case Pins[PinNr].Datatype of
              dtFSample : Canvas.Font.Color := clNavy;
              dtSingle  : Canvas.Font.Color := clGreen;
              dtInteger : Canvas.Font.Color := clOlive;
              dtEnum    : Canvas.Font.Color := clOlive;
              else Canvas.Font.Color := clBlack;
             end;

             TpPos := (PinNr + 1) * (TextHeight(Properties.Name) + 2) + 2;
             Canvas.Pen.Color := Canvas.Font.Color;

             case Pins[PinNr].Direction of
              drIn : begin
                      MoveTo(0, TpPos + 5);
                      LineTo(4, TpPos + 5);
                      TextOut(6, TpPos, Pins[PinNr].Name);
                     end;
              drParameter : TextOut((Width - TextWidth(Pins[PinNr].Name)) div 2, TpPos, '(' + Pins[PinNr].Name + ')');
              drOut : begin
                       TextOut(Width - 6 - TextWidth(Pins[PinNr].Name), TpPos, Pins[PinNr].Name);
                       MoveTo(Width - 4, TpPos + 5);
                       LineTo(Width, TpPos + 5);
                      end;
             end;
            end;

           Pen.Color := clGray;
           Brush.Color := clGray;
           FrameRect(ClipRect);

           // copy to png
           Png := TPNGObject.Create;
           with Png do
            try
             Png.Assign(Bitmap);
             Png.SaveToFile(FileName + ' - Module ' + IntToStr(PartNr + 1) + '.png');
            finally
             FreeAndNil(Png);
            end;
          finally
           FreeAndNil(Bitmap);
          end;
        finally
//         Close;
        end;
       end;

    except
    end;
  finally
   Free;
  end;
end;

var
  Dir : string;
  SR  : TSearchRec;
begin
 Writeln('Delphi ASIO & VST Project - SEM Screenshot Tool');

 if FileExists(ParamStr(1))
  then RenderScreenshot(ParamStr(1))
  else
   if FindFirst('*.SEM', faAnyFile, SR) = 0 then
    try
     repeat
      RenderScreenshot(SR.Name);
     until FindNext(SR) <> 0;
    finally
     // Must free up resources used by these successful finds
     FindClose(SR);
    end
   else
    begin
     Writeln('Wrong syntax!');
     Writeln('Add parameter or move this tool into a directory containing VST plugins');

     Dir := ExtractFileDir(ParamStr(0));
     SelectDirectory('Select Directory', '', Dir);
     if FindFirst(Dir + '\' + '*.SEM', faAnyFile, SR) = 0 then
      try
       repeat
        RenderScreenshot(Dir + '\' + SR.Name);
       until FindNext(SR) <> 0;
      finally
       // Must free up resources used by these successful finds
       FindClose(SR);
      end
    end;
end.
