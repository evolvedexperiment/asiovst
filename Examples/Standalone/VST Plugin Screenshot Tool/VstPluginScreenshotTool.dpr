program VstPluginScreenshotTool;

{$APPTYPE CONSOLE}

uses
  Windows, Classes, Controls, Forms, Graphics, SysUtils, FileCtrl, PngImage,
  DAV_VstHost;

procedure RenderScreenshot(FileName: TFileName);
var
  Form    : TForm;
  Bitmap  : TBitmap;
  Png     : TPNGObject;
  Rct     : TRect;
begin
 with TVstHost.Create(nil) do
  try
   ProductString := 'Vst Plugin Screenshot Tool';
   VendorString := 'Delphi ASIO & VST Project';

   with VstPlugIns.Add do
    try
     // check VST plugin is a valid plugin
     if not CheckValidPlugin(FileName) then exit;

     Writeln('Capturing: ' + FileName);

     // load from file
     LoadFromFile(FileName);

     // activate VST plugin
     Active := True;

     // create form for GUI rendering
     Form := TForm.Create(nil);
     try
      if FileExists(ParamStr(2))
       then LoadPreset(ParamStr(2)) else
      if FileExists(FileName + '.fxp')
       then LoadPreset(FileName + '.fxp');
      Form.BorderStyle := bsNone;
      Form.Position := poScreenCenter;
      ShowEdit(Form);
      Rct := GetRect;
      Form.ClientWidth := Rct.Right - Rct.Left;
      Form.ClientHeight := Rct.Bottom - Rct.Top;
      Form.Visible := True;
//      Form.Repaint;
      Application.ProcessMessages;
      Bitmap := TBitmap.Create;
      try
       RenderEditorToBitmap(Bitmap);
       Png := TPNGObject.Create;
       with Png do
        try
         Png.Assign(Bitmap);
         Png.SaveToFile(FileName + '.png');
        finally
         FreeAndNil(Png);
        end;
      finally
       FreeAndNil(Bitmap);
      end;
     finally
      CloseEdit;
      FreeAndNil(Form);
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
 Writeln('Delphi ASIO & VST Project - Vst Plugin Screenshot Tool');

 if FileExists(ParamStr(1))
  then RenderScreenshot(ParamStr(1))
  else
   if FindFirst('*.dll', faAnyFile, SR) = 0 then
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
     if FindFirst(Dir + '\' + '*.dll', faAnyFile, SR) = 0 then
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
