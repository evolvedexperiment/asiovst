program VstPluginScreenshotTool;

{$I DAV_Compiler.inc}

{$APPTYPE CONSOLE}

uses
{$IFNDEF FPC}
  PngImage, DAV_GuiPng,
{$ELSE}
  Interfaces, LazPng, imagesforlazarus,
{$ENDIF}
  Windows, Messages, Classes, Controls, Forms, Graphics, SysUtils, FileCtrl,
  DAV_VstEffect, DAV_VstHost, DAV_GuiPixelMap;

resourcestring
  RCStrProductString = 'Vst Plugin Screenshot Tool';
  RCStrVendorString = 'Delphi ASIO & VST Project';
  RCStrCapturing = 'Capturing';
  RCStrWrongSyntax = 'Wrong syntax!';
  RCStrAbout = 'Delphi ASIO & VST Project - Vst Plugin Screenshot Tool';

{-$DEFINE Alternative}

procedure RenderScreenshot(FileName: TFileName; OutputFileName: TFileName = '');
var
  Form      : TForm;
  Bitmap    : TBitmap;
  {$IFNDEF FPC}
  Png       : TPNGObject;
  {$ELSE}
  Png       : TPNGImage;
  {$ENDIF}
  Rct       : TRect;
begin
 with TVstHost.Create(nil) do
  try
   ProductString := RCStrProductString;
   VendorString := RCStrVendorString;

   with VstPlugIns.Add do
    try
     // check VST plugin is a valid plugin
     if not CheckValidPlugin(FileName) then exit;

     Writeln(RCStrCapturing + ': ' + FileName);

     // load from file
     LoadFromFile(FileName);

     // activate VST plugin
     Active := True;
     if not Active
      then raise Exception.Create('Plugin not active!');

     if not (effFlagsHasEditor in VstEffectPointer.EffectFlags)
      then raise Exception.Create('Plugin does not feature an editor!');

     // create form for GUI rendering
     Form := TForm.CreateNew(Application);
     try
      if FileExists(ParamStr(2))
       then LoadPreset(ParamStr(2)) else
      if FileExists(FileName + '.fxp')
       then LoadPreset(FileName + '.fxp');

      if ParamStr(3) <> '' then
       begin
        // add further parameters here (stream a sound file, etc...)
       end;

      ShowEdit(Form);

      Rct := GetRect;
      Form.BorderStyle := bsNone;
      Form.ClientWidth := Rct.Right - Rct.Left;
      Form.ClientHeight := Rct.Bottom - Rct.Top;
      Form.Left := -Form.ClientWidth;
      Form.Visible := True;
      Application.ProcessMessages;
      Bitmap := TBitmap.Create;
      try
       RenderEditorToBitmap(Bitmap);
       {$IFNDEF FPC}
       Png := TPNGObject.Create;
       {$ELSE}
       Png := TPNGImage.Create;
       {$ENDIF}
       with Png do
        try
         Png.Assign(Bitmap);
         if OutputFileName = ''
          then OutputFileName := ChangeFileExt(FileName, '.png');
         Png.SaveToFile(OutputFileName);
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
     on E:Exception do Writeln('Error: ' + E.Message);
    end;
  finally
   Free;
  end;
end;

var
  Dir : string;
  SR  : TSearchRec;

{$R *.res}

begin
 Writeln(RCStrAbout);

 if FileExists(ParamStr(1))
  then RenderScreenshot(ParamStr(1), ParamStr(2))
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
     Writeln(RCStrWrongSyntax);
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
