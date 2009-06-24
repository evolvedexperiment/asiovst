program PresetToExcel;

uses
  SysUtils,
  Classes,
  Dialogs,
  nExcel,   // you need to have the Native Excel 2 Suite installed to compile
  DAV_VSTHost;

resourcestring
  RCStrUnknownFileExtensi = 'Unknown file extension';

{$R *.res}

procedure ConvertVSTPresetToExcel(Filename: TFileName);
var
  Prgs, Params  : Integer;
  VstHost       : TVstHost;
  XlsFileName   : TFileName;
  FileExtention : string;
begin
 VstHost := TVstHost.Create(nil);
 try
  with VstHost.VstPlugIns.Add do
   try
    LoadFromFile(FileName);
    Active := True;

    // get output filename
    XlsFileName := ParamStr(2);
    if XlsFileName = '' then
     with TSaveDialog.Create(nil) do
      try
       DefaultExt := '.xls';
       Filter := 'Excel (*.xls)|*.xls|CSV (*.csv)|*.csv|RTF (*.rtf)|*.rtf|HTML (*.html)|*.html|Text (*.txt)|*.txt';
       Title := 'Save as...';
       if Execute then XlsFileName := FileName;
      finally
       Free;
      end;

    if XlsFileName = '' then exit;

    with TXLSWorkbook.Create do
     try
      // add new sheet
      with Sheets.Add do
       begin
        Name := EffectName;

        // programs title
        with RCRange[1, 3, 1, 2 + numPrograms] do
         begin
          Merge(False);
          Font.Size := 14;
          Value := 'Programs';
          Borders.ColorIndex := xlColorIndexAutomatic;
          HorizontalAlignment := xlHAlignCenter;
          VerticalAlignment := xlVAlignCenter;
          Borders[xlAround].Weight := xlMedium;
         end;

        // parameters title
        with RCRange[3, 1, 2 + numParams, 1] do
         begin
          Merge(False);
          Font.Size := 14;
          Value := 'Parameters';
          Borders.ColorIndex := xlColorIndexAutomatic;
          HorizontalAlignment := xlHAlignCenter;
          VerticalAlignment := xlVAlignCenter;
          ColumnWidth := 3;
          Orientation := 90;
          Borders[xlAround].Weight := xlMedium;
         end;

        // format programs
        with RCRange[2, 3, 2, 2 + numPrograms] do
         begin
          Font.Size := 10;
          Font.Bold := True;
          Borders.ColorIndex := xlColorIndexAutomatic;
          Borders[xlAround].Weight := xlMedium;
         end;

        // format parameters
        with RCRange[3, 2, 2 + numParams, 2] do
         begin
          Font.Size := 10;
          Font.Bold := True;
          ColumnWidth := 12;
          Borders.ColorIndex := xlColorIndexAutomatic;
          Borders[xlAround].Weight := xlMedium;
         end;

        // format parameter names
        for Params := 0 to numParams - 1 do
         begin
          if ParameterLabel[Params] <> ''
           then Cells.Item[3 + Params, 2].Value :=
            ParameterName[Params] + ' [' + ParameterLabel[Params] + ']'
           else Cells.Item[3 + Params, 2].Value := ParameterName[Params];
         end;

        // format parameter values
        with RCRange[3, 3, 2 + numParams, 2 + numPrograms] do
         begin
          Borders.ColorIndex := xlColorIndexAutomatic;
          Borders[xlAround].Weight := xlMedium;
         end;

        // parameter values
        for Prgs := 0 to numPrograms - 1 do
         begin
          ProgramNr := Prgs;
          Cells.Item[2, 3 + Prgs].Value := ProgramName;
          Cells.Item[2, 3 + Prgs].AutoFit(True);

          for Params := 0 to numParams - 1 do
           try
            Cells.Item[3 + Params, 3 + Prgs].Value := ParameterDisplay[Params];
           except
            Cells.Item[3 + Params, 3 + Prgs].Value := 'Error!';
           end;
         end;

        // autofit parameter values
        RCRange[2, 3, 2 + numParams, 2 + numPrograms].AutoFit(True);

        // autofit parameter names
        RCRange[3, 2, 2 + numParams, 2].AutoFit(True);
       end;

      FileExtention := LowerCase(ExtractFileExt(XlsFileName));
      if FileExtention = '.xls' then SaveAs(XlsFileName) else
      if FileExtention = '.csv' then SaveAs(XlsFileName, xlCSV) else
      if FileExtention = '.rtf' then SaveAs(XlsFileName, xlRTF) else
      if FileExtention = '.html' then SaveAs(XlsFileName, xlHTML) else
      if FileExtention = '.txt' then SaveAs(XlsFileName, xlText)
       else raise Exception.Create(RCStrUnknownFileExtensi);
     finally
      Free;
     end;
    Active := False;
   finally
    VstHost.VstPlugIns.Clear;
   end;
 finally
  FreeAndNil(VstHost);
 end;
end;

begin
 if CheckValidVstPlugin(ParamStr(1))
  then ConvertVSTPresetToExcel(ParamStr(1))
  else
   with TOpenDialog.Create(nil) do
    try
     DefaultExt := '.dll';
     Filter := 'VST Plugin (*.dll)|*.dll';
     Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
     Title := 'Load VST Plugin';
     if Execute
      then ConvertVSTPresetToExcel(Filename);
    finally
     Free;
    end;
end.
