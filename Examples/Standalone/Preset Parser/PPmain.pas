unit PPmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, DAV_VSTHost;

type
  TFmPresetParser = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    MIOpenVSTPlugin: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    MISaveAs: TMenuItem;
    Memo: TMemo;
    VstHost: TVstHost;
    MISettings: TMenuItem;
    MIDetectBufferOverflows: TMenuItem;
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenVSTPluginClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MIDetectBufferOverflowsClick(Sender: TObject);
  end;

var
  FmPresetParser: TFmPresetParser;

implementation

{$R *.dfm}

procedure TFmPresetParser.MIDetectBufferOverflowsClick(Sender: TObject);
begin
 MIDetectBufferOverflows.Checked := not MIDetectBufferOverflows.Checked;
 VSTHost.CheckStringLengths := MIDetectBufferOverflows.Checked;
end;

procedure TFmPresetParser.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmPresetParser.MIOpenVSTPluginClick(Sender: TObject);
var
  prgs, params : Integer;
  str          : string;
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Load VST Plugin';
   if Execute then
    begin
     with VstHost.VstPlugIns.Add do
      try
       LoadFromFile(FileName);
       Active := True;
       Memo.Lines.Clear;
       for prgs := 0 to numPrograms - 1 do
        begin
         ProgramNr := prgs;
         str := 'Program: ' + ProgramName;
         Memo.Lines.Add(str);
         FillChar(str[1], Length(str), '-');
         Memo.Lines.Add(str);
         for params := 0 to numParams - 1 do
          try
           Memo.Lines.Add('Parameter ' + IntToStr(params +  1) + ': ' +
             ParameterName[params] + ' = ' + ParameterDisplay[params] + ' ' +
             ParameterLabel[params]);
          except
           Memo.Lines.Add('Parameter ' + IntToStr(params +  1) + ': Error!');  
          end;
         Memo.Lines.Add('');
        end;
       Active := False;
      finally
       VstHost.VstPlugIns.Clear;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmPresetParser.MISaveAsClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := '.dll';
   Filter := 'Text (*.txt)|*.txt';
   Title := 'Save as...';
   if Execute
    then Memo.Lines.SaveToFile(FileName);
  finally
   Free;
  end;
end;

end.
