unit VOLGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  StdCtrls, Controls, Dialogs;

type
  TFmVOL = class(TForm)
    MOpcodeLog: TMemo;
    BtClear: TButton;
    BtUpdate: TButton;
    CBAutoUpdates: TCheckBox;
    BtSaveAs: TButton;
    SaveDialog: TSaveDialog;
    Sb1: TScrollBar;
    LbParameter: TLabel;
    LbParameterValue: TLabel;
    procedure FormShow(Sender: TObject);
    procedure BtUpdateClick(Sender: TObject);
    procedure BtClearClick(Sender: TObject);
    procedure BtSaveAsClick(Sender: TObject);
    procedure Sb1Change(Sender: TObject);
  public
    procedure UpdateParameter;
  end;

implementation

{$R *.DFM}

uses
  VOLDM;

procedure TFmVOL.BtClearClick(Sender: TObject);
begin
 with TVOLDataModule(Owner) do
  begin
   OpcodeLog.Clear;
   MOpcodeLog.Lines.Assign(OpcodeLog);
  end;
end;

procedure TFmVOL.BtUpdateClick(Sender: TObject);
begin
 with TVOLDataModule(Owner) do
  begin
   MOpcodeLog.Lines.Assign(OpcodeLog);
  end;
end;

procedure TFmVOL.FormShow(Sender: TObject);
begin
 UpdateParameter;
end;

procedure TFmVOL.Sb1Change(Sender: TObject);
begin
 with TVOLDataModule(Owner) do
  begin
   Parameter[1] := 0.01 * Sb1.Position;
  end;
end;

procedure TFmVOL.UpdateParameter;
begin
 with TVOLDataModule(Owner) do
  begin
   if Sb1.Position <> round(100 * Parameter[1])
    then Sb1.Position := round(100 * Parameter[1]);
   LbParameterValue.Caption := FloatToStrF(Parameter[1], ffGeneral, 3, 3);
  end;
end;

procedure TFmVOL.BtSaveAsClick(Sender: TObject);
var
  FormatSettings : TFormatSettings;
begin
 with SaveDialog do
  begin
   GetLocaleFormatSettings(SysLocale.DefaultLCID, FormatSettings);
   FormatSettings.ShortDateFormat := 'yyyymmdd';
   FormatSettings.LongTimeFormat := 'yyyymmdd';
   FormatSettings.ShortTimeFormat := 'hhmmss';
   FormatSettings.LongTimeFormat := 'hhmmsss';
   FileName := 'OpcodeLog - ' + DateTimeToStr(Now, FormatSettings) + '.log';
   if Execute then
    begin
     MOpcodeLog.Lines.SaveToFile(FileName);
    end;
  end;
end;

end.