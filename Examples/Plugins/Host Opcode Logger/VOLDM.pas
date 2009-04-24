unit VOLDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTEffect,
  DAV_VSTModule;

type
  TVOLDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FOpcodeLog  : TStringList;
    FLastOpcode : TDispatcherOpcode;
  protected
    procedure HostCallDispatchEffect(const opcode: TDispatcherOpcode;
      const Index: Integer; const Value: Integer; const ptr: Pointer;
      const opt: Single); override;
  public
    property OpcodeLog: TStringList read FOpcodeLog;
  end;

implementation

{$R *.DFM}

uses
  VOLGUI;

procedure TVOLDataModule.HostCallDispatchEffect(const Opcode: TDispatcherOpcode;
  const Index, Value: Integer; const ptr: Pointer; const opt: Single);
var
  FormatSettings : TFormatSettings;
  ChunkName      : TChunkName;
begin
 inherited;
 case Opcode of
  effOpen,
  effEditClose,
  effStartProcess,
  effGetNumProgramCategories,
  effStopProcess   : FOpcodeLog.Add(Opcode2String(Opcode));
  effSetSampleRate : FOpcodeLog.Add(Opcode2String(Opcode) +
                       ' SampleRate: ' + FloatToStr(Single(Opt)));
  effSetBlockSize  : FOpcodeLog.Add(Opcode2String(Opcode) +
                       ' BlockSize: ' + IntToStr(Value));
  effVendorSpecific :
   begin
    ChunkName := TChunkName(Index);
    if (ChunkName[0] in ['0'..'z']) and
       (ChunkName[1] in ['0'..'z']) and
       (ChunkName[2] in ['0'..'z']) and
       (ChunkName[3] in ['0'..'z'])
     then FOpcodeLog.Add(Opcode2String(Opcode) +
            ' Chunkname: ' + ChunkName +
            ' Value: ' + IntToStr(Value) +
            ' Pointer: ' + IntToStr(Integer(ptr)) +
            ' Single: ' + FloatToStr(Single(Opt)))
     else FOpcodeLog.Add(Opcode2String(Opcode) +
            ' Index: ' + IntToStr(Index) +
            ' Value: ' + IntToStr(Value) +
            ' Pointer: ' + IntToStr(Integer(ptr)) +
            ' Single: ' + FloatToStr(Single(Opt)));
   end;
  effIdle:
    if FLastOpcode <> effIdle
     then FOpcodeLog.Add(Opcode2String(Opcode));
  effEditIdle:
    if FLastOpcode <> effEditIdle
     then FOpcodeLog.Add(Opcode2String(Opcode));
  effCanDo: FOpcodeLog.Add(Opcode2String(Opcode) + ' ''' +
                           StrPas(PChar(Ptr)) + '''');
  effClose:
   begin
    FOpcodeLog.Add(Opcode2String(Opcode));
    GetLocaleFormatSettings(SysLocale.DefaultLCID, FormatSettings);
    FormatSettings.ShortDateFormat := 'yyyymmdd';
    FormatSettings.LongTimeFormat := 'yyyymmdd';
    FormatSettings.ShortTimeFormat := 'hhmmss';
    FormatSettings.LongTimeFormat := 'hhmmsss';
    FOpcodeLog.SaveToFile('OpcodeLog - ' + DateTimeToStr(Now, FormatSettings) + '.log');
   end;
  else FOpcodeLog.Add(Opcode2String(Opcode) +
                      ' Index: ' + IntToStr(Index) +
                      ' Value: ' + IntToStr(Value) +
                      ' Pointer: ' + IntToStr(Integer(ptr)) +
                      ' Single: ' + FloatToStr(Single(Opt)))
 end;
 FLastOpcode := Opcode;

 if EditorForm is TFmVOL then
  with TFmVOL(EditorForm) do
   begin
    if CBAutoUpdates.Checked
     then MOpcodeLog.Lines.Assign(OpcodeLog);
   end;
end;

procedure TVOLDataModule.VSTModuleCreate(Sender: TObject);
begin
 FOpcodeLog := TStringList.Create;
end;

procedure TVOLDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FOpcodeLog);
end;

procedure TVOLDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmVOL.Create(Self);
 TFmVOL(GUI).MOpcodeLog.Lines.Assign(OpcodeLog);
end;

end.