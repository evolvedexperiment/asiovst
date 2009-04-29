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
    procedure ParamChange(Sender: TObject;
      const Index: Integer; var Value: Single);
  private
    FOpcodeLog  : TStringList;
    FLastOpcode : TDispatcherOpcode;
    procedure SyncLogDisplay;
  protected
    procedure HostCallDispatchEffect(const opcode: TDispatcherOpcode;
      const Index: Integer; const Value: Integer; const ptr: Pointer;
      const opt: Single); override;
    procedure HostCallSetParameter(const Index: Integer; const Value: Single); override;
    function HostCallGetParameter(const Index: Integer): Single; override;
  public
    property OpcodeLog: TStringList read FOpcodeLog;
  end;

implementation

{$R *.DFM}

uses
  VOLGUI, DAV_VSTCustomModule;

procedure TVOLDataModule.HostCallDispatchEffect(const Opcode: TDispatcherOpcode;
  const Index, Value: Integer; const ptr: Pointer; const opt: Single);
var
  FormatSettings : TFormatSettings;
  ChunkName      : TChunkName;
begin
 inherited;

 if not assigned(FOpcodeLog)
  then exit;

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
     then FOpcodeLog.Add(Opcode2String(Opcode))
     else exit;
  effEditIdle:
    if FLastOpcode <> effEditIdle
     then FOpcodeLog.Add(Opcode2String(Opcode))
     else exit;
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

 SyncLogDisplay;
end;

function TVOLDataModule.HostCallGetParameter(const Index: Integer): Single;
begin
 result := inherited HostCallGetParameter(Index);

 if not assigned(FOpcodeLog)
  then exit;

 FOpcodeLog.Add('GetParameter' +
                ' Index: ' + IntToStr(Index) +
                ' Value: ' + FloatToStr(result));
 SyncLogDisplay;
end;

procedure TVOLDataModule.HostCallSetParameter(const Index: Integer;
  const Value: Single);
begin
 inherited;

 if not assigned(FOpcodeLog)
  then exit;

 FOpcodeLog.Add('GetParameter' +
                ' Index: ' + IntToStr(Index) +
                ' Value: ' + FloatToStr(Value));
 SyncLogDisplay;
end;

procedure TVOLDataModule.SyncLogDisplay;
begin
 if EditorForm is TFmVOL then
  with TFmVOL(EditorForm) do
   begin
    if CBAutoUpdates.Checked
     then MOpcodeLog.Lines.Assign(OpcodeLog);
   end;
end;

procedure TVOLDataModule.ParamChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if EditorForm is TFmVOL
  then TFmVOL(EditorForm).UpdateParameter;
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