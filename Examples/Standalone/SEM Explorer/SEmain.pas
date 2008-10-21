unit SEmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, DAV_SEModule, DAV_SEHost;

type
  TFmSEModuleExplorer = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIOpen: TMenuItem;
    MIExit: TMenuItem;
    Memo: TMemo;
    N1: TMenuItem;
    MISettings: TMenuItem;
    MIEnableWrapper: TMenuItem;
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MIEnableWrapperClick(Sender: TObject);
  private
    SEHost : TSEHost;
    procedure LoadSEModule(FileName: TFileName);
    procedure SavePatchedModule;
  end;

var
  FmSEModuleExplorer: TFmSEModuleExplorer;

implementation

uses
  IniFiles, DAV_DLLResources, DAV_SECommon;

{$R *.dfm}

procedure TFmSEModuleExplorer.MIOpenClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  begin
   DefaultExt := '.sem';
   Filter := 'SE Modules (*.sem)|*.sem';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select SE Module';
   if Execute then LoadSEModule(FileName);
  end;
end;

procedure TFmSEModuleExplorer.FormCreate(Sender: TObject);
begin
 SEHost := TSEHost.Create(Self);
 SEHost.HostedSEModules.Add;
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'SEMExplorer.ini') do
  try
   MIEnableWrapper.Checked := ReadBool('Layout', 'Enable Wrapper', MIEnableWrapper.Checked);
   Left := ReadInteger('Layout', 'Left', Left);
   Top := ReadInteger('Layout', 'Top', Top);
   Width := ReadInteger('Layout', 'Width', Width);
   Height := ReadInteger('Layout', 'Height', Height);
  finally
   Free;
  end;
end;

procedure TFmSEModuleExplorer.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'SEMExplorer.ini') do
  try
   WriteBool('Layout', 'Enable Wrapper', MIEnableWrapper.Checked);
   WriteInteger('Layout', 'Left', Left);
   WriteInteger('Layout', 'Top', Top);
   WriteInteger('Layout', 'Width', Width);
   WriteInteger('Layout', 'Height', Height);
  finally
   Free;
  end;
 FreeAndNil(SEHost);
end;

procedure TFmSEModuleExplorer.LoadSEModule(FileName: TFileName);
var
  PartNr, PinNr : Integer;
  Pin           : TSEPinProperties;
begin
 SEHost[0].LoadFromFile(FileName);
 Memo.Clear;
 for PartNr := 0 to SEHost[0].PartCount - 1 do
  with SEHost[0].Part[PartNr] do
   begin
    if PartNr > 0 then Memo.Lines.Add('');
    Memo.Lines.Add('Module '      + IntToStr(PartNr + 1));
    Memo.Lines.Add('Name: '       + Properties.Name);
    Memo.Lines.Add('ID: '         + Properties.ID);
    Memo.Lines.Add('About: '      + Properties.About);
    Memo.Lines.Add('Flags: '      + PropertyFlagsToString(Properties.Flags));
    Memo.Lines.Add('GuiFlags: '   + PropertyGUIFlagsToString(Properties.GuiFlags));
    Memo.Lines.Add('SdkVersion: ' + IntToStr(Properties.SdkVersion));
   end;
 if SEHost[0].PartCount > 0 then
  begin
   SEHost[0].Part[0].Instanciate;

   PinNr := 0;
   FillChar(Pin, SizeOf(TSEPinProperties), 0);
   while SEHost[0].Part[0].GetPinProperties(PinNr, Pin) do
    begin
     Memo.Lines.Add('');
     Memo.Lines.Add('Pin '            + IntToStr(PinNr + 1));
     case Pin.Direction of
      drIn           : Memo.Lines.Add('DataType: Input');
      drOut          : Memo.Lines.Add('DataType: Output');
      drContainer_IO : Memo.Lines.Add('DataType: Container I/O');
      drParameter    : Memo.Lines.Add('DataType: Parameter');
      else Memo.Lines.Add('Unknown');
     end;
     case Pin.Datatype of
      dtNone     : Memo.Lines.Add('DataType: None');
      dtEnum     : Memo.Lines.Add('DataType: Enum');
      dtText     : Memo.Lines.Add('DataType: Text');
      dtMidi2    : Memo.Lines.Add('DataType: Midi2');
      dtDouble   : Memo.Lines.Add('DataType: Double');
      dtBoolean  : Memo.Lines.Add('DataType: Boolean');
      dtFSample  : Memo.Lines.Add('DataType: Float Sample');
      dtSingle   : Memo.Lines.Add('DataType: Single');
      dtVstParam : Memo.Lines.Add('DataType: VST Parameter');
      dtInteger  : Memo.Lines.Add('DataType: Integer');
      dtBlob     : Memo.Lines.Add('DataType: Blob');
     end;
     Memo.Lines.Add('Name: '          + Pin.Name);
     Memo.Lines.Add('DefaultValue: '  + Pin.DefaultValue);
     if assigned(Pin.DatatypeExtra)
      then Memo.Lines.Add('DatatypeExtra: ' + Pin.DatatypeExtra);
     Memo.Lines.Add('Spare: '         + IntToStr(Pin.Spare));
     inc(PinNr);
    end;

   if SEHost[0].Part[0].Magic <> SepMagic then
    if not MIEnableWrapper.Checked
     then MessageBox(0, 'Magic number shreddered! It is likely that this SEM file is extracted from a VST plugin.' + #13#10#13#10 +
           'A special wrapper can be created by this tool that restores the functionality of the SEM.' + #13#10#13#10 +
           'However, this action is probably illegal depending on the SEM file and the laws in your country. ' + #13#10#13#10 +
           'To enable this function and create the wrapped SEM file please contact the author of this tool and receive a passkey.',
           'Magic number shreddered!', MB_ICONWARNING or MB_OK)
     else if (MessageBox(0, 'Magic number shreddered!' + #13#10#13#10 +
          'Create a wrapped SEM to enable the plugin to be used in SynthEdit again?',
          'Magic number shreddered!', MB_ICONWARNING or MB_YESNO) = idYes) then SavePatchedModule;

(*
   SEHost[0].Part[0].Open;
   try
//    SEHost[0].Part[0].
   finally
    SEHost[0].Part[0].Close;
   end;
*)
  end;
end;

procedure TFmSEModuleExplorer.SavePatchedModule;
var
  FN : TFileName;
  RS : TResourceStream;
  RM : TPEResourceModule;
  RD : TResourceDetails;
begin
 RM := TPEResourceModule.Create;
 with RM do
  try
   // load template
   RS := TResourceStream.Create(HInstance, 'SEMagicHealer', 'SEM');
   try
    LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;
   FN := SEHost[0].SEMFileName;

   // store VST Plugins
   with TMemoryStream.Create do
    try
     LoadFromFile(FN);
     RD := TResourceDetails.CreateResourceDetails(RM, 0, 'SEM', 'SEM', Size, Memory);
     AddResource(RD);
    finally
     Free;
    end;

   SortResources;
   SaveToFile(ExtractFilePath(FN) + 'X' + ExtractFileName(FN));
   ShowMessage('Patched SEM successfully created!');
  finally
   FreeAndNil(RM);
  end;
end;

procedure TFmSEModuleExplorer.MIEnableWrapperClick(Sender: TObject);
begin
 if InputBox('Enter PassKey', 'Enter PassKey', '') = 'SEM Explorer'
  then MIEnableWrapper.Checked := not MIEnableWrapper.Checked;
end;

procedure TFmSEModuleExplorer.MIExitClick(Sender: TObject);
begin
 Close;
end;

end.
