unit LoadVSTModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_VstHost;

type
  TPlugInPlugModule = class(TVSTModule)
    VstHost: TVstHost;
    function VST2ModuleCanDo(Sender: TObject; CanDoText: String): Integer;
    procedure VST2ModuleBeforeProgramChange(Sender: TObject);
    procedure VST2ModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VST2ModuleClose(Sender: TObject);
    procedure VST2ModuleCreate(Sender: TObject);
    procedure VST2ModuleEditIdle(Sender: TObject);
    procedure VST2ModuleEditSleep(Sender: TObject);
    procedure VST2ModuleEditTop(Sender: TObject);
    procedure VST2ModuleGetVU(var VU: Single);
    procedure VST2ModuleOpen(Sender: TObject);
    procedure VST2ModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VST2ModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VST2ModuleProcessReplacing(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VST2ModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VST2ModuleStartProcess(Sender: TObject);
    procedure VST2ModuleStopProcess(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  Dialogs;

procedure TPlugInPlugModule.VST2ModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 VstHost[0].Process(@inputs[0], @outputs[0], SampleFrames);
end;

procedure TPlugInPlugModule.VST2ModuleProcessReplacing(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 VstHost[0].ProcessReplacing(@inputs[0], @outputs[0], SampleFrames);
end;

procedure TPlugInPlugModule.VST2ModuleCreate(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.DLL)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   if Execute then
    begin
     VstHost[0].DLLFileName:=FileName;
    end;
  finally
   Free;
  end;
end;

procedure TPlugInPlugModule.VST2ModuleOpen(Sender: TObject);
begin
 VstHost[0].Active := True;
end;

procedure TPlugInPlugModule.VST2ModuleClose(Sender: TObject);
begin
 VstHost[0].Active := False;
end;                          

procedure TPlugInPlugModule.VST2ModuleEditIdle(Sender: TObject);
begin
 VstHost[0].EditIdle; 
end;

procedure TPlugInPlugModule.VST2ModuleBeforeProgramChange(Sender: TObject);
begin
 VstHost[0].ProgramNr:=CurrentProgram;
end;

procedure TPlugInPlugModule.VST2ModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
begin
 VstHost[0].SetBlockSizeAndSampleRate(BlockSize, SampleRate)
end;

function TPlugInPlugModule.VST2ModuleCanDo(Sender: TObject; CanDoText: String): Integer;
begin
 result := VstHost[0].CanDo(@CanDoText);
end;

procedure TPlugInPlugModule.VST2ModuleEditTop(Sender: TObject);
begin
 VstHost[0].EditActivate;
end;

procedure TPlugInPlugModule.VST2ModuleEditSleep(Sender: TObject);
begin
 VstHost[0].EditDeActivate;
end;

procedure TPlugInPlugModule.VST2ModuleGetVU(var VU: Single);
begin
 VU := VstHost[0].GetVu;
end;

procedure TPlugInPlugModule.VST2ModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 VstHost[0].Parameters[Index] := Value;
end;

procedure TPlugInPlugModule.VST2ModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 VstHost[0].SetSampleRate(SampleRate);
end;

procedure TPlugInPlugModule.VST2ModuleStartProcess(Sender: TObject);
begin
 VstHost[0].StartProcess;
end;

procedure TPlugInPlugModule.VST2ModuleStopProcess(Sender: TObject);
begin
 VstHost[0].StopProcess;
end;

procedure TPlugInPlugModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TForm.Create(Self);
 VstHost[0].ShowEdit(GUI);
end;

end.
