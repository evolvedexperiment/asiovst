unit WrapperDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_VstHost;

type
  TWrapperDataModule = class(TVSTModule)
    VstHost: TVstHost;
    function VSTModuleCanDo(Sender: TObject; CanDoText: String): Integer;
    procedure VSTModuleBeforeProgramChange(Sender: TObject);
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleEditIdle(Sender: TObject);
    procedure VSTModuleEditSleep(Sender: TObject);
    procedure VSTModuleEditTop(Sender: TObject);
    procedure VSTModuleGetVU(var VU: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleStartProcess(Sender: TObject);
    procedure VSTModuleStopProcess(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure CustomParameterDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure CustomParameterLabel(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  Dialogs, DAV_VSTEffect, DAV_VSTParameters;

procedure TWrapperDataModule.VSTModuleCreate(Sender: TObject);
var
  RS : TResourceStream;
  i  : Integer;
begin
 RS := TResourceStream.Create(hInstance, 'mdaDynamics', 'DLL');
 try
  VstHost[0].LoadFromStream(RS);
 finally
  FreeAndNil(RS);
 end;
 for i := 0 to VstHost[0].numParams - 1
  do ParameterProperties.Add;
end;

procedure TWrapperDataModule.VSTModuleOpen(Sender: TObject);
var
  i  : Integer;
  pp : TVstParameterPropertyRecord;
begin
 VstHost[0].Active := True;
 if VstHost[0].Active then
  begin
   OnEditIdle               := VSTModuleEditIdle;
   OnBeforeProgramChange    := VSTModuleBeforeProgramChange;
   OnBlockSizeChange        := VSTModuleBlockSizeChange;
   OnCanDo                  := VSTModuleCanDo;
   OnEditTop                := VSTModuleEditTop;
   OnEditSleep              := VSTModuleEditSleep;
   OnGetVU                  := VSTModuleGetVU;
   OnParameterChange        := VSTModuleParameterChange;
   OnStartProcess           := VSTModuleStartProcess;
   OnStopProcess            := VSTModuleStopProcess;
   OnProcess                := VSTModuleProcess;
   OnProcessReplacing       := VSTModuleProcessReplacing;
   OnProcessDoubleReplacing := VSTModuleProcessDoubleReplacing;
   OnSampleRateChange       := VSTModuleSampleRateChange;
   OnEditOpen               := VSTModuleEditOpen;

   for i := 0 to VstHost[0].numParams - 1 do
    begin
     pp := VstHost[0].GetParameterProperties(i);
     with ParameterProperties[i] do
      begin
       DisplayName      := pp.Caption;
       Flags            := pp.Flags;
       LargeStepFloat   := pp.LargeStepFloat;
       SmallStepFloat   := pp.SmallStepFloat;
       LargeStepInteger := pp.LargeStepInteger;
       MaxInteger       := pp.MaxInteger;
       MinInteger       := pp.MinInteger;
       ShortLabel       := pp.ShortLabel;
       if DisplayName = ''
        then DisplayName := VstHost[0].GetParamName(i);
       OnCustomParameterDisplay := CustomParameterDisplay;
       OnCustomParameterLabel   := CustomParameterLabel;
      end;
    end;

  end
 else
  begin
   OnEditIdle               := nil;
   OnBeforeProgramChange    := nil;
   OnBlockSizeChange        := nil;
   OnCanDo                  := nil;
   OnEditTop                := nil;
   OnEditSleep              := nil;
   OnGetVU                  := nil;
   OnParameterChange        := nil;
   OnStartProcess           := nil;
   OnStopProcess            := nil;
   OnProcess                := nil;
   OnProcessReplacing       := nil;
   OnProcessDoubleReplacing := nil;
   OnSampleRateChange       := nil;
   OnEditOpen               := nil;
  end;
end;

procedure TWrapperDataModule.VSTModuleClose(Sender: TObject);
begin
 VstHost[0].Active := False;
end;

procedure TWrapperDataModule.VSTModuleEditIdle(Sender: TObject);
begin
 VstHost[0].EditIdle;
end;

procedure TWrapperDataModule.VSTModuleBeforeProgramChange(Sender: TObject);
begin
 VstHost[0].ProgramNr := CurrentProgram;
end;

procedure TWrapperDataModule.VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
begin
 VstHost[0].SetBlockSizeAndSampleRate(BlockSize, SampleRate)
end;

function TWrapperDataModule.VSTModuleCanDo(Sender: TObject; CanDoText: String): Integer;
begin
 result := VstHost[0].CanDo(@CanDoText);
end;

procedure TWrapperDataModule.VSTModuleEditTop(Sender: TObject);
begin
 VstHost[0].EditActivate;
end;

procedure TWrapperDataModule.VSTModuleEditSleep(Sender: TObject);
begin
 VstHost[0].EditDeActivate;
end;

procedure TWrapperDataModule.VSTModuleGetVU(var VU: Single);
begin
 VU := VstHost[0].GetVu;
end;

procedure TWrapperDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 VstHost[0].Parameters[Index] := Value;
end;

procedure TWrapperDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 VstHost[0].SetSampleRate(SampleRate);
end;

procedure TWrapperDataModule.VSTModuleStartProcess(Sender: TObject);
begin
 VstHost[0].StartProcess;
end;

procedure TWrapperDataModule.VSTModuleStopProcess(Sender: TObject);
begin
 VstHost[0].StopProcess;
end;

procedure TWrapperDataModule.CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := VstHost[0].GetParamDisplay(Index);
end;

procedure TWrapperDataModule.CustomParameterLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := VstHost[0].GetParamLabel(Index);
end;

procedure TWrapperDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TForm.Create(Self);
 VstHost[0].ShowEdit(GUI);
end;

procedure TWrapperDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
begin
 VstHost[0].ProcessDoubleReplacing(@Inputs[0], @Outputs[0], SampleFrames);
end;

procedure TWrapperDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 VstHost[0].Process(@Inputs[0], @Outputs[0], SampleFrames);
end;

procedure TWrapperDataModule.VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 VstHost[0].ProcessReplacing(@Inputs[0], @Outputs[0], SampleFrames);
end;

end.
