unit LoadVSTModule;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_VstHost;

type
  TPlugInPlugModule = class(TVSTModule)
    VstHost: TVstHost;
    function VST2ModuleCanDo(Sender: TObject; CanDoText: AnsiString): Integer;
    procedure VST2ModuleCreate(Sender: TObject);
    procedure VST2ModuleBeforeProgramChange(Sender: TObject);
    procedure VST2ModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VST2ModuleClose(Sender: TObject);
    procedure VST2ModuleEditIdle(Sender: TObject);
    procedure VST2ModuleEditSleep(Sender: TObject);
    procedure VST2ModuleEditTop(Sender: TObject);
    procedure VST2ModuleGetVU(var VU: Single);
    procedure VST2ModuleOpen(Sender: TObject);
    procedure VST2ModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VST2ModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VST2ModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VST2ModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VST2ModuleStartProcess(Sender: TObject);
    procedure VST2ModuleStopProcess(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Dialogs;

procedure TPlugInPlugModule.VST2ModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 VstHost[0].Process(@Inputs[0], @Outputs[0], SampleFrames);
end;

procedure TPlugInPlugModule.VST2ModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 VstHost[0].Process32Replacing(@Inputs[0], @Outputs[0], SampleFrames);
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
     VstHost[0].DLLFileName := FileName;
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
 VstHost[0].CurrentProgram := CurrentProgram;
end;

procedure TPlugInPlugModule.VST2ModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
begin
 VstHost[0].SetBlockSizeAndSampleRate(BlockSize, SampleRate)
end;

function TPlugInPlugModule.VST2ModuleCanDo(Sender: TObject;
  CanDoText: AnsiString): Integer;
begin
 result := VstHost[0].VstCanDo(CanDoText);
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
 VstHost[0].Parameter[Index] := Value;
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
