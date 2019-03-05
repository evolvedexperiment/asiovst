{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit VPSmain;

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ComCtrls, StdCtrls, DAV_VSTHost;

{ -$DEFINE UseThreads }

{$IFDEF UseThreads}

const
  CNumThreads = 1;
{$ENDIF}

type
{$IFDEF UseThreads}
  TVSTScanThread = class(TThread)
  private
    FFileName: TFileName;
    FListViewItem: TListItem;
    FLoadTime: Single;
    FOpenTime: Single;
    FCloseTime: Single;
    FVstHost: TVstHost;
    procedure AddListViewItem;
    procedure ListPlainVSTproperties;
    procedure ListEnahncedVSTproperties;
    procedure QueryVSTFilename;
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;
{$ENDIF}
  { TFormVSTPluginScanner }

  TFormVSTPluginScanner = class(TForm)
    EditDirectory: TEdit;
    ButtonDirectorySelect: TButton;
    ListView: TListView;
    ButtonScan: TButton;
    StatusBar: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonDirectorySelectClick(Sender: TObject);
    procedure ButtonScanClick(Sender: TObject);
    procedure EditDirectoryClick(Sender: TObject);
    procedure EditDirectoryChange(Sender: TObject);
  private
{$IFDEF UseThreads}
    FThreads: array [0 .. CNumThreads - 1] of TVSTScanThread;
    FFileIndex: Integer;
    FFilesToScan: TStringList;
    procedure FreeeExistingThreads;
    function GetCurrentFileName: TFileName;
    procedure ThreadTerminated(Sender: TObject);
{$ELSE}
    FVstHost: TVstHost;
    FLoadTime: Single;
    FOpenTime: Single;
    FCloseTime: Single;
{$ENDIF}
  public
{$IFDEF UseThreads}
    property CurrentFileName: TFileName read GetCurrentFileName;
{$ENDIF}
  end;

var
  FormVSTPluginScanner: TFormVSTPluginScanner;

implementation

uses
  Registry, FileCtrl;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{$IFDEF UseThreads}

{ TVSTScanThread }

constructor TVSTScanThread.Create;
begin
  inherited Create(True);
  FVstHost := TVstHost.Create(FormVSTPluginScanner);
  FVstHost.VstPlugIns.Add;
  FreeOnTerminate := False;
  Resume;
end;

procedure TVSTScanThread.AddListViewItem;
begin
  try
    FListViewItem := FormVSTPluginScanner.ListView.Items.Add;
    FListViewItem.Caption := ExtractFileName(FFileName);
    FormVSTPluginScanner.StatusBar.SimpleText := FListViewItem.Caption;
  except
    FListViewItem := nil;
  end;
end;

procedure TVSTScanThread.ListPlainVSTproperties;
begin
  if Assigned(FListViewItem) then
    with FVstHost[0], FListViewItem.SubItems do
      try
        Add(UniqueID);
        Add(IntToStr(numPrograms));
        Add(IntToStr(numParams));
        Add(IntToStr(numInputs));
        Add(IntToStr(numOutputs));
        Add(EffOptions2String(EffectOptions));
        Add(IntToStr(InitialDelay));
        Add(IntToStr(RealQualities));
        Add(IntToStr(OffQualities));
        Add(FloatToStr(IORatio));
        Add(IntToStr(Version));
        Application.ProcessMessages;
      except
      end;
end;

procedure TVSTScanThread.ListEnahncedVSTproperties;
begin
  if Assigned(FListViewItem) then
    with FVstHost[0], FListViewItem.SubItems do
      try
        Add(EffectName);
        Add(ProductString);
        Add(VendorString);
        Add(FloatToStrF(FLoadTime, ffGeneral, 3, 3));
        Add(FloatToStrF(FOpenTime, ffGeneral, 3, 3));
        Application.ProcessMessages;
      except
      end;
end;

procedure TVSTScanThread.QueryVSTFilename;
begin
  try
    FFileName := FormVSTPluginScanner.CurrentFileName;
  except
    FFileName := '';
  end;
end;

procedure TVSTScanThread.Execute;
var
  A, B, C: Int64;
begin
  with FVstHost[0] do
    repeat
      Synchronize(QueryVSTFilename);
      if FileExists(FFileName) and (FFileName <> DLLFileName) then
        try
          Synchronize(AddListViewItem);
          QueryPerformanceFrequency(C);
          QueryPerformanceFrequency(C);
          QueryPerformanceCounter(A);
          LoadFromFile(FFileName);
          QueryPerformanceCounter(B);
          FLoadTime := (B - A) / C * 1000;

          // additional check if thread is terminated
          if Terminated then
            Exit;

          if Loaded then
          begin
            Synchronize(ListPlainVSTproperties);
            QueryPerformanceFrequency(C);
            QueryPerformanceCounter(A);
            Open;
            QueryPerformanceCounter(B);
            FOpenTime := (B - A) / C * 1000;
            if Active then
            begin
              Synchronize(ListEnahncedVSTproperties);
              QueryPerformanceFrequency(C);
              QueryPerformanceCounter(A);
              Close;
              QueryPerformanceCounter(B);
              FCloseTime := (B - A) / C * 1000;
            end;
            UnLoad;
          end;
        except
        end;
      Sleep(1);
    until Terminated or (FFileName = '');
end;
{$ENDIF}
{ TFormVSTPluginScanner }

procedure TFormVSTPluginScanner.FormCreate(Sender: TObject);
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey('SOFTWARE\Vst', False);

      if ValueExists('VstPluginsPath') then
        EditDirectory.Text := ReadString('VstPluginsPath');
      CloseKey;
    finally
      Free;
    end;

{$IFNDEF UseThreads}
  FVstHost := TVstHost.Create(Self);
  FVstHost.VstPlugIns.Add;
{$ENDIF}
end;

procedure TFormVSTPluginScanner.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey('SOFTWARE\Vst', True);

      WriteString('VstPluginsPath', EditDirectory.Text);
      CloseKey;
    finally
      Free;
    end;
end;

procedure TFormVSTPluginScanner.FormDestroy(Sender: TObject);
begin
{$IFDEF UseThreads}
  if Assigned(FFilesToScan) then
    FreeAndNil(FFilesToScan);

  // free existing threads
  FreeeExistingThreads;
{$ENDIF}
end;

procedure TFormVSTPluginScanner.FormShow(Sender: TObject);
begin
  if DirectoryExists(EditDirectory.Text) then
    ButtonScan.SetFocus;
end;

{$IFDEF UseThreads}

function TFormVSTPluginScanner.GetCurrentFileName: TFileName;
begin
  if (FFileIndex >= 0) and (FFileIndex < FFilesToScan.Count) and
    FileExists(FFilesToScan[FFileIndex]) then
  begin
    Result := FFilesToScan[FFileIndex];
    Inc(FFileIndex);
  end
  else
    Result := '';
end;
{$ENDIF}

procedure TFormVSTPluginScanner.ButtonDirectorySelectClick(Sender: TObject);
var
  Dir: string;
begin
  SelectDirectory('Select a directory', '', Dir);
  EditDirectory.Text := Dir;
  if DirectoryExists(EditDirectory.Text) then
    ButtonScan.SetFocus;
end;

{$IFDEF UseThreads}

procedure TFormVSTPluginScanner.FreeeExistingThreads;
var
  i: Integer;
begin
  // free existing threads
  for i := 0 to CNumThreads - 1 do
    if Assigned(FThreads[i]) then
      try
        FThreads[i].Terminate;
        if not Assigned(FThreads[i].FatalException) and not FThreads[i].Terminated
        then
          FThreads[i].WaitFor;
      finally
        FreeAndNil(FThreads[i]);
      end;
end;

procedure TFormVSTPluginScanner.ThreadTerminated(Sender: TObject);
begin
  if Sender is TThread then
    with TThread(Sender) do
    begin
      if FatalException is Exception then
        raise Exception(FatalException);
    end;
end;
{$ENDIF}

procedure TFormVSTPluginScanner.ButtonScanClick(Sender: TObject);
var
  SR: TSearchRec;
{$IFDEF UseThreads}
  i: Integer;
{$ELSE}
  A, B, C: Int64;
{$ENDIF}
begin
  ListView.Clear;
  if not DirectoryExists(EditDirectory.Text) then
    Exit;

{$IFDEF UseThreads}
  // free existing threads
  FreeeExistingThreads;
  FFileIndex := 0;

  if not Assigned(FFilesToScan) then
    FFilesToScan := TStringList.Create
  else
    FFilesToScan.Clear;

  if FindFirst(EditDirectory.Text + '\' + '*.dll', faAnyFile, SR) = 0 then
    try
      repeat
        FFilesToScan.Add(EditDirectory.Text + '\' + SR.Name);
      until FindNext(SR) <> 0;
    finally
      // Must free up resources used by these successful finds
      FindClose(SR);
    end;

  for i := 0 to CNumThreads - 1 do
  begin
    FThreads[i] := TVSTScanThread.Create;
    FThreads[i].OnTerminate := ThreadTerminated;
  end;

{$ELSE}
  if FindFirst(EditDirectory.Text + '\' + '*.dll', faAnyFile, SR) = 0 then
    try
      repeat
        with FVstHost[0], ListView.Items.Add do
          try
            Caption := SR.Name;
            QueryPerformanceFrequency(C);
            QueryPerformanceCounter(A);
            LoadFromFile(EditDirectory.Text + '\' + SR.Name);
            QueryPerformanceCounter(B);
            FLoadTime := (B - A) / C * 1000;
            if not Loaded then
              raise Exception.CreateFmt('Could not load %s', [SR.Name]);

            try
              SubItems.Add(UniqueID);
              SubItems.Add(IntToStr(numPrograms));
              SubItems.Add(IntToStr(numParams));
              SubItems.Add(IntToStr(numInputs));
              SubItems.Add(IntToStr(numOutputs));
              SubItems.Add(EffOptions2String(EffectOptions));
              SubItems.Add(IntToStr(InitialDelay));
              SubItems.Add(IntToStr(RealQualities));
              SubItems.Add(IntToStr(OffQualities));
              SubItems.Add(FloatToStr(IORatio));
              SubItems.Add(IntToStr(Version));

              Sleep(1);
              Application.ProcessMessages;
              QueryPerformanceFrequency(C);
              QueryPerformanceCounter(A);
              Open;
              QueryPerformanceCounter(B);
              FOpenTime := (B - A) / C * 1000;
              try
                Sleep(1);
                Application.ProcessMessages;
                SubItems.Add(EffectName);
                SubItems.Add(ProductString);
                SubItems.Add(VendorString);
                SubItems.Add(FloatToStrF(FLoadTime, ffGeneral, 3, 3));
                SubItems.Add(FloatToStrF(FOpenTime, ffGeneral, 3, 3));
              finally
                Close;
              end;
            finally
              Unload;
            end;
          except
            On E: Exception do
              MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
        Sleep(1);
        Application.ProcessMessages;
      until FindNext(SR) <> 0;

    finally
      // Must free up resources used by these successful finds
      FindClose(SR);
    end;
{$ENDIF}
end;

procedure TFormVSTPluginScanner.EditDirectoryChange(Sender: TObject);
begin
  ButtonScan.Enabled := DirectoryExists(EditDirectory.Text)
end;

procedure TFormVSTPluginScanner.EditDirectoryClick(Sender: TObject);
begin
  EditDirectory.SelectAll;
end;

end.
