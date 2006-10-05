unit HVSTMain;

{$IFDEF FPC}
 {$MODE DELPHI}
 {$WARNINGS OFF}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS ON}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdActns, Menus, DVSTHost;

type

  { TSimpleVSTHost }

  TSimpleVSTHost = class(TForm)
    MainMenu: TMainMenu;
    ActionList: TActionList;
    AcFileExit: TFileExit;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    MIOpen: TMenuItem;
    VSTHost: TVstHost;
    N1: TMenuItem;
    MIHelp: TMenuItem;
    MIAbout: TMenuItem;
    MIProcess: TMenuItem;
    N2: TMenuItem;
    procedure MIAboutClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIProcessClick(Sender: TObject);
  private
  public
  end;

var
  SimpleVSTHost: TSimpleVSTHost;

implementation

uses DVSTEffect, DDSPBase, HVSTAbout;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TSimpleVSTHost.MIOpenClick(Sender: TObject);
var PlugRect : ERect;
begin
 with TOpenDialog.Create(Self) do
  try
   InitialDir:=VSTHost.PlugInDir;
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Load a VST Plugin';
   if Execute then
    with VSTHost[0] do
     begin
      Active:=False;
      DLLFileName:=FileName;
      Active:=True;
      ShowEdit(Self);
      PlugRect:=EditGetRect;
      ClientWidth:=PlugRect.Right-PlugRect.Left;
      ClientHeight:=PlugRect.Bottom-PlugRect.Top;
     end;
  finally
   Free;
  end;
end;

procedure TSimpleVSTHost.MIAboutClick(Sender: TObject);
begin
 FmAbout.ShowModal;
end;

procedure TSimpleVSTHost.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TSimpleVSTHost.MIProcessClick(Sender: TObject);
var Input  : TArrayOfSingleArray;
    Output : TArrayOfSingleArray;
    i      : Integer;
begin
 with VSTHost[0] do
  begin
   SetLength(Input,numInputs);
   SetLength(Output,numOutputs);
   for i:=0 to numInputs-1 do begin SetLength(Input[i],VSTHost.Blocksize); Input[i,0]:=1; end;
   for i:=0 to numOutputs-1 do SetLength(Output[i],VSTHost.Blocksize);
   ProcessReplacing(@Input[0],@Output[0],VSTHost.Blocksize);
  end;
end;

initialization
{$IFDEF FPC}
 {$I HVSTMain.lrs}
{$ENDIF}

end.
