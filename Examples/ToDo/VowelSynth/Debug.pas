unit Debug;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DVSTHost, uPlugin, StdCtrls, Spin;

type
  TSingleArray   = array[0..0] of Single;
  PSingleArray   = ^TSingleArray;

  TFmVSTDebug = class(TForm)
    VSTHost: TVSTHost;
    Bt_ShowEditor: TButton;
    Bt_Process: TButton;
    GB_Performance: TGroupBox;
    GB_Host: TGroupBox;
    SE_BlockSize: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    SE_SampleRate: TSpinEdit;
    Lb_Blocks: TLabel;
    SE_Blocks: TSpinEdit;
    Bt_Go: TButton;
    Ed_Results: TEdit;
    Rb_Random: TRadioButton;
    Rb_Impulse: TRadioButton;
    CB_M: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Bt_ShowEditorClick(Sender: TObject);
    procedure SE_BlockSizeChange(Sender: TObject);
    procedure SE_SampleRateChange(Sender: TObject);
    procedure VSTHostVSTPlugIns0AudioMasterIdle(Sender: TObject);
    procedure VSTHostVSTPlugIns0AudioMasterNeedIdle(Sender: TObject);
    procedure VSTHostVSTPlugIns0AudioMasterUpdateDisplay(Sender: TObject);
    procedure Bt_ProcessClick(Sender: TObject);
    procedure Bt_GoClick(Sender: TObject);
    procedure CB_MClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmVSTDebug : TFmVSTDebug;
  Effect     : APlugin;

implementation

{$R *.DFM}

procedure TFmVSTDebug.FormCreate(Sender: TObject);
begin
 Effect:=APlugin.Create(audioMaster);
 VSTHost.VSTPlugIns[0].PVSTEffect:=Effect.Effect;
 VSTHost.VSTPlugIns[0].Open;
 SE_BlockSize.Value:=VSTHost.BlockSize;
 SE_SampleRate.Value:=round(VSTHost.VstTimeInfo.SampleRate);
end;

procedure TFmVSTDebug.FormDestroy(Sender: TObject);
begin
 VSTHost.VSTPlugIns[0].CloseEdit;
 VSTHost.VSTPlugIns[0].Close;
 VSTHost.VSTPlugIns[0].Free;
end;

procedure TFmVSTDebug.Bt_ShowEditorClick(Sender: TObject);
begin
 VSTHost.VSTPlugIns[0].ShowEdit;
end;

procedure TFmVSTDebug.SE_BlockSizeChange(Sender: TObject);
begin
 VSTHost.BlockSize:=SE_BlockSize.Value;
end;

procedure TFmVSTDebug.SE_SampleRateChange(Sender: TObject);
begin
 VSTHost.VstTimeInfo.SampleRate:=SE_SampleRate.Value;
end;

procedure TFmVSTDebug.VSTHostVSTPlugIns0AudioMasterIdle(Sender: TObject);
begin
 VSTHost.VSTPlugIns[0].Idle;
end;

procedure TFmVSTDebug.VSTHostVSTPlugIns0AudioMasterNeedIdle(
  Sender: TObject);
begin
 VSTHost.VSTPlugIns[0].EditIdle;
end;

procedure TFmVSTDebug.VSTHostVSTPlugIns0AudioMasterUpdateDisplay(
  Sender: TObject);
begin
 VSTHost.VSTPlugIns[0].EditIdle;
end;

procedure TFmVSTDebug.Bt_ProcessClick(Sender: TObject);
var SingleBuffer  : array of PSingleArray;
    i             : Integer;
begin
 SetLength(SingleBuffer,2);
 VSTHost.VSTPlugIns[0].SetBlockSizeAndSampleRate(VSTHost.BlockSize,VSTHost.VstTimeInfo.SampleRate);
 VSTHost.VSTPlugIns[0].MainsChanged(True);
 GetMem(SingleBuffer[0],VSTHost.BlockSize*SizeOf(Single));
 GetMem(SingleBuffer[1],VSTHost.BlockSize*SizeOf(Single));
 if Rb_Random.Checked then
  for i:=0 to VSTHost.BlockSize-1 do
   begin
    SingleBuffer[0]^[i]:=Random;
    SingleBuffer[1]^[i]:=Random;
   end
 else if Rb_Impulse.Checked then
  begin
   FillChar(SingleBuffer[0]^,VSTHost.BlockSize*SizeOf(Single),0);
   FillChar(SingleBuffer[1]^,VSTHost.BlockSize*SizeOf(Single),0);
   SingleBuffer[0]^[0]:=1;
   SingleBuffer[1]^[0]:=1;
  end;
 VSTHost.VSTPlugIns[0].processReplacing(@SingleBuffer[0], @SingleBuffer[0], VSTHost.BlockSize);
 VSTHost.VSTPlugIns[0].MainsChanged(False);
 FreeMem(SingleBuffer[0]);
 FreeMem(SingleBuffer[1]);
end;

procedure TFmVSTDebug.Bt_GoClick(Sender: TObject);
var SingleBuffer  : array of PSingleArray;
    InputBuffer   : array of PSingleArray;
    i,j           : Integer;
    A,B,freq      : Int64;
    time          : Double;
begin
 Bt_Go.Tag:=Bt_Go.Tag+1;
 SetLength(SingleBuffer,2);
 SetLength(InputBuffer,2);
 VSTHost.VSTPlugIns[0].SetBlockSizeAndSampleRate(VSTHost.BlockSize,VSTHost.VstTimeInfo.SampleRate);
 VSTHost.VSTPlugIns[0].MainsChanged(True);
 GetMem(SingleBuffer[0],VSTHost.BlockSize*SizeOf(Single));
 GetMem(SingleBuffer[1],VSTHost.BlockSize*SizeOf(Single));
 GetMem(InputBuffer[0],VSTHost.BlockSize*SizeOf(Single));
 GetMem(InputBuffer[1],VSTHost.BlockSize*SizeOf(Single));

 if Rb_Random.Checked then
  for i:=0 to VSTHost.BlockSize-1 do
   begin
    InputBuffer[0]^[i]:=Random;
    InputBuffer[1]^[i]:=Random;
   end
 else if Rb_Impulse.Checked then
  begin
   FillChar(InputBuffer[0]^,VSTHost.BlockSize*SizeOf(Single),0);
   FillChar(InputBuffer[1]^,VSTHost.BlockSize*SizeOf(Single),0);
   InputBuffer[0]^[0]:=1;
   InputBuffer[1]^[0]:=1;
  end;

 QueryPerformanceFrequency(freq);
 QueryPerformanceCounter(A);

 for j:=0 to SE_Blocks.Value
  do VSTHost.VSTPlugIns[0].processReplacing(@InputBuffer[0], @SingleBuffer[0], VSTHost.BlockSize);

 QueryPerformanceCounter(B);

 time:=(B-A)/freq;
 if CB_M.Checked
  then Ed_Results.Tag:=round((Ed_Results.Tag*(Bt_Go.Tag-1)+time*1000)/Bt_Go.Tag)
  else Ed_Results.Tag:=round(time*1000);
 Ed_Results.Text:='msec: '+IntToStr(Ed_Results.Tag);

 VSTHost.VSTPlugIns[0].MainsChanged(False);
 FreeMem(InputBuffer[0]);
 FreeMem(InputBuffer[1]);
 FreeMem(SingleBuffer[0]);
 FreeMem(SingleBuffer[1]);
end;

procedure TFmVSTDebug.CB_MClick(Sender: TObject);
begin
 Bt_Go.Tag:=0;
end;

end.
