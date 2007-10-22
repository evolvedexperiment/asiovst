unit ASIOVSTGUI;

{$I ASIOVST.INC}

interface

uses Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
     DASIOHost, Controls, StdCtrls;

type
  TFmASIOVST = class(TForm)
    Lb_ASIOOutput: TLabel;
    CB_ASIO: TComboBox;
    Memo: TMemo;
    CBShortCircuit: TCheckBox;
    procedure CB_ASIOChange(Sender: TObject);
    procedure Lb_ASIOOutputClick(Sender: TObject);
    procedure CBShortCircuitClick(Sender: TObject);
  public
    procedure DisplayASIOInformation;
  end;

implementation

{$R *.DFM}

uses ASIOVSTModule;

procedure TFmASIOVST.CBShortCircuitClick(Sender: TObject);
begin
 with TASIOVSTModule(Owner) do
  begin
   if not CBShortCircuit.Checked
    then AsioHost.OnBufferSwitch32 := AHBufferSwitch
    else AsioHost.OnBufferSwitch32 := AHShortCircuit;
  end;
end;

procedure TFmASIOVST.CB_ASIOChange(Sender: TObject);
begin
 with TASIOVSTModule(Owner) do
  begin
   Parameter[0] := CB_ASIO.ItemIndex;
   DisplayASIOInformation;
  end;
end;

procedure TFmASIOVST.DisplayASIOInformation;
begin
 with TASIOVSTModule(Owner) do
  begin
   Memo.Clear;
   Memo.Lines.Add('Driver: ' + AsioHost.DriverName);
   Memo.Lines.Add('Buffersize: ' + IntToStr(AsioHost.BufferSize));
   Memo.Lines.Add('Latency: ' + IntToStr(AsioHost.InputLatency + AsioHost.OutputLatency + AsioHost.BufferSize));
   Memo.Lines.Add('Channel 1: ' + AsioHost.OutputChannelInfos[0].name);
   Memo.Lines.Add('Channel 2: ' + AsioHost.OutputChannelInfos[1].name);
   Memo.Lines.Add('Format In 1: ' + ChannelTypeToString(AsioHost.OutputChannelInfos[0].vType));
   Memo.Lines.Add('Format In 2: ' + ChannelTypeToString(AsioHost.OutputChannelInfos[1].vType));
   Memo.Lines.Add('Format Out 1: ' + ChannelTypeToString(AsioHost.OutputChannelInfos[0].vType));
   Memo.Lines.Add('Format Out 2: ' + ChannelTypeToString(AsioHost.OutputChannelInfos[1].vType));
  end;
end;

procedure TFmASIOVST.Lb_ASIOOutputClick(Sender: TObject);
begin
 TASIOVSTModule(Owner).AsioHost.ControlPanel;
end;

end.
