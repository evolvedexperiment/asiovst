unit ASIOVSTGUI;

{$I DAV_Compiler.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Common,
  DAV_VSTModule, DAV_ASIOHost;

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

uses
  ASIOVSTModule;

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
 with TASIOVSTModule(Owner), AsioHost, Memo do
  begin
   Clear;
   Lines.Add('Driver: ' + DriverName);
   Lines.Add('Buffersize: ' + IntToStr(BufferSize));
   Lines.Add('Latency: ' + IntToStr(InputLatency + OutputLatency + Integer(BufferSize)));
   Lines.Add('Channel 1: ' + OutputChannelInfos[0].name);
   Lines.Add('Channel 2: ' + OutputChannelInfos[1].name);
   Lines.Add('Format In 1: ' + ChannelTypeToString(OutputChannelInfos[0].vType));
   Lines.Add('Format In 2: ' + ChannelTypeToString(OutputChannelInfos[1].vType));
   Lines.Add('Format Out 1: ' + ChannelTypeToString(OutputChannelInfos[0].vType));
   Lines.Add('Format Out 2: ' + ChannelTypeToString(OutputChannelInfos[1].vType));
  end;
end;

procedure TFmASIOVST.Lb_ASIOOutputClick(Sender: TObject);
begin
 TASIOVSTModule(Owner).AsioHost.ControlPanel;
end;

end.
