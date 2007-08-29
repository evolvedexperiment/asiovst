unit ASIOVSTGUI;

{$I ASIOVST.INC}

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule,
     DASIOHost, Controls, StdCtrls;

type
  TFmASIOVST = class(TForm)
    Lb_ASIOOutput: TLabel;
    CB_ASIO: TComboBox;
    Memo: TMemo;
    procedure CB_ASIOChange(Sender: TObject);
    procedure Lb_ASIOOutputClick(Sender: TObject);
  public
    procedure DisplayASIOInformation;
  end;

implementation

{$R *.DFM}

uses ASIOVSTModule;

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
  end;
end;

procedure TFmASIOVST.Lb_ASIOOutputClick(Sender: TObject);
begin
 TASIOVSTModule(Owner).AsioHost.ControlPanel;
end;

end.
