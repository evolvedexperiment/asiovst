unit VOLGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  StdCtrls, Controls;

type
  TFmVOL = class(TForm)
    MOpcodeLog: TMemo;
    BtClear: TButton;
    BtUpdate: TButton;
    CBAutoUpdates: TCheckBox;
    procedure BtUpdateClick(Sender: TObject);
    procedure BtClearClick(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses
  VOLDM;

procedure TFmVOL.BtClearClick(Sender: TObject);
begin
 with TVOLDataModule(Owner) do
  begin
   OpcodeLog.Clear;
   MOpcodeLog.Lines.Assign(OpcodeLog);
  end;
end;

procedure TFmVOL.BtUpdateClick(Sender: TObject);
begin
 with TVOLDataModule(Owner) do
  begin
   MOpcodeLog.Lines.Assign(OpcodeLog);
  end;
end;

end.