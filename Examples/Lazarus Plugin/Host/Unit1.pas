unit Unit1;

interface

uses
  Windows, Messages, SysUtils, DVSTEffect, Classes, Graphics, Controls, Forms,
  Dialogs, DVSTHost, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    VstHost1: TVstHost;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

function main(audioMaster: TAudioMasterCallbackFunc ): PVSTEffect; cdecl; external 'VSTPlugin';

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
 VSTHost1[0].Active:=True;
 ShowMessage(VSTHost1[0].uID);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 VSTHost1[0].PVstEffect:=Main(audioMaster);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
 with VSTHost1[0] do
  begin
   if Active and EditVisible
    then CloseEdit;
   Active:=False;
   VSTHost1[0].DLLFileName:='C:\ITA\Projects\VST Package\Examples\Lazarus Plugin\VSTplugin.dll';
   Active:=True;

   Idle;
   ShowEdit(TForm(Form1));
   Idle;
   EditIdle;
   Caption := 'VST Plugin Analyser - '+GetVendorString + ': ' +GetEffectName;
  end;
end;

end.
