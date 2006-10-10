unit OptionsForm;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf,  LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  DVSTEffect, MiniHostForm, DASIOHost, ASIO;

type
  TOptions = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Button1: TButton;
    info: TMemo;
    Label11: TLabel;
    Label12: TLabel;
    ScrollBar3: TScrollBar;
    ScrollBar1: TScrollBar;
    ScrollBar5: TScrollBar;
    ScrollBar6: TScrollBar;
    ScrollBar2: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ScrollBar3Change(Sender: TObject);
  public
    Host: TFmMiniHost;
    procedure FillInfo;
  end;

var Options: TOptions;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses DVstHost;

procedure TOptions.FormCreate(Sender: TObject);
begin
 info.left := 8;
end;

procedure TOptions.FillInfo;
var o, i: integer;
    s: TStrings;
    b: boolean;
begin
 Host.StopAudio;
 info.clear;
 with host.ASIOHost do
 begin
  o := DriverIndex;
  s := host.ASIOHost.DriverList;
  info.Lines.add('number of ASIO drivers: '+inttostr(s.Count));
  info.Lines.add('');
  for i := 0 to s.Count - 1 do
  begin
   DriverIndex := i;
   info.lines.add('driver #' + inttostr(i) + ': ');
   info.Lines.add('name: ' + DriverName);
   info.Lines.add('version: ' + inttostr(DriverVersion));
   info.Lines.add('input channels: ' + inttostr(InputChannels));
   info.Lines.add('output channels: ' + inttostr(OutputChannels));
   info.Lines.add('input format: ' +
    ChannelTypeToString(InputChannelInfos[0].vType) +
    ' (' + inttostr(InputChannelInfos[0].vType) + ')');
   info.Lines.add('output format: ' +
    ChannelTypeToString(OutputChannelInfos[0].vType) +
    ' (' + inttostr(OutputChannelInfos[0].vType) + ')');
   info.Lines.add('input latency: ' + inttostr(InputLatency));
   info.Lines.add('output latency: ' + inttostr(OutputLatency));
   info.Lines.add('buffer size: ' + inttostr(BufferSize));
   info.Lines.add('min size: ' + inttostr(BufferMinimum));
   info.Lines.add('max size: ' + inttostr(BufferMaximum));
   info.Lines.add('pref size: ' + inttostr(BufferPreferredSize));
   info.Lines.add('granularity: ' + inttostr(BufferGranularity));
   info.Lines.add('samplerate: ' + floattostr(samplerate));
   b := cansamplerate(8000) = ASE_OK;
   info.Lines.add('samplerate 8000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := cansamplerate(16000) = ASE_OK;
   info.Lines.add('samplerate 16000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := cansamplerate(22050) = ASE_OK;
   info.Lines.add('samplerate 22050 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := cansamplerate(32000) = ASE_OK;
   info.Lines.add('samplerate 32000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := cansamplerate(44100) = ASE_OK;
   info.Lines.add('samplerate 44100 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := cansamplerate(48000) = ASE_OK;
   info.Lines.add('samplerate 48000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := cansamplerate(96000) = ASE_OK;
   info.Lines.add('samplerate 96000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := cansamplerate(192000) = ASE_OK;
   info.Lines.add('samplerate 192000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   info.Lines.add('');
  end;
  DriverIndex := o;
  Host.StartAudio;
 end;
end;

procedure TOptions.Button1Click(Sender: TObject);
begin
 info.visible := not info.visible;
 if (info.lines.Count = 0) and (info.visible) then
  FillInfo;
end;

procedure TOptions.FormShow(Sender: TObject);
begin
 groupbox2.setfocus;
end;

procedure TOptions.ScrollBar3Change(Sender: TObject);
begin
 if not assigned(Host) then exit;
 Host.OverallVol := scrollbar2.position / 100;
 label6.caption := 'Overall Volume: ' + inttostr(scrollbar2.position);
 Host.VSTVol := scrollbar6.position / 100;
 if effFlagsIsSynth in Host.VSTHost.VSTPlugIns[0].EffectOptions
  then label12.caption := 'VST Volume: ' + inttostr(scrollbar6.position)
  else label12.caption := 'VST Dry/Wet Mix: ' + inttostr(scrollbar6.position);
 Host.InputVol := scrollbar5.position / 100;
 label11.caption := 'Input Volume: ' + inttostr(scrollbar5.position);
 Host.Wavefile.Vol := scrollbar1.position / 100;
 label5.caption := 'WAV Volume: ' + inttostr(scrollbar1.position);
 Host.VSTHost.Tempo := scrollbar3.position;
 label7.caption := 'Tempo: ' + inttostr(scrollbar3.position)+' bpm';
end;

{$IFDEF FPC}
initialization
  {$i OptionsForm.lrs}
  {$i OptionsForm.lrs}
{$ENDIF}

end.
