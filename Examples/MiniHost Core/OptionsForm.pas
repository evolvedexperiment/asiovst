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
    Info: TMemo;
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
 Info.left := 8;
end;

procedure TOptions.FillInfo;
var o, i: integer;
    s: TStrings;
    b: boolean;
begin
 Host.StopAudio;
 Info.clear;
 with host.ASIOHost do
 begin
  o := DriverIndex;
  s := host.ASIOHost.DriverList;
  Info.Lines.Add('number of ASIO drivers: '+IntToStr(s.Count));
  Info.Lines.Add('');
  for i := 0 to s.Count - 1 do
  begin
   DriverIndex := i;
   Info.lines.Add('driver #' + IntToStr(i) + ': ');
   Info.Lines.Add('name: ' + DriverName);
   Info.Lines.Add('version: ' + IntToStr(DriverVersion));
   Info.Lines.Add('input channels: ' + IntToStr(InputChannels));
   Info.Lines.Add('output channels: ' + IntToStr(OutputChannels));
   Info.Lines.Add('input format: ' +
    ChannelTypeToString(InputChannelInfos[0].vType) +
    ' (' + IntToStr(InputChannelInfos[0].vType) + ')');
   Info.Lines.Add('output format: ' +
    ChannelTypeToString(OutputChannelInfos[0].vType) +
    ' (' + IntToStr(OutputChannelInfos[0].vType) + ')');
   Info.Lines.Add('input latency: ' + IntToStr(InputLatency));
   Info.Lines.Add('output latency: ' + IntToStr(OutputLatency));
   Info.Lines.Add('buffer size: ' + IntToStr(BufferSize));
   Info.Lines.Add('min size: ' + IntToStr(BufferMinimum));
   Info.Lines.Add('max size: ' + IntToStr(BufferMaximum));
   Info.Lines.Add('pref size: ' + IntToStr(BufferPreferredSize));
   Info.Lines.Add('granularity: ' + IntToStr(BufferGranularity));
   Info.Lines.Add('samplerate: ' + FloatToStr(samplerate));
   b := canSamplerate(8000) = ASE_OK;
   Info.Lines.Add('samplerate 8000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := canSamplerate(16000) = ASE_OK;
   Info.Lines.Add('samplerate 16000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := canSamplerate(22050) = ASE_OK;
   Info.Lines.Add('samplerate 22050 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := canSamplerate(32000) = ASE_OK;
   Info.Lines.Add('samplerate 32000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := canSamplerate(44100) = ASE_OK;
   Info.Lines.Add('samplerate 44100 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := canSamplerate(48000) = ASE_OK;
   Info.Lines.Add('samplerate 48000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := canSamplerate(96000) = ASE_OK;
   Info.Lines.Add('samplerate 96000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   b := canSamplerate(192000) = ASE_OK;
   Info.Lines.Add('samplerate 192000 Hz possible: ' + booltostr(b{$IFNDEF FPC}, true{$ENDIF}));
   Info.Lines.Add('');
  end;
  DriverIndex := o;
  Host.StartAudio;
 end;
end;

procedure TOptions.Button1Click(Sender: TObject);
begin
 Info.Visible := not Info.Visible;
 if (Info.Lines.Count = 0) and (Info.Visible)
  then FillInfo;
end;

procedure TOptions.FormShow(Sender: TObject);
begin
 Groupbox2.SetFocus;
end;

procedure TOptions.ScrollBar3Change(Sender: TObject);
begin
 if not assigned(Host) then exit;
 Host.OverallVol := Scrollbar2.position * 0.01;
 Label6.caption := 'Overall Volume: ' + IntToStr(Scrollbar2.position);
 Host.VSTVol := Scrollbar6.position * 0.01;
 if effFlagsIsSynth in Host.VSTHost.VSTPlugIns[0].EffectOptions
  then label12.caption := 'VST Volume: ' + IntToStr(Scrollbar6.position)
  else label12.caption := 'VST Dry/Wet Mix: ' + IntToStr(Scrollbar6.position);
 Host.InputVol := Scrollbar5.position * 0.01;
 Label11.caption := 'Input Volume: ' + IntToStr(Scrollbar5.position);
 Host.Wavefile.Vol := scrollbar1.position * 0.01;
 Label5.caption := 'WAV Volume: ' + IntToStr(Scrollbar1.position);
 Host.VSTHost.Tempo := Scrollbar3.position;
 Label7.caption := 'Tempo: ' + IntToStr(Scrollbar3.position)+' bpm';
end;

{$IFDEF FPC}
initialization
  {$i OptionsForm.lrs}
  {$i OptionsForm.lrs}
{$ENDIF}

end.
