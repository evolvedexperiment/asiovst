unit OptionsForm;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf,  LResources, Buttons, LMessages,{$ELSE} Windows,
  Messages,{$ENDIF} Classes, Controls, Forms, StdCtrls, MiniHostForm;

type
  TFormOptions = class(TForm)
    ButtonInfo: TButton;
    GroupBoxASIO: TGroupBox;
    GroupBoxGlobalSetting: TGroupBox;
    LabelASIODriver: TLabel;
    LabelBufferSize: TLabel;
    LabelFormat: TLabel;
    LabelInputs: TLabel;
    LabelInputVolume: TLabel;
    LabelOutputs: TLabel;
    LabelOverallVolume: TLabel;
    LabelSampleRate: TLabel;
    LabelTempo: TLabel;
    LabelVSTVolume: TLabel;
    LabelWavVolume: TLabel;
    MemoInfo: TMemo;
    ScrollBarInputVolume: TScrollBar;
    ScrollBarOverallVolume: TScrollBar;
    ScrollBarTempo: TScrollBar;
    ScrollBarVSTVolume: TScrollBar;
    ScrollBarWavVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonInfoClick(Sender: TObject);
    procedure ScrollBarTempoChange(Sender: TObject);
  public
    Host: TFormMiniHost;
    procedure FillInfo;
  end;

var
  FormOptions : TFormOptions;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  SysUtils, DAV_VSTEffect, DAV_ASIOHost, DAV_ASIO;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  MemoInfo.left := 8;
end;

procedure TFormOptions.FillInfo;
var
  o, i: integer;
  s: TStrings;
begin
  Host.StopAudio;
  with Host.ASIOHost, MemoInfo do
  begin
    Clear;
    o := DriverIndex;
    s := host.ASIOHost.DriverList;
    Lines.Add('number of ASIO drivers: '+IntToStr(s.Count));
    Lines.Add('');
    for i := 0 to s.Count - 1 do
      with Lines do
      begin
        DriverIndex := i;
        Add('driver #' + IntToStr(i) + ': ');
        Add('name: ' + DriverName);
        Add('version: ' + IntToStr(DriverVersion));
        Add('input channels: ' + IntToStr(InputChannelCount));
        Add('output channels: ' + IntToStr(OutputChannelCount));
        Add('input format: ' +
          ChannelTypeToString(InputChannelInfos[0].SampleType) +
          ' (' + IntToStr(InputChannelInfos[0].SampleType) + ')');
        Add('output format: ' +
          ChannelTypeToString(OutputChannelInfos[0].SampleType) +
          ' (' + IntToStr(OutputChannelInfos[0].SampleType) + ')');
        Add('input latency: '  + IntToStr(InputLatency));
        Add('output latency: ' + IntToStr(OutputLatency));
        Add('buffer size: ' + IntToStr(BufferSize));
        Add('min size: '    + IntToStr(BufferMinimum));
        Add('max size: '    + IntToStr(BufferMaximum));
        Add('pref size: '   + IntToStr(BufferPreferredSize));
        Add('granularity: ' + IntToStr(BufferGranularity));
        Add('samplerate: '  + FloatToStr(SampleRate));
        Add('samplerate 8000 Hz possible: '   + BoolToStr(canSamplerate(  8000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
        Add('samplerate 16000 Hz possible: '  + BoolToStr(canSamplerate( 16000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
        Add('samplerate 22050 Hz possible: '  + BoolToStr(canSamplerate( 22050) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
        Add('samplerate 32000 Hz possible: '  + BoolToStr(canSamplerate( 32000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
        Add('samplerate 44100 Hz possible: '  + BoolToStr(canSamplerate( 44100) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
        Add('samplerate 48000 Hz possible: '  + BoolToStr(canSamplerate( 48000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
        Add('samplerate 96000 Hz possible: '  + BoolToStr(canSamplerate( 96000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
        Add('samplerate 192000 Hz possible: ' + BoolToStr(canSamplerate(192000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
        Add('');
      end;
    DriverIndex := o;
    Host.StartAudio;
  end;
end;

procedure TFormOptions.ButtonInfoClick(Sender: TObject);
begin
  MemoInfo.Visible := not MemoInfo.Visible;
  if (MemoInfo.Lines.Count = 0) and (MemoInfo.Visible) then
    FillInfo;
end;

procedure TFormOptions.FormShow(Sender: TObject);
begin
  GroupBoxGlobalSetting.SetFocus;
end;

procedure TFormOptions.ScrollBarTempoChange(Sender: TObject);
begin
  if not Assigned(Host) then
    Exit;
  Host.OverallVolume := ScrollBarOverallVolume.Position * 0.01;
  LabelOverallVolume.Caption := 'Overall Volume: ' + IntToStr(ScrollBarOverallVolume.Position);
  Host.VSTVol := ScrollBarVSTVolume.Position * 0.01;
  if effFlagsIsSynth in Host.VSTHost[0].EffectOptions then
    LabelVSTVolume.Caption := 'VST Volume: ' + IntToStr(ScrollBarVSTVolume.Position)
  else
    LabelVSTVolume.Caption := 'VST Dry/Wet Mix: ' + IntToStr(ScrollBarVSTVolume.Position);
  Host.InputVol := ScrollBarInputVolume.Position * 0.01;
  LabelInputVolume.Caption := 'Input Volume: ' + IntToStr(ScrollBarInputVolume.Position);
  Host.Wavefile.Volume := ScrollBarWavVolume.Position * 0.01;
  LabelWavVolume.Caption := 'WAV Volume: ' + IntToStr(ScrollBarWavVolume.Position);
  Host.VSTHost.Tempo := ScrollBarTempo.Position;
  LabelTempo.Caption := 'Tempo: ' + IntToStr(ScrollBarTempo.Position) + ' bpm';
end;

{$IFDEF FPC}
initialization
  {$i OptionsForm.lrs}
{$ENDIF}

end.
