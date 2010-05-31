unit FopMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ComCtrls, ExtCtrls, Spin, Math, TeEngine, Series, Dialogs,
  DAV_Types, DAV_AudioData, DAV_DspFilterChebyshevType1;

const
  C3rdOctaveFrequencyCount = 32;
  C3rdOctaveFrequencies : array [0..C3rdOctaveFrequencyCount - 1] of Single =
    (16, 20, 25, 31, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500,
     630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000,
     10000, 12500, 16000, 20000);
  CDS = 8;
  CBW = 0.4;
  CBandReserve = 0.25;

type
  TDownsampleFilterRecord = record
    Lowpass      : TChebyshev1LowpassFilter;
    Highpass     : TChebyshev1HighpassFilter;
    Downsampling : Integer;
    SampleCount  : Integer;
    Sum          : Double;
  end;

  TFmFractionalOctaveProcessor = class(TForm)
    BtProcess: TButton;
    LbInputFile: TLabel;
    EdInput: TEdit;
    LbOutputFile: TLabel;
    EdOutput: TEdit;
    BtSelectInputFile: TButton;
    BtSelectOutputFile: TButton;
    RbFilter: TRadioButton;
    LbBandSeparation: TLabel;
    RbFFT: TRadioButton;
    CbDownsampling: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtSelectInputFileClick(Sender: TObject);
    procedure BtSelectOutputFileClick(Sender: TObject);
    procedure BtProcessClick(Sender: TObject);
  protected
    FMaxDSStages     : Integer;
    FDownSampleCount : Integer;
    FDownSampleMax   : Integer;

    FFilterArray     : array of TDownsampleFilterRecord;
    procedure SetupFilters(AudioFileSampleRate: Single); virtual;
    procedure ProcessAudioData(Data: TAudioChannel32);
  end;

var
  FmFractionalOctaveProcessor: TFmFractionalOctaveProcessor;

implementation

{$R *.DFM}

uses
  Inifiles, Registry, DAV_Common, DAV_AudioFile, DAV_AudioFileWAV,
  DAV_AudioFileAIFF, DAV_AudioFileAU;

procedure TFmFractionalOctaveProcessor.FormShow(Sender: TObject);
begin
 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Processor.ini') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
  finally
   Free;
  end;
end;

procedure TFmFractionalOctaveProcessor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 // save settings
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Processor.ini') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
  finally
   Free;
  end;
end;

procedure TFmFractionalOctaveProcessor.BtSelectInputFileClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   Filter := 'WAV-File (*.wav)|*.wav|AIFF-File (*.aiff)|*.aiff|' + 
     'AU-File (*.au)|*.au';
   DefaultExt := '.wav';
      
   if Execute 
    then EdInput.Text := FileName;
  finally
   Free;
  end;
end;

procedure TFmFractionalOctaveProcessor.BtSelectOutputFileClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   Filter := 'Comma Separated File (*.csv)|*.csv|' +
     'Excel (*.xls)|*.xls';
   DefaultExt := '.csv';
      
   if Execute 
    then EdOutput.Text := FileName;
  finally
   Free;
  end;
end;

procedure TFmFractionalOctaveProcessor.BtProcessClick(Sender: TObject);
var
  AudioData : TAudioDataCollection32;
  Channel   : Integer;
begin
 if not FileExists(EdInput.Text)
  then MessageDlg('Input file does not exist!', mtError, [mbOK], 0);

 if EdOutput.Text = ''
  then MessageDlg('Output file invalid!', mtError, [mbOK], 0);
  
 with TAudioDataCollection32.Create(Self) do
  try
   LoadFromFile(EdInput.Text);

   SetupFilters(SampleRate);

   for Channel := 0 to ChannelCount - 1 do
    begin
     ProcessAudioData(TAudioChannel32(Channels[Channel]));
    end;   
  finally
   Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFmFractionalOctaveProcessor.SetupFilters(AudioFileSampleRate: Single);
var
  Band         : Integer;
  Downsampling : Integer;
  DesiredFreq  : Double;
const
  HalfThirdMulFak : Double = 1.1224620483093729814335330496792; // = Power(2,1/6)
begin
 Downsampling := 0;

 SetLength(FFilterArray, C3rdOctaveFrequencyCount);

 for Band := 0 to Length(FFilterArray) - 1 do
  begin
   // Lowpass
   DesiredFreq := C3rdOctaveFrequencies[C3rdOctaveFrequencyCount - Band - 1] * HalfThirdMulFak;
   if DesiredFreq > 0.499 * AudioFileSampleRate then DesiredFreq := 0.499 * AudioFileSampleRate;

   if CbDownsampling.Checked then
    while ((2 * DesiredFreq / AudioFileSampleRate) * (1 shl Downsampling)) < CBandReserve
     do Inc(Downsampling);

   // eventually create filter
   if not assigned(FFilterArray[Band].Lowpass)
    then FFilterArray[Band].Lowpass := TChebyshev1LowpassFilter.Create(10);

   with FFilterArray[Band].Lowpass do
    begin
     SampleRate := AudioFileSampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
   FFilterArray[Band].Downsampling := (1 shl Downsampling);

   // Highpass
   DesiredFreq := C3rdOctaveFrequencies[C3rdOctaveFrequencyCount - Band - 1] / HalfThirdMulFak;

   // eventually create filter
   if not assigned(FFilterArray[Band].Highpass)
    then FFilterArray[Band].Highpass := TChebyshev1HighpassFilter.Create(12);

   with FFilterArray[Band].Highpass do
    begin
     SampleRate := AudioFileSampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
  end;
 FDownSampleMax := 1 shl Downsampling;
end;

procedure TFmFractionalOctaveProcessor.ProcessAudioData(Data: TAudioChannel32);
var
  i, j, r : Integer;
  d, z, s : Double;
const
  cDenorm = 1E-32;
begin
 // clear filter records
 for j := 0 to Length(FFilterArray) - 1 do
  begin
   FFilterArray[j].Lowpass.ResetStates;
   FFilterArray[j].Highpass.ResetStates;
   FFilterArray[j].SampleCount := 0;
   FFilterArray[j].Sum := 0;
  end;

 if CbDownsampling.Checked then
  begin
   for i := 0 to Data.SampleCount - 1 do
    begin
     d := Data.ChannelDataPointer^[i];
     for j := 0 to Length(FFilterArray) - 1 do
      begin
       if (FDownSampleCount mod FFilterArray[j].Downsampling) <> 0
        then Break;

       d := FFilterArray[j].Lowpass.ProcessSample64(d + cDenorm);
       z := FFilterArray[j].Highpass.ProcessSample64(d + cDenorm);

       Inc(FFilterArray[j].SampleCount);
       FFilterArray[j].Sum := FFilterArray[j].Sum + Amp_to_dB(abs(z));
      end;
     Inc(FDownSampleCount);
     if FDownSampleCount >= FDownSampleMax then FDownSampleCount := 0;
    end;
  end
 else
  begin
   for i := 0 to Data.SampleCount - 1 do
    begin
     d := Data.ChannelDataPointer^[i];
     for j := 0 to Length(FFilterArray) - 1 do
      begin
       d := FFilterArray[j].Lowpass.ProcessSample64(d + cDenorm);
       z := FFilterArray[j].Highpass.ProcessSample64(d + cDenorm);

       Inc(FFilterArray[j].SampleCount);
       FFilterArray[j].Sum := FFilterArray[j].Sum + Amp_to_dB(abs(z));
      end;
    end;
  end;

(*
 // free filters
 for i := 0 to C3rdOctaveFrequencyCount - 1 do
  begin
   FreeAndNil(FFilterArray[i].Lowpass);
   FreeAndNil(FFilterArray[i].Highpass);
  end;
*)
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  Set8087CW(Default8087CW or $3F);

end.
