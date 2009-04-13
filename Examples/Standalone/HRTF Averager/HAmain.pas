unit HAmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, Spin, DAV_GuiAudioDataDisplay, ComCtrls,
  DAV_DspHrtf, DAV_AudioData, DAV_DspFftReal2Complex;

type
  TFmHrtfAverager = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MISelectDirectory: TMenuItem;
    MISaveAs: TMenuItem;
    N1: TMenuItem;
    MIExit: TMenuItem;
    AudioDataDisplayLeft: TGuiAudioDataDisplay;
    AudioDataDisplayRight: TGuiAudioDataDisplay;
    LbPolarUnit: TLabel;
    SEPolar: TSpinEdit;
    LbPolar: TLabel;
    LbAzimuthUnit: TLabel;
    SEAzimuth: TSpinEdit;
    LbAzimuth: TLabel;
    SEHrtfIndex: TSpinEdit;
    LbHrtfIndex: TLabel;
    ADHRIR: TAudioDataCollection32;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    procedure MIExitClick(Sender: TObject);
    procedure MISelectDirectoryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure SEHrtfIndexChange(Sender: TObject);
    procedure SEAzimuthChange(Sender: TObject);
    procedure SEPolarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FHRTFFile : THrtfs;
    FFft      : TFftReal2ComplexNativeFloat32;
    procedure HRTFFileChanged;
  end;

var
  FmHrtfAverager: TFmHrtfAverager;

implementation

{$R *.dfm}

uses
  DAV_Complex, FileCtrl;

const
  CDegToRad : Single = 2 * Pi / 360;

procedure TFmHrtfAverager.FormCreate(Sender: TObject);
begin
 FHRTFFile := THrtfs.Create;
 FFft      := TFftReal2ComplexNativeFloat32.Create;
end;

procedure TFmHrtfAverager.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FHRTFFile);
 FreeAndNil(FFft);
end;

procedure TFmHrtfAverager.FormResize(Sender: TObject);
begin
 AudioDataDisplayLeft.Height := (ClientHeight - SEHrtfIndex.Height - 48) div 2;
 AudioDataDisplayRight.Height := AudioDataDisplayLeft.Height;
 AudioDataDisplayRight.Top := AudioDataDisplayLeft.Top + AudioDataDisplayLeft.Height + 6;
end;

procedure TFmHrtfAverager.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmHrtfAverager.MISaveAsClick(Sender: TObject);
begin
 with SaveDialog do
  if Execute
   then FHRTFFile.SaveToFile(FileName);
end;

procedure TFmHrtfAverager.MISelectDirectoryClick(Sender: TObject);
var
  Dir     : string;
  SR      : TSearchRec;
  HRTF    : THrtfs;
  Hrir    : TCustomHrir;
  ADC     : TAudioDataCollection32;
  HRTFCnt : Integer;
  Sample  : Integer;
  Channel : Integer;
  i       : Integer;
  Scale   : array [0..1] of Double;
  FreqDom : array [0..1] of PDAVComplexSingleFixedArray;
begin
 Dir := 'C:\Users\Christian Budde\Projects\VSTPack\Resources\HRTFs';
 HRTFCnt := 0;
 SelectDirectory('Select a directory', '', Dir);

 if DirectoryExists(Dir) then
  begin
   FHRTFFile.ClearHrirs;
   HRTF := THrtfs.Create;
   try
    ADC := TAudioDataCollection32.Create(Self);
    try
     // set stereo
     ADC.ChannelCount := 2;

     if FindFirst(Dir + '\*.HRTF', 0, SR) = 0 then
      repeat

       // load HRTF file
       HRTF.ClearHrirs;
       HRTF.LoadFromFile(Dir + '\' + SR.Name);

       // inform user
       StatusBar.SimpleText := SR.Name;
       Application.ProcessMessages;

       // if HRTF file contains no data continue to next HRTF
       if HRTF.HrirCount = 0 then Continue;

       inc(HRTFCnt);

       if FHRTFFile.HrirCount = 0 then
        begin
         for i := 0 to HRTF.HrirCount - 1 do
//          if assigned(HRTF.Hrir[i]) then
           begin
            Hrir := TCustomHrir.Create;
            Hrir.Assign(HRTF.Hrir[i]);
            FHRTFFile.AddChunk(Hrir);
           end;
        end
       else
        begin
         // calculate scale factors
         Scale[0] := 1 / HRTFCnt;
         Scale[1] := 1 - Scale[0];

         for i := 0 to HRTF.HrirCount - 1 do
          begin
           ADC.SampleFrames := HRTF.Hrir[i].SampleFrames;
           HRTF.GetHrirByIndex(i, ADC.SampleFrames, ADC[0].ChannelDataPointer,
             ADC[1].ChannelDataPointer);

           ADHRIR.SampleFrames := ADC.SampleFrames;
           FHRTFFile.InterpolateHrir(HRTF.Hrir[i].Azimuth, HRTF.Hrir[i].Polar,
             ADC.SampleFrames, ADHRIR[0].ChannelDataPointer,
             ADHRIR[1].ChannelDataPointer);

           // actual interpolation
           GetMem(FreqDom[0], (ADHRIR.SampleFrames div 2 + 1) * SizeOf(Single));
           GetMem(FreqDom[1], (ADHRIR.SampleFrames div 2 + 1) * SizeOf(Single));
           try
            for Sample := 0 to ADHRIR.SampleFrames - 1 do
             for Channel := 0 to ADHRIR.ChannelCount - 1 do
              begin
               // ToDo: Perform FFT here -> FreqDom[0]
               // ToDo: Perform FFT here -> FreqDom[1]

               ADHRIR[Channel].ChannelDataPointer^[Sample] :=
                 Scale[0] * ADC[Channel].ChannelDataPointer^[Sample] +
                 Scale[1] * ADHRIR[Channel].ChannelDataPointer^[Sample];

               // ToDo: Perform iFFT here -> ADHRIR[Channel].ChannelDataPointer
              end;
           finally
            Dispose(FreqDom[0]);
            Dispose(FreqDom[1]);
           end;
           FHRTFFile.Hrir[i].AssignLeft32(ADHRIR[0].ChannelDataPointer, ADC.SampleFrames);
           FHRTFFile.Hrir[i].AssignRight32(ADHRIR[1].ChannelDataPointer, ADC.SampleFrames);
          end;
        end;
      until FindNext(SR) <> 0;
     FindClose(sr);
    finally
     FreeAndNil(ADC);
    end;
   finally
    FreeAndNil(HRTF);
   end;
  end;
 HRTFFileChanged;
end;

procedure TFmHrtfAverager.SEAzimuthChange(Sender: TObject);
begin
 FHRTFFile.InterpolateHrir(SEAzimuth.Value * CDegToRad,
   SEPolar.Value * CDegToRad, ADHRIR.SampleFrames,
   ADHRIR[0].ChannelDataPointer, ADHRIR[1].ChannelDataPointer);
 AudioDataDisplayLeft.Invalidate;
 AudioDataDisplayRight.Invalidate;
end;

procedure TFmHrtfAverager.SEHrtfIndexChange(Sender: TObject);
begin
 assert(SEHrtfIndex.Value < FHRTFFile.HrirCount);
 FHRTFFile.GetHrirByIndex(SEHrtfIndex.Value, ADHRIR.SampleFrames,
   ADHRIR[0].ChannelDataPointer, ADHRIR[1].ChannelDataPointer);
 AudioDataDisplayLeft.Invalidate;
 AudioDataDisplayRight.Invalidate;
 with FHRTFFile.Hrir[SEHrtfIndex.Value] do
  begin
   SEAzimuth.Value := round(360 / (2 * Pi) * Azimuth);
   SEPolar.Value := round(360 / (2 * Pi) * Polar);
  end;
end;

procedure TFmHrtfAverager.SEPolarChange(Sender: TObject);
begin
 FHRTFFile.InterpolateHrir(SEAzimuth.Value * CDegToRad,
   SEPolar.Value * CDegToRad, ADHRIR.SampleFrames,
   ADHRIR[0].ChannelDataPointer, ADHRIR[1].ChannelDataPointer);
 AudioDataDisplayLeft.Invalidate;
 AudioDataDisplayRight.Invalidate;
end;

procedure TFmHrtfAverager.HRTFFileChanged;
begin
 LbHrtfIndex.Enabled   := FHRTFFile.HrirCount > 0;
 SEHrtfIndex.Enabled   := LbHrtfIndex.Enabled;
 LbAzimuth.Enabled     := LbHrtfIndex.Enabled;
 SEAzimuth.Enabled     := LbHrtfIndex.Enabled;
 LbPolar.Enabled       := LbHrtfIndex.Enabled;
 SEPolar.Enabled       := LbHrtfIndex.Enabled;
 LbPolarUnit.Enabled   := LbHrtfIndex.Enabled;
 LbAzimuthUnit.Enabled := LbHrtfIndex.Enabled;

 if SEHrtfIndex.Enabled
  then SEHrtfIndex.MaxValue := FHRTFFile.HrirCount - 1;
end;

end.
