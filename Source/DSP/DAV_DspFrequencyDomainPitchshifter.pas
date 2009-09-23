unit DAV_DspFrequencyDomainPitchshifter;

// Yet unfinished!!!

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common, DAV_Classes, DAV_DspFftReal2Complex;

type
  TCustomFrequencyDomainPitchShifter = class(TDspSampleRatePersistent)
  private
    procedure SetPitch(const Value: Single);
  protected
    FPitch      : Single;
    procedure PitchChanged;
    procedure SampleRateChanged; override;
  public
    property Pitch: Single read FPitch write SetPitch;
  end;

  TFrequencyDomainPitchShifter32 = class(TCustomFrequencyDomainPitchShifter)
  protected
    FRealFFT : TFftReal2ComplexNativeFloat32;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessSamples(const Inputs, Outputs: PDavSingleFixedArray; const SampleFrames: Cardinal);
  end;

implementation

uses
  SysUtils;

{ TCustomFrequencyDomainPitchShifter }

procedure TCustomFrequencyDomainPitchShifter.PitchChanged;
begin
 //
end;

procedure TCustomFrequencyDomainPitchShifter.SampleRateChanged;
begin
 //
end;

procedure TCustomFrequencyDomainPitchShifter.SetPitch(const Value: Single);
begin
 if FPitch <> Value then
  begin
   FPitch := Value;
   PitchChanged;
  end;
end;

{ TFrequencyDomainPitchShifter32 }

constructor TFrequencyDomainPitchShifter32.Create;
begin
 inherited;
 FRealFFT := TFftReal2ComplexNativeFloat32.Create;
end;

destructor TFrequencyDomainPitchShifter32.Destroy;
begin
 FreeAndNil(FRealFFT);
 inherited;
end;

procedure TFrequencyDomainPitchShifter32.ProcessSamples(const Inputs,
  Outputs: PDavSingleFixedArray; const SampleFrames: Cardinal);
begin
 //
end;

end.

(****************************************************************************
*
* NAME: smbPitchShift.cpp
* VERSION: 1.2
* HOME URL: http://www.dspdimension.com
* KNOWN BUGS: none
*
* SYNOPSIS: Routine for doing pitch shifting while maintaining
* duration using the Short Time Fourier Transform.
*
* DESCRIPTION: The routine takes a pitchShift factor value which is between 0.5
* (one octave down) and 2. (one octave up). A value of exactly 1 does not change
* the pitch. numSampsToProcess tells the routine how many samples in indata[0...
* numSampsToProcess-1] should be pitch shifted and moved to outdata[0 ...
* numSampsToProcess-1]. The two buffers can be identical (ie. it can process the
* data in-place). fftFrameSize defines the FFT frame size used for the
* processing. Typical values are 1024, 2048 and 4096. It may be any value <=
* MAX_FRAME_LENGTH but it MUST be a power of 2. osamp is the STFT
* oversampling factor which also determines the overlap between adjacent STFT
* frames. It should at least be 4 for moderate scaling ratios. A value of 32 is
* recommended for best quality. sampleRate takes the sample rate for the signal
* in unit Hz, ie. 44100 for 44.1 kHz audio. The data passed to the routine in
* indata[] should be in the range [-1.0, 1.0), which is also the output range
* for the data, make sure you scale the data accordingly (for 16bit signed integers
* you would have to divide (and multiply) by 32768).
*
* COPYRIGHT 1999-2006 Stephan M. Bernsee <smb [AT] dspdimension [DOT] com>
*
*             The Wide Open License (WOL)
*
* Permission to use, copy, modify, distribute and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice and this license appear in all source copies. 
* THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY OF
* ANY KIND. See http://www.dspguru.com/wol.htm for more information.
*
*****************************************************************************)

const
  CMaxFrameLength = 8192

procedure smbFft(float *fftBuffer, long fftFrameSize, long sign);
function smbAtan2(x, y: Double): Double;


// -----------------------------------------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------------------------------------

(*
    PLEASE NOTE:

    There have been some reports on domain errors when the atan2() function was used
    as in the above code. Usually, a domain error should not interrupt the program flow
    (maybe except in Debug mode) but rather be handled "silently" and a global variable
    should be set according to this error. However, on some occasions people ran into
    this kind of scenario, so a replacement atan2() function is provided here.

    If you are experiencing domain errors and your program stops, simply replace all
    instances of atan2() with calls to the smbAtan2() function below.
*)


function smbAtan2(x, y: Double);
var
  signx : Double;
begin
  if (x > 0)
   then signx :=  1;
   else signx := -1;

  if (x = 0) then result := 0 else
  if (y = 0)
   then result := signx * Pi * 0.5
   else result := atan2(x, y);
end;

// -----------------------------------------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------------------------------------

procedure smbFft(fftBuffer: PSingle; fftFrameSize: Integer; sign: Integer);
(*
  FFT routine, (C)1996 S.M.Bernsee. Sign = -1 is FFT, 1 is iFFT (inverse)
  Fills fftBuffer[0...2*fftFrameSize-1] with the Fourier transform of the
  time domain data in fftBuffer[0...2*fftFrameSize-1]. The FFT array takes
  and returns the cosine and sine parts in an interleaved manner, ie.
  fftBuffer[0] = cosPart[0], fftBuffer[1] = sinPart[0], asf. fftFrameSize
  must be a power of 2. It expects a complex input signal (see footnote 2),
  ie. when working with 'common' audio signals our input signal has to be
  passed as beginin[0],0.,in[1],0.,in[2],0.,...end; asf. In that case, the transform
  of the frequencies of interest is in fftBuffer[0...fftFrameSize].
*)
begin
  float wr, wi, arg, *p1, *p2, temp;
  float tr, ti, ur, ui, *p1r, *p1i, *p2r, *p2i;
  long i, bitm, j, le, le2, k;

  for (i = 2; i < 2*fftFrameSize-2; i += 2) begin
    for (bitm = 2, j = 0; bitm < 2*fftFrameSize; bitm <<= 1) begin
      if (i & bitm) j++;
      j <<= 1;
    end;
    if (i < j) begin
      p1 = fftBuffer+i; p2 = fftBuffer+j;
      temp = *p1; *(p1++) = *p2;
      *(p2++) = temp; temp = *p1;
      *p1 = *p2; *p2 = temp;
    end;
  end;
  for (k = 0, le = 2; k < (long)(log(fftFrameSize)/log(2.)+.5); k++) begin
    le <<= 1;
    le2 = le>>1;
    ur = 1.0;
    ui = 0.0;
    arg = Pi / (le2>>1);
    wr = cos(arg);
    wi = sign*sin(arg);
    for (j = 0; j < le2; j += 2) begin
      p1r = fftBuffer+j; p1i = p1r+1;
      p2r = p1r+le2; p2i = p2r+1;
      for (i = j; i < 2*fftFrameSize; i += le) begin
        tr = *p2r * ur - *p2i * ui;
        ti = *p2r * ui + *p2i * ur;
        *p2r = *p1r - tr; *p2i = *p1i - ti;
        *p1r += tr; *p1i += ti;
        p1r += le; p1i += le;
        p2r += le; p2i += le;
      end;
      tr = ur*wr - ui*wi;
      ui = ur*wi + ui*wr;
      ur = tr;
    end;
  end;
end;

// -----------------------------------------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------------------------------------

procedure smbPitchShift(pitchShift: Single; numSampsToProcess: Integer; fftFrameSize: Integer; osamp: Integer; sampleRate: Single; indata, outdata: PSingle);
(*
  Routine smbPitchShift(). See top of file for explanation
  Purpose: doing pitch shifting while maintaining duration using the Short
  Time Fourier Transform.
  Author: (c)1999-2006 Stephan M. Bernsee <smb [AT] dspdimension [DOT] com>
*)
begin
  static float gInFIFO[CMaxFrameLength];
  static float gOutFIFO[CMaxFrameLength];
  static float gFFTworksp[2 * CMaxFrameLength];
  static float gLastPhase[CMaxFrameLength div 2 + 1];
  static float gSumPhase[CMaxFrameLength div 2 + 1];
  static float gOutputAccum[2 * CMaxFrameLength];
  static float gAnaFreq[CMaxFrameLength];
  static float gAnaMagn[CMaxFrameLength];
  static float gSynFreq[CMaxFrameLength];
  static float gSynMagn[CMaxFrameLength];
  static long gRover = false, gInit = false;
  double magn, phase, tmp, window, real, imag;
  double freqPerBin, expct;
  long i,k, qpd, index, inFifoLatency, stepSize, fftFrameSize2;

  (* set up some handy variables *)
  fftFrameSize2 = fftFrameSize * 0.5;
  stepSize = fftFrameSize/osamp;
  freqPerBin = sampleRate/(double)fftFrameSize;
  expct = 2.*Pi*(double)stepSize/(double)fftFrameSize;
  inFifoLatency = fftFrameSize-stepSize;
  if (gRover = False) then gRover := inFifoLatency;

  (* initialize our static arrays *)
  if (gInit = False) then
   begin
    memset(gInFIFO, 0, CMaxFrameLength * SizeOf(Single));
    memset(gOutFIFO, 0, CMaxFrameLength * SizeOf(Single));
    memset(gFFTworksp, 0, 2*CMaxFrameLength * SizeOf(Single));
    memset(gLastPhase, 0, (CMaxFrameLength div 2 + 1) * SizeOf(Single));
    memset(gSumPhase, 0, (CMaxFrameLength/2+1) * SizeOf(Single));
    memset(gOutputAccum, 0, 2*CMaxFrameLength * SizeOf(Single));
    memset(gAnaFreq, 0, CMaxFrameLength * SizeOf(Single));
    memset(gAnaMagn, 0, CMaxFrameLength * SizeOf(Single));
    gInit = true;
   end;

  (* main processing loop *)
  for (i = 0; i < numSampsToProcess; i++) do
   begin

    (* As long as we have not yet collected enough data just read in *)
    gInFIFO[gRover] = indata[i];
    outdata[i] = gOutFIFO[gRover-inFifoLatency];
    gRover++;

    (* now we have enough data for processing *)
    if (gRover >= fftFrameSize) begin
      gRover = inFifoLatency;

      (* do windowing and re,im interleave *)
      for (k = 0; k < fftFrameSize;k++) begin
        window = -.5*cos(2.*Pi*(double)k/(double)fftFrameSize)+.5;
        gFFTworksp[2*k] = gInFIFO[k] * window;
        gFFTworksp[2*k+1] = 0.;
      end;


      (* ***************** ANALYSIS ******************* *)
      (* do transform *)
      smbFft(gFFTworksp, fftFrameSize, -1);

      (* this is the analysis step *)
      for (k = 0; k <= fftFrameSize2; k++) begin

        (* de-interlace FFT buffer *)
        real = gFFTworksp[2*k];
        imag = gFFTworksp[2*k+1];

        (* compute magnitude and phase *)
        magn = 2.*sqrt(real*real + imag*imag);
        phase = atan2(imag,real);

        (* compute phase difference *)
        tmp = phase - gLastPhase[k];
        gLastPhase[k] = phase;

        (* subtract expected phase difference *)
        tmp -= (double)k*expct;

        (* map delta phase into +/- Pi interval *)
        qpd = tmp/Pi;
        if (qpd >= 0) qpd += qpd&1;
        else qpd -= qpd&1;
        tmp -= Pi*(double)qpd;

        (* get deviation from bin frequency from the +/- Pi interval *)
        tmp = osamp*tmp/(2.*Pi);

        (* compute the k-th partials' true frequency *)
        tmp = (double)k*freqPerBin + tmp*freqPerBin;

        (* store magnitude and true frequency in analysis arrays *)
        gAnaMagn[k] = magn;
        gAnaFreq[k] = tmp;

      end;

      (* ***************** PROCESSING ******************* *)
      (* this does the actual pitch shifting *)
      memset(gSynMagn, 0, fftFrameSize* SizeOf(Single));
      memset(gSynFreq, 0, fftFrameSize* SizeOf(Single));
      for (k = 0; k <= fftFrameSize2; k++) begin 
        index = k*pitchShift;
        if (index <= fftFrameSize2) begin 
          gSynMagn[index] += gAnaMagn[k]; 
          gSynFreq[index] = gAnaFreq[k] * pitchShift;
        end; 
      end;

      (* ***************** SYNTHESIS ******************* *)
      (* this is the synthesis step *)
      for (k = 0; k <= fftFrameSize2; k++) begin

        (* get magnitude and true frequency from synthesis arrays *)
        magn = gSynMagn[k];
        tmp = gSynFreq[k];

        (* subtract bin mid frequency *)
        tmp -= (double)k*freqPerBin;

        (* get bin deviation from freq deviation *)
        tmp /= freqPerBin;

        (* take osamp into account *)
        tmp = 2.*Pi*tmp/osamp;

        (* add the overlap phase advance back in *)
        tmp += (double)k*expct;

        (* accumulate delta phase to get bin phase *)
        gSumPhase[k] += tmp;
        phase = gSumPhase[k];

        (* get real and imag part and re-interleave *)
        gFFTworksp[2*k] = magn*cos(phase);
        gFFTworksp[2*k+1] = magn*sin(phase);
      end; 

      (* zero negative frequencies *)
      for (k = fftFrameSize+2; k < 2*fftFrameSize; k++) gFFTworksp[k] = 0.;

      (* do inverse transform *)
      smbFft(gFFTworksp, fftFrameSize, 1);

      (* do windowing and add to output accumulator *)
      for(k=0; k < fftFrameSize; k++) begin
        window = -.5*cos(2.*Pi*(double)k/(double)fftFrameSize)+.5;
        gOutputAccum[k] += 2.*window*gFFTworksp[2*k]/(fftFrameSize2*osamp);
      end;
      for (k = 0; k < stepSize; k++) gOutFIFO[k] = gOutputAccum[k];

      (* shift accumulator *)
      memmove(gOutputAccum, gOutputAccum+stepSize, fftFrameSize * SizeOf(Single));

      (* move input FIFO *)
      for (k = 0; k < inFifoLatency; k++) gInFIFO[k] = gInFIFO[k + stepSize];
    end;
  end;
end;

// -----------------------------------------------------------------------------------------------------------------
