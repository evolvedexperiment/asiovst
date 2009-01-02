unit WaveIOX;

{
Unit WaveIOX;
This unit reads WAV files of almost any format
and converts them into 32-bit floating point buffers.

The basic structure is from the WaveIO unit by
Carlos Barbosa (delphi@carlosb.com) with various
extensions like the reader/converter from me.
You can get the original I/O routines from his
site(http://www.carlosb.com).

Have fun!

Tobybear
www.tobybear.de
tobybear@web.de

History:
1.0: initial release
1.1: fixed some conversion errors,
     thanks to Christian Knufinke for finding them!
}

interface

{$I DAV_Compiler.inc}

uses
  Windows, Classes, MMSystem, SysUtils, MSACMX;

const
  cMPEGLayer3 = 85;
  cBufferSize = 12000;

  //High-Level functions
function LoadWAVFileMono(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt): Pointer;
function LoadWAVFile(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt): Pointer;
procedure SaveWAVFile(fn: string; fdata: pointer; sr, ch, bits, size: LongInt);
procedure SaveWAVFileSeparateStereo(fn: string; fdata1, fdata2: pointer; sr, ch, bits, size: LongInt);
procedure GetWAVFileInfo(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt);

type
  EWaveIOError = Exception;

  TWaveStream = class(TObject)
  private
    fFormat            : PWaveFormatEx;
    fDataOffset, fSize : LongInt;
    fFullsize, fFlSize : LongInt;
    fMM                : hmmIO;
    fPck, fPckRIFF     : TMMCKINFO;

    function GetPosition: LongInt;
    procedure SetPosition(Pos: LongInt);
    procedure CheckMMIOWave;

  public
    constructor Create(fMMIO: hmmIO; WriteFormat: PWaveFormatEx);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; virtual;
    function Seek(Offset: LongInt; Origin: Word): LongInt; virtual;
    function Write(var Buffer; Count: LongInt): LongInt; virtual;

    property Format: PWaveFormatEx read fFormat;
    property Position: LongInt read GetPosition write SetPosition;
    property Size: LongInt read fSize;
    property FloatSize: LongInt read fFlSize;
  end;

  TMemoryWaveStream = class(TWaveStream)
  private
    fMMIO: hmmIO;
  public
    constructor Create(Memory: Pointer; Size: LongInt;
      WriteFormat: PWaveFormatEx); virtual;
    destructor Destroy; override;
  end;

  TFilewaveStream = class(TWaveStream)
  private
    fMMIO: hmmIO;
  public
    constructor Create(FileName: String; WriteFormat: PWaveFormatEx); virtual;
    destructor Destroy; override;
  end;

  TPCMWaveReader = class(TObject)
  private
    fPosition              : LongInt;
    fStream                : TWaveStream;
    fSize                  : LongInt;
    fBufferLength          : LongInt;
    fACMStream             : Integer;
    fACMStreamHeader       : TACMStreamHeader;
    fDstWFx                : PWaveFormatEx;
    fPFullSize, fPFlSize   : LongInt;

    fRawBuffer             : Pointer;
    fRawBufferByteSize     : LongInt;
    fRawBufferSampleSize   : LongInt;

    fPCMBuffer             : Pointer;
    fPCMBufferByteSize     : LongInt;   // size of total buffer in Bytes
    fPCMBufferSampleSize   : LongInt;   // size of converted samples in buffer

    fPCMSamplesPerSample   : LongInt;
    fPCMBufferSamplePos    : LongInt;

    procedure AllocBuffers;
    procedure DestroyBuffers;
    procedure ReadSamples(Index: LongInt);
    procedure SetBufferLength(Value: LongInt);
    procedure SetPosition(Value: LongInt);

  public
    constructor Create(Stream: TWaveStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt;

    property Format: PWaveFormatEx read fDstWFx;
    property BufferLength: LongInt read fBufferLength write SetBufferLength;
    property Position: LongInt read fPosition write SetPosition;
    property Size: LongInt read fSize;
    property Stream: TWaveStream read fStream;
    function BufferToFloat(makemono: boolean): pointer;
  end;

  TWavWriter = class(TObject)
  public
    Format: TWaveFormatEx;
    Stream: TFilewaveStream;
    constructor Create(fn: string; sr, ch, bits: LongInt);
    procedure WriteFloatData(p: pointer; size: LongInt);
    procedure WriteFloatDataSeparateStereo(p1, p2: pointer; size: LongInt);
    destructor Destroy; override;
  end;

implementation


function mmioFourCC(Chr1: Char; Chr2: Char; Chr3: Char; Chr4: Char): DWord;
begin
  Result := Integer(Chr1) +
            Integer(Chr2) shl 8 +
            Integer(Chr3) shl 16 +
            Integer(Chr4) shl 24;
end;

constructor TWaveStream.Create(fMMIO: hmmIO; WriteFormat: PWaveFormatEx);
begin
  inherited Create;
  filemode := 0;
  fMM := fMMIO;

  if (WriteFormat <> nil) then
   begin
    // Create the output file RIFF chunk of form type 'WAVE'.
    fPckRIFF.fccType := mmioFOURCC('W', 'A', 'V', 'E');
    fPckRIFF.cksize := 0;
    if (mmioCreateChunk(fMM, @fPckRIFF, MMIO_CREATERIFF) <> 0) then
      raise EWaveIOError.Create('Error 01: Cannot create chunk.');

    // We are now descended into the 'RIFF' chunk we just created.
    // * Now create the 'fmt ' chunk. Since we know the size of this chunk,
    // specify it in the MMCKINFO structure so fMMIO doesn't have to seek
    // back and set the chunk size after ascending from the chunk.

    fPck.ckid := mmioFOURCC('f', 'm', 't', ' ');
    fPck.cksize := SizeOf(WriteFormat^) + WriteFormat^.cbSize;
    if (mmioCreateChunk(fMM, @fPck, 0) <> 0) then
      raise EWaveIOError.Create('Error 02: Cannot create chunk.');

    // Write the variable length size.
    if (mmioWrite(fMM, Pointer(WriteFormat), SizeOf(WriteFormat^) +
      WriteFormat^.cbSize) <> SizeOf(WriteFormat^) + WriteFormat^.cbSize) then
      raise EWaveIOError.Create('Error 03: Cannot write wave format.');

    GetMem(fFormat, SizeOf(fFormat^));
    CopyMemory(fFormat, WriteFormat, SizeOf(fFormat^));

    // Ascend out of the 'fmt ' chunk,back into the 'RIFF' chunk.
    if (mmioAscend(fMM, @fPck, 0) <> 0) then
      raise EWaveIOError.Create('Error 04: Cannot ascend chunk.');

    // We are now descended into the 'RIFF' chunk we just created.
    // * Now create the 'data' chunk.
    fPck.ckid := mmioFOURCC('d', 'a', 't', 'a');
    fPck.cksize := 0;
    if (mmioCreateChunk(fMM, @fPck, 0) <> 0) then
      raise EWaveIOError.Create('Error 05: Cannot create chunk.');
   end
  else
   begin
    CheckMMIOWave;

    // put in the beggining of the file
    mmioSeek(fMM, fDataOffset, SEEK_SET);
   end;
end;


destructor TWaveStream.Destroy;
begin
  if (fFormat <> nil) then FreeMem(fFormat);

  mmioSeek(fMM, 0, SEEK_END);

  if (mmioAscend(fMM, @fPck, 0) <> 0)
   then raise EWaveIOError.Create('Error 06: Cannot ascend chunk.');

  if (mmioAscend(fMM, @fPckRIFF, 0) <> 0)
   then raise EWaveIOError.Create('Error 07: Cannot ascend chunk.');

  if (mmioFlush(fMM, 0) <> 0)
   then raise EWaveIOError.Create('Error 08: Cannot flush.');

  inherited Destroy;
end;


function TWaveStream.GetPosition: LongInt;
begin
  Result := Seek(0, SEEK_CUR);
end;

procedure TWaveStream.SetPosition(Pos: LongInt);
begin
  Seek(Pos, SEEK_SET);
end;

function TWaveStream.Read(var Buffer; Count: LongInt): LongInt;
var
  p: Pointer;
begin
  p := @Buffer;

  Result := mmioRead(fMM, p, Count * fFormat^.nBlockAlign);
  if (Result = -1)
   then raise EWaveIOError.Create('Error 09: Cannot read from file.')
   else Result := Result div fFormat^.nBlockAlign;
end;

function TWaveStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
 if (Origin = SEEK_SET)
  then Result := (mmioSeek(fMM, Offset * fFormat^.nBlockAlign + fDataOffset, Origin) - fDataOffset) div fFormat^.nBlockAlign
  else Result := (mmioSeek(fMM, Offset * fFormat^.nBlockAlign, Origin) - fDataOffset) div fFormat^.nBlockAlign;

 if (Result < 0)
  then raise EWaveIOError.Create('Error 10: Cannot seek in file.');
end;

function TWaveStream.Write(var Buffer; Count: LongInt): LongInt;
begin
 Result := mmioWrite(fMM, @Buffer, Count * fFormat^.nBlockAlign);
 if (Result = -1)
  then raise EWaveIOError.Create('Error 11: Cannot write to file.')
  else Result := Result div fFormat^.nBlockAlign;
end;

procedure TWaveStream.CheckMMIOWave;
var
  mmIOInfo   : TMMIOInfo;
  FormatTmp  : TWaveFormatEx;
  ExtraAlloc : Word;
begin
  if (mmioDescend(fMM, @fPckRIFF, nil, 0) <> 0) then
    raise EWaveIOError.Create('Error 12: Invalid multimedia file!');

  if (fPckRIFF.ckid <> FOURCC_RIFF) or
    (fPckRIFF.fccType <> mmioFOURCC('W', 'A', 'V', 'E')) then
    raise EWaveIOError.Create('Error 13: Not a wave file!');

  // Search the input file for for the 'fmt ' chunk.     */
  fPck.ckid := mmioFOURCC('f', 'm', 't', ' ');
  if (mmioDescend(fMM, @fPck, @fPckRIFF, MMIO_FINDCHUNK) <> 0) then
    raise EWaveIOError.Create('Error 14: Cannot find ''fmt'' chunk!');

  // Expect the 'fmt' chunk to be at least as large as <PCMWAVEFORMAT>;
  // if there are extra parameters at the end,we'll ignore them */
  if (fPck.cksize < 16) then
    raise EWaveIOError.Create('Error 15: Abnormal ''fmt'' size!');

  // Read the 'fmt ' chunk into <pcmWaveFormat>.*/
  if (mmioRead(fMM, PChar(@FormatTmp), 16) <> 16) then
    raise EWaveIOError.Create('Error 16: Cannot read ''fmt'' chunk!');

  // Ok, allocate the waveformatex, but if its not PCM
  // format, read the next word and thats how many extra
  // Bytes to allocate.
  if (FormatTmp.wFormatTag = WAVE_FORMAT_PCM) or (FormatTmp.wFormatTag = 3)
   then ExtraAlloc := 0 else

  // Read in length of extra Bytes.
  if (mmioRead(fMM, @ExtraAlloc, SizeOf(ExtraAlloc)) <> SizeOf(ExtraAlloc))
   then raise EWaveIOError.Create('Error 17: Cannot read ''waveformatex'' length!');

  GetMem(fFormat, SizeOf(fFormat^) + ExtraAlloc);
  CopyMemory(fFormat, @FormatTmp, SizeOf(fFormat^));
  fFormat^.cbSize := ExtraAlloc;
  if (ExtraAlloc <> 0) then
   if (mmioRead(fMM, PChar(fFormat) + SizeOf(fFormat^), ExtraAlloc) <> ExtraAlloc)
    then raise EWaveIOError.Create('Error 18: Cannot read ''waveformatex''!');

  if (fFormat^.wFormatTag = cMPEGLayer3)
   then raise EWaveIOError.Create('Error 19: MPEG Layer-3 compression is not supported.');

  // Ascend the input file out of the 'fmt ' chunk. */
  if (mmioAscend(fMM, @fPck, 0) <> 0)
   then raise EWaveIOError.Create('Error 20: Cannot ascend from ''fmt'' chunk!');

  // Do a nice little seek...
  if (mmioSeek(fMM, fPckRIFF.dwDataOffset + SizeOf(FOURCC), SEEK_SET) = -1)
   then raise EWaveIOError.Create('Error 21: Cannot seek to data!');

  // Search the input file for the 'data' chunk.
  fPck.ckid := mmioFOURCC('d', 'a', 't', 'a');
  mmioDescend(fMM, @fPck, @fPckRIFF, MMIO_FINDCHUNK);

  if (mmioGetInfo(fMM, @mmioInfo, 0) <> 0) then
    raise EWaveIOError.Create('Error 22: Cannot get info!');

  fDataOffset := fPck.dwDataOffset;
  fSize := fPck.cksize div fFormat^.nBlockAlign;
  fFlSize := fSize;
end;


//***************************************************************
//        TMemoryWaveStream
//***************************************************************

constructor TMemoryWaveStream.Create(Memory: Pointer; Size: LongInt;
  WriteFormat: PWaveFormatEx);
var
  info: TMMIOINFO;
begin
  ZeroMemory(@info, SizeOf(info));
  with info do
   begin
    pchBuffer := Memory;
    fccIOProc := FOURCC_MEM;
    cchBuffer := Size;
   end;

  // Initialization...
  fMMIO := mmioOpen(nil, @info, MMIO_READ);
  if (fMMIO = 0) then
    raise EWaveIOError.Create('Error 23: Cannot open memory stream.');

  inherited Create(fMMIO, WriteFormat);
end;

destructor TMemoryWaveStream.Destroy;
begin
  inherited Destroy;

  if (fMMIO <> 0) then mmioClose(fMMIO, 0);
end;


//***************************************************************
//        TFilewaveStream
//***************************************************************

constructor TFilewaveStream.Create(FileName: String;
  WriteFormat: PWaveFormatEx);
var
  f: file of Byte;
begin
  // Initialization...
  if (WriteFormat = nil) then
   begin
    assignfile(f, filename);
   {$I-} reset(f);{$I+}
    if ioresult <> 0 then
      raise EWaveIOError.Create('Error 24: Cannot open file.')
    else
     begin
      fFullsize := filesize(f);
      closefile(f);
     end;
    fMMIO := mmioOpen(Pointer(FileName), nil, MMIO_READ or MMIO_ALLOCBUF)
   end
  else fMMIO := mmioOpen(Pointer(FileName), nil, MMIO_CREATE or MMIO_READWRITE or MMIO_ALLOCBUF);

  if (fMMIO = 0) then raise EWaveIOError.Create('Error 25: Cannot open file stream.');

  inherited Create(fMMIO, WriteFormat);
end;

destructor TFilewaveStream.Destroy;
begin
  inherited Destroy;
  if (fMMIO <> 0) then mmioClose(fMMIO, 0);
end;


//***************************************************************
//        TPCMWaveReader
//***************************************************************

constructor TPCMWaveReader.Create(Stream: TWaveStream);
begin
  inherited Create;

  fStream := Stream;
  GetMem(fDstWFx, SizeOf(fDstWFx^));

//  fBufferLength := 4096;
  fBufferLength := 20000;

  if (fStream.Format^.wFormatTag <> WAVE_FORMAT_PCM) and (fStream.Format^.wFormatTag <> 3) then
   begin
    // prepare acm stream converter
    ZeroMemory(fDstWFx, SizeOf(fDstWFx^));
    fDstWFx^.wFormatTag := WAVE_FORMAT_PCM;
    if (acmFormatSuggest(0, fStream.fFormat, fDstWFx, SizeOf(fDstWFx^),
      ACM_FORMATSUGGESTF_WFORMATTAG) <> 0) then
      raise EWaveIOError.Create('Error 26: Cannot suggest pcm format.');

    if (acmStreamOpen(PHACMSTREAM(@fACMStream), 0, fStream.Format^, fDstWFx^,
      nil, 0, 0, 0) <> 0) then
      raise EWaveIOError.Create('Error 27: Cannot open acm stream.');

    AllocBuffers;

    // prepare buffers
    ZeroMemory(@fACMStreamHeader, SizeOf(fACMStreamHeader));
    with fACMStreamHeader do
     begin
      cbStruct := SizeOf(fACMStreamHeader);
      pbSrc := Pointer(fRawBuffer);
      cbSrcLength := fRawBufferByteSize;
      dwSrcUser := cbSrcLength;
      pbDst := Pointer(fPCMBuffer);
      cbDstLength := fPCMBufferByteSize;
      dwDstUser := cbDstLength;
     end;

    if (acmStreamPrepareHeader(fACMStream, fACMStreamHeader, 0) <> 0) then
      raise EWaveIOError.Create('Error 28: Cannot prepare headers.');
   end
  else fDstWFx^ := fStream.Format^;

  if (fStream.Format^.wFormatTag <> WAVE_FORMAT_PCM) and
    (fStream.Format^.wFormatTag <> 3) then
    fPCMSamplesPerSample :=
      (fPCMBufferByteSize * fStream.Format^.nBlockAlign) div
      (fRawBufferByteSize * fDstWFx^.nBlockAlign)
  else fPCMSamplesPerSample := 1;
  fSize := fPCMSamplesPerSample * fStream.Size;
  fPCMBufferSampleSize := 0;
end;


destructor TPCMWaveReader.Destroy;
begin
  if (fACMStream <> 0) then
   begin
    fACMStreamHeader.cbSrcLength := fRawBufferByteSize;
    acmStreamUnprepareHeader(fACMStream, fACMStreamHeader, 0);
    acmStreamClose(fACMStream, 0);
   end;

  if (fDstWFx <> nil) then FreeMem(fDstWFx);

  DestroyBuffers;

  inherited Destroy;
end;


procedure TPCMWaveReader.AllocBuffers;
var
  ss, dd: DWord;
begin
  DestroyBuffers;

  // calc space needed for holding the decompression of one sample
  if (acmStreamSize(fACMStream, fStream.Format^.nBlockAlign, dd, ACM_STREAMSIZEF_SOURCE) <> 0)
   then raise EWaveIOError.Create('Error 29: Cannot recommend an acm stream size.');

  // calc minimum size block of the source
  if (acmStreamSize(fACMStream, dd, ss, ACM_STREAMSIZEF_DESTINATION) <> 0)
   then raise EWaveIOError.Create('Error 30: Cannot recommend an acm stream size.');

  // alloc source buffer(raw)
  fRawBufferSampleSize := fBufferLength div dd;
  if (fRawBufferSampleSize = 0)
   then fRawBufferSampleSize := 1;
  fRawBufferByteSize := fRawBufferSampleSize * ss;
  GetMem(fRawBuffer, fRawBufferByteSize);

  // Alloc destination buffer(decompressed)
  if (acmStreamSize(fACMStream, DWord(fRawBufferByteSize), DWord(fPCMBufferByteSize), ACM_STREAMSIZEF_SOURCE) <> 0)
   then raise EWaveIOError.Create('Error 31: Cannot recommend an acm stream size.');
  GetMem(fPCMBuffer, fPCMBufferByteSize);
end;


procedure TPCMWaveReader.DestroyBuffers;
begin
  fPCMBufferSampleSize := 0;

  if (fPCMBuffer <> nil) then FreeMem(fPCMBuffer);
  fPCMBuffer := nil;

  if (fRawBuffer <> nil) then FreeMem(fRawBuffer);
  fRawBuffer := nil;
end;

procedure TPCMWaveReader.SetBufferLength(Value: LongInt);
begin
  if (Value <> fBufferLength) and (Value >= 1024) then
   begin
    fBufferLength := Value;
    if (fPCMBuffer <> nil) then AllocBuffers;
   end;
end;

procedure TPCMWaveReader.SetPosition(Value: LongInt);
begin
  if (Value <> fPosition) then
   if (Value >= 0) and (Value < fSize)
    then fPosition := Value
    else raise EWaveIOError.Create('Error 32: Position out of bounds.');
end;

function TPCMWaveReader.Read(var Buffer; Count: LongInt): LongInt;
var
  pos, len: LongInt;
  posi, posf: LongInt;
begin
  if Count < 1 then
   begin
    Result := 0;
    Exit;
   end;

  if (fStream.Format^.wFormatTag = WAVE_FORMAT_PCM) or
    (fStream.Format^.wFormatTag = 3) then
   begin
    fStream.Position := fPosition;
    Result := fStream.Read(Buffer, Count);
    fPosition := fPosition + Result;
   end
  else
   begin
    if (Count + fPosition >= fSize) then
      Count := fSize - fPosition;                                 // limit to wave size
    if (fPosition >= fPCMBufferSamplePos) and
      (fPosition < fPCMBufferSamplePos + fPCMBufferSampleSize)
 // use current buffer data if possible
    then
     begin
      len := fPCMBufferSamplePos + fPCMBufferSampleSize - fPosition;
      if (len > Count) then
        len := Count;
      CopyMemory(PChar(@Buffer), PChar(fPCMBuffer) +
        (fPosition - fPCMBufferSamplePos) * fDstWFx^.nBlockAlign, len * fDstWFx^.nBlockAlign);
      pos := len;
     end
    else
      pos := 0;
    while (pos < Count) do // put next data
     begin
      ReadSamples((fPosition + pos) div fPCMSamplesPerSample);
 // calc. range of current pcm buffer that can be used to fill request
      posi := fPosition + pos;
      if (fPosition + pos < fPCMBufferSamplePos) then
        raise EWaveIOError.Create(
          'Error 33: Position smaller than PCMBufferSamplePos');
      if (fPosition + Count > fPCMBufferSamplePos + fPCMBufferSampleSize) then
        posf := fPCMBufferSamplePos + fPCMBufferSampleSize
      else
        posf := fPosition + Count;
      len := posf - posi;
      // put pcm buffer data into target
      CopyMemory(PChar(@Buffer) + (posi - fPosition) * fDstWFx^.nBlockAlign,
        PChar(fPCMBuffer) + (posi - fPCMBufferSamplePos) * fDstWFx^.nBlockAlign,
        len * fDstWFx^.nBlockAlign);
      pos := pos + len;
     end;
    fPosition := fPosition + Count;
    Result := Count;
   end;
end;

procedure TPCMWaveReader.ReadSamples(Index: LongInt);
begin
  fStream.Position := Index;
  fACMStreamHeader.cbSrcLength := fStream.Read(fRawBuffer^, fRawBufferSampleSize) * fStream.Format^.nBlockAlign;
  if (acmStreamConvert(fACMStream, fACMStreamHeader, ACM_STREAMCONVERTF_BLOCKALIGN) <> 0)
   then raise EWaveIOError.Create('Error 34: Unable to convert sample.');
  fPCMBufferSampleSize := fACMStreamHeader.cbDstLengthUsed div fDstWFx^.nBlockAlign;
  fPCMBufferSamplePos := Index * fPCMSamplesPerSample;
end;

function TPCMWaveReader.BufferToFloat(makemono: boolean): pointer;
var
  p              : ^shortint;
  p2             : ^Byte;
  ps, pf         : ^Single;
  fbuffer,
  buffer         : pointer;
  l              : LongInt;

  procedure Convert(n: LongInt);
  var
    cnt, cc, v : LongInt;
    v2         : smallint;
    s          : Single;
    y, y2, y3  : Byte;
    by         : Byte;
  begin
    cnt := 0;
    while (cnt <= format.nChannels * (n - 1)) do
      if (format.wBitsPerSample = 8) then
       begin
        for cc := 1 to format.nChannels do
         begin
          by := p2^;
          Inc(p2);
          s := ((2 * by / (1 shl 8 - 1)) - 1);
          if (not makemono) then
           begin
            pf^ := s;
            Inc(pf);
           end else
           begin
            pf^ := pf^ + s;
            if cc = format.nChannels then
             begin
              pf^ := pf^ / format.nChannels;
              Inc(pf);
             end;
           end;
          Inc(cnt);
         end;
       end else
      if (format.wBitsPerSample = 16) then
       begin
        for cc := 1 to format.nChannels do
         begin
          y := p^;
          Inc(p);
          y2 := p^;
          Inc(p);
          v2 := y + y2 * (1 shl 8);
          s := v2;
          if s > 0 then
            s := s / (1 shl 15 - 1)
          else
            s := s / (1 shl 15);
          if (not makemono) then
           begin
            pf^ := s;
            Inc(pf);
           end else
           begin
            pf^ := pf^ + s;
            if cc = format.nChannels then
             begin
              pf^ := pf^ / format.nChannels;
              Inc(pf);
             end;
           end;
          Inc(cnt);
         end;
       end else
      if (format.wBitsPerSample = 20) then
       begin
        for cc := 1 to format.nChannels do
         begin
          y := p^;
          Inc(p);
          y2 := p^;
          Inc(p);
          y3 := p^;
          Inc(p);
          if not (format.nBlockAlign mod 3 = 0) then
            Inc(p);
          v := (y + y2 * (1 shl 8) + y3 * (1 shl 16)) shr 4;
          if v >= 1 shl 19 then
            v := v - 1 shl 20;
          s := v;
          if s > 0 then
            s := s / (1 shl 19 - 1)
          else
            s := s / (1 shl 19);
          if (not makemono) then
           begin
            pf^ := s;
            Inc(pf);
           end else
           begin
            pf^ := pf^ + s;
            if cc = format.nChannels then
             begin
              pf^ := pf^ / format.nChannels;
              Inc(pf);
             end;
           end;
          Inc(cnt);
         end;
       end else
      if (format.wBitsPerSample = 24) then
       begin
        for cc := 1 to format.nChannels do
         begin
          y := p^;
          Inc(p);
          y2 := p^;
          Inc(p);
          y3 := p^;
          Inc(p);
          if not (format.nBlockAlign mod 3 = 0) then
            Inc(p);
          v := y + y2 * (1 shl 8) + y3 * (1 shl 16);
          if v >= (1 shl 23) then
            v := v - (1 shl 24);
          s := v;
          if s > 0 then
            s := s / (1 shl 23 - 1)
          else
            s := s / (1 shl 23);
          if (not makemono) then
           begin
            pf^ := s;
            Inc(pf);
           end else
           begin
            pf^ := pf^ + s;
            if cc = format.nChannels then
             begin
              pf^ := pf^ / format.nChannels;
              Inc(pf);
             end;
           end;
          Inc(cnt);
         end;
       end else
      if (format.wBitsPerSample = 32) then
        for cc := 1 to format.nChannels do
         begin
          s := ps^;
          Inc(ps);
          if (not makemono) then
           begin
            pf^ := s;
            Inc(pf);
           end else
           begin
            pf^ := pf^ + s;
            if cc = format.nChannels then
             begin
              pf^ := pf^ / format.nChannels;
              Inc(pf);
             end;
           end;
          Inc(cnt);
         end;
  end;

begin
  if fPFullSize > fPFlSize then fPFlSize := fPFullSize;
  l := format.nChannels * fPFlSize * SizeOf(Single);
  if makemono then l := l div format.nChannels;
  GetMem(FBuffer, l);
  GetMem(buffer, cBufferSize * SizeOf(smallint) * 4);
  fillchar(fbuffer^, l, 0);
  p := buffer;
  p2 := buffer;
  pf := fbuffer;
  l := Read(buffer^, cBufferSize);
  while (l > 0) do
   begin
    p := buffer;
    p2 := buffer;
    ps := buffer;
    convert(l);
    l := Read(buffer^, cBufferSize);
   end;
  if makemono
   then fPFlSize := fSize
   else fPFlSize := format.nChannels * fSize;
  FreeMem(buffer);
  Result := fbuffer;
end;

{ TWavWriter }

constructor TWavWriter.Create(fn: string; sr, ch, bits: Integer);
var
  p: PWaveFormatEx;
begin
  Format.nChannels := ch;
  Format.nSamplesPerSec := sr;
  if bits > 32 then bits := 32;
  Format.wBitsPerSample := bits;
  case bits of
   20, 24 :
    begin
     Format.nBlockAlign := 3;
     Format.wFormatTag := WAVE_FORMAT_PCM
    end;
   32 :
    begin
     Format.nBlockAlign := 4;
     Format.wFormatTag := 3
    end;
   else
    begin
     Format.nBlockAlign := (bits + 7) div 8;
     Format.wFormatTag := WAVE_FORMAT_PCM;
    end;
  end;
  Format.nAvgBytesPerSec := sr * ch * Format.nBlockAlign;
  p := @format;
  stream := TFilewaveStream.Create(fn, p);
end;

destructor TWavWriter.Destroy;
begin
  FreeAndNil(stream);
  Sleep(100);
  inherited Destroy;
end;

procedure TWavWriter.WriteFloatData(p: pointer; size: Integer);
var
  ps    : PSingle;
  l, li : LongInt;
  x     : Single;
begin
  ps := p;
  for l := 0 to size - 1 do
   begin
    if Format.wBitsPerSample = 32 then
     begin
      x := ps^;
      li := LongInt((@x)^);
     end
    else
      li := round(ps^ * (1 shl (Format.wBitsPerSample - 1)));
    stream.Write(li, 1);
    Inc(ps);
   end;
end;


procedure TWavWriter.WriteFloatDataSeparateStereo(p1, p2: Pointer; Size: Integer);
var
  ps, ps2 : PSingle;
  l, li   : LongInt;
  x       : Single;
begin
  ps := p1;
  ps2 := p2;
  for l := 0 to size - 1 do
   begin
    if Format.wBitsPerSample = 32 then
     begin
      x := ps^;
      li := LongInt((@x)^);
     end
    else
      li := round(ps^ * (1 shl (Format.wBitsPerSample - 1)));
    stream.Write(li, 1);
    Inc(ps);
    if Format.wBitsPerSample = 32 then
     begin
      x := ps2^;
      li := LongInt((@x)^);
     end
    else
      li := round(ps2^ * (1 shl (Format.wBitsPerSample - 1)));
    stream.Write(li, 1);
    Inc(ps2);
   end;
end;

{ High-Level functions }

function LoadWAVFile(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt): Pointer;
var
  wave: TFilewaveStream;
begin
  wave := TFilewaveStream.Create(FileName, nil);
  try
   with TPCMWaveReader.Create(wave) do
    try
     SampleRate := wave.format.nSamplesPerSec;
     Channels   := wave.format.nChannels;
     fPFullSize := wave.fFullsize;
     Result     := BufferToFloat(False);
     WavSize    := fPFlSize;
    finally
     Free;
    end;
  finally
   FreeAndNil(wave);
  end;
end;

function LoadWAVFileMono(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt): Pointer;
var
  Wave: TFilewaveStream;
begin
  Wave := TFilewaveStream.Create(FileName, nil);
  try
   with TPCMWaveReader.Create(Wave) do
    try
     SampleRate := Wave.format.nSamplesPerSec;
     fPFullSize := Wave.fFullsize;
     Channels   := 1;
     Result     := BufferToFloat(True);
     WavSize    := fPFlSize;
    finally
     Free;
    end;
  finally
   FreeAndNil(Wave);
  end;
end;

procedure GetWAVFileInfo(FileName: TFileName; var SampleRate, Channels, WavSize: LongInt);
begin
 with TFilewaveStream.Create(FileName, nil) do
  try
   SampleRate := format.nSamplesPerSec;
   Channels   := format.nChannels;
   WavSize    := Size;
  finally
   Free;
  end;
end;

procedure SaveWAVFile(fn: string; fdata: pointer; sr, ch, bits, size: LongInt);
var
  w     : TFilewaveStream;
  ps    : PSingle;
  l, li : LongInt;
  t     : TWaveFormatEx;
  p     : PWaveFormatEx;
  x     : Single;
begin
  t.nChannels := ch;
  t.nSamplesPerSec := sr;
  if bits > 32 then bits := 32;
  t.wBitsPerSample := bits;
  case bits of
    20, 24 :
     begin
      t.nBlockAlign := 3;
      t.wFormatTag := WAVE_FORMAT_PCM
     end;
    32 :
     begin
      t.nBlockAlign := 4;
      t.wFormatTag := 3
     end;
  else
   begin
    t.nBlockAlign := (bits + 7) div 8;
    t.wFormatTag := WAVE_FORMAT_PCM;
   end;
   end;
  t.nAvgBytesPerSec := sr * ch * t.nBlockAlign;
  p := @t;

  w := TFilewaveStream.Create(fn, p);
  ps := fdata;
  for l := 0 to size - 1 do
   begin
    if t.wBitsPerSample = 32 then
     begin
      x := ps^;
      li := LongInt((@x)^);
     end
    else
      li := round(ps^ * (1 shl (bits - 1)));
    w.Write(li, 1);
    Inc(ps);
   end;
  w.Free;
end;

procedure SaveWAVFileSeparateStereo(fn: string; fdata1, fdata2: pointer;
  sr, ch, bits, size: LongInt);
var
  w       : TFilewaveStream;
  ps, ps2 : PSingle;
  l, li   : LongInt;
  t       : TWaveFormatEx;
  p       : PWaveFormatEx;
  x       : Single;
begin
  t.nChannels := ch;
  t.nSamplesPerSec := sr;
  if bits > 32 then
    bits := 32;
  t.wBitsPerSample := bits;
  case bits of
    20, 24 :
     begin
      t.nBlockAlign := 3;
      t.wFormatTag := WAVE_FORMAT_PCM
     end;
    32 :
     begin
      t.nBlockAlign := 4;
      t.wFormatTag := 3
     end;
  else
   begin
    if bits <= 8 then t.nBlockAlign := 1
    else if bits <= 16 then t.nBlockAlign := 2
    else if bits <= 24 then t.nBlockAlign := 3
    else if bits <  32 then t.nBlockAlign := 4;
    t.wFormatTag := WAVE_FORMAT_PCM;
   end;
   end;
  t.nAvgBytesPerSec := sr * ch * t.nBlockAlign;
  p := @t;

  w := TFilewaveStream.Create(fn, p);
  ps := fdata1;
  ps2 := fdata2;
  for l := 0 to size - 1 do
   begin
    if t.wBitsPerSample = 32 then
     begin
      x := ps^;
      li := LongInt((@x)^);
     end
    else li := round(ps^ * (1 shl (bits - 1)));
    w.Write(li, 1);
    Inc(ps);
    if t.wBitsPerSample = 32 then
     begin
      x := ps2^;
      li := LongInt((@x)^);
     end
    else li := round(ps2^ * (1 shl (bits - 1)));
    w.Write(li, 1);
    Inc(ps2);
   end;
  FreeAndNil(w);
end;

end.
