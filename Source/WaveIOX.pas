unit WaveIOX;
{
Unit WaveIOX;
This unit reads WAV files of almost any format
and converts them into 32-bit floating point buffers.

The basic structure is from the WaveIO unit by by
Carlos Barbosa(delphi@carlosb.com) with various
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

uses
  Windows, Classes, MMSystem, SysUtils, MSACMX, Dialogs;

const
  MPEGLAYER3=85;
  buffer_size=12000;

  //High-Level functions
  function LoadWAVFileMono(fn:string;var sr,ch,size:longint):pointer;
  function LoadWAVFile(fn:string;var sr,ch,size:longint):pointer;
  procedure SaveWAVFile(fn:string;fdata:pointer;sr,ch,bits,size:longint);
  procedure SaveWAVFileSeparateStereo(fn:string;fdata1,fdata2:pointer;sr,ch,bits,size:longint);
  procedure GetWAVFileInfo(fn:string;var sr,ch,size:longint);

type
  EWaveIOError=Exception;

  TWaveStream=class(TObject)
  private
    FFormat: PWaveFormatEx;
    FDataOffset,FSize: Longint;
    fullsize,FlSize:Longint;
    mm: hmmIO;
    pck,pckRIFF: TMMCKINFO;

    function GetPosition: Longint;
    procedure SetPosition(Pos: Longint);
    procedure CheckMMIOWave;

  public
    constructor Create(mmIO: hmmIO; WriteFormat: PWaveFormatEx);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; virtual;
    function Seek(Offset: Longint; Origin: Word): Longint; virtual;
    function Write(var Buffer; Count: Longint): Longint; virtual;

    property Format: PWaveFormatEx read FFormat;
    property Position: Longint read GetPosition write SetPosition;
    property Size: Longint read FSize;
    property FloatSize:Longint read FlSize;
  end;

  TMemoryWaveStream=class(TWaveStream)
  private
    mmIO: hmmIO;

  public
    constructor Create(Memory: Pointer; Size: Longint;
      WriteFormat: PWaveFormatEx); virtual;
    destructor Destroy; override;
  end;

  TFileWaveStream=class(TWaveStream)
  private
    mmIO: hmmIO;

  public
    constructor Create(FileName: String; WriteFormat: PWaveFormatEx); virtual;
    destructor Destroy; override;
  end;

  TPCMWaveReader=class(TObject)
  private
    FPosition: Longint;
    FStream: TWaveStream;
    FSize: Longint;
    FBufferLength: Longint;
    acmStream: Integer;
    ash: TACMStreamHeader;
    dstwfx: PWaveFormatEx;
    PFullSize,PFlSize:Longint;

    RawBuffer: Pointer;
    RawBufferByteSize: Longint;
    RawBufferSampleSize: Longint;

    PCMBuffer: Pointer;
    PCMBufferByteSize: Longint;     // size of total buffer in bytes
    PCMBufferSampleSize: Longint;   // size of converted samples in buffer

    PCMSamplesPerSample,PCMBufferSamplePos: Longint;

    procedure AllocBuffers;
    procedure DestroyBuffers;
    procedure ReadSamples(Index: Longint);
    procedure SetBufferLength(Value: Longint);
    procedure SetPosition(Value: Longint);

  public
    constructor Create(Stream: TWaveStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint;

    property Format: PWaveFormatEx read dstwfx;
    property BufferLength: Longint read FBufferLength write SetBufferLength;
    property Position: Longint read FPosition write SetPosition;
    property Size: Longint read FSize;
    property Stream: TWaveStream read FStream;
    function BufferToFloat(makemono:boolean):pointer;
  end;

  TWavWriter=class(TObject)
  public
   Format:twaveformatex;
   stream:TfilewaveStream;
   constructor Create(fn:string;sr,ch,bits:longint);
   procedure WriteFloatData(p:pointer;size:longint);
   procedure WriteFloatDataSeparateStereo(p1,p2:pointer;size:longint);
   destructor Destroy;override;
  end;

implementation


function mmioFourCC(Chr1: Char; Chr2: Char; Chr3: Char; Chr4: Char): DWord;
begin
  Result:=Integer(Chr1)+(Integer(Chr2) shl 8)+(Integer(Chr3) shl 16)
   +(Integer(Chr4) shl 24);
end;

constructor TWaveStream.Create(mmIO: hmmIO; WriteFormat: PWaveFormatEx);
begin
  inherited Create;
  filemode:=0;
  mm:=mmIO;

  if(WriteFormat<>nil) then
  begin
    // Create the output file RIFF chunk of form type 'WAVE'.
    pckRIFF.fccType:=mmioFOURCC('W','A','V','E');
    pckRIFF.cksize:=0;
    if(mmioCreateChunk(mm,@pckRIFF,MMIO_CREATERIFF)<>0) then
      raise EWaveIOError.Create('Error 01: Cannot create chunk.');

    // We are now descended into the 'RIFF' chunk we just created.
    // * Now create the 'fmt ' chunk. Since we know the size of this chunk,
    // specify it in the MMCKINFO structure so MMIO doesn't have to seek
    // back and set the chunk size after ascending from the chunk.
    //
    pck.ckid:=mmioFOURCC('f','m','t',' ');
    pck.cksize:=sizeof(WriteFormat^)+WriteFormat^.cbSize;
    if(mmioCreateChunk(mm,@pck,0)<>0) then
      raise EWaveIOError.Create('Error 02: Cannot create chunk.');

    // Write the variable length size.
    if(mmioWrite(mm,Pointer(WriteFormat),sizeof(WriteFormat^)+WriteFormat^.cbSize)
     <>sizeof(WriteFormat^)+WriteFormat^.cbSize) then
      raise EWaveIOError.Create('Error 03: Cannot write wave format.');

    GetMem(FFormat,sizeof(FFormat^));
    CopyMemory(FFormat,WriteFormat,sizeof(FFormat^));

    // Ascend out of the 'fmt ' chunk,back into the 'RIFF' chunk.
    if(mmioAscend(mm,@pck,0)<>0) then
      raise EWaveIOError.Create('Error 04: Cannot ascend chunk.');

    // We are now descended into the 'RIFF' chunk we just created.
    // * Now create the 'data' chunk.
    pck.ckid:=mmioFOURCC('d','a','t','a');
    pck.cksize:=0;
    if(mmioCreateChunk(mm,@pck,0)<>0) then
      raise EWaveIOError.Create('Error 05: Cannot create chunk.');
  end
  else
  begin
    CheckMMIOWave;

    // put in the beggining of the file
    mmioSeek(mm,FDataOffset,SEEK_SET);
  end;
end;


destructor TWaveStream.Destroy;
begin
  if(FFormat<>nil) then
    FreeMem(FFormat);

  mmioSeek(mm,0,SEEK_END);

  if(mmioAscend(mm,@pck,0)<>0) then
    raise EWaveIOError.Create('Error 06: Cannot ascend chunk.');

  if(mmioAscend(mm,@pckRIFF,0)<>0) then
    raise EWaveIOError.Create('Error 07: Cannot ascend chunk.');

  if(mmioFlush(mm,0)<>0) then
    raise EWaveIOError.Create('Error 08: Cannot flush.');

  inherited Destroy;
end;


function TWaveStream.GetPosition: Longint;
begin
  Result:=Seek(0,SEEK_CUR);
end;

procedure TWaveStream.SetPosition(Pos: Longint);
begin
  Seek(Pos,SEEK_SET);
end;

function TWaveStream.Read(var Buffer; Count: Longint): Longint;
var
  p: Pointer;
begin
  p:=@Buffer;

  Result:=mmioRead(mm,p,Count * FFormat^.nBlockAlign);
  if(Result=-1) then
    raise EWaveIOError.Create('Error 09: Cannot read from file.')
  else
    Result:=Result div FFormat^.nBlockAlign;
end;

function TWaveStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if(Origin=SEEK_SET) then
    Result :=(mmioSeek(mm,Offset * FFormat^.nBlockAlign+FDataOffset,
      Origin)-FDataOffset) div FFormat^.nBlockAlign
  else
    Result :=(mmioSeek(mm,Offset * FFormat^.nBlockAlign,
      Origin)-FDataOffset) div FFormat^.nBlockAlign;

  if(Result<0) then
    raise EWaveIOError.Create('Error 10: Cannot seek in file.');
end;

function TWaveStream.Write(var Buffer; Count: Longint): Longint;
begin
  Result:=mmioWrite(mm,@Buffer,Count * FFormat^.nBlockAlign);
  if(Result=-1) then
    raise EWaveIOError.Create('Error 11: Cannot write to file.')
  else
    Result:=Result div FFormat^.nBlockAlign;
end;

procedure TWaveStream.CheckMMIOWave;
var
  mmIOInfo: TMMIOInfo;
  FormatTmp: TWaveFormatEx;
  ExtraAlloc: Word;
begin
  if(mmioDescend(mm,@pckRIFF,nil,0)<>0) then
    raise EWaveIOError.Create('Error 12: Invalid multimedia file!');

  if(pckRIFF.ckid<>FOURCC_RIFF)
  or(pckRIFF.fccType<>mmioFOURCC('W','A','V','E')) then
    raise EWaveIOError.Create('Error 13: Not a wave file!');

  // Search the input file for for the 'fmt ' chunk.     */
  pck.ckid:=mmioFOURCC('f','m','t',' ');
  if(mmioDescend(mm,@pck,@pckRIFF,MMIO_FINDCHUNK)<>0) then
    raise EWaveIOError.Create('Error 14: Cannot find ''fmt'' chunk!');

  // Expect the 'fmt' chunk to be at least as large as <PCMWAVEFORMAT>;
  // if there are extra parameters at the end,we'll ignore them */
  if(pck.cksize<16) then
    raise EWaveIOError.Create('Error 15: Abnormal ''fmt'' size!');

  // Read the 'fmt ' chunk into <pcmWaveFormat>.*/
  if(mmioRead(mm,PChar(@FormatTmp),16)<>16) then
    raise EWaveIOError.Create('Error 16: Cannot read ''fmt'' chunk!');

  // Ok,allocate the waveformatex,but if its not pcm
  // format,read the next word,and thats how many extra
  // bytes to allocate.
  if(FormatTmp.wFormatTag=WAVE_FORMAT_PCM) or
    (FormatTmp.wFormatTag=3) then
    ExtraAlloc:=0
  else
    // Read in length of extra bytes.
    if(mmioRead(mm,@ExtraAlloc,sizeof(ExtraAlloc)) <>
      sizeof(ExtraAlloc)) then
      raise EWaveIOError.Create('Error 17: Cannot read ''waveformatex'' length!');

  GetMem(FFormat,sizeof(FFormat^)+ExtraAlloc);
  CopyMemory(FFormat,@FormatTmp,sizeof(FFormat^));
  FFormat^.cbSize:=ExtraAlloc;
  if(ExtraAlloc<>0) then
    if(mmioRead(mm,PChar(FFormat)+sizeof(FFormat^),ExtraAlloc) <>
      ExtraAlloc) then
      raise EWaveIOError.Create('Error 18: Cannot read ''waveformatex''!');

  if(FFormat^.wFormatTag=MPEGLAYER3) then
    raise EWaveIOError.Create('Error 19: MPEG Layer-3 compression is not supported.');

  // Ascend the input file out of the 'fmt ' chunk. */
  if(mmioAscend(mm,@pck,0)<>0) then
    raise EWaveIOError.Create('Error 20: Cannot ascend from ''fmt'' chunk!');

  // Do a nice little seek...
  if(mmioSeek(mm,pckRIFF.dwDataOffset+sizeof(FOURCC),SEEK_SET)=-1) then
    raise EWaveIOError.Create('Error 21: Cannot seek to data!');

  //      Search the input file for the 'data' chunk.
  pck.ckid:=mmioFOURCC('d','a','t','a');
  mmioDescend(mm,@pck,@pckRIFF,MMIO_FINDCHUNK);

  if(mmioGetInfo(mm,@mmioInfo,0)<>0) then
    raise EWaveIOError.Create('Error 22: Cannot get info!');

  FDataOffset:=pck.dwDataOffset;
  FSize:=pck.cksize div FFormat^.nBlockAlign;
  FlSize:=FSize;
end;


//***************************************************************
//        TMemoryWaveStream
//***************************************************************

constructor TMemoryWaveStream.Create(Memory: Pointer; Size: Longint;
  WriteFormat: PWaveFormatEx);
var
  info: TMMIOINFO;
begin
  ZeroMemory(@info,sizeof(info));
  with info do
  begin
    pchBuffer:=Memory;
    fccIOProc:=FOURCC_MEM;
    cchBuffer:=Size;
  end;

  // Initialization...
  mmIO:=mmioOpen(nil, @info,MMIO_READ);
  if(mmIO=0) then
    raise EWaveIOError.Create('Error 23: Cannot open memory stream.');

  inherited Create(mmIO,WriteFormat);
end;

destructor TMemoryWaveStream.Destroy;
begin
  inherited Destroy;

  if(mmIO<>0) then mmioClose(mmIO,0);
end;


//***************************************************************
//        TFileWaveStream
//***************************************************************

constructor TFileWaveStream.Create(FileName: String;
  WriteFormat: PWaveFormatEx);
var f:file of byte;
begin
  // Initialization...
  if(WriteFormat=nil) then
  begin
    assignfile(f,filename);
   {$I-} reset(f);{$I+}
   if ioresult<>0 then
    raise EWaveIOError.Create('Error 24: Cannot open file.')
   else
   begin
    fullsize:=filesize(f);
    closefile(f);
    end;
    mmIO:=mmioOpen(Pointer(FileName), nil,MMIO_READ or MMIO_ALLOCBUF)
  end
  else
    mmIO:=mmioOpen(Pointer(FileName), nil,MMIO_CREATE or MMIO_READWRITE
      or MMIO_ALLOCBUF);

  if(mmIO=0) then
    raise EWaveIOError.Create('Error 25: Cannot open file stream.');

  inherited Create(mmIO,WriteFormat);
end;

destructor TFileWaveStream.Destroy;
begin
  inherited Destroy;
  if(mmIO<>0) then mmioClose(mmIO,0);
end;


//***************************************************************
//        TPCMWaveReader
//***************************************************************

constructor TPCMWaveReader.Create(Stream: TWaveStream);
begin
  inherited Create;

  FStream:=Stream;
  GetMem(dstwfx,sizeof(dstwfx^));

//  FBufferLength:=4096;
  FBufferLength:=20000;

  if(FStream.Format^.wFormatTag<>WAVE_FORMAT_PCM)
  and(FStream.Format^.wFormatTag<>3) then
  begin
    // prepare acm stream converter
    ZeroMemory(dstwfx,sizeof(dstwfx^));
    dstwfx^.wFormatTag:=WAVE_FORMAT_PCM;
    if(acmFormatSuggest(0,FStream.FFormat,dstwfx,sizeof(dstwfx^),
      ACM_FORMATSUGGESTF_WFORMATTAG)<>0) then
      raise EWaveIOError.Create('Error 26: Cannot suggest pcm format.');

    if(acmStreamOpen(PHACMSTREAM(@acmStream),0,FStream.Format^,dstwfx^,
      nil,0,0,0)<>0) then
      raise EWaveIOError.Create('Error 27: Cannot open acm stream.');

    AllocBuffers;

    // prepare buffers
    ZeroMemory(@ash,sizeof(ash));
    with ash do
    begin
      cbStruct:=sizeof(ash);
      pbSrc:=Pointer(RawBuffer);
      cbSrcLength:=RawBufferByteSize;
      dwSrcUser:=cbSrcLength;
      pbDst:=Pointer(PCMBuffer);
      cbDstLength:=PCMBufferByteSize;
      dwDstUser:=cbDstLength;
    end;

    if(acmStreamPrepareHeader(acmStream,ash,0)<>0) then
      raise EWaveIOError.Create('Error 28: Cannot prepare headers.');
  end
  else
    dstwfx^:=FStream.Format^;

  if(FStream.Format^.wFormatTag<>WAVE_FORMAT_PCM) and
    (FStream.Format^.wFormatTag<>3) then
    PCMSamplesperSample :=(PCMBufferByteSize * FStream.Format^.nBlockAlign)
      div(RawBufferByteSize * dstwfx^.nBlockAlign)
  else
    PCMSamplesPerSample:=1;
  FSize:=PCMSamplesPerSample * FStream.Size;
  PCMBufferSampleSize:=0;
end;


destructor TPCMWaveReader.Destroy;
begin
  if(acmStream<>0) then
  begin
    ash.cbSrcLength:=RawBufferByteSize;
    acmStreamUnprepareHeader(acmStream,ash,0);
    acmStreamClose(acmStream,0);
  end;

  if(dstwfx<>nil) then
    FreeMem(dstwfx);

  DestroyBuffers;

  inherited Destroy;
end;


procedure TPCMWaveReader.AllocBuffers;
var
   ss,dd: DWord;
begin
  DestroyBuffers;

  // calc space needed for holding the decompression of one sample
  if(acmStreamSize(acmStream,FStream.Format^.nBlockAlign,dd,
    ACM_STREAMSIZEF_SOURCE)<>0) then
    raise EWaveIOError.Create('Error 29: Cannot recommend an acm stream size.');

  // calc minimum size block of the source
  if(acmStreamSize(acmStream,dd,ss,ACM_STREAMSIZEF_DESTINATION)<>0) then
    raise EWaveIOError.Create('Error 30: Cannot recommend an acm stream size.');

  // alloc source buffer(raw)
  RawBufferSampleSize:=FBufferLength div dd;
  if(RawBufferSampleSize=0) then RawBufferSampleSize:=1;
  RawBufferByteSize:=RawBufferSampleSize * ss;
  GetMem(RawBuffer,RawBufferByteSize);

  // Alloc destination buffer(decompressed)
  if(acmStreamSize(acmStream,DWord(RawBufferByteSize),
    DWord(PCMBufferByteSize),ACM_STREAMSIZEF_SOURCE)<>0) then
    raise EWaveIOError.Create('Error 31: Cannot recommend an acm stream size.');
  GetMem(PCMBuffer,PCMBufferByteSize);
end;


procedure TPCMWaveReader.DestroyBuffers;
begin
  PCMBufferSampleSize:=0;

  if(PCMBuffer<>nil) then
    FreeMem(PCMBuffer);
  PCMBuffer:=nil;

  if(RawBuffer<>nil) then
    FreeMem(RawBuffer);
  RawBuffer:=nil;
end;

procedure TPCMWaveReader.SetBufferLength(Value: Longint);
begin
  if(Value<>FBufferLength) and(Value>=1024) then
  begin
    FBufferLength:=Value;
    if(PCMBuffer<>nil) then
      AllocBuffers;
  end;
end;

procedure TPCMWaveReader.SetPosition(Value: Longint);
begin
  if(Value<>FPosition) then
    if(Value>=0) and(Value<FSize) then
      FPosition:=Value
    else
      raise EWaveIOError.Create('Error 32: Position out of bounds.');
end;

function TPCMWaveReader.Read(var Buffer; Count: Longint): Longint;
var
  pos,len: Longint;
  posi,posf: Longint;
begin
 if Count<1 then begin Result:=0; Exit; end;

 if(FStream.Format^.wFormatTag=WAVE_FORMAT_PCM) or(FStream.Format^.wFormatTag=3)
  then
   begin
    FStream.Position:=FPosition;
    Result:=FStream.Read(Buffer,Count);
    FPosition:=FPosition+Result;
   end
  else
   begin
    if(Count+FPosition>=FSize) then Count:=FSize-FPosition;                                 // limit to wave size
    if(FPosition>=PCMBufferSamplePos) and(FPosition<PCMBufferSamplePos+PCMBufferSampleSize) // use current buffer data if possible
     then
      begin
       len:=PCMBufferSamplePos+PCMBufferSampleSize-FPosition;
       if(len>Count) then len:=Count;
       CopyMemory(PChar(@Buffer), PChar(PCMBuffer)+(FPosition-PCMBufferSamplePos)*dstwfx^.nBlockAlign, len*dstwfx^.nBlockAlign);
       pos:=len;
      end
     else pos:=0;
    while(pos<Count) do // put next data
     begin
      ReadSamples((FPosition+pos) div PCMSamplesPerSample); // calc. range of current pcm buffer that can be used to fill request
      posi:=FPosition+pos;
      if(FPosition+pos<PCMBufferSamplePos)
       then raise EWaveIOError.Create('Error 33: Position smaller than PCMBufferSamplePos');
      if(FPosition+Count>PCMBufferSamplePos+PCMBufferSampleSize)
       then posf:=PCMBufferSamplePos+PCMBufferSampleSize
       else posf:=FPosition+Count;
      len:=posf-posi;
      // put pcm buffer data into target
      CopyMemory(PChar(@Buffer)+(posi-FPosition)*dstwfx^.nBlockAlign, PChar(PCMBuffer)+(posi-PCMBufferSamplePos)*dstwfx^.nBlockAlign, len*dstwfx^.nBlockAlign);
      pos:=pos+len;
     end;
    FPosition:=FPosition+Count;
    Result:=Count;
   end;
end;

procedure TPCMWaveReader.ReadSamples(Index: Longint);
begin
 FStream.Position:=Index;
 ash.cbSrcLength:=FStream.Read(RawBuffer^,RawBufferSampleSize)*FStream.Format^.nBlockAlign;
 if(acmStreamConvert(acmStream,ash,ACM_STREAMCONVERTF_BLOCKALIGN)<>0) then
  raise EWaveIOError.Create('Error 34: Unable to convert sample.');
 PCMBufferSampleSize:=ash.cbDstLengthUsed div dstwfx^.nBlockAlign;
 PCMBufferSamplePos:=Index * PCMSamplesPerSample;
end;

function TPCMWaveReader.BufferToFloat(makemono:boolean):pointer;
var p:^shortint;
  p2:^byte;
  ps,pf:^single;
  fbuffer,buffer:pointer;
  l:longint;
procedure Convert(n:longint);
var
 cnt,cc,v:longint;
 v2:smallint;
 s:single;
 y,y2,y3:byte;
 by:byte;
begin
 cnt:=0;
 while(cnt<=format.nChannels*(n-1)) do
 begin
 if(format.wBitsPerSample=8) then
 begin
  for cc:=1 to format.nChannels do
  begin
  by:=p2^;
  inc(p2);
  s:=((2*by/(1 shl 8-1))-1);
  if(not makemono) then
  begin
   pf^:=s;
   inc(pf);
  end else
  begin
   pf^:=pf^+s;
   if cc=format.nChannels then
   begin
    pf^:=pf^/format.nChannels;
    inc(pf);
   end;
  end;
  inc(cnt);
  end;
 end else
 if(format.wBitsPerSample=16) then
 begin
  for cc:=1 to format.nChannels do
  begin
  y:=p^;
  inc(p);
  y2:=p^;
  inc(p);
  v2:=y+y2*(1 shl 8);
  s:=v2;
  if s>0 then s:=s/(1 shl 15-1) else s:=s/(1 shl 15);
  if(not makemono) then
  begin
   pf^:=s;
   inc(pf);
  end else
  begin
   pf^:=pf^+s;
   if cc=format.nChannels then
   begin
    pf^:=pf^/format.nChannels;
    inc(pf);
   end;
  end;
  inc(cnt);
  end;
 end else
 if(format.wBitsPerSample=20) then
 begin
  for cc:=1 to format.nChannels do
  begin
  y:=p^;
  inc(p);
  y2:=p^;
  inc(p);
  y3:=p^;
  inc(p);
  if not(format.nBlockAlign mod 3=0) then inc(p);
  v:=(y+y2*(1 shl 8)+y3*(1 shl 16))shr 4;
  if v>=1 shl 19 then v:=v-1 shl 20;
  s:=v;
  if s>0 then s:=s/(1 shl 19-1) else s:=s/(1 shl 19);
  if(not makemono) then
  begin
   pf^:=s;
   inc(pf);
  end else
  begin
   pf^:=pf^+s;
   if cc=format.nChannels then
   begin
    pf^:=pf^/format.nChannels;
    inc(pf);
   end;
  end;
  inc(cnt);
  end;
 end else
 if(format.wBitsPerSample=24) then
 begin
  for cc:=1 to format.nChannels do
  begin
  y:=p^;
  inc(p);
  y2:=p^;
  inc(p);
  y3:=p^;
  inc(p);
  if not(format.nBlockAlign mod 3=0) then inc(p);
  v:=y+y2*(1 shl 8)+y3*(1 shl 16);
  if v>=(1 shl 23) then v:=v-(1 shl 24);
  s:=v;
  if s>0 then s:=s/(1 shl 23-1) else s:=s/(1 shl 23);
  if(not makemono) then
  begin
   pf^:=s;
   inc(pf);
  end else
  begin
   pf^:=pf^+s;
   if cc=format.nChannels then
   begin
    pf^:=pf^/format.nChannels;
    inc(pf);
   end;
  end;
  inc(cnt);
  end;
 end else
 if(format.wBitsPerSample=32) then
 begin
  for cc:=1 to format.nChannels do
  begin
  s:=ps^;
  inc(ps);
  if(not makemono) then
  begin
   pf^:=s;
   inc(pf);
  end else
  begin
   pf^:=pf^+s;
   if cc=format.nChannels then
   begin
    pf^:=pf^/format.nChannels;
    inc(pf);
   end;
  end;
  inc(cnt);
  end;
 end;
 end;
end;

begin
 if pfullsize>pflsize then pflsize:=pfullsize;
 l:=format.nChannels*pflsize*sizeof(single);
 if makemono then l:=l div format.nChannels;
 getmem(FBuffer,l);
 getmem(buffer,buffer_size*sizeof(smallint)*4);
 fillchar(fbuffer^,l,0);
 p:=buffer;
 p2:=buffer;
 pf:=fbuffer;
 l:=Read(buffer^,BUFFER_SIZE);
 while(l>0) do
 begin
 p:=buffer;
 p2:=buffer;
 ps:=buffer;
 convert(l);
 l:=Read(buffer^,BUFFER_SIZE);
 end;
 if makemono then pflsize:=fsize
 else pflsize:=format.nChannels*fsize;
 freemem(buffer);
 result:=fbuffer;
end;

{ TWavWriter }

constructor TWavWriter.Create(fn: string; sr,ch,bits: Integer);
var p:pwaveformatex;
begin
 Format.nChannels:=ch;
 Format.nSamplesPerSec:=sr;
 if bits>32 then bits:=32;
 Format.wBitsPerSample:=bits;
 case bits of
 20,24:begin Format.nBlockAlign:=3;Format.wFormatTag:=WAVE_FORMAT_PCM end;
 32:begin Format.nBlockAlign:=4;Format.wFormatTag:=3 end;
 else begin
  if bits<=8 then Format.nBlockAlign:=1
  else if bits<=16 then Format.nBlockAlign:=2
  else if bits<=24 then Format.nBlockAlign:=3
  else if bits<32 then Format.nBlockAlign:=4;
  Format.wFormatTag:=WAVE_FORMAT_PCM;
 end;
 end;
 Format.nAvgBytesPerSec:=sr*ch*Format.nBlockAlign;
 p:=@format;
 stream:=TFileWaveStream.Create(fn,p);
end;

destructor TWavWriter.Destroy;
begin
 freeandnil(stream);
 sleep(100);
 inherited destroy;
end;

procedure TWavWriter.WriteFloatData(p: pointer; size: Integer);
var
  ps:psingle;
  l,li:longint;
  x:single;
begin
 ps:=p;
 for l:=0 to size-1 do
 begin
  if Format.wBitsPerSample=32 then
  begin x:=ps^;li:=longint((@x)^); end
  else li:=round(ps^*(1 shl(Format.wBitsPerSample-1)));
  stream.write(li,1);
  inc(ps);
 end;
end;


procedure TWavWriter.WriteFloatDataSeparateStereo(p1,p2: pointer;
  size: Integer);
var
  ps,ps2:psingle;
  l,li:longint;
  x:single;
begin
 ps:=p1;ps2:=p2;
 for l:=0 to size-1 do
 begin
  if Format.wBitsPerSample=32 then
  begin x:=ps^;li:=longint((@x)^); end
  else li:=round(ps^*(1 shl(Format.wBitsPerSample-1)));
  stream.write(li,1);
  inc(ps);
  if Format.wBitsPerSample=32 then
  begin x:=ps2^;li:=longint((@x)^); end
  else li:=round(ps2^*(1 shl(Format.wBitsPerSample-1)));
  stream.write(li,1);
  inc(ps2);
 end;
end;

{ High-Level functions }

function LoadWAVFile(fn:string;var sr,ch,size:longint):pointer;
var wave:TfilewaveStream;
    pcmw: TPCMWaveReader;
begin
 wave:=TfilewaveStream.Create(fn,nil);
 pcmw:=TPCMWaveReader.Create(wave);
 sr:=wave.format.nSamplesPerSec;
 ch:=wave.format.nChannels;
 pcmw.pfullsize:=wave.FullSize;
 result:=pcmw.buffertofloat(false);
 size:=pcmw.pFlSize;
 wave.Free;
 pcmw.Free;
end;

procedure GetWAVFileInfo(fn:string;var sr,ch,size:longint);
var wave:TfilewaveStream;
begin
 wave:=TfilewaveStream.Create(fn,nil);
 sr:=wave.format.nSamplesPerSec;
 ch:=wave.format.nChannels;
 size:=wave.Size;
 wave.Free;
end;

procedure SaveWAVFile(fn:string;fdata:pointer;sr,ch,bits,size:longint);
var
  w: TfilewaveStream;
  ps:psingle;
  l,li:longint;
  t:twaveformatex;
  p:pwaveformatex;
  x:single;
begin
 t.nChannels:=ch;
 t.nSamplesPerSec:=sr;
 if bits>32 then bits:=32;
 t.wBitsPerSample:=bits;
 case bits of
 20,24:begin t.nBlockAlign:=3;t.wFormatTag:=WAVE_FORMAT_PCM end;
 32:begin t.nBlockAlign:=4;t.wFormatTag:=3 end;
 else begin
  if bits<=8 then t.nBlockAlign:=1
  else if bits<=16 then t.nBlockAlign:=2
  else if bits<=24 then t.nBlockAlign:=3
  else if bits<32 then t.nBlockAlign:=4;
  t.wFormatTag:=WAVE_FORMAT_PCM;
 end;
 end;
 t.nAvgBytesPerSec:=sr*ch*t.nBlockAlign;
 p:=@t;

 w:=TFileWaveStream.Create(fn,p);
 ps:=fdata;
 for l:=0 to size-1 do
 begin
  if t.wBitsPerSample=32 then
  begin x:=ps^;li:=longint((@x)^); end
  else li:=round(ps^*(1 shl(bits-1)));
  w.write(li,1);
  inc(ps);
 end;
 w.Free;
end;

procedure SaveWAVFileSeparateStereo(fn:string;fdata1,fdata2:pointer;sr,ch,bits,size:longint);
var
  w: TFileWaveStream;
  ps,ps2:psingle;
  l,li:longint;
  t:twaveformatex;
  p:pwaveformatex;
  x:single;
begin
 t.nChannels:=ch;
 t.nSamplesPerSec:=sr;
 if bits>32 then bits:=32;
 t.wBitsPerSample:=bits;
 case bits of
 20,24:begin t.nBlockAlign:=3;t.wFormatTag:=WAVE_FORMAT_PCM end;
 32:begin t.nBlockAlign:=4;t.wFormatTag:=3 end;
 else begin
  if bits<=8 then t.nBlockAlign:=1
  else if bits<=16 then t.nBlockAlign:=2
  else if bits<=24 then t.nBlockAlign:=3
  else if bits<32 then t.nBlockAlign:=4;
  t.wFormatTag:=WAVE_FORMAT_PCM;
 end;
 end;
 t.nAvgBytesPerSec:=sr*ch*t.nBlockAlign;
 p:=@t;

 w:=TFileWaveStream.Create(fn,p);
 ps:=fdata1;
 ps2:=fdata2;
 for l:=0 to size-1 do
 begin
  if t.wBitsPerSample=32 then
  begin x:=ps^;li:=longint((@x)^); end
  else li:=round(ps^*(1 shl(bits-1)));
  w.write(li,1);
  inc(ps);
  if t.wBitsPerSample=32 then
  begin x:=ps2^;li:=longint((@x)^); end
  else li:=round(ps2^*(1 shl(bits-1)));
  w.write(li,1);
  inc(ps2);
 end;
 w.Free;
end;

function LoadWAVFileMono(fn:string;var sr,ch,size:longint):pointer;
var wave:TfilewaveStream;
    pcmw: TPCMWaveReader;
begin
 wave:=TfilewaveStream.Create(fn,nil);
 pcmw:=TPCMWaveReader.Create(wave);
 sr:=wave.format.nSamplesPerSec;
 pcmw.pfullsize:=wave.FullSize;
 ch:=1;
 result:=pcmw.buffertofloat(true);
 size:=pcmw.pFlSize;
 wave.Free;
 pcmw.Free;
end;

end.
