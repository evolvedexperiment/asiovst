unit DAV_MpegAudio;

interface

{$I DAV_Compiler.Inc}
{$DEFINE SEEK_STOP}

uses
  SysUtils, Classes, DAV_Common, DAV_CRC, DAV_SynthFilter, DAV_BitReserve,
  DAV_StereoBuffer, DAV_MpegAudioLayer3;

const
  SSLIMIT = 18;
  SBLIMIT = 32;

  // Size of the table of whole numbers raised to 4/3 power.
  // This may be adjusted for performance without any problems.
  CNrOfSFBBlock: array[0..5, 0..2, 0..3] of Cardinal = (
    (( 6,  5, 5, 5), ( 9,  9,  9, 9), ( 6,  9,  9, 9)),
    (( 6,  5, 7, 3), ( 9,  9, 12, 6), ( 6,  9, 12, 6)),
    ((11, 10, 0, 0), (18, 18,  0, 0), (15, 18,  0, 0)),
    (( 7,  7, 7, 0), (12, 12, 12, 0), ( 6, 15, 12, 0)),
    (( 6,  6, 6, 3), (12,  9,  9, 6), ( 6, 12,  9, 6)),
    (( 8,  8, 5, 0), (15, 12,  9, 0), ( 6, 18,  9, 0)));

  // factors and offsets for sample requantization:
  CTableFactor: array[0..14] of Single = (0, 2/3, 2/7, 2/15, 2/31, 2/63,
    2/127, 2/255, 2/511, 2/1023, 2/2047, 2/4095, 2/8191, 2/16383, 2/32767);

  CTableOffset: array[0..14] of Single = (0, -2/3, -6/7, -14/15, -30/31,
    -62/63, -126/127, -254/255, -510/511, -1022/1023, -2046/2047, -4094/4095,
    -8190/8191, -16382/16383, - 32766/32767);

  // this table contains 3 requantized samples for each legal codeword
  // when grouped in 5 bits, i.e. 3 quantizationsteps per sample
  CGrouping5Bits: array[0..27 * 3 - 1] of Single = (
    -2/3, -2/3, -2/3, 0, -2/3, -2/3, 2/3, -2/3, -2/3, -2/3, 0, -2/3, 0, 0,
    -2/3, 2/3, 0, -2/3, -2/3, 2/3, -2/3, 0, 2/3, -2/3, 2/3, 2/3, -2/3, -2/3,
    -2/3, 0, 0, -2/3, 0, 2/3, -2/3, 0, -2/3, 0, 0, 0, 0, 0, 2/3, 0, 0, -2/3,
     2/3, 0, 0, 2/3, 0, 2/3, 2/3, 0, -2/3, -2/3, 2/3, 0, -2/3, 2/3, 2/3, -2/3,
     2/3, -2/3, 0, 2/3, 0, 0, 2/3, 2/3, 0, 2/3, -2/3, 2/3, 2/3, 0, 2/3, 2/3,
     2/3, 2/3, 2/3);

  // this table contains 3 requantized samples for each legal codeword
  // when grouped in 7 bits, i.e. 5 quantizationsteps per sample
  CGrouping7Bits: array[0..125 * 3 - 1] of Single = (
      -0.8, -0.8, -0.8, -0.4, -0.8, -0.8, 0, -0.8, -0.8, 0.4, -0.8, -0.8, 0.8, -0.8, -0.8,
      -0.8, -0.4, -0.8, -0.4, -0.4, -0.8, 0, -0.4, -0.8, 0.4, -0.4, -0.8, 0.8, -0.4, -0.8,
      -0.8,  0.0, -0.8, -0.4,  0.0, -0.8, 0,  0.0, -0.8, 0.4,  0.0, -0.8, 0.8,  0.0, -0.8,
      -0.8,  0.4, -0.8, -0.4,  0.4, -0.8, 0,  0.4, -0.8, 0.4,  0.4, -0.8, 0.8,  0.4, -0.8,
      -0.8,  0.8, -0.8, -0.4,  0.8, -0.8, 0,  0.8, -0.8, 0.4,  0.8, -0.8, 0.8,  0.8, -0.8,
      -0.8, -0.8, -0.4, -0.4, -0.8, -0.4, 0, -0.8, -0.4, 0.4, -0.8, -0.4, 0.8, -0.8, -0.4,
      -0.8, -0.4, -0.4, -0.4, -0.4, -0.4, 0, -0.4, -0.4, 0.4, -0.4, -0.4, 0.8, -0.4, -0.4,
      -0.8,  0.0, -0.4, -0.4,  0.0, -0.4, 0,  0.0, -0.4, 0.4,  0.0, -0.4, 0.8,  0.0, -0.4,
      -0.8,  0.4, -0.4, -0.4,  0.4, -0.4, 0,  0.4, -0.4, 0.4,  0.4, -0.4, 0.8,  0.4, -0.4,
      -0.8,  0.8, -0.4, -0.4,  0.8, -0.4, 0,  0.8, -0.4, 0.4,  0.8, -0.4, 0.8,  0.8, -0.4,
      -0.8, -0.8,  0.0, -0.4, -0.8,  0.0, 0, -0.8,  0.0, 0.4, -0.8,  0.0, 0.8, -0.8,  0.0,
      -0.8, -0.4,  0.0, -0.4, -0.4,  0.0, 0, -0.4,  0.0, 0.4, -0.4,  0.0, 0.8, -0.4,  0.0,
      -0.8,  0.0,  0.0, -0.4,  0.0,  0.0, 0,  0.0,  0.0, 0.4,  0.0,  0.0, 0.8,  0.0,  0.0,
      -0.8,  0.4,  0.0, -0.4,  0.4,  0.0, 0,  0.4,  0.0, 0.4,  0.4,  0.0, 0.8,  0.4,  0.0,
      -0.8,  0.8,  0.0, -0.4,  0.8,  0.0, 0,  0.8,  0.0, 0.4,  0.8,  0.0, 0.8,  0.8,  0.0,
      -0.8, -0.8,  0.4, -0.4, -0.8,  0.4, 0, -0.8,  0.4, 0.4, -0.8,  0.4, 0.8, -0.8,  0.4,
      -0.8, -0.4,  0.4, -0.4, -0.4,  0.4, 0, -0.4,  0.4, 0.4, -0.4,  0.4, 0.8, -0.4,  0.4,
      -0.8,  0.0,  0.4, -0.4,  0.0,  0.4, 0,  0.0,  0.4, 0.4,  0.0,  0.4, 0.8,  0.0,  0.4,
      -0.8,  0.4,  0.4, -0.4,  0.4,  0.4, 0,  0.4,  0.4, 0.4,  0.4,  0.4, 0.8,  0.4,  0.4,
      -0.8,  0.8,  0.4, -0.4,  0.8,  0.4, 0,  0.8,  0.4, 0.4,  0.8,  0.4, 0.8,  0.8,  0.4,
      -0.8, -0.8,  0.8, -0.4, -0.8,  0.8, 0, -0.8,  0.8, 0.4, -0.8,  0.8, 0.8, -0.8,  0.8,
      -0.8, -0.4,  0.8, -0.4, -0.4,  0.8, 0, -0.4,  0.8, 0.4, -0.4,  0.8, 0.8, -0.4,  0.8,
      -0.8,  0.0,  0.8, -0.4,  0.0,  0.8, 0,  0.0,  0.8, 0.4,  0.0,  0.8, 0.8,  0.0,  0.8,
      -0.8,  0.4,  0.8, -0.4,  0.4,  0.8, 0,  0.4,  0.8, 0.4,  0.4,  0.8, 0.8,  0.4,  0.8,
      -0.8,  0.8,  0.8, -0.4,  0.8,  0.8, 0,  0.8,  0.8, 0.4,  0.8,  0.8, 0.8,  0.8,  0.8);

  // this table contains 3 requantized samples for each legal codeword
  // when grouped in 10 bits, i.e. 9 quantizationsteps per sample
  CGrouping10Bits: array[0..729 * 3 - 1] of Single = (
    -8/9, -8/9, -8/9, -6/9, -8/9, -8/9, -4/9, -8/9, -8/9, -2/9, -8/9, -8/9,
       0, -8/9, -8/9,  2/9, -8/9, -8/9,  4/9, -8/9, -8/9,  6/9, -8/9, -8/9,
     8/9, -8/9, -8/9, -8/9, -6/9, -8/9, -6/9, -6/9, -8/9, -4/9, -6/9, -8/9,
    -2/9, -6/9, -8/9,    0, -6/9, -8/9,  2/9, -6/9, -8/9,  4/9, -6/9, -8/9,
     6/9, -6/9, -8/9,  8/9, -6/9, -8/9, -8/9, -4/9, -8/9, -6/9, -4/9, -8/9,
    -4/9, -4/9, -8/9, -2/9, -4/9, -8/9,    0, -4/9, -8/9,  2/9, -4/9, -8/9,
     4/9, -4/9, -8/9,  6/9, -4/9, -8/9,  8/9, -4/9, -8/9, -8/9, -2/9, -8/9,
    -6/9, -2/9, -8/9, -4/9, -2/9, -8/9, -2/9, -2/9, -8/9,    0, -2/9, -8/9,
     2/9, -2/9, -8/9,  4/9, -2/9, -8/9,  6/9, -2/9, -8/9,  8/9, -2/9, -8/9,
    -8/9,    0, -8/9, -6/9,    0, -8/9, -4/9,    0, -8/9, -2/9,    0, -8/9,
       0,    0, -8/9,  2/9,    0, -8/9,  4/9,    0, -8/9,  6/9,    0, -8/9,
     8/9,    0, -8/9, -8/9,  2/9, -8/9, -6/9,  2/9, -8/9, -4/9,  2/9, -8/9,
    -2/9,  2/9, -8/9,    0,  2/9, -8/9,  2/9,  2/9, -8/9,  4/9,  2/9, -8/9,
     6/9,  2/9, -8/9,  8/9,  2/9, -8/9, -8/9,  4/9, -8/9, -6/9,  4/9, -8/9,
    -4/9,  4/9, -8/9, -2/9,  4/9, -8/9,    0,  4/9, -8/9,  2/9,  4/9, -8/9,
     4/9,  4/9, -8/9,  6/9,  4/9, -8/9,  8/9,  4/9, -8/9, -8/9,  6/9, -8/9,
    -6/9,  6/9, -8/9, -4/9,  6/9, -8/9, -2/9,  6/9, -8/9,    0,  6/9, -8/9,
     2/9,  6/9, -8/9,  4/9,  6/9, -8/9,  6/9,  6/9, -8/9,  8/9,  6/9, -8/9,
    -8/9,  8/9, -8/9, -6/9,  8/9, -8/9, -4/9,  8/9, -8/9, -2/9,  8/9, -8/9,
       0,  8/9, -8/9,  2/9,  8/9, -8/9,  4/9,  8/9, -8/9,  6/9,  8/9, -8/9,
     8/9,  8/9, -8/9, -8/9, -8/9, -6/9, -6/9, -8/9, -6/9, -4/9, -8/9, -6/9,
    -2/9, -8/9, -6/9,    0, -8/9, -6/9,  2/9, -8/9, -6/9,  4/9, -8/9, -6/9,
     6/9, -8/9, -6/9,  8/9, -8/9, -6/9, -8/9, -6/9, -6/9, -6/9, -6/9, -6/9,
    -4/9, -6/9, -6/9, -2/9, -6/9, -6/9,    0, -6/9, -6/9,  2/9, -6/9, -6/9,
     4/9, -6/9, -6/9,  6/9, -6/9, -6/9,  8/9, -6/9, -6/9, -8/9, -4/9, -6/9,
    -6/9, -4/9, -6/9, -4/9, -4/9, -6/9, -2/9, -4/9, -6/9,    0, -4/9, -6/9,
     2/9, -4/9, -6/9,  4/9, -4/9, -6/9,  6/9, -4/9, -6/9,  8/9, -4/9, -6/9,
    -8/9, -2/9, -6/9, -6/9, -2/9, -6/9, -4/9, -2/9, -6/9, -2/9, -2/9, -6/9,
       0, -2/9, -6/9,  2/9, -2/9, -6/9,  4/9, -2/9, -6/9,  6/9, -2/9, -6/9,
     8/9, -2/9, -6/9, -8/9,    0, -6/9, -6/9,    0, -6/9, -4/9,    0, -6/9,
    -2/9,    0, -6/9,    0,    0, -6/9,  2/9,    0, -6/9,  4/9,    0, -6/9,
     6/9,    0, -6/9,  8/9,    0, -6/9, -8/9,  2/9, -6/9, -6/9,  2/9, -6/9,
    -4/9,  2/9, -6/9, -2/9,  2/9, -6/9,    0,  2/9, -6/9,  2/9,  2/9, -6/9,
     4/9,  2/9, -6/9,  6/9,  2/9, -6/9,  8/9,  2/9, -6/9, -8/9,  4/9, -6/9,
    -6/9,  4/9, -6/9, -4/9,  4/9, -6/9, -2/9,  4/9, -6/9,    0,  4/9, -6/9,
     2/9,  4/9, -6/9,  4/9,  4/9, -6/9,  6/9,  4/9, -6/9,  8/9,  4/9, -6/9,
    -8/9,  6/9, -6/9, -6/9,  6/9, -6/9, -4/9,  6/9, -6/9, -2/9,  6/9, -6/9,
       0,  6/9, -6/9,  2/9,  6/9, -6/9,  4/9,  6/9, -6/9,  6/9,  6/9, -6/9,
     8/9,  6/9, -6/9, -8/9,  8/9, -6/9, -6/9,  8/9, -6/9, -4/9,  8/9, -6/9,
    -2/9,  8/9, -6/9,    0,  8/9, -6/9,  2/9,  8/9, -6/9,  4/9,  8/9, -6/9,
     6/9,  8/9, -6/9,  8/9,  8/9, -6/9, -8/9, -8/9, -4/9, -6/9, -8/9, -4/9,
    -4/9, -8/9, -4/9, -2/9, -8/9, -4/9,    0, -8/9, -4/9,  2/9, -8/9, -4/9,
     4/9, -8/9, -4/9,  6/9, -8/9, -4/9,  8/9, -8/9, -4/9, -8/9, -6/9, -4/9,
    -6/9, -6/9, -4/9, -4/9, -6/9, -4/9, -2/9, -6/9, -4/9,    0, -6/9, -4/9,
     2/9, -6/9, -4/9,  4/9, -6/9, -4/9,  6/9, -6/9, -4/9,  8/9, -6/9, -4/9,
    -8/9, -4/9, -4/9, -6/9, -4/9, -4/9, -4/9, -4/9, -4/9, -2/9, -4/9, -4/9,
       0, -4/9, -4/9,  2/9, -4/9, -4/9,  4/9, -4/9, -4/9,  6/9, -4/9, -4/9,
     8/9, -4/9, -4/9, -8/9, -2/9, -4/9, -6/9, -2/9, -4/9, -4/9, -2/9, -4/9,
    -2/9, -2/9, -4/9,    0, -2/9, -4/9,  2/9, -2/9, -4/9,  4/9, -2/9, -4/9,
     6/9, -2/9, -4/9,  8/9, -2/9, -4/9, -8/9,    0, -4/9, -6/9,    0, -4/9,
    -4/9,    0, -4/9, -2/9,    0, -4/9,    0,    0, -4/9,  2/9,    0, -4/9,
     4/9,    0, -4/9,  6/9,    0, -4/9,  8/9,    0, -4/9, -8/9,  2/9, -4/9,
    -6/9,  2/9, -4/9, -4/9,  2/9, -4/9, -2/9,  2/9, -4/9,    0,  2/9, -4/9,
     2/9,  2/9, -4/9,  4/9,  2/9, -4/9,  6/9,  2/9, -4/9,  8/9,  2/9, -4/9,
    -8/9,  4/9, -4/9, -6/9,  4/9, -4/9, -4/9,  4/9, -4/9, -2/9,  4/9, -4/9,
       0,  4/9, -4/9,  2/9,  4/9, -4/9,  4/9,  4/9, -4/9,  6/9,  4/9, -4/9,
     8/9,  4/9, -4/9, -8/9,  6/9, -4/9, -6/9,  6/9, -4/9, -4/9,  6/9, -4/9,
    -2/9,  6/9, -4/9,    0,  6/9, -4/9,  2/9,  6/9, -4/9,  4/9,  6/9, -4/9,
     6/9,  6/9, -4/9,  8/9,  6/9, -4/9, -8/9,  8/9, -4/9, -6/9,  8/9, -4/9,
    -4/9,  8/9, -4/9, -2/9,  8/9, -4/9,    0,  8/9, -4/9,  2/9,  8/9, -4/9,
     4/9,  8/9, -4/9,  6/9,  8/9, -4/9,  8/9,  8/9, -4/9, -8/9, -8/9, -2/9,
    -6/9, -8/9, -2/9, -4/9, -8/9, -2/9, -2/9, -8/9, -2/9,    0, -8/9, -2/9,
     2/9, -8/9, -2/9,  4/9, -8/9, -2/9,  6/9, -8/9, -2/9,  8/9, -8/9, -2/9,
    -8/9, -6/9, -2/9, -6/9, -6/9, -2/9, -4/9, -6/9, -2/9, -2/9, -6/9, -2/9,
       0, -6/9, -2/9,  2/9, -6/9, -2/9,  4/9, -6/9, -2/9,  6/9, -6/9, -2/9,
     8/9, -6/9, -2/9, -8/9, -4/9, -2/9, -6/9, -4/9, -2/9, -4/9, -4/9, -2/9,
    -2/9, -4/9, -2/9,    0, -4/9, -2/9,  2/9, -4/9, -2/9,  4/9, -4/9, -2/9,
     6/9, -4/9, -2/9,  8/9, -4/9, -2/9, -8/9, -2/9, -2/9, -6/9, -2/9, -2/9,
    -4/9, -2/9, -2/9, -2/9, -2/9, -2/9,    0, -2/9, -2/9,  2/9, -2/9, -2/9,
     4/9, -2/9, -2/9,  6/9, -2/9, -2/9,  8/9, -2/9, -2/9, -8/9,    0, -2/9,
    -6/9,    0, -2/9, -4/9,    0, -2/9, -2/9,    0, -2/9,    0,    0, -2/9,
     2/9,    0, -2/9,  4/9,    0, -2/9,  6/9,    0, -2/9,  8/9,    0, -2/9, 
    -8/9,  2/9, -2/9, -6/9,  2/9, -2/9, -4/9,  2/9, -2/9, -2/9,  2/9, -2/9,
       0,  2/9, -2/9,  2/9,  2/9, -2/9,  4/9,  2/9, -2/9,  6/9,  2/9, -2/9,
     8/9,  2/9, -2/9, -8/9,  4/9, -2/9, -6/9,  4/9, -2/9, -4/9,  4/9, -2/9,
    -2/9,  4/9, -2/9,    0,  4/9, -2/9,  2/9,  4/9, -2/9,  4/9,  4/9, -2/9,
     6/9,  4/9, -2/9,  8/9,  4/9, -2/9, -8/9,  6/9, -2/9, -6/9,  6/9, -2/9,
    -4/9,  6/9, -2/9, -2/9,  6/9, -2/9,    0,  6/9, -2/9,  2/9,  6/9, -2/9, 
     4/9,  6/9, -2/9,  6/9,  6/9, -2/9,  8/9,  6/9, -2/9, -8/9,  8/9, -2/9,
    -6/9,  8/9, -2/9, -4/9,  8/9, -2/9, -2/9,  8/9, -2/9,    0,  8/9, -2/9,
     2/9,  8/9, -2/9,  4/9,  8/9, -2/9,  6/9,  8/9, -2/9,  8/9,  8/9, -2/9,
    -8/9, -8/9,    0, -6/9, -8/9,    0, -4/9, -8/9,    0, -2/9, -8/9,    0,
       0, -8/9,    0,  2/9, -8/9,    0,  4/9, -8/9,    0,  6/9, -8/9,    0,
     8/9, -8/9,    0, -8/9, -6/9,    0, -6/9, -6/9,    0, -4/9, -6/9,    0,
    -2/9, -6/9,    0,    0, -6/9,    0,  2/9, -6/9,    0,  4/9, -6/9,    0,
     6/9, -6/9,    0,  8/9, -6/9,    0, -8/9, -4/9,    0, -6/9, -4/9,    0,
    -4/9, -4/9,    0, -2/9, -4/9,    0,    0, -4/9,    0,  2/9, -4/9,    0, 
     4/9, -4/9,    0,  6/9, -4/9,    0,  8/9, -4/9,    0, -8/9, -2/9,    0,
    -6/9, -2/9,    0, -4/9, -2/9,    0, -2/9, -2/9,    0,    0, -2/9,    0,
     2/9, -2/9,    0,  4/9, -2/9,    0,  6/9, -2/9,    0,  8/9, -2/9,    0,
    -8/9,    0,    0, -6/9,    0,    0, -4/9,    0,    0, -2/9,    0,    0,
       0,    0,    0,  2/9,    0,    0,  4/9,    0,    0,  6/9,    0,    0,
     8/9,    0,    0, -8/9,  2/9,    0, -6/9,  2/9,    0, -4/9,  2/9,    0,
    -2/9,  2/9,    0,    0,  2/9,    0,  2/9,  2/9,    0,  4/9,  2/9,    0,
     6/9,  2/9,    0,  8/9,  2/9,    0, -8/9,  4/9,    0, -6/9,  4/9,    0,
    -4/9,  4/9,    0, -2/9,  4/9,    0,    0,  4/9,    0,  2/9,  4/9,    0,
     4/9,  4/9,    0,  6/9,  4/9,    0,  8/9,  4/9,    0, -8/9,  6/9,    0,
    -6/9,  6/9,    0, -4/9,  6/9,    0, -2/9,  6/9,    0,    0,  6/9,    0,
     2/9,  6/9,    0,  4/9,  6/9,    0,  6/9,  6/9,    0,  8/9,  6/9,    0,
    -8/9,  8/9,    0, -6/9,  8/9,    0, -4/9,  8/9,    0, -2/9,  8/9,    0,
       0,  8/9,    0,  2/9,  8/9,    0,  4/9,  8/9,    0,  6/9,  8/9,    0,
     8/9,  8/9,    0, -8/9, -8/9,  2/9, -6/9, -8/9,  2/9, -4/9, -8/9,  2/9,
    -2/9, -8/9,  2/9,    0, -8/9,  2/9,  2/9, -8/9,  2/9,  4/9, -8/9,  2/9,
     6/9, -8/9,  2/9,  8/9, -8/9,  2/9, -8/9, -6/9,  2/9, -6/9, -6/9,  2/9,
    -4/9, -6/9,  2/9, -2/9, -6/9,  2/9,    0, -6/9,  2/9,  2/9, -6/9,  2/9,
     4/9, -6/9,  2/9,  6/9, -6/9,  2/9,  8/9, -6/9,  2/9, -8/9, -4/9,  2/9,
    -6/9, -4/9,  2/9, -4/9, -4/9,  2/9, -2/9, -4/9,  2/9,    0, -4/9,  2/9,
     2/9, -4/9,  2/9,  4/9, -4/9,  2/9,  6/9, -4/9,  2/9,  8/9, -4/9,  2/9,
    -8/9, -2/9,  2/9, -6/9, -2/9,  2/9, -4/9, -2/9,  2/9, -2/9, -2/9,  2/9,
       0, -2/9,  2/9,  2/9, -2/9,  2/9,  4/9, -2/9,  2/9,  6/9, -2/9,  2/9,
     8/9, -2/9,  2/9, -8/9,    0,  2/9, -6/9,    0,  2/9, -4/9,    0,  2/9,
    -2/9,    0,  2/9,    0,    0,  2/9,  2/9,    0,  2/9,  4/9,    0,  2/9,
     6/9,    0,  2/9,  8/9,    0,  2/9, -8/9,  2/9,  2/9, -6/9,  2/9,  2/9,
    -4/9,  2/9,  2/9, -2/9,  2/9,  2/9,    0,  2/9,  2/9,  2/9,  2/9,  2/9,
     4/9,  2/9,  2/9,  6/9,  2/9,  2/9,  8/9,  2/9,  2/9, -8/9,  4/9,  2/9,
    -6/9,  4/9,  2/9, -4/9,  4/9,  2/9, -2/9,  4/9,  2/9,    0,  4/9,  2/9,
     2/9,  4/9,  2/9,  4/9,  4/9,  2/9,  6/9,  4/9,  2/9,  8/9,  4/9,  2/9,
    -8/9,  6/9,  2/9, -6/9,  6/9,  2/9, -4/9,  6/9,  2/9, -2/9,  6/9,  2/9,
       0,  6/9,  2/9,  2/9,  6/9,  2/9,  4/9,  6/9,  2/9,  6/9,  6/9,  2/9,
     8/9,  6/9,  2/9, -8/9,  8/9,  2/9, -6/9,  8/9,  2/9, -4/9,  8/9,  2/9,
    -2/9,  8/9,  2/9,    0,  8/9,  2/9,  2/9,  8/9,  2/9,  4/9,  8/9,  2/9,
     6/9,  8/9,  2/9,  8/9,  8/9,  2/9, -8/9, -8/9,  4/9, -6/9, -8/9,  4/9,
    -4/9, -8/9,  4/9, -2/9, -8/9,  4/9,    0, -8/9,  4/9,  2/9, -8/9,  4/9,
     4/9, -8/9,  4/9,  6/9, -8/9,  4/9,  8/9, -8/9,  4/9, -8/9, -6/9,  4/9,
    -6/9, -6/9,  4/9, -4/9, -6/9,  4/9, -2/9, -6/9,  4/9,    0, -6/9,  4/9,
     2/9, -6/9,  4/9,  4/9, -6/9,  4/9,  6/9, -6/9,  4/9,  8/9, -6/9,  4/9,
    -8/9, -4/9,  4/9, -6/9, -4/9,  4/9, -4/9, -4/9,  4/9, -2/9, -4/9,  4/9,
       0, -4/9,  4/9,  2/9, -4/9,  4/9,  4/9, -4/9,  4/9,  6/9, -4/9,  4/9,
     8/9, -4/9,  4/9, -8/9, -2/9,  4/9, -6/9, -2/9,  4/9, -4/9, -2/9,  4/9,
    -2/9, -2/9,  4/9,    0, -2/9,  4/9,  2/9, -2/9,  4/9,  4/9, -2/9,  4/9,
     6/9, -2/9,  4/9,  8/9, -2/9,  4/9, -8/9,    0,  4/9, -6/9,    0,  4/9,
    -4/9,    0,  4/9, -2/9,    0,  4/9,    0,    0,  4/9,  2/9,    0,  4/9,
     4/9,    0,  4/9,  6/9,    0,  4/9,  8/9,    0,  4/9, -8/9,  2/9,  4/9,
    -6/9,  2/9,  4/9, -4/9,  2/9,  4/9, -2/9,  2/9,  4/9,    0,  2/9,  4/9,
     2/9,  2/9,  4/9,  4/9,  2/9,  4/9,  6/9,  2/9,  4/9,  8/9,  2/9,  4/9,
    -8/9,  4/9,  4/9, -6/9,  4/9,  4/9, -4/9,  4/9,  4/9, -2/9,  4/9,  4/9,
       0,  4/9,  4/9,  2/9,  4/9,  4/9,  4/9,  4/9,  4/9,  6/9,  4/9,  4/9,
     8/9,  4/9,  4/9, -8/9,  6/9,  4/9, -6/9,  6/9,  4/9, -4/9,  6/9,  4/9,
    -2/9,  6/9,  4/9,    0,  6/9,  4/9,  2/9,  6/9,  4/9,  4/9,  6/9,  4/9,
     6/9,  6/9,  4/9,  8/9,  6/9,  4/9, -8/9,  8/9,  4/9, -6/9,  8/9,  4/9,
    -4/9,  8/9,  4/9, -2/9,  8/9,  4/9,    0,  8/9,  4/9,  2/9,  8/9,  4/9,
     4/9,  8/9,  4/9,  6/9,  8/9,  4/9,  8/9,  8/9,  4/9, -8/9, -8/9,  6/9,
    -6/9, -8/9,  6/9, -4/9, -8/9,  6/9, -2/9, -8/9,  6/9,    0, -8/9,  6/9,
     2/9, -8/9,  6/9,  4/9, -8/9,  6/9,  6/9, -8/9,  6/9,  8/9, -8/9,  6/9,
    -8/9, -6/9,  6/9, -6/9, -6/9,  6/9, -4/9, -6/9,  6/9, -2/9, -6/9,  6/9,
       0, -6/9,  6/9,  2/9, -6/9,  6/9,  4/9, -6/9,  6/9,  6/9, -6/9,  6/9,
     8/9, -6/9,  6/9, -8/9, -4/9,  6/9, -6/9, -4/9,  6/9, -4/9, -4/9,  6/9,
    -2/9, -4/9,  6/9,    0, -4/9,  6/9,  2/9, -4/9,  6/9,  4/9, -4/9,  6/9,
     6/9, -4/9,  6/9,  8/9, -4/9,  6/9, -8/9, -2/9,  6/9, -6/9, -2/9,  6/9,
    -4/9, -2/9,  6/9, -2/9, -2/9,  6/9,    0, -2/9,  6/9,  2/9, -2/9,  6/9,
     4/9, -2/9,  6/9,  6/9, -2/9,  6/9,  8/9, -2/9,  6/9, -8/9,    0,  6/9,
    -6/9,    0,  6/9, -4/9,    0,  6/9, -2/9,    0,  6/9,    0,    0,  6/9,
     2/9,    0,  6/9,  4/9,    0,  6/9,  6/9,    0,  6/9,  8/9,    0,  6/9,
    -8/9,  2/9,  6/9, -6/9,  2/9,  6/9, -4/9,  2/9,  6/9, -2/9,  2/9,  6/9,
       0,  2/9,  6/9,  2/9,  2/9,  6/9,  4/9,  2/9,  6/9,  6/9,  2/9,  6/9,
     8/9,  2/9,  6/9, -8/9,  4/9,  6/9, -6/9,  4/9,  6/9, -4/9,  4/9,  6/9,
    -2/9,  4/9,  6/9,    0,  4/9,  6/9,  2/9,  4/9,  6/9,  4/9,  4/9,  6/9,
     6/9,  4/9,  6/9,  8/9,  4/9,  6/9, -8/9,  6/9,  6/9, -6/9,  6/9,  6/9,
    -4/9,  6/9,  6/9, -2/9,  6/9,  6/9,    0,  6/9,  6/9,  2/9,  6/9,  6/9,
     4/9,  6/9,  6/9,  6/9,  6/9,  6/9,  8/9,  6/9,  6/9, -8/9,  8/9,  6/9,
    -6/9,  8/9,  6/9, -4/9,  8/9,  6/9, -2/9,  8/9,  6/9,    0,  8/9,  6/9,
     2/9,  8/9,  6/9,  4/9,  8/9,  6/9,  6/9,  8/9,  6/9,  8/9,  8/9,  6/9,
    -8/9, -8/9,  8/9, -6/9, -8/9,  8/9, -4/9, -8/9,  8/9, -2/9, -8/9,  8/9,
       0, -8/9,  8/9,  2/9, -8/9,  8/9,  4/9, -8/9,  8/9,  6/9, -8/9,  8/9,
     8/9, -8/9,  8/9, -8/9, -6/9,  8/9, -6/9, -6/9,  8/9, -4/9, -6/9,  8/9,
    -2/9, -6/9,  8/9,    0, -6/9,  8/9,  2/9, -6/9,  8/9,  4/9, -6/9,  8/9,
     6/9, -6/9,  8/9,  8/9, -6/9,  8/9, -8/9, -4/9,  8/9, -6/9, -4/9,  8/9,
    -4/9, -4/9,  8/9, -2/9, -4/9,  8/9,    0, -4/9,  8/9,  2/9, -4/9,  8/9,
     4/9, -4/9,  8/9,  6/9, -4/9,  8/9,  8/9, -4/9,  8/9, -8/9, -2/9,  8/9,
    -6/9, -2/9,  8/9, -4/9, -2/9,  8/9, -2/9, -2/9,  8/9,    0, -2/9,  8/9,
     2/9, -2/9,  8/9,  4/9, -2/9,  8/9,  6/9, -2/9,  8/9,  8/9, -2/9,  8/9,
    -8/9,    0,  8/9, -6/9,    0,  8/9, -4/9,    0,  8/9, -2/9,    0,  8/9,
       0,    0,  8/9,  2/9,    0,  8/9,  4/9,    0,  8/9,  6/9,    0,  8/9,
     8/9,    0,  8/9, -8/9,  2/9,  8/9, -6/9,  2/9,  8/9, -4/9,  2/9,  8/9,
    -2/9,  2/9,  8/9,    0,  2/9,  8/9,  2/9,  2/9,  8/9,  4/9,  2/9,  8/9,
     6/9,  2/9,  8/9,  8/9,  2/9,  8/9, -8/9,  4/9,  8/9, -6/9,  4/9,  8/9,
    -4/9,  4/9,  8/9, -2/9,  4/9,  8/9,    0,  4/9,  8/9,  2/9,  4/9,  8/9,
     4/9,  4/9,  8/9,  6/9,  4/9,  8/9,  8/9,  4/9,  8/9, -8/9,  6/9,  8/9,
    -6/9,  6/9,  8/9, -4/9,  6/9,  8/9, -2/9,  6/9,  8/9,    0,  6/9,  8/9,
     2/9,  6/9,  8/9,  4/9,  6/9,  8/9,  6/9,  6/9,  8/9,  8/9,  6/9,  8/9,
    -8/9,  8/9,  8/9, -6/9,  8/9,  8/9, -4/9,  8/9,  8/9, -2/9,  8/9,  8/9,
       0,  8/9,  8/9,  2/9,  8/9,  8/9,  4/9,  8/9,  8/9,  6/9,  8/9,  8/9,
     8/9,  8/9,  8/9);

  // data taken from ISO/IEC DIS 11172,  Annexes 3-B.2[abcd] and 3-B.4:

  // subbands 0-2 in tables 3-B.2a and 2b: (index is allocation)
  // bits per codeword
  CTableAB1CodeLength: array[0..15] of Cardinal = (
    0, 5, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);

  // pointer to sample grouping table, or NULL-pointer if ungrouped
  CTableAB1GroupingTables: array[0..15] of PDAV1024SingleArray = (
    nil, @CGrouping5Bits, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil);

  // factor for requantization: (real)sample * factor - 1.0 gives requantized sample
  CTableABFactor: array[0..15] of Single = (0, 0.5, 0.25, 0.125, 1/16, 1/32,
    1/64, 1/128, 1/256, 1/512, 1/1024, 1/2048, 1/4096, 1/8192, 1/16384, 1/32768);

  // factor c for requantization from table 3-B.4
  CTableAB1C: array[0..15] of Single = (0, 1.33333333333, 1.14285714286,
    1.06666666666, 1.03225806452, 1.01587301587, 1.00787401575, 1.00392156863,
    1.00195694716, 1.00097751711, 1.00048851979, 1.00024420024, 1.00012208522,
    1.00006103888, 1.00003051851, 1.00001525902);

  // addend CAnnex3B3Table for requantization from table 3-B.4
  CTableAB1D: array[0..15] of Single = (0, 0.5, 0.25, 0.125, 0.0625, 0.03125,
    0.015625, 0.0078125, 0.00390625, 0.001953125, 9.765625E-4, 4.8828125E-4,
    2.4414063E-4, 1.2207031E-4, 6.103516E-5, 3.051758E-5);

  // subbands 3-... tables 3-B.2a and 2b:
  CTableAB234GroupingTables: array[0..15] of PDAV1024SingleArray = (
    nil, @CGrouping5Bits, @CGrouping7Bits, nil, @CGrouping10Bits, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil);

  // subbands 3-10 in tables 3-B.2a and 2b:
  CTableAB2CodeLength: array[0..15] of Cardinal = (
    0, 5, 7, 3, 10, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 16);

  CTableAB2C: array[0..15] of Single = (
    0,    1.33333333333,   1.6,   1.14285714286, 1.77777777777, 1.06666666666,
    1.03225806452, 1.01587301587, 1.00787401575, 1.00392156863, 1.00195694716,
    1.00097751711, 1.00048851979, 1.00024420024, 1.00012208522, 1.00001525902);

  CTableAB2D: array[0..15] of Single = (0, 0.5, 0.5, 0.25, 0.5, 0.125, 0.0625,
    0.03125, 0.015625, 7.8125E-3, 3.90625E-3, 1.95312500E-3, 9.7656250E-4,
    4.8828125E-4, 2.4414063E-4, 3.051758E-5);

  // subbands 11-22 in tables 3-B.2a and 2b:
  CTableAB3CodeLength: array[0..7] of Cardinal = (0, 5, 7, 3, 10, 4, 5, 16);

  CTableAB3Factor: array[0..7] of Single = (
    0.0, 0.5, 0.25, 0.25, 0.125, 0.125, 1/16, 1/32768);

  CTableAB3C: array[0..7] of Single = (0, 1.33333333333, 1.6, 1.14285714286,
    1.77777777777, 1.06666666666, 1.03225806452, 1.00001525902);

  CTableAB3D: array[0..7] of Single = (0, 0.5, 0.5, 0.25, 0.5, 0.125,
    0.0625, 3.051758E-5);

  // subbands 23-... in tables 3-B.2a and 2b:
  CTableAB4CodeLength: array[0..3] of Cardinal = (0, 5, 7, 16);

  CTableAB4Factor: array[0..3] of Single = (0, 0.5, 0.25, 1/32768);

  CTableAB4C: array[0..3] of Single = (0, 1.33333333333, 1.6, 1.00001525902);

  CTableAB4D: array[0..3] of Single = (0, 0.5, 0.5, 0.00003051758);

  // subbands in tables 3-B.2c and 2d:
  CTableCDCodeLength: array[0..15] of Cardinal = (
    0, 5, 7, 10, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);

  CTableCDGroupingTables: array[0..15] of PDAV1024SingleArray = (
    nil, @CGrouping5Bits, @CGrouping7Bits, @CGrouping10Bits, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, nil);

  CTableCDFactor: array[0..15] of Single = (0, 0.5, 0.25, 0.125, 0.125, 1/16,
    1/32, 1/64, 1/128, 1/256, 1/512, 1/1024, 1/2048, 1/4096, 1/8192, 1/16384);

  CTableCDC: array[0..15] of Single = (0, 1.33333333333, 1.6, 1.77777777777,
    1.06666666666, 1.03225806452, 1.01587301587, 1.00787401575, 1.00392156863,
    1.00195694716, 1.00097751711, 1.00048851979, 1.00024420024, 1.00012208522,
    1.00006103888, 1.00003051851);

  CTableCDD: array[0..15] of Single = (0, 0.5, 0.5, 0.5, 0.125, 0.0625,
    0.03125, 0.015625, 0.0078125, 0.00390625, 0.001953125, 0.00097656250,
    0.00048828125, 0.00024414063, 0.00012207031, 6.103516E-5);

  // CScaleFactors for layer I and II, Annex 3-B.1 in ISO/IEC DIS 11172:
  CScaleFactors: array[0..63] of Single = (2, 1.5874010519682,
    1.25992104989487, 1, 0.7937005259841, 0.62996052494744, 0.5,
    0.39685026299205, 0.31498026247372, 0.25, 0.19842513149602,
    0.15749013123686, 0.125, 0.09921256574801, 0.07874506561843, 0.0625,
    0.04960628287401, 0.03937253280921, 0.03125, 0.024803141437,
    0.01968626640461, 0.015625, 1.24015707185E-2, 9.8431332023E-3,
    7.8125E-3, 6.20078535925E-3, 4.92156660115E-3, 3.90625E-3,
    3.10039267963E-3, 2.46078330058E-3, 1.953125E-3, 1.55019633981E-3,
    1.23039165029E-3, 9.765625E-4, 7.7509816991E-4, 6.1519582514E-4,
    4.8828125E-4, 3.8754908495E-4, 3.0759791257E-4, 2.44140625E-4,
    1.9377454248E-4, 1.5379895629E-4, 1.220703125E-4, 9.688727124E-5,
    7.689947814E-5, 6.103515625E-5, 4.844363562E-5, 3.844973907E-5,
    3.051757813E-5, 2.422181781E-5, 1.922486954E-5, 1.525878906E-5,
    1.21109089E-5, 9.61243477E-6, 7.62939453E-6, 6.05545445E-6, 4.80621738E-6,
    3.81469727E-6, 3.02772723E-6, 2.40310869E-6, 1.90734863E-6, 1.51386361E-6,
    1.20155435E-6, 0 (* illegal scalefactor *));

  CBufferIntSize = 433; // max. 1730 bytes per frame: 144 * 384kbit/s / 32000 Hz + 2 Bytes CRC

  CBitMask: array[0..17] of Cardinal = (0, $00000001, $00000003, $00000007,
    $0000000F, $0000001F, $0000003F, $0000007F, $000000FF, $000001FF,
    $000003FF, $000007FF, $00000FFF, $00001FFF, $00003FFF, $00007FFF,
    $0000FFFF, $0001FFFF);

type
  TSyncMode = (smInitialSync, imStrictSync);
  TChannels = (Both, Left, Right, Downmix);
  TVersion         = (MPEG2_LSF, MPEG1);
  TMode            = (Stereo, JointStereo, DualChannel, SingleChannel);
  TSampleFrequency = (FourtyFourPointOne, FourtyEight, ThirtyTwo, Unknown);

const
  CFrequencies: array[TVersion, TSampleFrequency] of Cardinal = (
    (22050, 24000, 16000, 1),
    (44100, 48000, 32000, 1));
  CmsPerFrameArray: array[0..2, TSampleFrequency] of Single = (
    (8.707483,  8.0, 12.0, 0),
    (26.12245, 24.0, 36.0, 0),
    (26.12245, 24.0, 36.0, 0));
  CBitrates: array[TVersion, 0..2, 0..15] of Cardinal = (
    ((0 {free format}, 32000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 144000, 160000, 176000, 192000 ,224000, 256000, 0),
     (0 {free format}, 8000, 16000, 24000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 144000, 160000, 0),
     (0 {free format}, 8000, 16000, 24000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 144000, 160000, 0)),
    ((0 {free format}, 32000, 64000, 96000, 128000, 160000, 192000, 224000, 256000, 288000, 320000, 352000, 384000, 416000, 448000, 0),
     (0 {free format}, 32000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 320000, 384000, 0),
     (0 {free format}, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 320000, 0))
    );

type
  // Class to extract bitstrings from files:
  TBitStream = class
  private
    FStream             : TStream;
    FOwnedStream        : Boolean;
    FBuffer             : array[0..CBufferIntSize - 1] of Cardinal;
    FFrameSize          : Cardinal;  // number of valid bytes in buffer
    FWordPointer        : PCardinal; // position of next unsigned int for get_bits()
    FBitIndex           : Cardinal;  // number (0-31, from MSB to LSB) of next bit for get_bits()
    FSyncWord           : Cardinal;
    FSingleChMode       : Boolean;
    FCurrentFrameNumber : Integer;
    FLastFrameNumber    : Integer;
    FNonSeekable        : Boolean;
  public
    constructor Create(FileName: TFileName); overload;
    constructor Create(Stream: TStream); overload;
    destructor Destroy; override;

    function Restart: Boolean;

    // get next 32 bits from bitstream in an unsigned int,
    // returned value False => end of stream
    function GetHeader(HeaderString: PCardinal; SyncMode: TSyncMode): Boolean;

    // fill buffer with data from bitstream, returned value False => end of stream
    function ReadFrame(ByteSize: Cardinal): Boolean;

    // read bits (1 <= number_of_bits <= 16) from buffer into the lower bits
    // of an unsigned int. The LSB contains the latest read bit of the stream.
    function GetBits(NumberOfBits: Cardinal): Cardinal;

    // read bits (1 <= number_of_bits <= 16) from buffer into the lower bits
    // of a floating point. The LSB contains the latest read bit of the stream.
    function GetBitsFloat(NumberOfBits: Cardinal): Single;

    // Set the word we want to sync the header to, in
    // Big-Endian byte order
    procedure SetSyncWord(SyncWord: Cardinal);

    // Returns the size, in bytes, of the input file.
    function StreamSize: Cardinal;

    // Seeks to frames
    function Seek(Frame: Integer; FrameSize: Integer): Boolean;

    // Seeks frames for 44.1 or 22.05 kHz (padded) files
    function SeekPad(Frame: Integer; FrameSize: Integer; var Header: TObject{THeader}; Offset: PCardinalArray): Boolean;

    property CurrentFrame: Integer read FCurrentFrameNumber;
    property LastFrame: Integer read FLastFrameNumber;
  end;

  // Class for extraction information from a frame header:
  THeader = class
  private
    FLayer                : Cardinal;
    FProtectionBit        : Cardinal;
    FBitrateIndex         : Cardinal;
    FPaddingBit           : Cardinal;
    FModeExtension        : Cardinal;
    FVersion              : TVersion;
    FMode                 : TMode;
    FSampleFrequency      : TSampleFrequency;
    FNumberOfSubbands     : Cardinal;
    FIntensityStereoBound : Cardinal;
    FCopyright            : Boolean;
    FOriginal             : Boolean;
    FInitialSync          : Boolean;
    FCRC                  : TCRC16;
    FOffset               : PCardinalArray;
    FChecksum             : Cardinal;
    FFrameSize            : Cardinal;
    FNumSlots             : Cardinal;
    function GetFrequency: Cardinal;
    function GetChecksums: Boolean;
    function GetChecksumOK: Boolean;
    function GetPadding: Boolean;
  public
    property Version: TVersion read FVersion;
    property Layer: Cardinal read FLayer;
    property BitrateIndex: Cardinal read FBitrateIndex;
    property SampleFrequency: TSampleFrequency read FSampleFrequency;
    property Frequency: Cardinal read GetFrequency;
    property Mode: TMode read FMode;
    property Checksums: Boolean read GetChecksums;
    property Copyright: Boolean read FCopyright;
    property Original: Boolean read FOriginal;
    property ChecksumOK: Boolean read GetChecksumOK; // compares computed checksum with stream checksum
    property Padding: Boolean read GetPadding;
    property Slots: Cardinal read FNumSlots;
    property ModeExtension: Cardinal read FModeExtension;
    property NumberOfSubbands: Cardinal read FNumberOfSubbands; // returns the number of subbands in the current frame
    property IntensityStereoBound: Cardinal read FIntensityStereoBound;
    // (Layer II joint stereo only)
    // returns the number of subbands which are in stereo mode,
    // subbands above that limit are in intensity stereo mode

    constructor Create;
    destructor Destroy; override;

    function ReadHeader(Stream: TBitStream; var CRC: TCRC16): Boolean; // read a 32-bit header from the bitstream
    function Bitrate: Cardinal;
    function CalculateFrameSize: Cardinal;

    // Scrolling stuff
    function StreamSeek(Stream: TBitStream; SeekPos: Cardinal): Boolean;
    function MaxNumberOfFrames(Stream: TBitStream): Integer;
    function MinNumberOfFrames(Stream: TBitStream): Integer;

    function MSPerFrame: Single;  // milliseconds per frame, for time display
    function TotalMS(Stream: TBitStream): Single;
  end;

  TSubBand = class
  public
    procedure ReadAllocation(Stream: TBitStream; Header: THeader; CRC: TCRC16); virtual; abstract;
    procedure ReadScaleFactor(Stream: TBitStream; Header: THeader); virtual; abstract;
    function ReadSampleData(Stream: TBitStream): Boolean; virtual; abstract;
    function PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): Boolean; virtual; abstract;
  end;

  // class for layer I subbands in single channel mode:
  TSubBandLayer1 = class(TSubBand)
  protected
    FSubBandNumber   : Cardinal;
    FSampleNumber    : Cardinal;
    FAllocation      : Cardinal;
    FScaleFactor     : Single;
    FSampleLength    : Cardinal;
    FSample          : Single;
    FFactor, FOffset : Single;
  public
    constructor Create(SubBandNumber: Cardinal); virtual;
    procedure ReadAllocation(Stream: TBitStream; Header: THeader; CRC: TCRC16); override;
    procedure ReadScaleFactor(Stream: TBitStream; Header: THeader); override;
    function ReadSampleData(Stream: TBitStream): Boolean; override;
    function PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): Boolean; override;
  end;

  // class for layer I subbands in joint stereo mode:
  TSubBandLayer1IntensityStereo = class(TSubBandLayer1)
  protected
    FChannel2ScaleFactor: Single;
  public
    procedure ReadScaleFactor(Stream: TBitStream; Header: THeader); override;
    function PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): Boolean; override;
  end;

  // class for layer I subbands in stereo mode:
  TSubBandLayer1Stereo = class(TSubBandLayer1)
  protected
    FChannel2Allocation   : Cardinal;
    FChannel2ScaleFactor  : Single;
    FChannel2SampleLength : Cardinal;
    FChannel2Sample       : Single;
    FChannel2Factor       : Single;
    FChannel2Offset       : Single;
  public
    procedure ReadAllocation(Stream: TBitStream; Header: THeader; CRC: TCRC16); override;
    procedure ReadScaleFactor(Stream: TBitStream; Header: THeader); override;
    function ReadSampleData(Stream: TBitStream): Boolean; override;
    function PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): Boolean; override;
  end;

  // class for layer II subbands in single channel mode:
  TSubBandLayer2 = class(TSubBand)
  protected
    FSubBandNumber : Cardinal;
    FAllocation    : Cardinal;
    FSCFSI         : Cardinal;
    FScaleFactor   : array [0..2] of Single;
    FCodeLength    : Cardinal;
    FGroupingTable : PDAV1024SingleArray;
    FFactor        : Single;
    FGroupNumber   : Cardinal;
    FSampleNumber  : Cardinal;
    FSamples       : array[0..2] of Single;
    FC, FD         : Single;
    function GetAllocationLength(Header: THeader): Cardinal; virtual;
    procedure PrepareSampleReading(Header: THeader; Allocation: Cardinal;
      var GroupingTable: PDAV1024SingleArray; var Factor: Single; var CodeLength: Cardinal;
      var C, D: Single); virtual;
  public
    constructor Create(SubBandNumber: Cardinal); virtual;
    procedure ReadAllocation(Stream: TBitStream; Header: THeader; CRC: TCRC16); override;
    procedure ReadScaleFactorSelection(Stream: TBitStream; CRC: TCRC16); virtual;
    procedure ReadScaleFactor(Stream: TBitStream; Header: THeader); override;
    function ReadSampleData(Stream: TBitStream): boolean; override;
    function PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): boolean; override;
  end;

  // class for layer II subbands in joint stereo mode:
  TSubbandLayer2IntensityStereo = class(TSubbandLayer2)
  protected
    FChannel2SCFSI       : Cardinal;
    FChannel2ScaleFactor : array [0..2] of Single;
  public
    procedure ReadScaleFactorSelection(Stream: TBitStream; CRC: TCRC16); override;
    procedure ReadScaleFactor(Stream: TBitStream; Header: THeader); override;
    function PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): boolean; override;
  end;

  // class for layer II subbands in stereo mode:
  TSubbandLayer2Stereo = class(TSubbandLayer2)
  protected
    FChannel2Allocation    : Cardinal;
    FChannel2SCFSI         : Cardinal;
    FChannel2ScaleFactor   : array [0..2] of Single;
    FChannel2Grouping      : Boolean;
    FChannel2CodeLength    : Cardinal;
    FChannel2GroupingTable : PDAV1024SingleArray;
    FChannel2Factor        : Single;
    FChannel2Samples       : array[0..2] of Single;
    FChannel2C, FChannel2D : Single;
  public
    procedure ReadAllocation(Stream: TBitStream; Header: THeader; CRC: TCRC16); override;
    procedure ReadScaleFactorSelection(Stream: TBitStream; CRC: TCRC16); override;
    procedure ReadScaleFactor(Stream: TBitStream; Header: THeader); override;
    function ReadSampleData(Stream: TBitStream): boolean; override;
    function PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): boolean; override;
  end;

type
  PSArray = ^TSArray;
  TSArray = array[0..SBLIMIT-1, 0..SSLIMIT-1] of Single;

  TLayerIII_Decoder = class
  private
    FRO            : array[0..1] of TSArray;     
    FLR            : array[0..1] of TSArray;     
    FIs1D          : array[0..(SBLIMIT*SSLIMIT)-1] of Integer;     
    FOut1D         : array[0..(SBLIMIT*SSLIMIT)-1] of Single;     
    FPrevBlock     : array[0..1, 0..(SBLIMIT*SSLIMIT)-1] of Single;     
    FK             : array[0..1, 0..(SBLIMIT*SSLIMIT)-1] of Single;     
    FNonZero       : array[0..1] of Integer;     
    FBuffer        : TStereoBuffer;     
    FBitStream     : TBitStream;     
    FHeader        : THeader;
    FFilter        : array [0..1] of TSynthesisFilter;
    FWhichChannels : TChannels;
    FBitReserve    : TBitReserve;
    FSideInfo      : PIIISideInfo;
    FScaleFac      : TIIIScaleFac;
    FMaxGr         : Cardinal;
    FFrameStart    : Integer;
    FPart2Start    : Cardinal;
    FChannels      : Cardinal;
    FFirstChannel  : Cardinal;
    FLastChannel   : Cardinal;
    FSFreq         : Cardinal;
    function GetSideInfo: Boolean;
    procedure GetScaleFactors(ch: Cardinal; gr: Cardinal);
    procedure GetLSFScaleData(ch: Cardinal; gr: Cardinal);
    procedure GetLSFScaleFactors(ch: Cardinal; gr: Cardinal);
    procedure HuffmanDecode(ch: Cardinal; gr: Cardinal);
    procedure IStereoKValues(IsPos: Cardinal; IOType: Cardinal; i: Cardinal);
    procedure DequantizeSample(var xr: TSArray; ch: Cardinal; gr: Cardinal);
    procedure Reorder(xr: PSArray; ch: Cardinal; gr: Cardinal);
    procedure Stereo(gr: Cardinal);
    procedure Antialias(ch: Cardinal; gr: Cardinal);
    procedure Hybrid(ch: Cardinal; gr: Cardinal);
    procedure DoDownmix;
  public
    constructor Create(Stream: TBitStream; Header: THeader; FilterA, FilterB: TSynthesisFilter; Buffer: TStereoBuffer; Which_Ch: TChannels);
    destructor Destroy; override;
    procedure SeekNotify; // Notify decoder that a seek is being made
    procedure Decode; // Decode one frame, filling the buffer with the output samples
  end;

  TMpegAudio = class(TObject)
  private
    FBitStream  : TBitStream;
    FMPEGHeader : THeader;
    FWhichC     : TChannels;
    FFilter     : array [0..1] of TSynthesisFilter;
    FCRC        : TCRC16;
    FBuffer     : TStereoBuffer;
    FLayer      : Cardinal;
    FLayer3     : TLayerIII_Decoder;
    FOPos       : Integer;
    procedure DoDecode;
    procedure NewPCMSample(Sender: TObject; Sample: Single);
  protected
    constructor Create; overload; virtual;
    function GetLength: Integer;
    function GetMode: TMode;
    function GetChannels: TChannels;
    function GetVersion: TVersion;
    function GetLayer: Integer;
    function GetFrequency: Integer;
    function GetBitrate: Integer;
  public
    constructor Create(Filename: TFileName); overload; virtual;
    constructor Create(Stream: TStream); overload; virtual;
    destructor Destroy; override;
    procedure Reset;
    function ReadBuffer(Left, Right: PDAVSingleFixedArray; Size: Integer): Integer;
  published
    property Length: Integer read GetLength;
    property Mode: TMode read GetMode;
    property Channels: TChannels read GetChannels;
    property Version: TVersion read GetVersion;
    property Layer: Integer read GetLayer;
    property Frequency: Integer read GetFrequency;
    property Bitrate: Integer read GetBitrate;
  end;

function SwapInt32(Value: Cardinal): Cardinal;

implementation

uses
  Math, DAV_Huffman, DAV_InvMDCT;

function SwapInt32(Value: Cardinal): Cardinal;
begin
 Result := (Value shl 24) or ((Value shl 8) and $00ff0000) or
           ((Value shr 8) and $0000ff00) or (Value shr 24);
end;


{ TBitStream }

constructor TBitStream.Create(Stream: TStream);
begin
 FStream := Stream;
 Restart;
 FNonSeekable := False;
 FOwnedStream := False;
end;

constructor TBitStream.Create(FileName: TFileName);
begin
 Create(TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone));
 FOwnedStream := True;
end;

destructor TBitStream.Destroy;
begin
 if FOwnedStream
  then FreeAndNil(FStream);
 inherited Destroy;
end;

function TBitStream.StreamSize: Cardinal;
begin
 Result := FStream.Size;
end;

function TBitStream.GetBits(NumberOfBits: Cardinal): Cardinal;
var
  ReturnValue: Cardinal;
  Sum: Cardinal;
begin
 Sum := FBitIndex + NumberOfBits;

 if (sum <= 32) then
  begin
   // all bits contained in *wordpointer
   Result := (FWordPointer^ shr (32 - sum)) and CBitMask[NumberOfBits];
   Inc(FBitIndex, NumberOfBits);
   if (FBitIndex = 32) then
    begin
     FBitIndex := 0;
     Inc(FWordPointer);
    end;
    exit;
  end;

 {$IFDEF LITTLE_ENDIAN}
 PWord(@PByteArray(@ReturnValue)[2])^ := PWord(FWordPointer)^;
 Inc(FWordPointer);
 PWord(@ReturnValue)^ := PWord(@PByteArray(FWordPointer)[2])^;
 {$ELSE}
 PWord(@ReturnValue)^ := PWord(@PByteArray(FWordPointer)[2])^;
 Inc(FWordPointer);
 PWord(@PByteArray(@ReturnValue)[2])^ := PWord(FWordPointer)^;
 {$ENDIF}

 ReturnValue := ReturnValue shr (48 - Sum);
 Result := ReturnValue and CBitMask[NumberOfBits];
 FBitIndex := Sum - 32;
end;

function TBitStream.GetBitsFloat(NumberOfBits: Cardinal): Single;
begin
 PCardinal(@Result)^ := GetBits(NumberOfBits);
end;

function TBitStream.GetHeader(HeaderString: PCardinal;
  SyncMode: TSyncMode): Boolean;
var
  Sync: Boolean;
  NumRead: Integer;
begin
 repeat
  // Read 4 bytes from the file, placing the number of bytes actually read in numread
  NumRead := FStream.Read(HeaderString^, 4);
  Result := (NumRead = 4);
  if (not Result) then exit;

  if (SyncMode = smInitialSync)
   then Sync := ((HeaderString^ and $0000F0FF) = $0000F0FF)
   else Sync := ((HeaderString^ and $000CF8FF) = FSyncWord) and
                (((HeaderString^ and $C0000000) = $C0000000) = FSingleChMode);

//  if ((HeaderString^ and $0000FFFF) = $0000FFFF) then
//    Sync := false;

  if (not Sync) then FStream.Seek(-3, soFromCurrent); // rewind 3 bytes in the file so we can try to sync again, if successful set result to TRUE
 until (Sync) or (not Result);

 if (not Result) then exit;

 {$IFDEF LITTLE_ENDIAN}
 HeaderString^ := SwapInt32(HeaderString^);
 {$ENDIF}

 Inc(FCurrentFrameNumber);

 {$IFDEF SEEK_STOP}
 if (FLastFrameNumber < FCurrentFrameNumber) then FLastFrameNumber := FCurrentFrameNumber;
 {$ENDIF}

 Result := True;
end;

function TBitStream.ReadFrame(ByteSize: Cardinal): Boolean;
var
  NumRead: Integer;
  {$IFDEF LITTLE_ENDIAN}
  WordP: PCardinal;
  {$ENDIF}
begin
 // read bytesize bytes from the file, placing the number of bytes
 // actually read in numread and setting result to TRUE if
 // successful
 NumRead := FStream.Read(FBuffer, ByteSize);

 FWordPointer := @FBuffer;
 FBitIndex := 0;
 FFrameSize := ByteSize;

 {$IFDEF LITTLE_ENDIAN}
 WordP := @FBuffer[(ByteSize-1) shr 2];
 while (Cardinal(WordP) >= Cardinal(@FBuffer)) do
  begin
   WordP^ := SwapInt32(WordP^);
   dec(WordP);
  end;
 {$ENDIF}

 Result := Cardinal(NumRead) = FFrameSize;
end;

function TBitStream.Restart: Boolean;
begin
 FStream.Seek(0, soFromBeginning);
 FWordPointer := @FBuffer;
 FBitIndex := 0;

 // Seeking variables
 FCurrentFrameNumber := -1;
 FLastFrameNumber := -1;
 Result := true;
end;

function TBitStream.Seek(Frame, FrameSize: Integer): Boolean;
begin
 FCurrentFrameNumber := Frame - 1;
 if FNonSeekable then
  begin
   Result := False;
   exit;
  end;

 FStream.Seek(Frame * (FrameSize + 4), soFromBeginning);
 Result := True;
end;

function TBitStream.SeekPad(Frame, FrameSize: Integer;
  var Header: TObject; Offset: PCardinalArray): Boolean;
var
  CRC            : TCRC16;
  TotalFrameSize : Integer;
  Diff           : Integer;
begin
 // base_frame_size is the frame size _without_ padding.
 if FNonSeekable then
  begin
   Result := False;
   exit;
  end;

 CRC := nil;

 TotalFrameSize := FrameSize + 4;

 if (FLastFrameNumber < Frame) then
  begin
   if (FLastFrameNumber >= 0)
    then Diff := Offset[FLastFrameNumber]
    else Diff := 0;

   // set the file pointer to ((last_frame_number+1) * total_frame_size)
   // bytes after the beginning of the file
   FStream.Seek((FLastFrameNumber + 1) * TotalFrameSize + Diff, soFromBeginning);
   FCurrentFrameNumber := FLastFrameNumber;

   repeat
    if (not THeader(Header).ReadHeader(Self, CRC)) then
     begin
      Result := False;
      exit;
     end;
   until (FLastFrameNumber >= Frame);

   Result := True;
  end
 else
  begin
   if (Frame > 0)
    then Diff := Offset[Frame - 1]
    else Diff := 0;

   // set the file pointer to (frame * total_frame_size  + diff) bytes
   // after the beginning of the file
   FStream.Seek(Frame * TotalFrameSize + Diff, soFrombeginning);
   FCurrentFrameNumber := Frame - 1;
   Result := THeader(Header).ReadHeader(Self, CRC);
  end;

 if assigned(CRC) then FreeAndNil(CRC);
end;

procedure TBitStream.SetSyncWord(SyncWord: Cardinal);
begin
 {$IFDEF LITTLE_ENDIAN}
 FSyncWord := SwapInt32(Syncword and $FFFFFF3F);
 {$ELSE}
 FSyncWord := SyncWord and $FFFFFF3F;
 {$ENDIF}

 FSingleChMode := ((SyncWord and $000000C0) = $000000C0);
end;


{ THeader }

constructor THeader.Create;
begin
 FFrameSize := 0;
 FNumSlots := 0;
 FCRC := nil;
 FOffset := nil;
 FInitialSync := False;
end;

destructor THeader.Destroy;
begin
 if assigned(FOffset) then Dispose(FOffset);
 inherited;
end;

function THeader.Bitrate: Cardinal;
begin
  Result := CBitrates[FVersion, FLayer - 1, FBitrateIndex];
end;

// calculates framesize in bytes excluding header size
function THeader.CalculateFrameSize: Cardinal;
var
  Val1, Val2: Cardinal;
begin
 if (FLayer = 1) then
  begin
   FFramesize := (12 * CBitrates[FVersion, 0, FBitrateIndex]) div CFrequencies[FVersion, FSampleFrequency];
   if (FPaddingBit <> 0) then Inc(FFrameSize);
   FFrameSize := FFrameSize shl 2;  // one slot is 4 bytes long
   FNumSlots := 0;
  end
 else
  begin
   FFrameSize := (144 * CBitrates[FVersion, FLayer - 1, FBitrateIndex]) div CFrequencies[FVersion, FSampleFrequency];
   if (FVersion = MPEG2_LSF) then FFrameSize := FFrameSize shr 1;
   if (FPaddingBit <> 0) then Inc(FFrameSize);

   // Layer III slots
   if (FLayer = 3) then
    begin
     if (FVersion = MPEG1) then
      begin
       if (FMode = SingleChannel)
        then Val1 := 17
        else Val1 := 32;
       if (FProtectionBit <> 0)
        then Val2 := 0
        else Val2 := 2;
       FNumSlots := FFramesize - Val1 - Val2 - 4;                      // header size
      end
     else
      begin  // MPEG-2 LSF
       if (FMode = SingleChannel)
        then Val1 := 9
        else Val1 := 17;
       if (FProtectionBit <> 0)
        then Val2 := 0
        else Val2 := 2;
       FNumSlots := FFramesize - Val1 - Val2 - 4;                      // header size
      end;
    end
   else FNumSlots := 0;
  end;

 dec(FFrameSize, 4);  // subtract header size
 Result := FFrameSize;
end;

function THeader.GetChecksumOK: Boolean;
begin
 Result := (FChecksum = FCRC.Checksum);
end;

function THeader.GetChecksums: Boolean;
begin
 Result := (FProtectionBit = 0);
end;

function THeader.GetFrequency: Cardinal;
begin
 Result := CFrequencies[FVersion, FSampleFrequency];
end;

function THeader.GetPadding: Boolean;
begin
 Result := (FPaddingBit <> 0);
end;

// Returns the maximum number of frames in the stream
function THeader.MaxNumberOfFrames(Stream: TBitStream): Integer;
begin
 Result := Stream.StreamSize div (FFrameSize + 4 - FPaddingBit);
end;

// Returns the minimum number of frames in the stream
function THeader.MinNumberOfFrames(Stream: TBitStream): Integer;
begin
 Result := Stream.StreamSize div (FFrameSize + 5 - FPaddingBit);
end;

function THeader.MSPerFrame: Single;
begin
 Result := CmsPerFrameArray[FLayer - 1, FSampleFrequency];
end;

function THeader.ReadHeader(Stream: TBitStream; var CRC: TCRC16): Boolean;
var
  HeaderString   : Cardinal;
  ChannelBitrate : Cardinal;
  Max, Cf, Lf    : Integer;
begin
 Result := True;
 try
  if (not FInitialSync) then
   begin
    if (not Stream.GetHeader(@HeaderString, smInitialSync))
     then raise Exception.Create('Header not found');
    FVersion := TVersion((HeaderString shr 19) and 1);
    FSampleFrequency := TSampleFrequency((HeaderString shr 10) and 3);
    if (FSampleFrequency = Unknown)
     then raise Exception.Create('Header not supported');
    Stream.SetSyncWord(HeaderString and $FFF80CC0);
    FInitialSync := True;
   end else
  if (not Stream.GetHeader(@HeaderString, imStrictSync))
   then raise Exception.Create('Header not found');

  FLayer         := 4 - (HeaderString shr 17) and 3;
  FProtectionBit := (HeaderString shr 16) and 1;
  FBitrateIndex  := (HeaderString shr 12) and $F;
  FPaddingBit    := (HeaderString shr 9) and 1;
  FMode          := TMode((HeaderString shr 6) and 3);
  FModeExtension := (HeaderString shr 4) and 3;

  if (FMode = JointStereo)
   then FIntensityStereoBound := (FModeExtension shl 2) + 4
   else FIntensityStereoBound := 0;  // should never be used

  FCopyright := ((HeaderString shr 3) and 1 <> 0);
  FOriginal := ((HeaderString shr 2) and 1 <> 0);

  // calculate number of subbands:
  if (FLayer = 1)
   then FNumberOfSubbands := 32
   else
    begin
     ChannelBitrate := FBitrateIndex;
     // calculate bitrate per channel:
     if (FMode <> SingleChannel) then
      if (ChannelBitrate = 4)
       then ChannelBitrate := 1
       else dec(ChannelBitrate, 4);
     if ((ChannelBitrate = 1) or (ChannelBitrate = 2)) then
      begin
       if (FSampleFrequency = ThirtyTwo)
        then FNumberOfSubbands := 12
        else FNumberOfSubbands := 8;
      end
     else
      if ((FSampleFrequency = FourtyEight) or ((ChannelBitrate >= 3) and (ChannelBitrate <= 5)))
       then FNumberOfSubbands := 27
       else FNumberOfSubbands := 30;
    end;

  if (FIntensityStereoBound > FNumberOfSubbands)
   then FIntensityStereoBound := FNumberOfSubbands;

  CalculateFrameSize; // calculate framesize and nSlots

  // read framedata:
  if (not Stream.ReadFrame(FFrameSize))
   then raise Exception.Create('Frame read error!');

  if (FProtectionBit = 0) then
   begin
    // frame contains a crc checksum
    FChecksum := Stream.GetBits(16);
    if (FCRC = nil) then FCRC := TCRC16.Create;
    FCRC.AddBits(HeaderString, 16);
    CRC := FCRC;
   end
  else CRC := nil;

  {$IFDEF SEEK_STOP}
  if (FSampleFrequency = FourtyFourPointOne) then
   begin
    if (FOffset = nil) then
     begin
      Max := MaxNumberOfFrames(Stream);
      GetMem(FOffset, Max * SizeOf(Cardinal));
      FillChar(FOffset^, Max * SizeOf(Cardinal), 0);
     end;
    Cf := Stream.CurrentFrame;
    Lf := Stream.LastFrame;
    if ((Cf > 0) and (Cf = Lf))
     then FOffset[Cf] := FOffset[Cf - 1] + FPaddingBit
     else FOffset[0] := FPaddingBit;
   end;
  {$ENDIF}

 except
  Result := False;
 end;
end;

// Stream searching routines
function THeader.StreamSeek(Stream: TBitStream; SeekPos: Cardinal): Boolean;
begin
 if (FSampleFrequency = FourtyFourPointOne)
  then Result := Stream.SeekPad(SeekPos, FFrameSize - FPaddingBit, TObject(Self), FOffset)
  else Result := Stream.Seek(SeekPos, FFrameSize);
end;

function THeader.TotalMS(Stream: TBitStream): Single;
begin
 Result := MaxNumberOfFrames(Stream) * MSPerFrame;
end;


{ TSubBandLayer1 }

constructor TSubBandLayer1.Create(SubBandNumber: Cardinal);
begin
 FSubBandNumber := SubBandNumber;
 FSampleNumber := 0;
end;

function TSubBandLayer1.PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): Boolean;
var ScaledSample: Single;
begin
 if (FAllocation <> 0) and (Channels <> Right) then
  begin
   ScaledSample := (FSample * FFactor + FOffset) * FScalefactor;
   Filter1.InputSample(ScaledSample, FSubBandNumber);
  end;
 Result := True;
end;

procedure TSubBandLayer1.ReadAllocation(Stream: TBitStream;
  Header: THeader; CRC: TCRC16);
begin
 FAllocation := Stream.GetBits(4);
 if (FAllocation = 15) then ; //  cerr << "WARNING: stream contains an illegal allocation!\n"; // MPEG-stream is corrupted!
 if (CRC <> nil) then CRC.AddBits(FAllocation, 4);
 if (FAllocation <> 0) then
  begin
   FSampleLength := FAllocation + 1;
   FFactor := CTableFactor[FAllocation];
   FOffset := CTableOffset[FAllocation];
  end;
end;

function TSubBandLayer1.ReadSampleData(Stream: TBitStream): Boolean;
begin
 if (FAllocation <> 0) then FSample := Stream.GetBitsFloat(FSampleLength);

 Inc(FSampleNumber);
 if (FSampleNumber = 12) then
  begin
   FSampleNumber := 0;
   Result := True;
  end
 else Result := False;
end;

procedure TSubBandLayer1.ReadScaleFactor(Stream: TBitStream; Header: THeader);
begin
 if (FAllocation <> 0) then FScalefactor := CScaleFactors[Stream.GetBits(6)];
end;

{ TSubBandLayer1IntensityStereo }

function TSubBandLayer1IntensityStereo.PutNextSample(Channels: TChannels;
  Filter1, Filter2: TSynthesisFilter): Boolean;
var Sample1, Sample2: Single;
begin
 if (FAllocation <> 0) then
  begin
  FSample := FSample * FFactor + FOffset;  // requantization
  if (Channels = Both) then
   begin
    Sample1 := FSample * FScalefactor;
    Sample2 := FSample * FChannel2ScaleFactor;
    Filter1.InputSample(Sample1, FSubBandNumber);
    Filter2.InputSample(Sample2, FSubBandNumber);
   end
  else if (Channels = Left) then
   begin
    Sample1 := FSample * FScaleFactor;
    Filter1.InputSample(Sample1, FSubBandNumber);
   end
  else
   begin
    Sample2 := FSample * FChannel2ScaleFactor;
    Filter2.InputSample(Sample2, FSubBandNumber);
   end;
  end;
 Result := True;
end;

procedure TSubBandLayer1IntensityStereo.ReadScaleFactor(Stream: TBitStream;
  Header: THeader);
begin
 if (FAllocation <> 0) then
  begin
   FScaleFactor := CScaleFactors[Stream.GetBits(6)];
   FChannel2ScaleFactor := CScaleFactors[Stream.GetBits(6)];
  end;
end;

{ TSubBandLayer1Stereo }

function TSubBandLayer1Stereo.PutNextSample(Channels: TChannels; Filter1,
  Filter2: TSynthesisFilter): Boolean;
var Sample2: Single;
begin
 inherited PutNextSample(Channels, Filter1, Filter2);
 if (FChannel2Allocation <> 0) and (Channels <> Left) then
  begin
   Sample2 := (FChannel2Sample * FChannel2Factor + FChannel2Offset) * FChannel2ScaleFactor;
   if (Channels = Both)
    then Filter2.InputSample(Sample2, FSubBandNumber)
    else Filter1.InputSample(Sample2, FSubBandNumber);
  end;
 Result := True;
end;

procedure TSubBandLayer1Stereo.ReadAllocation(Stream: TBitStream; Header: THeader; CRC: TCRC16);
begin
 FAllocation := Stream.GetBits(4);
 FChannel2Allocation := Stream.GetBits(4);
 if (CRC <> nil) then
  begin
   CRC.AddBits(FAllocation, 4);
   CRC.AddBits(FChannel2Allocation, 4);
  end;
 if (FAllocation <> 0) then
  begin
   FSamplelength := FAllocation + 1;
   FFactor := CTableFactor[FAllocation];
   FOffset := CTableOffset[FAllocation];
  end;

 if (FChannel2Allocation <> 0) then
  begin
   FChannel2SampleLength := FChannel2Allocation + 1;
   FChannel2Factor := CTableFactor[FChannel2Allocation];
   FChannel2Offset := CTableOffset[FChannel2Allocation];
  end;
end;

function TSubBandLayer1Stereo.ReadSampleData(Stream: TBitStream): Boolean;
begin
 Result := inherited ReadSampleData(Stream);
 if (FChannel2Allocation <> 0)
  then FChannel2Sample := Stream.GetBitsFloat(FChannel2SampleLength);
end;

procedure TSubBandLayer1Stereo.ReadScaleFactor(Stream: TBitStream;
  Header: THeader);
begin
 if (FAllocation <> 0) then FScaleFactor := CScaleFactors[Stream.GetBits(6)];
 if (FChannel2Allocation <> 0) then FChannel2ScaleFactor := CScaleFactors[Stream.GetBits(6)];
end;

constructor TSubBandLayer2.Create(SubBandNumber: Cardinal);
begin
 FSubBandNumber := SubBandNumber;
 FGroupNumber := 0;
 FSampleNumber := 0;
end;

function TSubBandLayer2.GetAllocationLength(Header: THeader): Cardinal;
var ChannelBitrate: Cardinal;
begin
 if (Header.Version = MPEG1) then
  begin
   ChannelBitrate := Header.BitrateIndex;

   // calculate bitrate per channel:
   if (Header.Mode <> SingleChannel) then
    if (ChannelBitrate = 4)
     then ChannelBitrate := 1
     else dec(ChannelBitrate, 4);

    if (ChannelBitrate = 1) or (ChannelBitrate = 2) then
     begin // table 3-B.2c or 3-B.2d
      if (FSubBandNumber <= 1) then Result := 4 else Result := 3;
      exit;
     end
    else
     begin
      // tables 3-B.2a or 3-B.2b
      if (FSubBandNumber <= 10) then Result := 4
      else if (FSubBandNumber <= 22) then Result := 3
      else Result := 2;
      exit;
    end;
  end
 else
  begin  // MPEG-2 LSF -- Jeff
   // table B.1 of ISO/IEC 13818-3
   if (FSubBandNumber <= 3) then Result := 4
    else if (FSubBandNumber <= 10) then Result := 3
    else Result := 2;
    exit;
  end;
end;

procedure TSubBandLayer2.PrepareSampleReading(Header: THeader; Allocation: Cardinal;
  var GroupingTable: PDAV1024SingleArray; var Factor: Single; var CodeLength: Cardinal; var C,
  D: Single);
var ChannelBitrate: Cardinal;
begin
  ChannelBitrate := Header.BitrateIndex;
  // calculate bitrate per channel:
  if (Header.Mode <> SingleChannel) then
   if (ChannelBitrate = 4)
    then ChannelBitrate := 1
    else dec(ChannelBitrate, 4);

  if (ChannelBitrate = 1) or (ChannelBitrate = 2) then
   begin // table 3-B.2c or 3-B.2d
    GroupingTable := CTableCDGroupingTables[Allocation];
    Factor := CTableCDFactor[Allocation];
    CodeLength := CTableCDCodeLength[Allocation];
    C := CTableCDC[Allocation];
    D := CTableCDD[Allocation];
  end
 else
  begin // tables 3-B.2a or 3-B.2b
   if (FSubBandNumber <= 2) then
    begin
     Groupingtable := CTableAB1GroupingTables[Allocation];
     Factor := CTableABFactor[Allocation];
     CodeLength := CTableAB1CodeLength[Allocation];
     C := CTableAB1C[Allocation];
     D := CTableAB1D[Allocation];
    end
   else
    begin
     GroupingTable := CTableAB234GroupingTables[Allocation];
     if (FSubBandNumber <= 10) then
      begin
       Factor := CTableABFactor[Allocation];
       CodeLength := CTableAB2CodeLength[Allocation];
       C := CTableAB2C[Allocation];
       D := CTableAB2D[Allocation];
      end
     else if (FSubBandNumber <= 22) then
      begin
       Factor := CTableAB3Factor[Allocation];
       CodeLength := CTableAB3CodeLength[Allocation];
       C := CTableAB3C[Allocation];
       D := CTableAB3D[Allocation];
      end
     else
      begin
       Factor := CTableAB4Factor[Allocation];
       CodeLength := CTableAB4CodeLength[Allocation];
       C := CTableAB4C[Allocation];
       D := CTableAB4D[Allocation];
      end;
    end;
  end;
end;

function TSubBandLayer2.PutNextSample(Channels: TChannels; Filter1,
  Filter2: TSynthesisFilter): boolean;
var Sample: Single;
begin
 if (FAllocation <> 0) and (Channels <> Right) then
  begin
   Sample := FSamples[FSampleNumber];
   if (FGroupingTable = nil) then Sample := (Sample + FD) * FC;
   if (FGroupNumber <= 4) then Sample := Sample * FScaleFactor[0]
   else if (FGroupNumber <= 8) then Sample := Sample * FScaleFactor[1]
   else Sample := Sample * FScaleFactor[2];
   Filter1.InputSample(Sample, FSubBandNumber);
  end;
 Inc(FSampleNumber);
 Result:=(FSampleNumber=3);
end;

procedure TSubBandLayer2.ReadAllocation(Stream: TBitStream; Header: THeader; CRC: TCRC16);
var Length: Cardinal;
begin
 Length := GetAllocationLength(Header);
 FAllocation := Stream.GetBits(Length);
 if (CRC <> nil) then CRC.AddBits(FAllocation, Length);
end;

function TSubBandLayer2.ReadSampleData(Stream: TBitStream): boolean;
var SampleCode: Cardinal;
begin
 if (FAllocation <> 0) then
  if (FGroupingTable <> nil) then
   begin
    SampleCode := Stream.GetBits(FCodeLength);
    // create requantized samples:
    Inc(SampleCode, SampleCode shl 1);
    FSamples[0] := FGroupingTable[SampleCode];
    FSamples[1] := FGroupingTable[SampleCode+1];
    FSamples[2] := FGroupingTable[SampleCode+2];
   end
  else
   begin
    FSamples[0] := Stream.GetBits(FCodeLength) * FFactor - 1.0;
    FSamples[1] := Stream.GetBits(FCodeLength) * FFactor - 1.0;
    FSamples[2] := Stream.GetBits(FCodeLength) * FFactor - 1.0;
   end;

 FSampleNumber := 0;
 Inc(FGroupNumber);
 Result:=(FGroupnumber=12);
end;

procedure TSubBandLayer2.ReadScaleFactor(Stream: TBitStream;
  Header: THeader);
begin
 if (FAllocation <> 0) then
  begin
   case FSCFSI of
    0: begin
        FScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FScaleFactor[1] := CScaleFactors[Stream.GetBits(6)];
        FScaleFactor[2] := CScaleFactors[Stream.GetBits(6)];
       end;

    1: begin
        FScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FScaleFactor[1] := FScaleFactor[0];
        FScaleFactor[2] := CScaleFactors[Stream.GetBits(6)];
       end;

    2: begin
        FScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FScaleFactor[1] := FScaleFactor[0];
        FScaleFactor[2] := FScaleFactor[0];
       end;

    3: begin
        FScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FScaleFactor[1] := CScaleFactors[Stream.GetBits(6)];
        FScaleFactor[2] := FScaleFactor[1];
       end;
   end;
   PrepareSampleReading(Header, FAllocation, FGroupingtable, FFactor, FCodeLength, FC, FD);
  end;
end;

procedure TSubBandLayer2.ReadScaleFactorSelection(Stream: TBitStream; CRC: TCRC16);
begin
  if (FAllocation <> 0) then
   begin
    FSCFSI := Stream.GetBits(2);
    if (CRC <> nil) then CRC.AddBits(FSCFSI, 2);
  end;
end;

{ TSubbandLayer2IntensityStereo }

function TSubbandLayer2IntensityStereo.PutNextSample(Channels: TChannels;
  Filter1, Filter2: TSynthesisFilter): boolean;
var Sample, Sample2: Single;
begin
 if (FAllocation <> 0) then
  begin
   Sample := FSamples[FSampleNumber];
   if (FGroupingTable = nil) then Sample := (Sample + FD) * FC;
   if (Channels = Both) then
    begin
     Sample2 := Sample;
     if (FGroupNumber <= 4) then
      begin
       Sample := Sample * FScaleFactor[0];
       Sample2 := Sample2 * FChannel2ScaleFactor[0];
      end
     else if (FGroupNumber <= 8) then
      begin
       Sample := Sample * FScaleFactor[1];
       Sample2 := Sample2 * FChannel2ScaleFactor[1];
      end
     else
      begin
       Sample := Sample * FScaleFactor[2];
       Sample2 := Sample2 * FChannel2ScaleFactor[2];
      end;
     Filter1.InputSample(Sample, FSubBandNumber);
     Filter2.InputSample(Sample2, FSubBandNumber);
    end
   else if (Channels = Left) then
    begin
     if (FGroupNumber <= 4) then  Sample := Sample * FScaleFactor[0]
     else if (FGroupNumber <= 8) then Sample := Sample * FScaleFactor[1]
     else Sample := Sample * FScaleFactor[2];
     Filter1.InputSample(Sample, FSubBandNumber);
    end
   else
    begin
     if (FGroupNumber <= 4) then Sample := Sample * FChannel2ScaleFactor[0]
     else if (FGroupNumber <= 8) then Sample := Sample * FChannel2ScaleFactor[1]
     else Sample := Sample * FChannel2ScaleFactor[2];
     Filter1.InputSample(Sample, FSubBandNumber);
    end;
  end;
 Inc(FSampleNumber);
 Result:=(FSampleNumber = 3);
end;

procedure TSubbandLayer2IntensityStereo.ReadScaleFactor(Stream: TBitStream; Header: THeader);
begin
 if (FAllocation <> 0) then
  begin
   inherited ReadScaleFactor(Stream, Header);
   case FChannel2SCFSI of
    0: begin
        FChannel2ScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[1] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[2] := CScaleFactors[Stream.GetBits(6)];
       end;

    1: begin
        FChannel2ScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[1] := FChannel2ScaleFactor[0];
        FChannel2ScaleFactor[2] := CScaleFactors[Stream.GetBits(6)];
       end;

    2: begin
        FChannel2ScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[1] := FChannel2ScaleFactor[0];
        FChannel2ScaleFactor[2] := FChannel2ScaleFactor[0];
       end;

    3: begin
        FChannel2ScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[1] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[2] := FChannel2ScaleFactor[1];
       end;
   end;
  end;
end;

procedure TSubbandLayer2IntensityStereo.ReadScaleFactorSelection(Stream: TBitStream; CRC: TCRC16);
begin
 if (FAllocation <> 0) then
  begin
   FSCFSI := Stream.GetBits(2);
   FChannel2SCFSI := Stream.GetBits(2);
   if (CRC <> nil) then
    begin
     CRC.AddBits(FSCFSI, 2);
     CRC.AddBits(FChannel2SCFSI, 2);
    end;
  end;
end;

{ TSubbandLayer2Stereo }

function TSubbandLayer2Stereo.PutNextSample(Channels: TChannels; Filter1,
  Filter2: TSynthesisFilter): boolean;
var Sample: Single;
begin
 Result := inherited PutNextSample(Channels, Filter1, Filter2);

 if (FChannel2Allocation <> 0) and (Channels <> Left) then
  begin
   Sample := FChannel2Samples[FSampleNumber - 1];
   if (FChannel2GroupingTable = nil) then Sample := (Sample + FChannel2D) * FChannel2C;
   if (FGroupNumber <= 4) then Sample := Sample * FChannel2ScaleFactor[0]
   else if (FGroupNumber <= 8) then Sample := Sample * FChannel2ScaleFactor[1]
   else Sample := Sample * FChannel2ScaleFactor[2];
   if (Channels = Both)
    then Filter2.InputSample(Sample, FSubBandNumber)
    else Filter1.InputSample(Sample, FSubBandNumber);
  end;
end;

procedure TSubbandLayer2Stereo.ReadAllocation(Stream: TBitStream; Header: THeader; CRC: TCRC16);
var Length: Cardinal;
begin
  Length := GetAllocationLength(Header);
  FAllocation := Stream.GetBits(Length);
  FChannel2Allocation := Stream.GetBits(Length);
  if (CRC <> nil) then
   begin
    CRC.AddBits(FAllocation, Length);
    CRC.AddBits(FChannel2Allocation, Length);
   end;
end;

function TSubbandLayer2Stereo.ReadSampleData(Stream: TBitStream): boolean;
var SampleCode: Cardinal;
begin
  Result := inherited ReadSampleData(Stream);
  if (FChannel2Allocation <> 0) then
   if (FChannel2GroupingTable <> nil) then
    begin
     SampleCode := Stream.GetBits(FChannel2CodeLength);
     Inc(SampleCode, SampleCode shl 1); // create requantized samples:
     FChannel2Samples[0] := FChannel2GroupingTable[SampleCode];
     FChannel2Samples[1] := FChannel2GroupingTable[SampleCode+1];
     FChannel2Samples[2] := FChannel2GroupingTable[SampleCode+2];
    end
   else
    begin
     FChannel2Samples[0] := Stream.GetBits(FChannel2CodeLength) * FChannel2Factor - 1.0;
     FChannel2Samples[1] := Stream.GetBits(FChannel2CodeLength) * FChannel2Factor - 1.0;
     FChannel2Samples[2] := Stream.GetBits(FChannel2CodeLength) * FChannel2Factor - 1.0;
    end;
end;

procedure TSubbandLayer2Stereo.ReadScaleFactor(Stream: TBitStream; Header: THeader);
begin
 inherited ReadScaleFactor(Stream, Header);
 if (FChannel2Allocation <> 0) then
  begin
   case FChannel2SCFSI of
    0: begin
        FChannel2ScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[1] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[2] := CScaleFactors[Stream.GetBits(6)];
       end;

    1: begin
        FChannel2ScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[1] := FChannel2ScaleFactor[0];
        FChannel2ScaleFactor[2] := CScaleFactors[Stream.GetBits(6)];
       end;

    2: begin
        FChannel2ScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[1] := FChannel2ScaleFactor[0];
        FChannel2ScaleFactor[2] := FChannel2ScaleFactor[0];
       end;

    3: begin
        FChannel2ScaleFactor[0] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[1] := CScaleFactors[Stream.GetBits(6)];
        FChannel2ScaleFactor[2] := FChannel2ScaleFactor[1];
       end;
   end;
   PrepareSampleReading(Header, FChannel2Allocation, FChannel2GroupingTable, FChannel2Factor, FChannel2CodeLength, FChannel2C, FChannel2D);
  end;
end;

procedure TSubbandLayer2Stereo.ReadScaleFactorSelection(Stream: TBitStream;
  CRC: TCRC16);
begin
 if (FAllocation <> 0) then
  begin
   FSCFSI := Stream.GetBits(2);
   if (CRC <> nil) then CRC.AddBits(FSCFSI, 2);
  end;
 if (FChannel2Allocation <> 0) then
  begin
   FChannel2SCFSI := Stream.GetBits(2);
   if (CRC <> nil) then CRC.AddBits(FChannel2SCFSI, 2);
  end;
end;

var
  GScaleFacBuffer: array[0..53] of Cardinal;

{ TLayerIII_Decoder }

constructor TLayerIII_Decoder.Create(Stream: TBitStream; Header: THeader;
  FilterA, FilterB: TSynthesisFilter; Buffer: TStereoBuffer; Which_Ch: TChannels);
begin
 FBitStream := Stream;
 FHeader := Header;
 FFilter[0] := FilterA;
 FFilter[1] := FilterB;
 fBuffer := Buffer;
 FWhichChannels := Which_Ch;
 FFrameStart := 0;
 if (FHeader.Mode = SingleChannel)
  then FChannels := 1
  else FChannels := 2;
 if (FHeader.Version = MPEG1)
  then FMaxGr := 2
  else FMaxGr := 1;
 FSFreq := Cardinal(FHeader.SampleFrequency);//FHeader.Frequency;
 if (FHeader.Version = MPEG1)
  then FSFreq := FSFreq + 3;
 if (FChannels = 2) then
  begin
   case FWhichChannels of
    Left, Downmix : begin FFirstChannel := 0; FLastChannel := 0; end;
            Right : begin FFirstChannel := 1; FLastChannel := 1; end;
             Both : begin FFirstChannel := 0; FLastChannel := 1; end;
    else            begin FFirstChannel := 0; FLastChannel := 1; end;
   end;
  end
 else
  begin
   FFirstChannel := 0;
   FLastChannel := 0;
  end;

 FillChar(FPrevBlock, Sizeof(FPrevBlock), 0);
 FNonZero[0] := 576;
 FNonZero[1] := 576;
 FBitReserve := TBitReserve.Create;
 FSideInfo := AllocMem(Sizeof(TIIISideInfo));
end;

destructor TLayerIII_Decoder.Destroy;
begin
 FreeAndNil(FBitReserve);
 if assigned(FSideInfo)
  then Dispose(FSideInfo);
 inherited Destroy;
end;

procedure TLayerIII_Decoder.Antialias(ch, gr: Cardinal);
var
  Ss, Sb18, Ssb18lim: Cardinal;
  GrInfo: PGRInfo;
  bu, bd: Single;
  SrcIdx: array [0..1] of Integer;
begin
 GrInfo := @FSideInfo.ch[ch].gr[gr];

 // 31 alias-reduction operations between each pair of sub-bands
 // with 8 butterflies between each pair
 if (GrInfo.WindowSwitchingFlag <> 0) and (GrInfo.BlockType = 2) and (GrInfo.MixedBlockFlag = 0) then Exit;

 if (GrInfo.WindowSwitchingFlag <> 0) and (GrInfo.MixedBlockFlag <> 0) and (GrInfo.BlockType = 2)
  then Ssb18lim := 18
  else Ssb18lim := 558;

 Sb18 := 0;
 while (Sb18 < Ssb18lim) do
  begin
   for Ss := 0 to 7 do
    begin
     SrcIdx[0] := Sb18 + 17 - Ss;
     SrcIdx[1] := Sb18 + 18 + Ss;
     bu := FOut1D[SrcIdx[0]];
     bd := FOut1D[SrcIdx[1]];
     FOut1D[SrcIdx[0]] := (bu * cs[Ss]) - (bd * ca[Ss]);
     FOut1D[SrcIdx[1]] := (bd * cs[Ss]) + (bu * ca[Ss]);
    end;
   Inc(Sb18, 18);
 end;
end;

procedure TLayerIII_Decoder.Decode;
var
  nSlots           : Cardinal;
  FlushMain        : Cardinal;
  ch, ss, sb, sb18 : Cardinal;
  MainDataEnd      : Integer;
  BytesToDiscard   : Integer;
  i, gr            : Cardinal;
begin
 nSlots := FHeader.Slots;
 GetSideInfo;
 for i := 0 to nSlots - 1
  do FBitReserve.WriteToBitstream(FBitStream.GetBits(8));
 MainDataEnd := FBitReserve.TotalBits shr 3;  // of previous frame
 FlushMain := (FBitReserve.TotalBits and 7);
 if (FlushMain <> 0) then
  begin
   FBitReserve.GetBits(8 - FlushMain);
   Inc(MainDataEnd);
  end;

 BytesToDiscard := FFrameStart - MainDataEnd - FSideInfo.MainDataBegin;
 Inc(FFrameStart, nSlots);

 if (BytesToDiscard < 0) then Exit;

 if (MainDataEnd > 4096) then
  begin
   Dec(FFrameStart, 4096);
   FBitReserve.RewindBytes(4096);
  end;

 while (BytesToDiscard > 0) do
  begin
   FBitReserve.GetBits(8);
   Dec(BytesToDiscard);
  end;

 for gr := 0 to FMaxGr - 1 do
  begin
   for ch := 0 to FChannels - 1 do
    begin
     FPart2Start := FBitReserve.TotalBits;

     if (FHeader.version = MPEG1)
      then GetScaleFactors(ch, gr)
      else GetLSFScaleFactors(ch, gr); // MPEG-2 LSF
     HuffmanDecode(ch, gr);
     DequantizeSample(FRO[ch], ch, gr);
    end;

   Stereo(gr);

   if ((FWhichChannels = Downmix) and (FChannels > 1))
    then DoDownmix;

   for ch := FFirstChannel to FLastChannel do
    begin
     Reorder(@FLR[ch], ch, gr);
     Antialias(ch, gr);
     Hybrid(ch, gr);

     sb18 := 18;
     while (sb18 < 576) do
      begin  // Frequency inversion
       ss := 1;
       while (ss < SSLIMIT) do
        begin
         FOut1D[sb18 + ss] := -FOut1D[sb18 + ss];
         Inc(ss, 2);
        end;

       Inc(sb18, 36);
      end;

     if ((ch = 0) or (FWhichChannels = Right)) then
      begin
       for ss := 0 to SSLIMIT-1 do
        begin  // Polyphase synthesis
         sb := 0;
         sb18 := 0;
         while (sb18 < 576) do
          begin
           FFilter[0].InputSample(FOut1D[sb18 + ss], sb);
           Inc(sb18, 18);
           Inc(sb);
          end;

         FFilter[0].CalculatePCMSamples;
        end;
      end
     else
      begin
       for ss := 0 to SSLIMIT-1 do
        begin  // Polyphase synthesis
         sb := 0;
         sb18 := 0;
         while (sb18 < 576) do
          begin
           FFilter[1].InputSample(FOut1D[sb18 + ss], sb);
           Inc(sb18, 18);
           Inc(sb);
          end;

         FFilter[1].CalculatePCMSamples;
        end;
      end;
    end;
  end;
end;

procedure TLayerIII_Decoder.DequantizeSample(var xr: TSArray; ch,
  gr: Cardinal);
var
  GrInfo            : PGRInfo;
  Cb                : Integer;
  j, NextCbBoundary : Integer;
  CbBegin, CbWidth  : Integer;
  Index, t_index    : Integer;
  g_gain            : Single;
  xr1d              : PDAV1024SingleArray;
  abv, idx          : Cardinal;
begin
 GrInfo := @FSideInfo.ch[ch].gr[gr];
 Cb := 0;
 Index := 0;
 CbBegin := 0;
 CbWidth := 0;
 xr1d := @xr[0, 0];

 // choose correct scalefactor band per block type, initalize boundary
 if (GrInfo.WindowSwitchingFlag <> 0) and (GrInfo.BlockType = 2) then
  begin
   if (GrInfo.MixedBlockFlag <> 0)
    then NextCbBoundary := sfBandIndex[FSFreq].Long[1]  // LONG blocks: 0,1,3
    else
     begin
      CbWidth := sfBandIndex[FSFreq].Short[1];
      NextCbBoundary := (CbWidth shl 2) - CbWidth;
      CbBegin := 0;
     end;
  end
   else NextCbBoundary := sfBandIndex[FSFreq].Long[1];  // LONG blocks: 0,1,3

 // Compute overall (global) scaling.
 g_gain := Power(2.0 , (0.25 * (GrInfo.GlobalGain - 210.0)));

 for j := 0 to FNonZero[ch]-1 do
  begin
   if (FIs1D[j] = 0)
    then xr1d[j] := 0.0
    else
     begin
      abv := FIs1D[j];
      if (FIs1D[j] > 0)
       then xr1d[j] := g_gain * t_43[abv]
       else xr1d[j] := -g_gain * t_43[-abv];
     end;
  end;

 // apply formula per block type
 for j := 0 to FNonZero[ch]-1 do
  begin
   if (Index = NextCbBoundary) then
    begin  // Adjust critical band boundary
     if (GrInfo.WindowSwitchingFlag <> 0) and (GrInfo.BlockType = 2) then
      begin
       if (GrInfo.MixedBlockFlag <> 0) then
        begin
          if (Index = sfBandIndex[FSFreq].Long[8]) then
           begin
            NextCbBoundary := sfBandIndex[FSFreq].Short[4];
            NextCbBoundary := (NextCbBoundary shl 2) - NextCbBoundary;
            Cb := 3;
            CbWidth := sfBandIndex[FSFreq].Short[4] - sfBandIndex[FSFreq].Short[3];
            CbBegin := sfBandIndex[FSFreq].Short[3];
            CbBegin := (CbBegin shl 2) - CbBegin;
          end else
         if (Index < sfBandIndex[FSFreq].Long[8]) then
          begin
           Inc(Cb);
           NextCbBoundary := sfBandIndex[FSFreq].Long[Cb+1];
          end
         else
          begin
           Inc(Cb);
           NextCbBoundary := sfBandIndex[FSFreq].Short[Cb+1];
           NextCbBoundary := (NextCbBoundary shl 2) - NextCbBoundary;
           CbBegin := sfBandIndex[FSFreq].Short[Cb];
           CbWidth := sfBandIndex[FSFreq].Short[Cb+1] - CbBegin;
           CbBegin := (CbBegin shl 2) - CbBegin;
          end;
        end
       else
        begin
         Inc(Cb);
         NextCbBoundary := sfBandIndex[FSFreq].Short[Cb+1];
         NextCbBoundary := (NextCbBoundary shl 2) - NextCbBoundary;
         CbBegin := sfBandIndex[FSFreq].Short[Cb];
         CbWidth := sfBandIndex[FSFreq].Short[Cb+1] - CbBegin;
         CbBegin := (CbBegin shl 2) - CbBegin;
        end;
      end
     else
      begin  // long blocks
       Inc(Cb);
       NextCbBoundary := sfBandIndex[FSFreq].Long[Cb+1];
      end;
    end;

    // Do long/short dependent scaling operations
    if (GrInfo.WindowSwitchingFlag <> 0) and (((GrInfo.BlockType = 2) and (GrInfo.MixedBlockFlag = 0)) or
       ((GrInfo.BlockType = 2) and (GrInfo.MixedBlockFlag <> 0) and (j >= 36))) then begin
      t_index := (Index - CbBegin) div CbWidth;
(*            xr[sb,ss] *= pow(2.0, ((-2.0 * GrInfo->subblock_gain[t_index])
                                    -(0.5 * (1.0 + GrInfo->scalefac_scale)
                                      * scalefac[ch].Short[t_index,Cb]))); *)
      idx := FScaleFac[ch].Short[t_index,Cb] shl GrInfo.ScaleFactorScale;
      idx := idx + (GrInfo.SubblockGain[t_index] shl 2);
      xr1d[j] := xr1d[j] * CTwoToNegativeHalfPow[idx];
    end else begin  // LONG block types 0,1,3 & 1st 2 subbands of switched blocks
(*    xr[sb,ss] := xr[sb,ss] * Power(2, -0.5 * (1 + GrInfo.ScaleFactorScale)
                                * (FScaleFac[ch].Long[Cb] + GrInfo.Preflag * pretab[Cb])); *)
      idx := FScaleFac[ch].Long[Cb];
      if (GrInfo.Preflag <> 0)
       then idx := idx + pretab[Cb];

      idx := idx shl GrInfo.ScaleFactorScale;
      xr1d[j] := xr1d[j] * CTwoToNegativeHalfPow[idx];
    end;
    Inc(Index);
  end;

 for j := FNonZero[ch] to 575 do xr1d[j] := 0.0;
end;

procedure TLayerIII_Decoder.DoDownmix;
var
  ss, sb: Cardinal;
begin
 for sb := 0 to SBLIMIT - 1 do
  begin
   ss := 0;
   while (ss < SSLIMIT) do
    begin
     FLR[0, sb, ss    ] := (FLR[0, sb, ss    ] + FLR[1, sb, ss    ]) * 0.5;
     FLR[0, sb, ss + 1] := (FLR[0, sb, ss + 1] + FLR[1, sb, ss + 1]) * 0.5;
     FLR[0, sb, ss + 2] := (FLR[0, sb, ss + 2] + FLR[1, sb, ss + 2]) * 0.5;
     Inc(ss, 3);
    end;
  end;
end;

procedure TLayerIII_Decoder.GetLSFScaleData(ch, gr: Cardinal);
var
  NewSLength: array[0..3] of Cardinal;
  ScaleFactorComp, IntScalefactorComp: Cardinal;
  ModeExt: Cardinal;
  m: Integer;
  BlockTypeNumber, BlockNumber: Integer;
  GrInfo: PGRInfo;
  x, i, j: Cardinal;
begin
 ModeExt := FHeader.ModeExtension;
 GrInfo := @FSideInfo.ch[ch].gr[gr];
 ScaleFactorComp := GrInfo.ScaleFactorCompress;
 BlockNumber := 0;

 if (GrInfo.BlockType = 2) then
  begin
   if (GrInfo.MixedBlockFlag = 0)
    then BlockTypeNumber := 1
    else
     if (GrInfo.MixedBlockFlag = 1)
      then BlockTypeNumber := 2
      else BlockTypeNumber := 0;
  end else BlockTypeNumber := 0;

  if (not (((ModeExt = 1) or (ModeExt = 3)) and (ch = 1))) then begin
    if (ScaleFactorComp < 400) then begin
      NewSLength[0] := (ScaleFactorComp shr 4) div 5;
      NewSLength[1] := (ScaleFactorComp shr 4) mod 5;
      NewSLength[2] := (ScaleFactorComp and $F) shr 2;
      NewSLength[3] := (ScaleFactorComp and 3);
      FSideInfo.ch[ch].gr[gr].Preflag := 0;
      BlockNumber := 0;
    end else if (ScaleFactorComp < 500) then begin
      NewSLength[0] := ((ScaleFactorComp - 400) shr 2) div 5;
      NewSLength[1] := ((ScaleFactorComp - 400) shr 2) mod 5;
      NewSLength[2] := (ScaleFactorComp - 400) and 3;
      NewSLength[3] := 0;
      FSideInfo.ch[ch].gr[gr].Preflag := 0;
      BlockNumber := 1;
    end else if (ScaleFactorComp < 512) then begin
      NewSLength[0] := (ScaleFactorComp - 500) div 3;
      NewSLength[1] := (ScaleFactorComp - 500) mod 3;
      NewSLength[2] := 0;
      NewSLength[3] := 0;
      FSideInfo.ch[ch].gr[gr].Preflag := 1;
      BlockNumber := 2;
    end;
  end;

  if ((((ModeExt = 1) or (ModeExt = 3)) and (ch = 1))) then begin
    IntScalefactorComp := ScaleFactorComp shr 1;

    if (IntScalefactorComp < 180) then begin
      NewSLength[0] := IntScalefactorComp div 36;
      NewSLength[1] := (IntScalefactorComp mod 36 ) div 6;
      NewSLength[2] := (IntScalefactorComp mod 36) mod 6;
      NewSLength[3] := 0;
      FSideInfo.ch[ch].gr[gr].Preflag := 0;
      BlockNumber := 3;
    end else if (IntScalefactorComp < 244) then begin
      NewSLength[0] := ((IntScalefactorComp - 180) and $3F) shr 4;
      NewSLength[1] := ((IntScalefactorComp - 180) and $F) shr 2;
      NewSLength[2] := (IntScalefactorComp - 180) and 3;
      NewSLength[3] := 0;
      FSideInfo.ch[ch].gr[gr].Preflag := 0;
      BlockNumber := 4;
    end else if (IntScalefactorComp < 255) then begin
      NewSLength[0] := (IntScalefactorComp - 244) div 3;
      NewSLength[1] := (IntScalefactorComp - 244) mod 3;
      NewSLength[2] := 0;
      NewSLength[3] := 0;
      FSideInfo.ch[ch].gr[gr].Preflag := 0;
      BlockNumber := 5;
    end;
  end;

 for x := 0 to 44 do GScaleFacBuffer[x] := 0; // why 45, not 54?

 m := 0;
 for i := 0 to 3 do
  for j := 0 to CNrOfSFBBlock[BlockNumber, BlockTypeNumber, i] do
   begin
    if (NewSLength[i] = 0)
     then GScaleFacBuffer[m] := 0
     else GScaleFacBuffer[m] := FBitReserve.GetBits(NewSLength[i]);
    Inc(m);
   end;
end;

procedure TLayerIII_Decoder.GetLSFScaleFactors(ch, gr: Cardinal);
var
  m, Sfb : Cardinal;
  Window : Cardinal;
  GrInfo : PGRInfo;
begin
 m := 0;
 GrInfo := @FSideInfo.ch[ch].gr[gr];
 GetLSFScaleData(ch, gr);

  if ((GrInfo.WindowSwitchingFlag <> 0) and (GrInfo.BlockType = 2)) then
   begin
    if (GrInfo.MixedBlockFlag <> 0) then
     begin  // MIXED
      for Sfb := 0 to 7 do
       begin
        FScaleFac[ch].Long[Sfb] := GScaleFacBuffer[m];
        Inc(m);
       end;
      for Sfb := 3 to 11 do
       for Window := 0 to 2 do
        begin
         FScaleFac[ch].Short[Window,Sfb] := GScaleFacBuffer[m];
         Inc(m);
        end;
      for Window := 0 to 2 do FScaleFac[ch].Short[Window, 12] := 0;
     end
    else
     begin  // SHORT
      for Sfb := 0 to 11 do
       for Window := 0 to 2 do
        begin
         FScaleFac[ch].Short[Window, Sfb] := GScaleFacBuffer[m];
         Inc(m);
        end;
      for Window := 0 to 2 do FScaleFac[ch].Short[Window, 12] := 0;
     end;
   end
  else
   begin  // LONG types 0,1,3
    for Sfb := 0 to 20 do
     begin
      FScaleFac[ch].Long[Sfb] := GScaleFacBuffer[m];
      Inc(m);
     end;
    FScaleFac[ch].Long[21] := 0; // Jeff
    FScaleFac[ch].Long[22] := 0;
  end;
end;

procedure TLayerIII_Decoder.GetScaleFactors(ch, gr: Cardinal);
var
  sfb, window      : Integer;
  GrInfo           : PGRInfo;
  scale_comp       : Integer;
  length0, length1 : Integer;
begin
 GrInfo := @FSideInfo.ch[ch].gr[gr];
 scale_comp := GrInfo.ScaleFactorCompress;
 length0 := slen[0, scale_comp];
 length1 := slen[1, scale_comp];

 with FScaleFac[ch] do
  if ((GrInfo.WindowSwitchingFlag <> 0) and (GrInfo.BlockType = 2)) then
   begin
    if (GrInfo.MixedBlockFlag <> 0) then
     begin  // MIXED
      for sfb := 0 to 7 do Long[sfb] := FBitReserve.GetBits(slen[0, GrInfo.ScaleFactorCompress]);
      for sfb := 3 to 5 do
       for window := 0 to 2 do Short[window, sfb] := FBitReserve.GetBits(slen[0, GrInfo.ScaleFactorCompress]);
      for sfb := 6 to 11 do
       for window := 0 to 2 do Short[window, sfb] := FBitReserve.GetBits(slen[1, GrInfo.ScaleFactorCompress]);
      sfb := 12;
      for window := 0 to 2 do Short[window, sfb] := 0;
     end
    else
     begin  // SHORT
      Short[0, 0]  := FBitReserve.GetBits(length0);
      Short[1, 0]  := FBitReserve.GetBits(length0);
      Short[2, 0]  := FBitReserve.GetBits(length0);
      Short[0, 1]  := FBitReserve.GetBits(length0);
      Short[1, 1]  := FBitReserve.GetBits(length0);
      Short[2, 1]  := FBitReserve.GetBits(length0);
      Short[0, 2]  := FBitReserve.GetBits(length0);
      Short[1, 2]  := FBitReserve.GetBits(length0);
      Short[2, 2]  := FBitReserve.GetBits(length0);
      Short[0, 3]  := FBitReserve.GetBits(length0);
      Short[1, 3]  := FBitReserve.GetBits(length0);
      Short[2, 3]  := FBitReserve.GetBits(length0);
      Short[0, 4]  := FBitReserve.GetBits(length0);
      Short[1, 4]  := FBitReserve.GetBits(length0);
      Short[2, 4]  := FBitReserve.GetBits(length0);
      Short[0, 5]  := FBitReserve.GetBits(length0);
      Short[1, 5]  := FBitReserve.GetBits(length0);
      Short[2, 5]  := FBitReserve.GetBits(length0);
      Short[0, 6]  := FBitReserve.GetBits(length1);
      Short[1, 6]  := FBitReserve.GetBits(length1);
      Short[2, 6]  := FBitReserve.GetBits(length1);
      Short[0, 7]  := FBitReserve.GetBits(length1);
      Short[1, 7]  := FBitReserve.GetBits(length1);
      Short[2, 7]  := FBitReserve.GetBits(length1);
      Short[0, 8]  := FBitReserve.GetBits(length1);
      Short[1, 8]  := FBitReserve.GetBits(length1);
      Short[2, 8]  := FBitReserve.GetBits(length1);
      Short[0, 9]  := FBitReserve.GetBits(length1);
      Short[1, 9]  := FBitReserve.GetBits(length1);
      Short[2, 9]  := FBitReserve.GetBits(length1);
      Short[0, 10] := FBitReserve.GetBits(length1);
      Short[1, 10] := FBitReserve.GetBits(length1);
      Short[2, 10] := FBitReserve.GetBits(length1);
      Short[0, 11] := FBitReserve.GetBits(length1);
      Short[1, 11] := FBitReserve.GetBits(length1);
      Short[2, 11] := FBitReserve.GetBits(length1);
      Short[0, 12] := 0;
      Short[1, 12] := 0;
      Short[2, 12] := 0;
     end;
   end
  else
   begin  // LONG types 0,1,3
    if ((FSideInfo.ch[ch].scfsi[0] = 0) or (gr = 0)) then
     begin
      Long[0]  := FBitReserve.GetBits(length0);
      Long[1]  := FBitReserve.GetBits(length0);
      Long[2]  := FBitReserve.GetBits(length0);
      Long[3]  := FBitReserve.GetBits(length0);
      Long[4]  := FBitReserve.GetBits(length0);
      Long[5]  := FBitReserve.GetBits(length0);
     end;
    if ((FSideInfo.ch[ch].scfsi[1] = 0) or (gr = 0)) then
     begin
      Long[6]  := FBitReserve.GetBits(length0);
      Long[7]  := FBitReserve.GetBits(length0);
      Long[8]  := FBitReserve.GetBits(length0);
      Long[9]  := FBitReserve.GetBits(length0);
      Long[10] := FBitReserve.GetBits(length0);
     end;

    if ((FSideInfo.ch[ch].scfsi[2] = 0) or (gr = 0)) then
     begin
      Long[11] := FBitReserve.GetBits(length1);
      Long[12] := FBitReserve.GetBits(length1);
      Long[13] := FBitReserve.GetBits(length1);
      Long[14] := FBitReserve.GetBits(length1);
      Long[15] := FBitReserve.GetBits(length1);
     end;

    if ((FSideInfo.ch[ch].scfsi[3] = 0) or (gr = 0)) then
     begin
      Long[16] := FBitReserve.GetBits(length1);
      Long[17] := FBitReserve.GetBits(length1);
      Long[18] := FBitReserve.GetBits(length1);
      Long[19] := FBitReserve.GetBits(length1);
      Long[20] := FBitReserve.GetBits(length1);
     end;

    Long[21] := 0;
    Long[22] := 0;
   end;
end;

// Reads the side info from the stream, assuming the entire
// frame has been read already.

// Mono   : 136 bits (= 17 bytes)
// Stereo : 256 bits (= 32 bytes)
function TLayerIII_Decoder.GetSideInfo: Boolean;
var
  ch, gr: Cardinal;
begin
 if (FHeader.Version = MPEG1) then
  begin
   FSideInfo.MainDataBegin := FBitStream.GetBits(9);
   if (FChannels = 1)
    then FSideInfo.PrivateBits := FBitStream.GetBits(5)
    else FSideInfo.PrivateBits := FBitStream.GetBits(3);

   for ch := 0 to FChannels - 1 do
    with FSideInfo.ch[ch] do
     begin
      scfsi[0] := FBitStream.GetBits(1);
      scfsi[1] := FBitStream.GetBits(1);
      scfsi[2] := FBitStream.GetBits(1);
      scfsi[3] := FBitStream.GetBits(1);
    end;

   for gr := 0 to 1 do
    begin
     for ch := 0 to FChannels - 1 do
      with FSideInfo.ch[ch].gr[gr] do
       begin
        part2_3_length := FBitStream.GetBits(12);
        BigValues := FBitStream.GetBits(9);
        GlobalGain := FBitStream.GetBits(8);
        ScaleFactorCompress := FBitStream.GetBits(4);
        WindowSwitchingFlag := FBitStream.GetBits(1);
        if (WindowSwitchingFlag <> 0) then
         begin
          BlockType := FBitStream.GetBits(2);
          MixedBlockFlag := FBitStream.GetBits(1);

          TableSelect[0] := FBitStream.GetBits(5);
          TableSelect[1] := FBitStream.GetBits(5);

          SubblockGain[0] := FBitStream.GetBits(3);
          SubblockGain[1] := FBitStream.GetBits(3);
          SubblockGain[2] := FBitStream.GetBits(3);

          // Set region_count parameters since they are implicit in this case.
          if (BlockType = 0) then
           begin
            // Side info bad: BlockType == 0 in split block
            Result := False;
            Exit;
           end else
          if (BlockType = 2) and (MixedBlockFlag = 0)
           then RegionCount[0] := 8
           else RegionCount[0] := 7;

          RegionCount[1] := 20 - RegionCount[0];
         end
        else
         begin
          TableSelect[0] := FBitStream.GetBits(5);
          TableSelect[1] := FBitStream.GetBits(5);
          TableSelect[2] := FBitStream.GetBits(5);
          RegionCount[0] := FBitStream.GetBits(4);
          RegionCount[1] := FBitStream.GetBits(3);
          BlockType := 0;
         end;
        Preflag := FBitStream.GetBits(1);
        ScaleFactorScale := FBitStream.GetBits(1);
        Count1TableSelect := FBitStream.GetBits(1);
       end;
    end;
  end
 else
  begin  // MPEG-2 LSF
   FSideInfo.MainDataBegin := FBitStream.GetBits(8);
   if (FChannels = 1)
    then FSideInfo.PrivateBits := FBitStream.GetBits(1)
    else FSideInfo.PrivateBits := FBitStream.GetBits(2);
   for ch := 0 to FChannels-1 do
    begin
     FSideInfo.ch[ch].gr[0].part2_3_length := FBitStream.GetBits(12);
     FSideInfo.ch[ch].gr[0].BigValues := FBitStream.GetBits(9);
     FSideInfo.ch[ch].gr[0].GlobalGain := FBitStream.GetBits(8);
     FSideInfo.ch[ch].gr[0].ScaleFactorCompress := FBitStream.GetBits(9);
     FSideInfo.ch[ch].gr[0].WindowSwitchingFlag := FBitStream.GetBits(1);

     if (FSideInfo.ch[ch].gr[0].WindowSwitchingFlag <> 0) then
      with FSideInfo.ch[ch].gr[0] do
       begin
        BlockType := FBitStream.GetBits(2);
        MixedBlockFlag := FBitStream.GetBits(1);
        TableSelect[0] := FBitStream.GetBits(5);
        TableSelect[1] := FBitStream.GetBits(5);

        SubblockGain[0] := FBitStream.GetBits(3);
        SubblockGain[1] := FBitStream.GetBits(3);
        SubblockGain[2] := FBitStream.GetBits(3);

        // Set region_count parameters since they are implicit in this case.
        if (BlockType = 0) then
         begin
          // Side info bad: BlockType = 0 in split block
          Result := False;
          Exit;
         end
        else
         if (BlockType = 2) and (MixedBlockFlag = 0)
          then RegionCount[0] := 8
          else
           begin
            RegionCount[0] := 7;
            RegionCount[1] := 20 - RegionCount[0];
           end;
       end
     else
      with FSideInfo.ch[ch].gr[0] do
       begin
        TableSelect[0] := FBitStream.GetBits(5);
        TableSelect[1] := FBitStream.GetBits(5);
        TableSelect[2] := FBitStream.GetBits(5);
        RegionCount[0] := FBitStream.GetBits(4);
        RegionCount[1] := FBitStream.GetBits(3);
        BlockType := 0;
       end;

     FSideInfo.ch[ch].gr[0].ScaleFactorScale := FBitStream.GetBits(1);
     FSideInfo.ch[ch].gr[0].Count1TableSelect := FBitStream.GetBits(1);
    end;
  end;
 Result := True;
end;

procedure TLayerIII_Decoder.HuffmanDecode(ch, gr: Cardinal);
var
  i            : Cardinal;
  x, y, v, w   : Integer;
  part2_3_end  : Integer;
  NumBits      : Integer;
  Region1Start : Cardinal;
  Region2Start : Cardinal;
  Index        : Integer;
  h            : PHuffmanCodeTable;
begin
 part2_3_end := FPart2Start + FSideInfo.ch[ch].gr[gr].part2_3_length;

 // Find region boundary for short block case
 if ((FSideInfo.ch[ch].gr[gr].WindowSwitchingFlag <> 0) and (FSideInfo.ch[ch].gr[gr].BlockType = 2)) then
  begin
   // Region2.
   Region1Start := 36;   // sfb[9/3]*3=36
   Region2Start := 576;  // No Region2 for short block case
  end
 else
  begin  // Find region boundary for long block case
   Region1Start := sfBandIndex[FSFreq].Long[FSideInfo.ch[ch].gr[gr].RegionCount[0] + 1];
   Region2Start := sfBandIndex[FSFreq].Long[FSideInfo.ch[ch].gr[gr].RegionCount[0] + FSideInfo.ch[ch].gr[gr].RegionCount[1] + 2];  // MI
  end;

 Index := 0;
 // Read bigvalues area
 i := 0;
 while (i < (FSideInfo.ch[ch].gr[gr].BigValues shl 1)) do
  begin
   if (i < Region1Start) then h := @GHuffmanCodeTable[FSideInfo.ch[ch].gr[gr].TableSelect[0]]
   else if (i < Region2Start)
    then h := @GHuffmanCodeTable[FSideInfo.ch[ch].gr[gr].TableSelect[1]]
    else h := @GHuffmanCodeTable[FSideInfo.ch[ch].gr[gr].TableSelect[2]];

   HuffmanDecoder(h, x, y, v, w, FBitReserve);

   FIs1D[Index] := x;
   FIs1D[Index+1] := y;

   Inc(Index, 2);
   Inc(i, 2);
  end;

 // Read count1 area
 h := @GHuffmanCodeTable[FSideInfo.ch[ch].gr[gr].Count1TableSelect + 32];
 NumBits := FBitReserve.TotalBits;

 while ((NumBits < part2_3_end) and (Index < 576)) do
  begin
   HuffmanDecoder(h, x, y, v, w, FBitReserve);

   FIs1D[Index] := v;
   FIs1D[Index+1] := w;
   FIs1D[Index+2] := x;
   FIs1D[Index+3] := y;

   Inc(Index, 4);
   NumBits := FBitReserve.TotalBits;
  end;

 if (NumBits > part2_3_end) then
  begin
   FBitReserve.RewindBits(NumBits - part2_3_end);
   Dec(Index, 4);
  end;

 NumBits := FBitReserve.TotalBits;

 // Dismiss stuffing bits
 if (NumBits < part2_3_end) then FBitReserve.GetBits(part2_3_end - NumBits);

 // Zero out rest
 if (Index < 576)
  then FNonZero[ch] := Index
  else FNonZero[ch] := 576;

 // may not be necessary
 while (Index < 576) do
  begin
   FIs1D[Index] := 0;
   Inc(Index);
  end;
end;

procedure TLayerIII_Decoder.Hybrid(ch, gr: Cardinal);
var
  rawout: array[0..35] of Single;
  bt: Cardinal;
  GrInfo: PGRInfo;
  tsOut: PDAV1024SingleArray;
  prvblk: PDAV1024SingleArray;
  sb18: Cardinal;
begin
 GrInfo := @FSideInfo.ch[ch].gr[gr];

 sb18 := 0;
 while (sb18 < 576) do
  begin
   if (GrInfo.WindowSwitchingFlag <> 0) and (GrInfo.MixedBlockFlag <> 0) and (sb18 < 36)
    then bt := 0
    else bt := GrInfo.BlockType;

   tsOut := @FOut1D[sb18];
   InvMDCT(tsOut, @rawout, bt);

   // overlap addition
   prvblk := @FPrevblock[ch, sb18];

   tsOut[0]   := rawout[0]  + prvblk[0];
   prvblk[0]  := rawout[18];
   tsOut[1]   := rawout[1]  + prvblk[1];
   prvblk[1]  := rawout[19];
   tsOut[2]   := rawout[2]  + prvblk[2];
   prvblk[2]  := rawout[20];
   tsOut[3]   := rawout[3]  + prvblk[3];
   prvblk[3]  := rawout[21];
   tsOut[4]   := rawout[4]  + prvblk[4];
   prvblk[4]  := rawout[22];
   tsOut[5]   := rawout[5]  + prvblk[5];
   prvblk[5]  := rawout[23];
   tsOut[6]   := rawout[6]  + prvblk[6];
   prvblk[6]  := rawout[24];
   tsOut[7]   := rawout[7]  + prvblk[7];
   prvblk[7]  := rawout[25];
   tsOut[8]   := rawout[8]  + prvblk[8];
   prvblk[8]  := rawout[26];
   tsOut[9]   := rawout[9]  + prvblk[9];
   prvblk[9]  := rawout[27];
   tsOut[10]  := rawout[10] + prvblk[10];
   prvblk[10] := rawout[28];
   tsOut[11]  := rawout[11] + prvblk[11];
   prvblk[11] := rawout[29];
   tsOut[12]  := rawout[12] + prvblk[12];
   prvblk[12] := rawout[30];
   tsOut[13]  := rawout[13] + prvblk[13];
   prvblk[13] := rawout[31];
   tsOut[14]  := rawout[14] + prvblk[14];
   prvblk[14] := rawout[32];
   tsOut[15]  := rawout[15] + prvblk[15];
   prvblk[15] := rawout[33];
   tsOut[16]  := rawout[16] + prvblk[16];
   prvblk[16] := rawout[34];
   tsOut[17]  := rawout[17] + prvblk[17];
   prvblk[17] := rawout[35];

   Inc(sb18, 18);
  end;
end;

procedure TLayerIII_Decoder.IStereoKValues(IsPos, IOType, i: Cardinal);
begin
 if (IsPos = 0) then
  begin
   FK[0, i] := 1.0;
   FK[1, i] := 1.0;
  end
 else if (IsPos and 1 <> 0) then
  begin
   FK[0, i] := io[IOType, (IsPos + 1) shr 1];
   FK[1, i] := 1.0;
  end
 else
  begin
   FK[0, i] := 1.0;
   FK[1, i] := io[IOType, IsPos shr 1];
  end;
end;

procedure TLayerIII_Decoder.Reorder(xr: PSArray; ch, gr: Cardinal);
var
  GrInfo: PGRInfo;
  freq, freq3: Cardinal;
  sfb, sfb_start, sfb_start3, sfb_lines: Cardinal;
  src_line, des_line: Integer;
  xr1d: PDAV1024SingleArray;
  index: Cardinal;
begin
 xr1d := @xr[0, 0];
 GrInfo := @FSideInfo.ch[ch].gr[gr];
 if (GrInfo.WindowSwitchingFlag <> 0) and (GrInfo.BlockType = 2) then
  begin
   for index := 0 to 576-1 do FOut1D[index] := 0.0;

   if (GrInfo.MixedBlockFlag <> 0) then
    begin // NO REORDER FOR LOW 2 SUBBANDS
     for index := 0 to 36-1 do FOut1D[index] := xr1d[index];

     // REORDERING FOR REST SWITCHED SHORT
     sfb_start := sfBandIndex[FSFreq].Short[3];
     sfb_lines := Cardinal(sfBandIndex[FSFreq].Short[4]) - sfb_start;
     for sfb := 3 to 12 do
      begin
       sfb_start3 := (sfb_start shl 2) - sfb_start;
       freq3 := 0;
       for freq := 0 to sfb_lines-1 do
        begin
         src_line := sfb_start3 + freq;
         des_line := sfb_start3 + freq3;
         FOut1D[des_line] := xr1d[src_line];
         Inc(src_line, sfb_lines);
         Inc(des_line);
         FOut1D[des_line] := xr1d[src_line];
         Inc(src_line, sfb_lines);
         Inc(des_line);
         FOut1D[des_line] := xr1d[src_line];
         Inc(freq3, 3);
        end;
       sfb_start := sfBandIndex[FSFreq].Short[sfb];
       sfb_lines := Cardinal(sfBandIndex[FSFreq].Short[sfb+1]) - sfb_start;
      end;
    end
   else for index := 0 to 576-1 do FOut1D[index] := xr1d[reorder_table[FSFreq,index]]; // pure short
  end
 else for index := 0 to 576-1 do FOut1D[index] := xr1d[index]; // long blocks
end;

procedure TLayerIII_Decoder.SeekNotify;
begin
 FFrameStart := 0;
 FillChar(FPrevBlock, Sizeof(FPrevBlock), 0);
 FreeAndNil(FBitReserve);
 FBitReserve := TBitReserve.Create;
end;

procedure TLayerIII_Decoder.Stereo(gr: Cardinal);
var
  sb, ss              : Integer;
  IsPos               : array[0..575] of Cardinal;
  IsRatio             : array[0..575] of Single;
  GrInfo              : PGRInfo;
  ModeExt, IoType     : Cardinal;
  sfx, i, j, Lines    : Integer;
  temp, temp2         : Integer;
  MSStereo, IStereo   : Boolean;
  lsf                 : Boolean;
  MaxSfb, SfbCnt, Sfb : Integer;
begin
 if (FChannels = 1) then
  begin  // mono , bypass xr[0,,] to lr[0,,]
   for sb := 0 to SBLIMIT-1 do
    begin
     ss := 0;
     while (ss < SSLIMIT) do
      begin
       FLR[0, sb, ss    ] := FRO[0, sb, ss    ];
       FLR[0, sb, ss + 1] := FRO[0, sb, ss + 1];
       FLR[0, sb, ss + 2] := FRO[0, sb, ss + 2];
       Inc(ss, 3);
      end;
    end;
  end
 else
  begin
   GrInfo := @FSideInfo.ch[0].gr[gr];
   ModeExt := FHeader.ModeExtension;
   MSStereo := (FHeader.Mode = JointStereo) and (ModeExt and $2 <> 0);
   IStereo := (FHeader.Mode = JointStereo) and (ModeExt and $1 <> 0);
   lsf := (FHeader.Version = MPEG2_LSF);
   IoType := (GrInfo.ScaleFactorCompress and 1);

   // initialization
   for i := 0 to 575 do IsPos[i] := 7;

   if (IStereo) then
    begin
     if (GrInfo.WindowSwitchingFlag <> 0) and (GrInfo.BlockType = 2) then
      begin
       if (GrInfo.MixedBlockFlag <> 0) then
        begin
         MaxSfb := 0;

         for j := 0 to 2 do
          begin
           SfbCnt := 2;
           Sfb := 12;
           while (Sfb >= 3) do
            begin
             i := sfBandIndex[FSFreq].Short[Sfb];
             Lines := sfBandIndex[FSFreq].Short[Sfb+1] - i;
             i := (i shl 2) - i + (j+1) * Lines - 1;

             while (Lines > 0) do
              begin
               if (FRO[1,ss_div[i],ss_mod[i]] <> 0.0) then
                begin
                 SfbCnt := Sfb;
                 Sfb := -10;
                 Lines := -10;
                end;

               Dec(Lines);
               Dec(i);
              end;

             Dec(Sfb);
            end;
           Sfb := SfbCnt + 1;

           if (Sfb > MaxSfb) then MaxSfb := Sfb;

           while (Sfb < 12) do
            begin
             temp := sfBandIndex[FSFreq].Short[Sfb];
             sb := sfBandIndex[FSFreq].Short[Sfb+1] - temp;
             i := (temp shl 2) - temp + j * sb;

             while (sb > 0) do
              begin
               IsPos[i] := FScaleFac[1].Short[j,Sfb];
               if (IsPos[i] <> 7) then
                if (lsf)
                 then IStereoKValues(IsPos[i], IoType, i)
                 else IsRatio[i] := CTan12[IsPos[i]];

               Inc(i);
               Dec(sb);
              end;
             Inc(Sfb);
            end;

           Sfb := sfBandIndex[FSFreq].Short[10];
           sb := sfBandIndex[FSFreq].Short[11] - Sfb;
           Sfb := (Sfb shl 2) - Sfb + j * sb;
           temp := sfBandIndex[FSFreq].Short[11];
           sb := sfBandIndex[FSFreq].Short[12] - temp;
           i := (temp shl 2) - temp + j * sb;

           while (sb > 0) do
            begin
             IsPos[i] := IsPos[Sfb];

             if (lsf) then
              begin
               FK[0,i] := FK[0,Sfb];
               FK[1,i] := FK[1,Sfb];
              end
             else IsRatio[i] := IsRatio[Sfb];

             Inc(i);
             Dec(sb);
            end;
          end;

          if (MaxSfb <= 3) then
           begin
            i := 2;
            ss := 17;
            sb := -1;
            while (i >= 0) do
             begin
              if (FRO[1,i,ss] <> 0.0) then
               begin
                sb := (i shl 4) + (i shl 1) + ss;
                i := -1;
               end
              else
               begin
                Dec(ss);
                if (ss < 0) then
                 begin
                  Dec(i);
                  ss := 17;
                 end;
               end;
             end;

            i := 0;
            while (sfBandIndex[FSFreq].Long[i] <= sb) do Inc(i);

            Sfb := i;
            i := sfBandIndex[FSFreq].Long[i];
            while (Sfb < 8) do begin
              sb := sfBandIndex[FSFreq].Long[Sfb+1] - sfBandIndex[FSFreq].Long[Sfb];
              while (sb > 0) do begin
                IsPos[i] := FScaleFac[1].Long[Sfb];
                if (IsPos[i] <> 7) then
                  if (lsf) then
                    IStereoKValues(IsPos[i], IoType, i)
                  else
                    IsRatio[i] := CTan12[IsPos[i]];

                Inc(i);
                Inc(sb);
              end;
              Inc(Sfb);
            end;
          end;
        end
       else
        begin  // if (GrInfo->MixedBlockFlag)
         for j := 0 to 2 do
          begin
           SfbCnt := -1;
           Sfb := 12;
           while (Sfb >= 0) do
            begin
             temp := sfBandIndex[FSFreq].Short[Sfb];
             Lines := sfBandIndex[FSFreq].Short[Sfb+1] - temp;
             i := (temp shl 2) - temp + (j+1) * Lines - 1;

             while (Lines > 0) do
              begin
               if (FRO[1,ss_div[i],ss_mod[i]] <> 0.0) then
                begin
                 SfbCnt := Sfb;
                 Sfb := -10;
                 Lines := -10;
                end;

               Dec(Lines);
               Dec(i);
              end;
             Dec(Sfb);
            end;

           Sfb := SfbCnt + 1;
           while (Sfb < 12) do
            begin
             temp := sfBandIndex[FSFreq].Short[Sfb];
             sb := sfBandIndex[FSFreq].Short[Sfb+1] - temp;
             i := (temp shl 2) - temp + j * sb;
             while (sb > 0) do
              begin
               // Dec(sb);
               IsPos[i] := FScaleFac[1].Short[j,Sfb];
               if (IsPos[i] <> 7) then
                if (lsf)
                 then IStereoKValues(IsPos[i], IoType, i)
                 else IsRatio[i] := CTan12[IsPos[i]];

               Inc(i);
               Dec(sb);
              end;

             Inc(Sfb);
            end;

           temp := sfBandIndex[FSFreq].Short[10];
           temp2 := sfBandIndex[FSFreq].Short[11];
           sb   := temp2 - temp;
           Sfb  := (temp shl 2) - temp + j * sb;
           sb   := sfBandIndex[FSFreq].Short[12] - temp2;
           i    := (temp2 shl 2) - temp2 + j * sb;

           while (sb > 0) do
            begin
             IsPos[i] := IsPos[Sfb];

             if (lsf) then
              begin
               FK[0,i] := FK[0,Sfb];
               FK[1,i] := FK[1,Sfb];
              end
             else IsRatio[i] := IsRatio[Sfb];

             Inc(i);
             Dec(sb);
            end;
          end;
        end;
      end
     else
      begin  // if (GrInfo->WindowSwitchingFlag ...
       i := 31;
       ss := 17;
       sb := 0;
       while (i >= 0) do
        begin
         if (FRO[1,i,ss] <> 0.0) then
          begin
           sb := (i shl 4) + (i shl 1) + ss;
           i := -1;
          end
         else
          begin
           Dec(ss);
           if (ss < 0) then
            begin
             Dec(i);
             ss := 17;
            end;
          end;
        end;

       i := 0;
       while (sfBandIndex[FSFreq].Long[i] <= sb) do Inc(i);

       Sfb := i;
       i := sfBandIndex[FSFreq].Long[i];
       while (Sfb < 21) do
        begin
         sb := sfBandIndex[FSFreq].Long[Sfb+1] - sfBandIndex[FSFreq].Long[Sfb];
         while (sb > 0) do
          begin
           IsPos[i] := FScaleFac[1].Long[Sfb];
           if (IsPos[i] <> 7) then
            if (lsf)
             then IStereoKValues(IsPos[i], IoType, i)
             else IsRatio[i] := CTan12[IsPos[i]];

           Inc(i);
           Dec(sb);
          end;
         Inc(Sfb);
        end;

       Sfb := sfBandIndex[FSFreq].Long[20];
       sb := 576 - sfBandIndex[FSFreq].Long[21];
       while (sb > 0) and (i < 576) do
        begin
         IsPos[i] := IsPos[Sfb]; // error here : i >=576
         if (lsf) then
          begin
           FK[0,i] := FK[0,Sfb];
           FK[1,i] := FK[1,Sfb];
          end
         else IsRatio[i] := IsRatio[Sfb];

         Inc(i);
         Dec(sb);
        end;
      end;
    end;

    i := 0;
    for sb := 0 to SBLIMIT-1 do
     for ss := 0 to SSLIMIT-1 do
      begin
       if (IsPos[i] = 7) then
        begin
         if (MSStereo) then
          begin
           FLR[0,sb,ss] := (FRO[0,sb,ss] + FRO[1,sb,ss]) * 0.707106781;
           FLR[1,sb,ss] := (FRO[0,sb,ss] - FRO[1,sb,ss]) * 0.707106781;
          end
         else
          begin
           FLR[0,sb,ss] := FRO[0,sb,ss];
           FLR[1,sb,ss] := FRO[1,sb,ss];
          end;
        end
       else
        if (IStereo) then
         begin
          if (lsf) then
           begin
            FLR[0,sb,ss] := FRO[0,sb,ss] * FK[0,i];
            FLR[1,sb,ss] := FRO[0,sb,ss] * FK[1,i];
           end
          else
           begin
            FLR[1, sb, ss] := FRO[0, sb, ss] / (1 + IsRatio[i]);
            FLR[0, sb, ss] := FLR[1, sb, ss] * IsRatio[i];
           end;
         end;
       Inc(i);
      end;
  end;
end;


{ TMPEGAudio }

constructor TMpegAudio.Create;
begin
 FMPEGHeader := THeader.Create;
 FFilter[0] := TSynthesisFilter.Create;
 FFilter[1] := TSynthesisFilter.Create;
 FFilter[0].OnNewPCMSample := NewPCMSample;
 FFilter[1].OnNewPCMSample := NewPCMSample;
 FBuffer := TStereoBuffer.Create;
 FCRC := nil;
 FOPos := 0;
end;

constructor TMpegAudio.Create(Filename: TFileName);
begin
 Create;
 FBitStream := TBitStream.Create(FileName);
 FWhichC := Both;
 FMPEGHeader.ReadHeader(FBitStream, FCRC);
end;

constructor TMpegAudio.Create(Stream: TStream);
begin
 Create;
 FBitStream := TBitStream.Create(Stream);
 FWhichC := Both;
 FMPEGHeader.ReadHeader(FBitStream, FCRC);
end;

destructor TMPEGAudio.Destroy;
begin
 if Assigned(FCRC) then FreeAndNil(FCRC);
 if Assigned(FBitStream) then FreeAndNil(FBitStream);
 if Assigned(FLayer3) then FreeAndNil(FLayer3);

 FreeAndNil(FFilter[0]);
 FreeAndNil(FFilter[1]);
 FreeAndNil(FMPEGHeader);
 FreeAndNil(FBuffer);
end;

procedure TMPEGAudio.NewPCMSample(Sender: TObject; Sample: Single);
begin
 if Sender = FFilter[0]
  then FBuffer.Append(0, Sample)
  else FBuffer.Append(1, Sample)
end;

procedure TMPEGAudio.DoDecode;
var
  Mode           : TMode;
  NumSubBands, i : Cardinal;
  SubBands       : array[0..31] of TSubBand;
  ReadReady      : Boolean;
  WriteReady     : Boolean;
begin
 // is there a change in important parameters?
 // (bitrate switching is allowed)
 if (FMPEGHeader.Layer <> FLayer) then
  begin // layer switching is allowed
   if (FMPEGHeader.Layer = 3)
    then FLayer3 := TLayerIII_Decoder.Create(FBitStream, FMPEGHeader, FFilter[0], FFilter[1], FBuffer, FWhichC)
    else
   if (FLayer = 3) then FreeAndNil(FLayer3);
   FLayer := FMPEGHeader.Layer;
  end;

 if (FLayer <> 3) then
  begin
   NumSubBands := FMPEGHeader.NumberOfSubbands;
   Mode := FMPEGHeader.Mode;

   // create subband objects:
   if (FLayer = 1) then
    begin  // Layer I
     if (Mode = SingleChannel) then
      for i := 0 to NumSubBands - 1
       do SubBands[i] := TSubbandLayer1.Create(i)
     else
      if (Mode = JointStereo) then
       begin
        for i := 0 to FMPEGHeader.IntensityStereoBound - 1 do
          SubBands[i] := TSubbandLayer1Stereo.Create(i);
        i := FMPEGHeader.IntensityStereoBound;
        while (Cardinal(i) < NumSubBands) do
         begin
          SubBands[i] := TSubbandLayer1IntensityStereo.Create(i);
          Inc(i);
         end;
       end
      else
        for i := 0 to NumSubBands - 1 do
          SubBands[i] := TSubbandLayer1Stereo.Create(i);
     end
    else
     begin  // Layer II
      if (Mode = SingleChannel) then
        for i := 0 to NumSubBands - 1 do
          SubBands[i] := TSubbandLayer2.Create(i)
      else
      if (Mode = JointStereo) then
       begin
        for i := 0 to FMPEGHeader.IntensityStereoBound - 1 do
          SubBands[i] := TSubbandLayer2Stereo.Create(i);
        i := FMPEGHeader.IntensityStereoBound;
        while (Cardinal(i) < NumSubBands) do
         begin
          SubBands[i] := TSubbandLayer2IntensityStereo.Create(i);
          Inc(i);
         end;
       end
      else
        for i := 0 to NumSubBands - 1 do
          SubBands[i] := TSubbandLayer2Stereo.Create(i);
     end;

    // start to read audio data:
    for i := 0 to NumSubBands - 1
     do SubBands[i].ReadAllocation(FBitStream, FMPEGHeader, FCRC);
    if (FLayer = 2) then
     for i := 0 to NumSubBands - 1
      do TSubBandLayer2(SubBands[i]).ReadScaleFactorSelection(FBitStream, FCRC);

    if (FCRC = nil) or (FMPEGHeader.ChecksumOK) then
     begin // no checksums or checksum ok, continue reading from stream:
      for i := 0 to NumSubBands - 1 do
        SubBands[i].ReadScaleFactor(FBitStream, FMPEGHeader);
      repeat
        ReadReady := True;
        for i := 0 to NumSubBands - 1 do
          ReadReady := SubBands[i].ReadSampleData(FBitStream);
        repeat
          WriteReady := True;
          for i := 0 to NumSubBands - 1 do
            WriteReady := SubBands[i].PutNextSample(FWhichC, FFilter[0], FFilter[1]);
          FFilter[0].CalculatePCMSamples;
          if ((FWhichC = Both) and (Mode <> SingleChannel)) then
            FFilter[1].CalculatePCMSamples;
        until (WriteReady);
      until (ReadReady);
     end;

    for i := 0 to NumSubBands - 1
     do FreeAndNil(SubBands[i]);
   end
  else FLayer3.Decode; // Layer III
end;

function TMPEGAudio.GetBitrate: Integer;
begin
 Result := FMPEGHeader.Bitrate;
end;

function TMPEGAudio.GetChannels: TChannels;
begin
 Result := FWhichC;
end;

function TMPEGAudio.GetFrequency: Integer;
begin
 Result := FMPEGHeader.Frequency;
end;

function TMPEGAudio.GetLayer: Integer;
begin
 Result := FMPEGHeader.Layer;
end;

function TMPEGAudio.GetLength: Integer;
begin
 Result := Round(FMPEGHeader.TotalMS(FBitStream) / 1000);
end;

function TMPEGAudio.GetMode: TMode;
begin
 Result := FMPEGHeader.Mode;
end;

function TMPEGAudio.GetVersion: TVersion;
begin
 Result := FMPEGHeader.Version;
end;

function TMPEGAudio.ReadBuffer(Left, Right: PDAVSingleFixedArray;
  Size: Integer): Integer;
var
  FR : Boolean;
  i  : Integer;
begin
 Result := 0;
 FR := True;
 repeat
  if (FOPos = 0) and Assigned(FBitStream) then
   if (FR and (FBitStream.CurrentFrame + 20 < FMPEGHeader.MaxNumberOfFrames(FBitStream))) and
      (FBitStream.CurrentFrame < FMPEGHeader.MaxNumberOfFrames(FBitStream)) then
    begin
     DoDecode;
     FR := FMPEGHeader.ReadHeader(FBitStream, FCRC);
    end
   else FBuffer.Clear;
  if (Size - Result) > (FBuffer.BufferSize - FOPos)
   then i := (FBuffer.BufferSize - FOPos)
   else i := (Size - Result);
  Move(FBuffer.Output[0, FOPos], Left[Result],  i * SizeOf(Single));
  Move(FBuffer.Output[1, FOPos], Right[Result], i * SizeOf(Single));
  FOPos := FOPos + i;
  Result := Result + i;
  if FOPos >= FBuffer.BufferSize then
   begin
    FOPos := 0;
    FBuffer.Reset;
   end;
 until Result >= Size;
 assert(Result <= Size);
end;

procedure TMpegAudio.Reset;
begin
 FBitStream.Restart;
end;

end.
