unit DAV_SynthFilter;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common;

type
  TNewPCMSample = procedure(Sender: TObject; Sample: Single) of object;

  // A class for the synthesis filter bank:
  // This class does a fast downsampling from 32, 44.1 or 48 kHz to 8 kHz, if ULAW is defined.
  // Frequencies above 4 kHz are removed by ignoring higher subbands.
  TSynthesisFilter = class
  private
    FVector         : array[0..1, 0..511] of Single;
    FActualVector   : PDAV512SingleArray;     // FVector[0] or FVector[1]
    FActualWritePos : Cardinal;               // 0-15
    FSample         : array[0..31] of Single; // 32 new subband samples
    FOnNewPCMSample : TNewPCMSample;
    procedure ComputeNewVector;
    procedure ComputePCMSample;
  public
    constructor Create;
    procedure InputSample(Sample: Single; SubBandNumber: Cardinal);
    procedure CalculatePCMSamples; // calculate 32 PCM samples
    procedure Reset; // reset the synthesis filter

    property OnNewPCMSample: TNewPCMSample read FOnNewPCMSample write FOnNewPCMSample;
  end;

implementation

const
  CAnnex3B3Table: array[0..511] of Single = (
    // Note: These values are not in the same order
    // as in Annex 3-B.3 of the ISO/IEC DIS 11172-3
     0.000000000, -0.000442505,  0.003250122, -0.007003784,
     0.031082153, -0.078628540,  0.100311279, -0.572036743,
     1.144989014,  0.572036743,  0.100311279,  0.078628540,
     0.031082153,  0.007003784,  0.003250122,  0.000442505,
    -0.000015259, -0.000473022,  0.003326416, -0.007919312,
     0.030517578, -0.084182739,  0.090927124, -0.600219727,
     1.144287109,  0.543823242,  0.108856201,  0.073059082,
     0.031478882,  0.006118774,  0.003173828,  0.000396729,
    -0.000015259, -0.000534058,  0.003387451, -0.008865356,
     0.029785156, -0.089706421,  0.080688477, -0.628295898,
     1.142211914,  0.515609741,  0.116577148,  0.067520142,
     0.031738281,  0.005294800,  0.003082275,  0.000366211,
    -0.000015259, -0.000579834,  0.003433228, -0.009841919,
     0.028884888, -0.095169067,  0.069595337, -0.656219482,
     1.138763428,  0.487472534,  0.123474121,  0.061996460,
     0.031845093,  0.004486084,  0.002990723,  0.000320435,
    -0.000015259, -0.000625610,  0.003463745, -0.010848999,
     0.027801514, -0.100540161,  0.057617188, -0.683914185,
     1.133926392,  0.459472656,  0.129577637,  0.056533813,
     0.031814575,  0.003723145,  0.002899170,  0.000289917,
    -0.000015259, -0.000686646,  0.003479004, -0.011886597,
     0.026535034, -0.105819702,  0.044784546, -0.711318970,
     1.127746582,  0.431655884,  0.134887695,  0.051132202,
     0.031661987,  0.003005981,  0.002792358,  0.000259399,
    -0.000015259, -0.000747681,  0.003479004, -0.012939453,
     0.025085449, -0.110946655,  0.031082153, -0.738372803,
     1.120223999,  0.404083252,  0.139450073,  0.045837402,
     0.031387329,  0.002334595,  0.002685547,  0.000244141,
    -0.000030518, -0.000808716,  0.003463745, -0.014022827,
     0.023422241, -0.115921021,  0.016510010, -0.765029907,
     1.111373901,  0.376800537,  0.143264771,  0.040634155,
     0.031005859,  0.001693726,  0.002578735,  0.000213623,
    -0.000030518, -0.000885010,  0.003417969, -0.015121460,
     0.021575928, -0.120697021,  0.001068115, -0.791213989,
     1.101211548,  0.349868774,  0.146362305,  0.035552979,
     0.030532837,  0.001098633,  0.002456665,  0.000198364,
    -0.000030518, -0.000961304,  0.003372192, -0.016235352,
     0.019531250, -0.125259399, -0.015228271, -0.816864014,
     1.089782715,  0.323318481,  0.148773193,  0.030609131,
     0.029937744,  0.000549316,  0.002349854,  0.000167847,
    -0.000030518, -0.001037598,  0.003280640, -0.017349243,
     0.017257690, -0.129562378, -0.032379150, -0.841949463,
     1.077117920,  0.297210693,  0.150497437,  0.025817871,
     0.029281616,  0.000030518,  0.002243042,  0.000152588,
    -0.000045776, -0.001113892,  0.003173828, -0.018463135,
     0.014801025, -0.133590698, -0.050354004, -0.866363525,
     1.063217163,  0.271591187,  0.151596069,  0.021179199,
     0.028533936, -0.000442505,  0.002120972,  0.000137329,
    -0.000045776, -0.001205444,  0.003051758, -0.019577026,
     0.012115479, -0.137298584, -0.069168091, -0.890090942,
     1.048156738,  0.246505737,  0.152069092,  0.016708374,
     0.027725220, -0.000869751,  0.002014160,  0.000122070,
    -0.000061035, -0.001296997,  0.002883911, -0.020690918,
     0.009231567, -0.140670776, -0.088775635, -0.913055420,
     1.031936646,  0.221984863,  0.151962280,  0.012420654,
     0.026840210, -0.001266479,  0.001907349,  0.000106812,
    -0.000061035, -0.001388550,  0.002700806, -0.021789551,
     0.006134033, -0.143676758, -0.109161377, -0.935195923,
     1.014617920,  0.198059082,  0.151306152,  0.008316040,
     0.025909424, -0.001617432,  0.001785278,  0.000106812,
    -0.000076294, -0.001480103,  0.002487183, -0.022857666,
     0.002822876, -0.146255493, -0.130310059, -0.956481934,
     0.996246338,  0.174789429,  0.150115967,  0.004394531,
     0.024932861, -0.001937866,  0.001693726,  0.000091553,
    -0.000076294, -0.001586914,  0.002227783, -0.023910522,
    -0.000686646, -0.148422241, -0.152206421, -0.976852417,
     0.976852417,  0.152206421,  0.148422241,  0.000686646,
     0.023910522, -0.002227783,  0.001586914,  0.000076294,
    -0.000091553, -0.001693726,  0.001937866, -0.024932861,
    -0.004394531, -0.150115967, -0.174789429, -0.996246338,
     0.956481934,  0.130310059,  0.146255493, -0.002822876,
     0.022857666, -0.002487183,  0.001480103,  0.000076294,
    -0.000106812, -0.001785278,  0.001617432, -0.025909424,
    -0.008316040, -0.151306152, -0.198059082, -1.014617920,
     0.935195923,  0.109161377,  0.143676758, -0.006134033,
     0.021789551, -0.002700806,  0.001388550,  0.000061035,
    -0.000106812, -0.001907349,  0.001266479, -0.026840210,
    -0.012420654, -0.151962280, -0.221984863, -1.031936646,
     0.913055420,  0.088775635,  0.140670776, -0.009231567,
     0.020690918, -0.002883911,  0.001296997,  0.000061035,
    -0.000122070, -0.002014160,  0.000869751, -0.027725220,
    -0.016708374, -0.152069092, -0.246505737, -1.048156738,
     0.890090942,  0.069168091,  0.137298584, -0.012115479,
     0.019577026, -0.003051758,  0.001205444,  0.000045776,
    -0.000137329, -0.002120972,  0.000442505, -0.028533936,
    -0.021179199, -0.151596069, -0.271591187, -1.063217163,
     0.866363525,  0.050354004,  0.133590698, -0.014801025,
     0.018463135, -0.003173828,  0.001113892,  0.000045776,
    -0.000152588, -0.002243042, -0.000030518, -0.029281616,
    -0.025817871, -0.150497437, -0.297210693, -1.077117920,
     0.841949463,  0.032379150,  0.129562378, -0.017257690,
     0.017349243, -0.003280640,  0.001037598,  0.000030518,
    -0.000167847, -0.002349854, -0.000549316, -0.029937744,
    -0.030609131, -0.148773193, -0.323318481, -1.089782715,
     0.816864014,  0.015228271,  0.125259399, -0.019531250,
     0.016235352, -0.003372192,  0.000961304,  0.000030518,
    -0.000198364, -0.002456665, -0.001098633, -0.030532837,
    -0.035552979, -0.146362305, -0.349868774, -1.101211548,
     0.791213989, -0.001068115,  0.120697021, -0.021575928,
     0.015121460, -0.003417969,  0.000885010,  0.000030518,
    -0.000213623, -0.002578735, -0.001693726, -0.031005859,
    -0.040634155, -0.143264771, -0.376800537, -1.111373901,
     0.765029907, -0.016510010,  0.115921021, -0.023422241,
     0.014022827, -0.003463745,  0.000808716,  0.000030518,
    -0.000244141, -0.002685547, -0.002334595, -0.031387329,
    -0.045837402, -0.139450073, -0.404083252, -1.120223999,
     0.738372803, -0.031082153,  0.110946655, -0.025085449,
     0.012939453, -0.003479004,  0.000747681,  0.000015259,
    -0.000259399, -0.002792358, -0.003005981, -0.031661987,
    -0.051132202, -0.134887695, -0.431655884, -1.127746582,
     0.711318970, -0.044784546,  0.105819702, -0.026535034,
     0.011886597, -0.003479004,  0.000686646,  0.000015259,
    -0.000289917, -0.002899170, -0.003723145, -0.031814575,
    -0.056533813, -0.129577637, -0.459472656, -1.133926392,
     0.683914185, -0.057617188,  0.100540161, -0.027801514,
     0.010848999, -0.003463745,  0.000625610,  0.000015259,
    -0.000320435, -0.002990723, -0.004486084, -0.031845093,
    -0.061996460, -0.123474121, -0.487472534, -1.138763428,
     0.656219482, -0.069595337,  0.095169067, -0.028884888,
     0.009841919, -0.003433228,  0.000579834,  0.000015259,
    -0.000366211, -0.003082275, -0.005294800, -0.031738281,
    -0.067520142, -0.116577148, -0.515609741, -1.142211914,
     0.628295898, -0.080688477,  0.089706421, -0.029785156,
     0.008865356, -0.003387451,  0.000534058,  0.000015259,
    -0.000396729, -0.003173828, -0.006118774, -0.031478882,
    -0.073059082, -0.108856201, -0.543823242, -1.144287109,
     0.600219727, -0.090927124,  0.084182739, -0.030517578,
     0.007919312, -0.003326416,  0.000473022,  0.000015259);

var
  GCos1_64, GCos3_64, GCos5_64, GCos7_64, GCos9_64, GCos11_64, GCos13_64, GCos15_64,
  GCos17_64, GCos19_64, GCos21_64, GCos23_64, GCos25_64, GCos27_64, GCos29_64,
  GCos31_64, GCos1_32, GCos3_32, GCos5_32, GCos7_32, GCos9_32, GCos11_32, GCos13_32,
  GCos15_32, GCos1_16, GCos3_16, GCos5_16, GCos7_16, GCos1_8, GCos3_8, GCos1_4: Single;

{ TSynthesisFilter }

constructor TSynthesisFilter.Create;
begin
 Reset;
end;

procedure TSynthesisFilter.CalculatePCMSamples;
begin
 ComputeNewVector;
 ComputePCMSample;
 FActualWritePos := (FActualWritePos + 1) and $F;
 if (FActualVector = @FVector[0])
  then FActualVector := @FVector[1]
  else FActualVector := @FVector[0];
 FillChar(FSample, Sizeof(FSample), 0);
end;

procedure TSynthesisFilter.ComputeNewVector;
var
  new_v  : array[0..31] of Single;  // new V[0-15] and V[33-48] of Figure 3-A.2 in ISO DIS 11172-3
  p      : array[0..15] of Single;
  pp     : array[0..15] of Single;
  x1, x2 : PDAV512SingleArray;
  tmp    : array [0..1] of Single;
begin
 // compute new values via a fast cosine transform:
 x1 := @FSample;

 p[ 0] := x1[ 0] + x1[31];
 p[ 1] := x1[ 1] + x1[30];
 p[ 2] := x1[ 2] + x1[29];
 p[ 3] := x1[ 3] + x1[28];
 p[ 4] := x1[ 4] + x1[27];
 p[ 5] := x1[ 5] + x1[26];
 p[ 6] := x1[ 6] + x1[25];
 p[ 7] := x1[ 7] + x1[24];
 p[ 8] := x1[ 8] + x1[23];
 p[ 9] := x1[ 9] + x1[22];
 p[10] := x1[10] + x1[21];
 p[11] := x1[11] + x1[20];
 p[12] := x1[12] + x1[19];
 p[13] := x1[13] + x1[18];
 p[14] := x1[14] + x1[17];
 p[15] := x1[15] + x1[16];

 pp[ 0] :=  p[0] + p[15];
 pp[ 1] :=  p[1] + p[14];
 pp[ 2] :=  p[2] + p[13];
 pp[ 3] :=  p[3] + p[12];
 pp[ 4] :=  p[4] + p[11];
 pp[ 5] :=  p[5] + p[10];
 pp[ 6] :=  p[6] + p[ 9];
 pp[ 7] :=  p[7] + p[ 8];
 pp[ 8] := (p[0] - p[15]) * GCos1_32;
 pp[ 9] := (p[1] - p[14]) * GCos3_32;
 pp[10] := (p[2] - p[13]) * GCos5_32;
 pp[11] := (p[3] - p[12]) * GCos7_32;
 pp[12] := (p[4] - p[11]) * GCos9_32;
 pp[13] := (p[5] - p[10]) * GCos11_32;
 pp[14] := (p[6] - p[ 9]) * GCos13_32;
 pp[15] := (p[7] - p[ 8]) * GCos15_32;

 p[ 0] :=  pp[0] + pp[7];
 p[ 1] :=  pp[1] + pp[6];
 p[ 2] :=  pp[2] + pp[5];
 p[ 3] :=  pp[3] + pp[4];
 p[ 4] := (pp[0] - pp[7]) * GCos1_16;
 p[ 5] := (pp[1] - pp[6]) * GCos3_16;
 p[ 6] := (pp[2] - pp[5]) * GCos5_16;
 p[ 7] := (pp[3] - pp[4]) * GCos7_16;
 p[ 8] :=  pp[8] + pp[15];
 p[ 9] :=  pp[9] + pp[14];
 p[10] :=  pp[10] + pp[13];
 p[11] :=  pp[11] + pp[12];
 p[12] := (pp[8] - pp[15]) * GCos1_16;
 p[13] := (pp[9] - pp[14]) * GCos3_16;
 p[14] := (pp[10] - pp[13]) * GCos5_16;
 p[15] := (pp[11] - pp[12]) * GCos7_16;

 pp[ 0] :=  p[0] + p[3];
 pp[ 1] :=  p[1] + p[2];
 pp[ 2] := (p[0] - p[3]) * GCos1_8;
 pp[ 3] := (p[1] - p[2]) * GCos3_8;
 pp[ 4] :=  p[4] + p[7];
 pp[ 5] :=  p[5] + p[6];
 pp[ 6] := (p[4] - p[7]) * GCos1_8;
 pp[ 7] := (p[5] - p[6]) * GCos3_8;
 pp[ 8] :=  p[8] + p[11];
 pp[ 9] :=  p[9] + p[10];
 pp[10] := (p[8] - p[11]) * GCos1_8;
 pp[11] := (p[9] - p[10]) * GCos3_8;
 pp[12] :=  p[12] + p[15];
 pp[13] :=  p[13] + p[14];
 pp[14] := (p[12] - p[15]) * GCos1_8;
 pp[15] := (p[13] - p[14]) * GCos3_8;

 p[ 0] :=  pp[0] + pp[1];
 p[ 1] := (pp[0] - pp[1]) * GCos1_4;
 p[ 2] :=  pp[2] + pp[3];
 p[ 3] := (pp[2] - pp[3]) * GCos1_4;
 p[ 4] :=  pp[4] + pp[5];
 p[ 5] := (pp[4] - pp[5]) * GCos1_4;
 p[ 6] :=  pp[6] + pp[7];
 p[ 7] := (pp[6] - pp[7]) * GCos1_4;
 p[ 8] :=  pp[8] + pp[9];
 p[ 9] := (pp[8] - pp[9]) * GCos1_4;
 p[10] :=  pp[10] + pp[11];
 p[11] := (pp[10] - pp[11]) * GCos1_4;
 p[12] :=  pp[12] + pp[13];
 p[13] := (pp[12] - pp[13]) * GCos1_4;
 p[14] :=  pp[14] + pp[15];
 p[15] := (pp[14] - pp[15]) * GCos1_4;

 new_v[12] := p[7];
 new_v[4] := new_v[12] + p[5];
 new_v[36-17] := -new_v[4] - p[6];
 new_v[44-17] := -p[6] - p[7] - p[4];
 new_v[14] := p[15];
 new_v[10] := new_v[14] + p[11];
 new_v[6] := new_v[10] + p[13];
 new_v[2] := p[15] + p[13] + p[9];
 new_v[34-17] := -new_v[2] - p[14];
 tmp[0] := -p[14] - p[15] - p[10] - p[11];
 new_v[38-17] := tmp[0] - p[13];
 new_v[46-17] := -p[14] - p[15] - p[12] - p[8];
 new_v[42-17] := tmp[0] - p[12];
 new_v[48-17] := -p[0];
 new_v[0] := p[1];
 new_v[8] := p[3];
 new_v[40-17] := -new_v[8] - p[2];

 p[ 0] := (x1[ 0] - x1[31]) * GCos1_64;
 p[ 1] := (x1[ 1] - x1[30]) * GCos3_64;
 p[ 2] := (x1[ 2] - x1[29]) * GCos5_64;
 p[ 3] := (x1[ 3] - x1[28]) * GCos7_64;
 p[ 4] := (x1[ 4] - x1[27]) * GCos9_64;
 p[ 5] := (x1[ 5] - x1[26]) * GCos11_64;
 p[ 6] := (x1[ 6] - x1[25]) * GCos13_64;
 p[ 7] := (x1[ 7] - x1[24]) * GCos15_64;
 p[ 8] := (x1[ 8] - x1[23]) * GCos17_64;
 p[ 9] := (x1[ 9] - x1[22]) * GCos19_64;
 p[10] := (x1[10] - x1[21]) * GCos21_64;
 p[11] := (x1[11] - x1[20]) * GCos23_64;
 p[12] := (x1[12] - x1[19]) * GCos25_64;
 p[13] := (x1[13] - x1[18]) * GCos27_64;
 p[14] := (x1[14] - x1[17]) * GCos29_64;
 p[15] := (x1[15] - x1[16]) * GCos31_64;

 pp[0] := p[0] + p[15];
 pp[1] := p[1] + p[14];
 pp[2] := p[2] + p[13];
 pp[3] := p[3] + p[12];
 pp[4] := p[4] + p[11];
 pp[5] := p[5] + p[10];
 pp[6] := p[6] + p[9];
 pp[7] := p[7] + p[8];
 pp[8] := (p[0] - p[15]) * GCos1_32;
 pp[9] := (p[1] - p[14]) * GCos3_32;
 pp[10] := (p[2] - p[13]) * GCos5_32;
 pp[11] := (p[3] - p[12]) * GCos7_32;
 pp[12] := (p[4] - p[11]) * GCos9_32;
 pp[13] := (p[5] - p[10]) * GCos11_32;
 pp[14] := (p[6] - p[9]) * GCos13_32;
 pp[15] := (p[7] - p[8]) * GCos15_32;

 p[0] := pp[0] + pp[7];
 p[1] := pp[1] + pp[6];
 p[2] := pp[2] + pp[5];
 p[3] := pp[3] + pp[4];
 p[4] := (pp[0] - pp[7]) * GCos1_16;
 p[5] := (pp[1] - pp[6]) * GCos3_16;
 p[6] := (pp[2] - pp[5]) * GCos5_16;
 p[7] := (pp[3] - pp[4]) * GCos7_16;
 p[8] := pp[8] + pp[15];
 p[9] := pp[9] + pp[14];
 p[10] := pp[10] + pp[13];
 p[11] := pp[11] + pp[12];
 p[12] := (pp[8] - pp[15]) * GCos1_16;
 p[13] := (pp[9] - pp[14]) * GCos3_16;
 p[14] := (pp[10] - pp[13]) * GCos5_16;
 p[15] := (pp[11] - pp[12]) * GCos7_16;

 pp[0] := p[0] + p[3];
 pp[1] := p[1] + p[2];
 pp[2] := (p[0] - p[3]) * GCos1_8;
 pp[3] := (p[1] - p[2]) * GCos3_8;
 pp[4] := p[4] + p[7];
 pp[5] := p[5] + p[6];
 pp[6] := (p[4] - p[7]) * GCos1_8;
 pp[7] := (p[5] - p[6]) * GCos3_8;
 pp[8] := p[8] + p[11];
 pp[9] := p[9] + p[10];
 pp[10] := (p[8] - p[11]) * GCos1_8;
 pp[11] := (p[9] - p[10]) * GCos3_8;
 pp[12] := p[12] + p[15];
 pp[13] := p[13] + p[14];
 pp[14] := (p[12] - p[15]) * GCos1_8;
 pp[15] := (p[13] - p[14]) * GCos3_8;

 p[0] := pp[0] + pp[1];
 p[1] := (pp[0] - pp[1]) * GCos1_4;
 p[2] := pp[2] + pp[3];
 p[3] := (pp[2] - pp[3]) * GCos1_4;
 p[4] := pp[4] + pp[5];
 p[5] := (pp[4] - pp[5]) * GCos1_4;
 p[6] := pp[6] + pp[7];
 p[7] := (pp[6] - pp[7]) * GCos1_4;
 p[8] := pp[8] + pp[9];
 p[9] := (pp[8] - pp[9]) * GCos1_4;
 p[10] := pp[10] + pp[11];
 p[11] := (pp[10] - pp[11]) * GCos1_4;
 p[12] := pp[12] + pp[13];
 p[13] := (pp[12] - pp[13]) * GCos1_4;
 p[14] := pp[14] + pp[15];
 p[15] := (pp[14] - pp[15]) * GCos1_4;

 new_v[15] := p[15];
 new_v[13] := new_v[15] + p[7];
 new_v[11] := new_v[13] + p[11];
 new_v[5] := new_v[11] + p[5] + p[13];
 new_v[9] := p[15] + p[11] + p[3];
 new_v[7] := new_v[9] + p[13];
 tmp[0] := p[13] + p[15] + p[9];
 new_v[1] := tmp[0] + p[1];
 new_v[33-17] := -new_v[1]- p[14];
 new_v[3] := tmp[0] + p[5] + p[7];
 new_v[35-17] := -new_v[3] - p[6] - p[14];

 tmp[0] := -p[10] - p[11] - p[14] - p[15];
 new_v[39-17] := tmp[0] - p[13] - p[2] - p[3];
 new_v[37-17] := tmp[0] - p[13] - p[5] - p[6] - p[7];
 new_v[41-17] := tmp[0] - p[12] - p[2] - p[3];
 tmp[1] := p[4] + p[6] + p[7];
 new_v[43-17] := tmp[0] - p[12] - tmp[1];
 tmp[0] := -p[8] - p[12] - p[14] - p[15];
 new_v[47-17] := tmp[0] - p[0];
 new_v[45-17] := tmp[0] - tmp[1];

 // insert V[0-15] (== new_v[0-15]) into actual v:
 x1 := @new_v;
 x2 := @FActualVector[FActualWritePos];
 x2[0] := x1[0];
 x2[16] := x1[1];
 x2[32] := x1[2];
 x2[48] := x1[3];
 x2[64] := x1[4];
 x2[80] := x1[5];
 x2[96] := x1[6];
 x2[112] := x1[7];
 x2[128] := x1[8];
 x2[144] := x1[9];
 x2[160] := x1[10];
 x2[176] := x1[11];
 x2[192] := x1[12];
 x2[208] := x1[13];
 x2[224] := x1[14];
 x2[240] := x1[15];

 // V[16] is always 0.0:
 x2[256] := 0.0;

 // insert V[17-31] (== -new_v[15-1]) into actual v:
 x2[272] := -x1[15];
 x2[288] := -x1[14];
 x2[304] := -x1[13];
 x2[320] := -x1[12];
 x2[336] := -x1[11];
 x2[352] := -x1[10];
 x2[368] := -x1[9];
 x2[384] := -x1[8];
 x2[400] := -x1[7];
 x2[416] := -x1[6];
 x2[432] := -x1[5];
 x2[448] := -x1[4];
 x2[464] := -x1[3];
 x2[480] := -x1[2];
 x2[496] := -x1[1];

 // insert V[32] (== -new_v[0]) into other v:
 if (FActualVector = @FVector[0])
  then x2 := @FVector[1][FActualWritePos]
  else x2 := @FVector[0][FActualWritePos];

 x2[0] := -x1[0];

 // insert V[33-48] (== new_v[16-31]) into other v:
 x2[16] := x1[16];
 x2[32] := x1[17];
 x2[48] := x1[18];
 x2[64] := x1[19];
 x2[80] := x1[20];
 x2[96] := x1[21];
 x2[112] := x1[22];
 x2[128] := x1[23];
 x2[144] := x1[24];
 x2[160] := x1[25];
 x2[176] := x1[26];
 x2[192] := x1[27];
 x2[208] := x1[28];
 x2[224] := x1[29];
 x2[240] := x1[30];
 x2[256] := x1[31];

 // insert V[49-63] (==new_v[30-16]) into other v:
 x2[272] := x1[30];
 x2[288] := x1[29];
 x2[304] := x1[28];
 x2[320] := x1[27];
 x2[336] := x1[26];
 x2[352] := x1[25];
 x2[368] := x1[24];
 x2[384] := x1[23];
 x2[400] := x1[22];
 x2[416] := x1[21];
 x2[432] := x1[20];
 x2[448] := x1[19];
 x2[464] := x1[18];
 x2[480] := x1[17];
 x2[496] := x1[16];
end;

procedure TSynthesisFilter.ComputePCMSample;
var
  vp          : PDAV512SingleArray;
  Coefficient : PDAV512SingleArray;
  PcmSample   : Single;
begin
 if not assigned(FOnNewPCMSample) then exit;

 vp := FActualVector;
 case FActualWritePos of
  0: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[ 0] * Coefficient[ 0]) + (vp[15] * Coefficient[ 1]) +
                      (vp[14] * Coefficient[ 2]) + (vp[13] * Coefficient[ 3]) +
                      (vp[12] * Coefficient[ 4]) + (vp[11] * Coefficient[ 5]) +
                      (vp[10] * Coefficient[ 6]) + (vp[ 9] * Coefficient[ 7]) +
                      (vp[ 8] * Coefficient[ 8]) + (vp[ 7] * Coefficient[ 9]) +
                      (vp[ 6] * Coefficient[10]) + (vp[ 5] * Coefficient[11]) +
                      (vp[ 4] * Coefficient[12]) + (vp[ 3] * Coefficient[13]) +
                      (vp[ 2] * Coefficient[14]) + (vp[ 1] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

  1: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[ 1] * Coefficient[ 0]) + (vp[ 0] * Coefficient[ 1]) +
                      (vp[15] * Coefficient[ 2]) + (vp[14] * Coefficient[ 3]) +
                      (vp[13] * Coefficient[ 4]) + (vp[12] * Coefficient[ 5]) +
                      (vp[11] * Coefficient[ 6]) + (vp[10] * Coefficient[ 7]) +
                      (vp[ 9] * Coefficient[ 8]) + (vp[ 8] * Coefficient[ 9]) +
                      (vp[ 7] * Coefficient[10]) + (vp[ 6] * Coefficient[11]) +
                      (vp[ 5] * Coefficient[12]) + (vp[ 4] * Coefficient[13]) +
                      (vp[ 3] * Coefficient[14]) + (vp[ 2] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

  2: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[ 2] * Coefficient[ 0]) + (vp[ 1] * Coefficient[ 1]) +
                      (vp[ 0] * Coefficient[ 2]) + (vp[15] * Coefficient[ 3]) +
                      (vp[14] * Coefficient[ 4]) + (vp[13] * Coefficient[ 5]) +
                      (vp[12] * Coefficient[ 6]) + (vp[11] * Coefficient[ 7]) +
                      (vp[10] * Coefficient[ 8]) + (vp[ 9] * Coefficient[ 9]) +
                      (vp[ 8] * Coefficient[10]) + (vp[ 7] * Coefficient[11]) +
                      (vp[ 6] * Coefficient[12]) + (vp[ 5] * Coefficient[13]) +
                      (vp[ 4] * Coefficient[14]) + (vp[ 3] * Coefficient[15])
                );
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

  3: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[ 3] * Coefficient[ 0]) + (vp[ 2] * Coefficient[ 1]) +
                      (vp[ 1] * Coefficient[ 2]) + (vp[ 0] * Coefficient[ 3]) +
                      (vp[15] * Coefficient[ 4]) + (vp[14] * Coefficient[ 5]) +
                      (vp[13] * Coefficient[ 6]) + (vp[12] * Coefficient[ 7]) +
                      (vp[11] * Coefficient[ 8]) + (vp[10] * Coefficient[ 9]) +
                      (vp[ 9] * Coefficient[10]) + (vp[ 8] * Coefficient[11]) +
                      (vp[ 7] * Coefficient[12]) + (vp[ 6] * Coefficient[13]) +
                      (vp[ 5] * Coefficient[14]) + (vp[ 4] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

  4: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[ 4] * Coefficient[ 0]) + (vp[ 3] * Coefficient[ 1]) +
                      (vp[ 2] * Coefficient[ 2]) + (vp[ 1] * Coefficient[ 3]) +
                      (vp[ 0] * Coefficient[ 4]) + (vp[15] * Coefficient[ 5]) +
                      (vp[14] * Coefficient[ 6]) + (vp[13] * Coefficient[ 7]) +
                      (vp[12] * Coefficient[ 8]) + (vp[11] * Coefficient[ 9]) +
                      (vp[10] * Coefficient[10]) + (vp[ 9] * Coefficient[11]) +
                      (vp[ 8] * Coefficient[12]) + (vp[ 7] * Coefficient[13]) +
                      (vp[ 6] * Coefficient[14]) + (vp[ 5] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

  5: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[ 5] * Coefficient[ 0]) + (vp[ 4] * Coefficient[ 1]) +
                      (vp[ 3] * Coefficient[ 2]) + (vp[ 2] * Coefficient[ 3]) +
                      (vp[ 1] * Coefficient[ 4]) + (vp[ 0] * Coefficient[ 5]) +
                      (vp[15] * Coefficient[ 6]) + (vp[14] * Coefficient[ 7]) +
                      (vp[13] * Coefficient[ 8]) + (vp[12] * Coefficient[ 9]) +
                      (vp[11] * Coefficient[10]) + (vp[10] * Coefficient[11]) +
                      (vp[ 9] * Coefficient[12]) + (vp[ 8] * Coefficient[13]) +
                      (vp[ 7] * Coefficient[14]) + (vp[ 6] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

  6: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[ 6] * Coefficient[ 0]) + (vp[ 5] * Coefficient[ 1]) +
                      (vp[ 4] * Coefficient[ 2]) + (vp[ 3] * Coefficient[ 3]) +
                      (vp[ 2] * Coefficient[ 4]) + (vp[ 1] * Coefficient[ 5]) +
                      (vp[ 0] * Coefficient[ 6]) + (vp[15] * Coefficient[ 7]) +
                      (vp[14] * Coefficient[ 8]) + (vp[13] * Coefficient[ 9]) +
                      (vp[12] * Coefficient[10]) + (vp[11] * Coefficient[11]) +
                      (vp[10] * Coefficient[12]) + (vp[ 9] * Coefficient[13]) +
                      (vp[ 8] * Coefficient[14]) + (vp[ 7] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

  7: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[ 7] * Coefficient[ 0]) + (vp[ 6] * Coefficient[ 1]) +
                      (vp[ 5] * Coefficient[ 2]) + (vp[ 4] * Coefficient[ 3]) +
                      (vp[ 3] * Coefficient[ 4]) + (vp[ 2] * Coefficient[ 5]) +
                      (vp[ 1] * Coefficient[ 6]) + (vp[ 0] * Coefficient[ 7]) +
                      (vp[15] * Coefficient[ 8]) + (vp[14] * Coefficient[ 9]) +
                      (vp[13] * Coefficient[10]) + (vp[12] * Coefficient[11]) +
                      (vp[11] * Coefficient[12]) + (vp[10] * Coefficient[13]) +
                      (vp[ 9] * Coefficient[14]) + (vp[ 8] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

  8: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[ 8] * Coefficient[ 0]) + (vp[ 7] * Coefficient[ 1]) +
                      (vp[ 6] * Coefficient[ 2]) + (vp[ 5] * Coefficient[ 3]) +
                      (vp[ 4] * Coefficient[ 4]) + (vp[ 3] * Coefficient[ 5]) +
                      (vp[ 2] * Coefficient[ 6]) + (vp[ 1] * Coefficient[ 7]) +
                      (vp[ 0] * Coefficient[ 8]) + (vp[15] * Coefficient[ 9]) +
                      (vp[14] * Coefficient[10]) + (vp[13] * Coefficient[11]) +
                      (vp[12] * Coefficient[12]) + (vp[11] * Coefficient[13]) +
                      (vp[10] * Coefficient[14]) + (vp[ 9] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

  9: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[ 9] * Coefficient[ 0]) + (vp[ 8] * Coefficient[ 1]) +
                      (vp[ 7] * Coefficient[ 2]) + (vp[ 6] * Coefficient[ 3]) +
                      (vp[ 5] * Coefficient[ 4]) + (vp[ 4] * Coefficient[ 5]) +
                      (vp[ 3] * Coefficient[ 6]) + (vp[ 2] * Coefficient[ 7]) +
                      (vp[ 1] * Coefficient[ 8]) + (vp[ 0] * Coefficient[ 9]) +
                      (vp[15] * Coefficient[10]) + (vp[14] * Coefficient[11]) +
                      (vp[13] * Coefficient[12]) + (vp[12] * Coefficient[13]) +
                      (vp[11] * Coefficient[14]) + (vp[10] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

 10: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[10] * Coefficient[ 0]) + (vp[ 9] * Coefficient[ 1]) +
                      (vp[ 8] * Coefficient[ 2]) + (vp[ 7] * Coefficient[ 3]) +
                      (vp[ 6] * Coefficient[ 4]) + (vp[ 5] * Coefficient[ 5]) +
                      (vp[ 4] * Coefficient[ 6]) + (vp[ 3] * Coefficient[ 7]) +
                      (vp[ 2] * Coefficient[ 8]) + (vp[ 1] * Coefficient[ 9]) +
                      (vp[ 0] * Coefficient[10]) + (vp[15] * Coefficient[11]) +
                      (vp[14] * Coefficient[12]) + (vp[13] * Coefficient[13]) +
                      (vp[12] * Coefficient[14]) + (vp[11] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

 11: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[11] * Coefficient[ 0]) + (vp[10] * Coefficient[ 1]) +
                      (vp[ 9] * Coefficient[ 2]) + (vp[ 8] * Coefficient[ 3]) +
                      (vp[ 7] * Coefficient[ 4]) + (vp[ 6] * Coefficient[ 5]) +
                      (vp[ 5] * Coefficient[ 6]) + (vp[ 4] * Coefficient[ 7]) +
                      (vp[ 3] * Coefficient[ 8]) + (vp[ 2] * Coefficient[ 9]) +
                      (vp[ 1] * Coefficient[10]) + (vp[ 0] * Coefficient[11]) +
                      (vp[15] * Coefficient[12]) + (vp[14] * Coefficient[13]) +
                      (vp[13] * Coefficient[14]) + (vp[12] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

 12: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[12] * Coefficient[ 0]) + (vp[11] * Coefficient[ 1]) +
                      (vp[10] * Coefficient[ 2]) + (vp[ 9] * Coefficient[ 3]) +
                      (vp[ 8] * Coefficient[ 4]) + (vp[ 7] * Coefficient[ 5]) +
                      (vp[ 6] * Coefficient[ 6]) + (vp[ 5] * Coefficient[ 7]) +
                      (vp[ 4] * Coefficient[ 8]) + (vp[ 3] * Coefficient[ 9]) +
                      (vp[ 2] * Coefficient[10]) + (vp[ 1] * Coefficient[11]) +
                      (vp[ 0] * Coefficient[12]) + (vp[15] * Coefficient[13]) +
                      (vp[14] * Coefficient[14]) + (vp[13] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
    end;

 13: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[13] * Coefficient[ 0]) + (vp[12] * Coefficient[ 1]) +
                      (vp[11] * Coefficient[ 2]) + (vp[10] * Coefficient[ 3]) +
                      (vp[ 9] * Coefficient[ 4]) + (vp[ 8] * Coefficient[ 5]) +
                      (vp[ 7] * Coefficient[ 6]) + (vp[ 6] * Coefficient[ 7]) +
                      (vp[ 5] * Coefficient[ 8]) + (vp[ 4] * Coefficient[ 9]) +
                      (vp[ 3] * Coefficient[10]) + (vp[ 2] * Coefficient[11]) +
                      (vp[ 1] * Coefficient[12]) + (vp[ 0] * Coefficient[13]) +
                      (vp[15] * Coefficient[14]) + (vp[14] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

 14: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[14] * Coefficient[ 0]) + (vp[13] * Coefficient[ 1]) +
                      (vp[12] * Coefficient[ 2]) + (vp[11] * Coefficient[ 3]) +
                      (vp[10] * Coefficient[ 4]) + (vp[ 9] * Coefficient[ 5]) +
                      (vp[ 8] * Coefficient[ 6]) + (vp[ 7] * Coefficient[ 7]) +
                      (vp[ 6] * Coefficient[ 8]) + (vp[ 5] * Coefficient[ 9]) +
                      (vp[ 4] * Coefficient[10]) + (vp[ 3] * Coefficient[11]) +
                      (vp[ 2] * Coefficient[12]) + (vp[ 1] * Coefficient[13]) +
                      (vp[ 0] * Coefficient[14]) + (vp[15] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;

 15: begin
      Coefficient := @CAnnex3B3Table;
      while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + 2048) do
       begin
        PcmSample := ((vp[15] * Coefficient[ 0]) + (vp[14] * Coefficient[ 1]) +
                      (vp[13] * Coefficient[ 2]) + (vp[12] * Coefficient[ 3]) +
                      (vp[11] * Coefficient[ 4]) + (vp[10] * Coefficient[ 5]) +
                      (vp[ 9] * Coefficient[ 6]) + (vp[ 8] * Coefficient[ 7]) +
                      (vp[ 7] * Coefficient[ 8]) + (vp[ 6] * Coefficient[ 9]) +
                      (vp[ 5] * Coefficient[10]) + (vp[ 4] * Coefficient[11]) +
                      (vp[ 3] * Coefficient[12]) + (vp[ 2] * Coefficient[13]) +
                      (vp[ 1] * Coefficient[14]) + (vp[ 0] * Coefficient[15]));
        FOnNewPCMSample(Self, PcmSample);
        Inc(Cardinal(Coefficient), 64);
        Inc(Cardinal(vp), 64);
       end;
     end;
  end;
end;

procedure TSynthesisFilter.InputSample(Sample: Single; SubBandNumber: Cardinal);
begin
 FSample[SubBandNumber] := Sample;
end;

procedure TSynthesisFilter.Reset;
begin
 FillChar(FVector[0], SizeOf(FVector[0]), 0);
 FillChar(FVector[1], SizeOf(FVector[1]), 0);
 FillChar(FSample, SizeOf(FSample), 0);
 FActualVector := @FVector[0];
 FActualWritePos := 15;
end;

const
  COne64th = 1 / 64;
  COne32th = 1 / 32;
  COne16th = 1 / 16;
  COne8th  = 1 /  8;

begin
 GCos1_64  := 1.0 / (2.0 * cos(Pi        * COne64th));
 GCos3_64  := 1.0 / (2.0 * cos(Pi * 3.0  * COne64th));
 GCos5_64  := 1.0 / (2.0 * cos(Pi * 5.0  * COne64th));
 GCos7_64  := 1.0 / (2.0 * cos(Pi * 7.0  * COne64th));
 GCos9_64  := 1.0 / (2.0 * cos(Pi * 9.0  * COne64th));
 GCos11_64 := 1.0 / (2.0 * cos(Pi * 11.0 * COne64th));
 GCos13_64 := 1.0 / (2.0 * cos(Pi * 13.0 * COne64th));
 GCos15_64 := 1.0 / (2.0 * cos(Pi * 15.0 * COne64th));
 GCos17_64 := 1.0 / (2.0 * cos(Pi * 17.0 * COne64th));
 GCos19_64 := 1.0 / (2.0 * cos(Pi * 19.0 * COne64th));
 GCos21_64 := 1.0 / (2.0 * cos(Pi * 21.0 * COne64th));
 GCos23_64 := 1.0 / (2.0 * cos(Pi * 23.0 * COne64th));
 GCos25_64 := 1.0 / (2.0 * cos(Pi * 25.0 * COne64th));
 GCos27_64 := 1.0 / (2.0 * cos(Pi * 27.0 * COne64th));
 GCos29_64 := 1.0 / (2.0 * cos(Pi * 29.0 * COne64th));
 GCos31_64 := 1.0 / (2.0 * cos(Pi * 31.0 * COne64th));
 GCos1_32  := 1.0 / (2.0 * cos(Pi        * COne32th));
 GCos3_32  := 1.0 / (2.0 * cos(Pi * 3.0  * COne32th));
 GCos5_32  := 1.0 / (2.0 * cos(Pi * 5.0  * COne32th));
 GCos7_32  := 1.0 / (2.0 * cos(Pi * 7.0  * COne32th));
 GCos9_32  := 1.0 / (2.0 * cos(Pi * 9.0  * COne32th));
 GCos11_32 := 1.0 / (2.0 * cos(Pi * 11.0 * COne32th));
 GCos13_32 := 1.0 / (2.0 * cos(Pi * 13.0 * COne32th));
 GCos15_32 := 1.0 / (2.0 * cos(Pi * 15.0 * COne32th));
 GCos1_16  := 1.0 / (2.0 * cos(Pi        * COne16th));
 GCos3_16  := 1.0 / (2.0 * cos(Pi * 3.0  * COne16th));
 GCos5_16  := 1.0 / (2.0 * cos(Pi * 5.0  * COne16th));
 GCos7_16  := 1.0 / (2.0 * cos(Pi * 7.0  * COne16th));
 GCos1_8   := 1.0 / (2.0 * cos(Pi        * COne8th));
 GCos3_8   := 1.0 / (2.0 * cos(Pi * 3.0  * COne8th));
 GCos1_4   := 1.0 / (2.0 * cos(Pi / 4.0));
end.
