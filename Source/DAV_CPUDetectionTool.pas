unit DAV_CPUDetectionTool;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this Project is Christian-W. Budde               //
//                                                                            //
//  The code in this unit was donated by Chris Holton (?)                     //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  SysUtils;

type
  TCPUVendor = (cvUnknown, cvAMD, cvCentaur, cvCyrix, cvIntel, cvTransmeta,
                cvNexGen, cvRise, cvUMC, cvNSC, cvSiS);

  TCPUInstruction = (isFPU, isTSC, isCX8, isSEP, isCMOV, isMMX, isFXSR, isSSE, isSSE2, isSSE3,
                     isMONITOR, isCX16, isX64, isExMMX, isEx3DNow, is3DNow);

  TCPUInstructions = set of TCPUInstruction;

  TCPU = class
  private
    FVendor           : TCPUVendor;
    FSignature        : Cardinal;
    FEffFamily        : Byte;
    FEffModel         : Byte;
    FCodeL1CacheSize  : Word;
    FDataL1CacheSize  : Word;
    FL2CacheSize      : Word;
    FL3CacheSize      : Word;
    FInstructions     : TCPUInstructions;
    procedure GetCPUInfo;
    procedure GetCPUVendor;
    procedure GetCPUFeatures;
    procedure GetCPUExtendedFeatures;
    procedure GetProcessorCacheInfo;
    procedure GetExtendedProcessorCacheInfo;
    procedure VerifyOSSupportForXMMRegisters;
  public
    constructor Create;
  published
    property Vendor: TCPUVendor read FVendor;
    property Signature: Cardinal read FSignature;
    property EffFamily: Byte read FEffFamily;
    property EffModel: Byte read FEffModel;
    property CodeL1CacheSize: Word read FCodeL1CacheSize;
    property DataL1CacheSize: Word read FDataL1CacheSize;
    property L2CacheSize: Word read FL2CacheSize;
    property L3CacheSize: Word read fL3CacheSize;
    property Instructions: TCPUInstructions read FInstructions;
  end;

const 
  CVendorStr: array[Low(TCPUVendor)..High(TCPUVendor)] of ShortString =
    ('Unknown', 'AMD', 'Centaur (VIA)', 'Cyrix', 'Intel', 'Transmeta',
     'NexGen', 'Rise', 'UMC', 'National Semiconductor', 'SiS');

      InstructionSupportStr: array[Low(TCPUInstruction)..High(TCPUInstruction)] of ShortString =
       ('FPU', 'TSC', 'CX8', 'SEP', 'CMOV', 'MMX', 'FXSR', 'SSE', 'SSE2',
        'SSE3', 'MONITOR', 'CX16', 'X64', 'MMX+', '3DNow!+', '3DNow!');

var DetectedCPU: TCPU;

implementation

type
  TRegisters = record EAX, EBX, ECX, EDX: Cardinal;  end;
  TVendorStr = string[12];
  TCpuFeatures = ( cfFPU, cfVME, cfDE, cfPSE, cfTSC, cfMSR, cfPAE, cfMCE,
                   cfCX8, cfAPIC, cf_d10, cfSEP, cfMTRR, cfPGE, cfMCA, cfCMOV,
                   cfPAT, cfPSE36, cfPSN, cfCLFSH, cf_d20, cfDS, cfACPI, cfMMX,
                   cfFXSR, cfSSE, cfSSE2, cfSS, cfHTT, cfTM, cfIA_64, cfPBE,
                   cfSSE3, cf_c1, cf_c2, cfMON, cfDS_CPL, cf_c5, cf_c6, cfEIST,
                   cfTM2, cf_c9, cfCID, cf_c11, cf_c12, cfCX16, cfxTPR, cf_c15,
                   cf_c16, cf_c17, cf_c18, cf_c19, cf_c20, cf_c21, cf_c22, cf_c23,
                   cf_c24, cf_c25, cf_c26, cf_c27, cf_c28, cf_c29, cf_c30, cf_c31);
  TCpuFeatureSet = set of TCpuFeatures;

  TCpuExtendedFeatures = (cefFPU, cefVME, cefDE, cefPSE, cefTSC, cefMSR, cefPAE,
                          cefMCE, cefCX8, cefAPIC, cef_10, cefSEP, cefMTRR,
                          cefPGE, cefMCA, cefCMOV, cefPAT, cefPSE36, cef_18,
                          ceMPC, ceNX, cef_21, cefExMMX, cefMMX, cefFXSR, cef_25,
                          cef_26, cef_27, cef_28, cefLM, cefEx3DNow, cef3DNow);
  TCpuExtendedFeatureSet = set of TCpuExtendedFeatures;

const
  CVendorIDString: array[Low(TCPUVendor)..High(TCPUVendor)] of TVendorStr =
  ('', 'AuthenticAMD', 'CentaurHauls', 'CyrixInstead', 'GenuineIntel', 'GenuineTMx86',
   'NexGenDriven', 'RiseRiseRise', 'UMC UMC UMC ', 'Geode by NSC', 'SiS SiS SiS');

  {CPU signatures}

  IntelLowestSEPSupportSignature = $633;
  K7DuronA0Signature = $630;
  C3Samuel2EffModel = 7;
  C3EzraEffModel = 8;
  PMBaniasEffModel = 9;
  PMDothanEffModel = $D;
  P3LowestEffModel = 7;


{$WARNINGS ON}
function IsCPUID_Available: Boolean; register;
asm
  PUSHFD                 {save EFLAGS to stack}
  POP     EAX            {store EFLAGS in EAX}
  MOV     EDX, EAX       {save in EDX for later testing}
  XOR     EAX, $200000;  {flip ID bit in EFLAGS}
  PUSH    EAX            {save new EFLAGS Value on stack}
  POPFD                  {replace current EFLAGS Value}
  PUSHFD                 {get new EFLAGS}
  POP     EAX            {store new EFLAGS in EAX}
  XOR     EAX, EDX       {check if ID bit changed}
  JZ      @exit          {no, CPUID not available}
  MOV     EAX, True      {yes, CPUID is available}
@exit:
end;

function IsFPU_Available: Boolean;
var _FCW, _FSW: Word;
asm
  MOV     EAX, False     {initialize return register}
  MOV     _FSW, $5A5A    {store a non-zero Value}
  FNINIT                 {must use non-wait form}
  FNSTSW  _FSW           {store the status}
  CMP     _FSW, 0        {was the correct status read?}
  JNE     @exit          {no, FPU not available}
  FNSTCW  _FCW           {yes, now save control word}
  MOV     DX, _FCW       {get the control word}
  AND     DX, $103F      {mask the proper status bits}
  CMP     DX, $3F        {is a numeric processor installed?}
  JNE     @exit          {no, FPU not installed}
  MOV     EAX, True      {yes, FPU is installed}
@exit:
end;

procedure GetCPUID(Param: Cardinal; var Registers: TRegisters);
asm
  PUSH    EBX                         {save affected registers}
  PUSH    EDI
  MOV     EDI, Registers
  XOR     EBX, EBX                    {clear EBX register}
  XOR     ECX, ECX                    {clear ECX register}
  XOR     EDX, EDX                    {clear EDX register}
  DB $0F, $A2                         {CPUID opcode}
  MOV     TRegisters(EDI).&EAX, EAX   {save EAX register}
  MOV     TRegisters(EDI).&EBX, EBX   {save EBX register}
  MOV     TRegisters(EDI).&ECX, ECX   {save ECX register}
  MOV     TRegisters(EDI).&EDX, EDX   {save EDX register}
  POP     EDI                         {restore registers}
  POP     EBX
end;

constructor TCPU.Create;
begin
  GetCPUInfo;
end;

procedure TCPU.GetCPUVendor;
var  
  VendorStr: TVendorStr;
  Registers: TRegisters;
begin
 GetCPUID(0, Registers);
 SetLength(VendorStr, 12);
 Move(Registers.EBX, VendorStr[1], 4);
 Move(Registers.EDX, VendorStr[5], 4);
 Move(Registers.ECX, VendorStr[9], 4);
 FVendor := High(TCPUVendor);
 while (VendorStr <> CVendorIDString[FVendor]) and (FVendor > Low(TCPUVendor))
  do Dec(FVendor);
end;

procedure TCPU.GetCPUFeatures;
type 
  _Int64 = packed record
    Lo: Longword;
    Hi: Longword;
  end;
var 
  Registers: TRegisters;
  CpuFeatures: TCpuFeatureSet;
begin
 GetCPUID($00000001, Registers);
 FSignature := Registers.EAX;
 FEffFamily := FSignature and $00000F00 shr 8;
 FEffModel := FSignature and $000000F0 shr 4;
 if FEffFamily = $F then
  begin
   FEffFamily := FEffFamily + (FSignature and $0FF00000 shr 20);
   FEffModel := FEffModel + (FSignature and $000F0000 shr 12);
  end;
 Move(Registers.EDX, _Int64(CpuFeatures).Lo, 4);
 Move(Registers.ECX, _Int64(CpuFeatures).Hi, 4);
 if cfFPU in CpuFeatures then Include(FInstructions, isFPU);
 if cfTSC in CpuFeatures then Include(FInstructions, isTSC);
 if cfCX8 in CpuFeatures then Include(FInstructions, isCX8);
 if cfSEP in CpuFeatures then
  begin
   Include(FInstructions, isSEP);
   if (FVendor = cvIntel) and (FSignature and $0FFF3FFF < IntelLowestSEPSupportSignature)
    then Exclude(FInstructions, isSEP);
  end;
 if cfCMOV in CpuFeatures then Include(FInstructions, isCMOV);
 if cfFXSR in CpuFeatures then Include(FInstructions, isFXSR);
 if cfMMX in CpuFeatures then Include(FInstructions, isMMX);
 if cfSSE in CpuFeatures then Include(FInstructions, isSSE);
 if cfSSE2 in CpuFeatures then Include(FInstructions, isSSE2);
 if cfSSE3 in CpuFeatures then Include(FInstructions, isSSE3);
 if (FVendor = cvIntel) and (cfMON in CpuFeatures) then Include(FInstructions, isMONITOR);
 if cfCX16 in CpuFeatures then Include(FInstructions, isCX16);
end;

procedure TCPU.GetCPUExtendedFeatures;
var
  Registers     : TRegisters;
  CpuExFeatures : TCpuExtendedFeatureSet;
begin
 GetCPUID($80000001, Registers);
 CPUExFeatures := TCPUExtendedFeatureSet(Registers.EDX);
 if cefLM in CpuExFeatures then Include(FInstructions, isX64);
 if cefExMMX in CpuExFeatures then Include(FInstructions, isExMMX);
 if cefEx3DNow in CpuExFeatures then Include(FInstructions, isEx3DNow);
 if cef3DNow in CpuExFeatures then Include(FInstructions, is3DNow);
end;

procedure TCPU.GetProcessorCacheInfo;
type
  TConfigDescriptor = packed array[0..15] of Byte;
var
  Registers  : TRegisters;
  i,j        : Integer;
  QueryCount : Byte;
begin
 GetCPUID($00000002, Registers);
 QueryCount := Registers.EAX and $FF;
 for i := 1 to QueryCount do
  begin
   for j := 1 to 15 do
    case TConfigDescriptor(Registers)[j] of
      $06: FCodeL1CacheSize := 8;
      $08: FCodeL1CacheSize := 16;
      $0A: FDataL1CacheSize := 8;
      $0C: FDataL1CacheSize := 16;
      $22: fL3CacheSize := 512;
      $23: fL3CacheSize := 1024;
      $25: fL3CacheSize := 2048;
      $29: fL3CacheSize := 4096;
      $2C: FDataL1CacheSize := 32;
      $30: FCodeL1CacheSize := 32;
      $39: FL2CacheSize := 128;
      $3B: FL2CacheSize := 128;
      $3C: FL2CacheSize := 256;
      $40: if FL2CacheSize <> 0 then fL3CacheSize := 0;
      $41: FL2CacheSize := 128;
      $42: FL2CacheSize := 256;
      $43: FL2CacheSize := 512;
      $44: FL2CacheSize := 1024;
      $45: FL2CacheSize := 2048;
      $60: FDataL1CacheSize := 16;
      $66: FDataL1CacheSize := 8;
      $67: FDataL1CacheSize := 16;
      $68: FDataL1CacheSize := 32;
      $70: if not (FVendor in [cvCyrix, cvNSC]) then FCodeL1CacheSize := 12;
      $71: FCodeL1CacheSize := 16;
      $72: FCodeL1CacheSize := 32;
      $78: FL2CacheSize := 1024;
      $79: FL2CacheSize := 128;
      $7A: FL2CacheSize := 256;
      $7B: FL2CacheSize := 512;
      $7C: FL2CacheSize := 1024;
      $7D: FL2CacheSize := 2048;
      $7F: FL2CacheSize := 512;
      $80: if FVendor in [cvCyrix, cvNSC] then
            begin
             FCodeL1CacheSize := 8;
             FDataL1CacheSize := 8;
            end;
      $82: FL2CacheSize := 256;
      $83: FL2CacheSize := 512;
      $84: FL2CacheSize := 1024;
      $85: FL2CacheSize := 2048;
      $86: FL2CacheSize := 512;
      $87: FL2CacheSize := 1024;
     end;
    if i < QueryCount then GetCPUID(2, Registers);
  end;
end;

procedure TCPU.GetExtendedProcessorCacheInfo;
var
  Registers: TRegisters;
begin
 GetCPUID($80000005, Registers);
 if not (FVendor in [cvIntel, cvCyrix]) then
  begin
   FCodeL1CacheSize := Registers.EDX shr 24;
   FDataL1CacheSize := Registers.ECX shr 24;
  end;
 GetCPUID($80000006, Registers);
 if (FVendor = cvAMD) and (FSignature and $FFF = K7DuronA0Signature)
  then FL2CacheSize := 64
  else if (FVendor = cvCentaur) and (FEffFamily = 6) and (FEffModel in [C3Samuel2EffModel, C3EzraEffModel])
   then FL2CacheSize := Registers.ECX shr 24
   else FL2CacheSize := Registers.ECX shr 16;
end;

procedure TCPU.VerifyOSSupportForXMMRegisters;
begin
 try
  asm
   DB $0F, $54, $C0
  end
 except
  on E: Exception do
   begin
    Exclude(FInstructions, isSSE);
    Exclude(FInstructions, isSSE2);
    Exclude(FInstructions, isSSE3);
   end;
 end;
end;

procedure TCPU.GetCPUInfo;
var
  Registers  : TRegisters;
  MaxCPUID   : Cardinal;
  MaxExCPUID : Cardinal;
begin
//   FillChar(fCPU, SizeOf(fCPU), 0);
 try
  if not IsCPUID_Available then
   if IsFPU_Available then Include(FInstructions, isFPU) else
  else
   begin
    GetCPUID($00000000, Registers);
    MaxCPUID := Registers.EAX;
    GetCPUVendor;
    if MaxCPUID >= $00000001 then GetCPUFeatures;
    if MaxCPUID >= $00000002 then GetProcessorCacheInfo;
    GetCPUID($80000000, Registers);
    MaxExCPUID := Registers.EAX;
    if MaxExCPUID >= $80000001 then GetCPUExtendedFeatures;
    if isSSE in FInstructions then VerifyOSSupportForXMMRegisters;
    if MaxExCPUID >= $80000006 then GetExtendedProcessorCacheInfo;
    end;
  except
    on E: Exception do raise;
  end;
end;

initialization
  DetectedCPU := TCPU.Create;

end.
