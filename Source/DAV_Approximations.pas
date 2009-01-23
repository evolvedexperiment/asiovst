unit DAV_Approximations;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common, {$IFDEF FPC} LCLIntf; {$DEFINE PUREPASCAL}{$ELSE}
  Windows {$IFDEF UseNativeTypes}, Types{$ENDIF};{$ENDIF}

  {$IFNDEF FPC}
  function FastExp(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}

  function FastRoot(i: Single; n: Integer): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastIntPower(i: Single; n: Integer): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastPower(base, exp: Double) : Double; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2(const Value: Single): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function FastPower2(const Value: Single): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function FastFloorLn2(const Value: Single): Integer; {$IFDEF useinlining} inline; {$ENDIF}
  function FastArctanLike(const Value: Single): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function FastArctanLike(const Value: Double): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}

  function FastSinLike(const Value: Single): Single; overload;
  function FastSinLike(const Value: Double): Double; overload;
  function FastCosLike(const Value: Single): Single; overload;
  function FastCosLike(const Value: Double): Double; overload;
  function FastArcTan2(const Y, X: Extended): Extended;
  function FastTan(const Value: Extended): Extended;
  function FastCoTan(const Value: Extended): Extended;
  function FastLog10(const Value: Extended): Extended;
  {$ENDIF}

  { Trigonomic Approximations }

  // 3-Term: Accurate to about 3.2 decimal digits over the range [0, pi/2].
  function FastCosPart3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCosPart3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCosPart3TermFPU(const Value: Single): Single; overload;
  function FastCosPart3TermFPU(const Value: Double): Double; overload;
  function FastCos3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCos3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSin3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSin3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSec3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSec3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCsc3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCsc3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  // 4-Term: Accurate to about 5.2 decimal digits over the range [0, pi/2].
  function FastCosPart4Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCosPart4Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCosPart4TermFPU(const Value: Single): Single; overload;
  function FastCosPart4TermFPU(const Value: Double): Double; overload;
  function FastCos4Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCos4Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSin4Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSin4Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSec4Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSec4Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCsc4Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCsc4Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  // 5-Term: Accurate to about 7.3 decimal digits over the range [0, pi/2].
  function FastCosPart5Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCosPart5Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCosPart5TermFPU(const Value: Single): Single; overload;
  function FastCosPart5TermFPU(const Value: Double): Double; overload;
  function FastCosInBounds5Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCosInBounds5Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCos5Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCos5Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSin5Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSin5Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSec5Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSec5Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCsc5Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCsc5Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  // 6-Term: Accurate to about ?.? decimal digits over the range [0, pi/2].
  function FastCosPart6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCosPart6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCosPart6TermFPU(const Value: Single): Single; overload;
  function FastCosPart6TermFPU(const Value: Double): Double; overload;
  function FastCos6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCos6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSin6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSin6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSec6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSec6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCsc6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCsc6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  // 7-Term: Accurate to about 12.1 decimal digits over the range [0, pi/2].
  function FastCosPart7Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCosPart7Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCos7Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCos7Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSin7Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSin7Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSec7Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastSec7Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCsc7Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCsc7Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  // 2-Term: Accurate to about 3.2 decimal digits over the range [0, pi/4].
  function FastTanPart2Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPart2Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPInv2Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPInv2Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTan2Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTan2Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCoTan2Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCoTan2Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  // 3-Term: Accurate to about 5.6 decimal digits over the range [0, pi/4].
  function FastTanPart3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPart3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPInv3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPInv3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTan3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTan3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCoTan3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCoTan3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  // 4-Term: Accurate to about 8.2 decimal digits over the range [0, pi/4].
  function FastTanPart4Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPart4Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPInv4Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPInv4Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTan4Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTan4Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCoTan4Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCoTan4Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  // 6-Term: Accurate to about 14 decimal digits over the range [0, pi/4].
  function FastTanPart6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPart6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPInv6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanPInv6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTan6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTan6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCoTan6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastCoTan6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  // 3-Term: Accurate to about 6.6 decimal digits over the range [0, pi/12].
  function FastArcTanPart3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastArcTanPart3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastArcTan3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastArcTan3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastArcCotan3Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastArcCotan3Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  // 6-Term: Accurate to about 13.7 decimal digits over the range [0, pi/12].
  function FastArcTanPart6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastArcTanPart6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastArcTan6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastArcTan6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastArcCotan6Term(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastArcCotan6Term(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;


  { 2^x Approximations }

  function FastPower2MinError2(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastPower2ContinousError2(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastPower2MinError3(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastPower2ContinousError3(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastPower2MinError4(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastPower2ContinousError4(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastPower2MinError5(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastPower2ContinousError5(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}


  { Log2 Approximations }

  function FastLog2ContinousError2(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2Laurent2(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2MinError2(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2ContinousError3(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2Laurent3(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2MinError3(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2ContinousError4(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2Laurent4(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2MinError4(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2ContinousError5(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2Laurent5(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2MinError5(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}


  { TanH Approximations }

  function FastTanhOpt3Term(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt4Term(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt5Term(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt6Term(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt7Term(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt3Term(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt4Term(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt5Term(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt6Term(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt7Term(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  function FastTanhOpt3TermFPU(const Input: Single): Single; assembler; overload;
  function FastTanhOpt4TermFPU(const Input: Single): Single; assembler; overload;
  function FastTanhOpt5TermFPU(const Input: Single): Single; assembler; overload;
  function FastTanhOpt6TermFPU(const Input: Single): Single; assembler; overload;
  function FastTanhOpt7TermFPU(const Input: Single): Single; assembler; overload;
  function FastTanhOpt3TermFPU(const Input: Double): Double; assembler; overload;
  function FastTanhOpt4TermFPU(const Input: Double): Double; assembler; overload;
  function FastTanhOpt5TermFPU(const Input: Double): Double; assembler; overload;
  function FastTanhOpt6TermFPU(const Input: Double): Double; assembler; overload;
  function FastTanhOpt7TermFPU(const Input: Double): Double; assembler; overload;

  function FastTanh2Like4Term(const Input: Single): Single;
  function FastTanh2Like3Term(const Input: Single): Single;
  function FastTanh2Like2Term(const Input: Single): Single;
  function FastTanh2Like1Term(const Input: Single): Single;

  function FastdBtoAmpMinError2(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpMinError2(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpContinousError2(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpContinousError2(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpMinError3(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpMinError3(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpContinousError3(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpContinousError3(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpMinError4(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpMinError4(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpContinousError4(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpContinousError4(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpMinError5(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpMinError5(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpContinousError5(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastdBtoAmpContinousError5(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  function FastAmptodBMinError2(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBMinError2(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBContinousError2(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBContinousError2(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBLaurent2(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBLaurent2(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBMinError3(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBMinError3(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBContinousError3(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBContinousError3(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBLaurent3(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBLaurent3(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBMinError4(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBMinError4(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBContinousError4(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBContinousError4(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBLaurent4(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBLaurent4(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBMinError5(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBMinError5(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBContinousError5(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBContinousError5(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBLaurent5(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastAmptodBLaurent5(const Value: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

var
  Ln10, Ln2, Ln2Half, Ln2Rez   : Double;
  TanSixthPi32, TanTwelfthPi32 : Single;
  TanSixthPi64, TanTwelfthPi64 : Double;

const
  CMinusOneThird    : Double = -1/3;
  CMinusTwoThird    : Double = -2/3;
  CTwo32            : Single = 2;
  CTwo64            : Double = 2;
  CTwoDivPi32       : Single = 2.0 / Pi;
  CTwoDivPi64       : Double = 2.0 / Pi;
  CPiHalf32         : Single = Pi * 0.5;
  CPiHalf64         : Double = Pi * 0.5;
  CThreeHalfPi32    : Single = 1.5 * pi;  // pi times 3/2, used in tan routines
  CThreeHalfPi64    : Double = 1.5 * pi;  // pi times 3/2, used in tan routines
  CFourDivPi32      : Single = 4.0 / Pi;  // 4 / pi, used in tan routines
  CFourDivPi64      : Double = 4.0 / Pi;  // 4 / pi, used in tan routines
  CFourthPi32       : Single = Pi * 0.25; // pi / 4.0, used in tan routines
  CFourthPi64       : Double = Pi * 0.25; // pi / 4.0, used in tan routines
  CSixthPi32        : Single = Pi / 6.0;  // pi/6.0, used in atan routines
  CSixthPi64        : Double = Pi / 6.0;  // pi/6.0, used in atan routines
  CTwelfthPi32      : Single = Pi / 12.0; // pi/12.0, used in atan routines
  CTwelfthPi64      : Double = Pi / 12.0; // pi/12.0, used in atan routines
  CdBtoAmpExpGain32 : Single = 1.5051499783199059760686944736225E-2;
  CdBtoAmpExpGain64 : Double = 1.5051499783199059760686944736225E-2;
  CFactor2IndB32    : Single = 6.0205999132796239042747778944899;
  CFactor2IndB64    : Double = 6.0205999132796239042747778944899;

const
  CArcTanLike : Array [0..4] of Single = (0.0208351, -0.085133, 0.180141,
    -0.3302995, 0.999866);
  CCos3Term : array [0..2] of Single = (9.99410127468481235E-1,
    -4.95614819206541213E-1, 3.68101531561626713E-2);
  CCos4Term : array [0..3] of Single = (9.99993413205793602E-1,
    -4.99913469025972335E-1, 4.14891801631523741E-2, -1.27168190108224634E-3);
  CCos5Term : array [0..4] of Double = (9.99999954384273471E-1,
    -4.99999068400003466E-1, 4.16636211009559776E-2, -1.38539804095743203E-3,
     2.31603397215404142E-5);
  CCos6Term : array [0..5] of Double = (9.99999999785851190E-1,
    -4.99999993711054302E-1, 4.16666367367911419E-2, -1.38883676220687841E-3,
     2.47604862741507171E-5, -2.60573608883968794E-7);
  CCos7Term : array [0..6] of Double = (9.9999999999925182E-1,
   -4.9999999997024012E-1, 4.1666666473384543E-2, -1.388888418000423E-3,
    2.48010406484558E-5, -2.752469638432E-7, 1.9907856854E-9);
  CTan2Term : array [0..1] of Single = (-3.6112171, -4.6133253);
  CTan3Term : array [0..2] of Single = (-3.16783027, 0.134516124, -4.033321984);
  CTan4Term : array [0..3] of Double = (211.849369664121, -12.5288887278448,
    269.7350131214121, -71.4145309347748);
  CTan5Term : array [0..4] of Double = (8.38820505317477848E+22,
    -3.70665302360166393E+21, -5.32928201354683187E+19,
     1.06801947663613863E+23, -2.66798401001070002E+22);
  CTan6Term : array [0..5] of Double = (-34287.4662577359568109624,
    2566.7175462315050423295, -26.5366371951731325438,
    -43656.1579281292375769579, 12244.4839556747426927793,
    -336.611376245464339493);
  CArcTan3Term : array [0..2] of Single = (1.6867629106, 0.4378497304,
    1.6867633134); 
  CArcTan6Term : array [0..5] of Double = (48.70107004404898384,
    49.5326263772254345, 9.40604244231624, 48.70107004404996166,
    65.7663163908956299, 21.587934067020262);
  CP2ContError5 : array [0..4] of Single = (6.93147084150589565E-1,
    2.40221342016562145E-1, 5.55055126727891784E-2, 9.67692162036261003E-3,
    1.33355654772594713E-3);

implementation

uses
  Math, SysUtils;

{$IFNDEF FPC}
{$WARNINGS OFF}
function FastArcTan2(const Y, X: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 Result := ArcTan2(Y, X);
{$ELSE}
asm
 fld Y
 fld X
 fpatan
{$ENDIF}
end;

function FastTan(const Value: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 Result := Tan(X);
{$ELSE}
asm
 fld Value
 fptan
 fstp st(0)
{$ENDIF}
end;

function FastCoTan(const Value: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 Result := CoTan(X);
{$ELSE}
asm
 fld Value
 fptan
 fdivrp
{$ENDIF}
end;

function FastLog10(const Value: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 Result := Log10(X);
{$ELSE}
asm
 fldlg2
 fld Value
 fyl2x
{$ENDIF}
end;
{$ENDIF}

function FastExp(const Value: Single): Single;
begin
 Result := Exp(Value * Ln2);
end;


{ Trigonomic Approximations }

type
  TQuadrant = 0..3;
  TOctant   = 0..7;


// 3-Term: Accurate to about 3.2 decimal digits over the range [0, pi/2].

function FastCosPart3Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := CCos3Term[0] + Result * (CCos3Term[1] + CCos3Term[2] * Result);
end;

function FastCosPart3Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := CCos3Term[0] + Result * (CCos3Term[1] + CCos3Term[2] * Result);
end;

function FastCosPart3TermFPU(const Value: Single): Single;
asm
 fld   Value
 fmul  st(0), st(0)                // Value�
 fld   [CCos3Term + 4 * 2].Single  // CCos3Term[2], Value�
 fmul  st(0), st(1)                // Value� * CCos3Term[2], Value�
 fadd  [CCos3Term + 4 * 1].Single  // ...
 fmulp
 fadd  [CCos3Term].Single
end;

function FastCosPart3TermFPU(const Value: Double): Double;
asm
 fld   Value
 fmul  st(0), st(0)                // Value�
 fld   [CCos3Term + 4 * 2].Single  // CCos3Term[2], Value�
 fmul  st(0), st(1)                // Value� * CCos3Term[2], Value�
 fadd  [CCos3Term + 4 * 1].Single  // ...
 fmulp
 fadd  [CCos3Term].Single
end;

function FastCos3Term(const Value: Single): Single;
begin
 Result := abs(FastMod(Value, CTwoPi32));            // Get rid of values > 2 * pi
 case round(Result * CTwoDivPi32 - CHalf32) of
  0 : Result :=  FastCosPart3Term(Result);
  1 : Result := -FastCosPart3Term(Pi - Result);
  2 : Result := -FastCosPart3Term(Result - Pi);
  3 : Result :=  FastCosPart3Term(CTwoPI32 - Result);
  4 : Result :=  FastCosPart3Term(Result);
 end;
end;

function FastCos3Term(const Value: Double): Double;
begin
 Result := abs(FastMod(Value, CTwoPi64));            // Get rid of values > 2 * pi
 case round(Result * CTwoDivPi64 - CHalf64) of
  0 : Result :=  FastCosPart3Term(Result);
  1 : Result := -FastCosPart3Term(Pi - Result);
  2 : Result := -FastCosPart3Term(Result - Pi);
  3 : Result :=  FastCosPart3Term(CTwoPI64 - Result);
  4 : Result :=  FastCosPart3Term(Result);
 end;
end;

function FastSin3Term(const Value: Single): Single;
begin
  Result := FastCos3Term(CPiHalf32 - Value);
end;

function FastSin3Term(const Value: Double): Double;
begin
  Result := FastCos3Term(CPiHalf64 - Value);
end;

function FastSec3Term(const Value: Single): Single;
begin
  Result := 1 / FastCos3Term(Value);
end;

function FastSec3Term(const Value: Double): Double;
begin
  Result := 1 / FastCos3Term(Value);
end;

function FastCsc3Term(const Value: Single): Single;
begin
  Result := 1 / FastCos3Term(CPiHalf32 - Value);
end;

function FastCsc3Term(const Value: Double): Double;
begin
  Result := 1 / FastCos3Term(CPiHalf64 - Value);
end;


// 4-Term: Accurate to about 5.2 decimal digits over the range [0, pi/2].

function FastCosPart4Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := CCos4Term[0] + Result * (CCos4Term[1] + Result * (CCos4Term[2] + CCos4Term[3] * Result));
end;

function FastCosPart4Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := CCos4Term[0] + Result * (CCos4Term[1] + Result * (CCos4Term[2] + CCos4Term[3] * Result));
end;

function FastCosPart4TermFPU(const Value: Single): Single;
asm
 fld   Value
 fmul  st(0), st(0)                // Value�
 fld   [CCos4Term + 4 * 3].Single  // CCos4Term[3], Value�
 fmul  st(0), st(1)                // Value� * CCos4Term[3], Value�
 fadd  [CCos4Term + 4 * 2].Single  // ...
 fmul  st(0), st(1)
 fadd  [CCos4Term + 4 * 1].Single
 fmulp
 fadd  [CCos4Term].Single
end;

function FastCosPart4TermFPU(const Value: Double): Double;
asm
 fld   Value
 fmul  st(0), st(0)                // Value�
 fld   [CCos4Term + 4 * 3].Single  // CCos4Term[3], Value�
 fmul  st(0), st(1)                // Value� * CCos4Term[3], Value�
 fadd  [CCos4Term + 4 * 2].Single  // ...
 fmul  st(0), st(1)
 fadd  [CCos4Term + 4 * 1].Single
 fmulp
 fadd  [CCos4Term].Single
end;

function FastCos4Term(const Value: Single): Single;
begin
  Result := abs(FastMod(Value, CTwoPi32));            // Get rid of values > 2 * pi
  case round(Result * CTwoDivPi32 - CHalf32) of
   0 : Result :=  FastCosPart4Term(Result);
   1 : Result := -FastCosPart4Term(Pi - Result);
   2 : Result := -FastCosPart4Term(Result - Pi);
   3 : Result :=  FastCosPart4Term(CTwoPI32 - Result);
   4 : Result :=  FastCosPart4Term(Result);
  end;
end;

function FastCos4Term(const Value: Double): Double;
begin
  Result := abs(FastMod(Value, CTwoPi64));            // Get rid of values > 2 * pi
  case round(Result * CTwoDivPi64 - CHalf64) of
   0 : Result :=  FastCosPart4Term(Result);
   1 : Result := -FastCosPart4Term(Pi - Result);
   2 : Result := -FastCosPart4Term(Result - Pi);
   3 : Result :=  FastCosPart4Term(CTwoPI64 - Result);
   4 : Result :=  FastCosPart4Term(Result);
  end;
end;

function FastSin4Term(const Value: Single): Single;
begin
  Result := FastCos4Term(CPiHalf32 - Value);
end;

function FastSin4Term(const Value: Double): Double;
begin
  Result := FastCos4Term(CPiHalf64 - Value);
end;

function FastSec4Term(const Value: Single): Single;
begin
  Result := 1 / FastCos4Term(Value);
end;

function FastSec4Term(const Value: Double): Double;
begin
  Result := 1 / FastCos4Term(Value);
end;

function FastCsc4Term(const Value: Single): Single;
begin
  Result := 1 / FastCos4Term(CPiHalf32 - Value);
end;

function FastCsc4Term(const Value: Double): Double;
begin
  Result := 1 / FastCos4Term(CPiHalf64 - Value);
end;


// 5-Term: Accurate to about 7.3 decimal digits over the range [0, pi/2].

function FastCosPart5Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := CCos5Term[0] + Result * (CCos5Term[1] + Result * (CCos5Term[2] + Result * (CCos5Term[3] + CCos5Term[4] * Result)));
end;

function FastCosPart5Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := CCos5Term[0] + Result * (CCos5Term[1] + Result * (CCos5Term[2] + Result * (CCos5Term[3] + CCos5Term[4] * Result)));
end;

function FastCosPart5TermFPU(const Value: Single): Single;
asm
 fld   Value
 fmul  st(0), st(0)                // Value�
 fld   [CCos5Term + 4 * 4].Single  // CCos5Term[4], Value�
 fmul  st(0), st(1)                // Value� * CCos5Term[4], Value�
 fadd  [CCos5Term + 4 * 3].Single  // ...
 fmul  st(0), st(1)
 fadd  [CCos5Term + 4 * 2].Single
 fmul  st(0), st(1)
 fadd  [CCos5Term + 4 * 1].Single
 fmulp
 fadd  [CCos5Term].Single
end;

function FastCosPart5TermFPU(const Value: Double): Double;
asm
 fld   Value
 fmul  st(0), st(0)                // Value�
 fld   [CCos5Term + 4 * 4].Single  // CCos5Term[4], Value�
 fmul  st(0), st(1)                // Value� * CCos5Term[4], Value�
 fadd  [CCos5Term + 4 * 3].Single  // ...
 fmul  st(0), st(1)
 fadd  [CCos5Term + 4 * 2].Single
 fmul  st(0), st(1)
 fadd  [CCos5Term + 4 * 1].Single
 fmulp
 fadd  [CCos5Term].Single
end;

function FastCosInBounds5Term(const Value: Single): Single;
begin
 case round(Value * CTwoDivPi32 - CHalf32) of
  0 : Result :=  FastCosPart5Term(Result);
  1 : Result := -FastCosPart5Term(Pi - Result);
  2 : Result := -FastCosPart5Term(Result - Pi);
  3 : Result :=  FastCosPart5Term(CTwoPI32 - Result);
  4 : Result :=  FastCosPart5Term(Result);
 end;
end;

function FastCosInBounds5Term(const Value: Double): Double;
begin
 case round(Value * CTwoDivPi64 - CHalf64) of
  0 : Result :=  FastCosPart5Term(Result);
  1 : Result := -FastCosPart5Term(Pi - Result);
  2 : Result := -FastCosPart5Term(Result - Pi);
  3 : Result :=  FastCosPart5Term(CTwoPI64 - Result);
  4 : Result :=  FastCosPart5Term(Result);
 end;
end;

function FastCos5Term(const Value: Single): Single;
begin
 // Get rid of values > 2 * pi
 Result := abs(FastMod(Value, CTwoPi32));
 Result := FastCosInBounds5Term(Result);
end;

function FastCos5Term(const Value: Double): Double;
begin
 // Get rid of values > 2 * pi
 Result := abs(FastMod(Value, CTwoPi64));
 Result := FastCosInBounds5Term(Result);
end;

function FastSin5Term(const Value: Single): Single;
begin
 Result := FastCos5Term(CPiHalf32 - Value);
end;

function FastSin5Term(const Value: Double): Double;
begin
 Result := FastCos5Term(CPiHalf64 - Value);
end;

function FastSec5Term(const Value: Single): Single;
begin
 Result := 1 / FastCos5Term(Value);
end;

function FastSec5Term(const Value: Double): Double;
begin
 Result := 1 / FastCos5Term(Value);
end;

function FastCsc5Term(const Value: Single): Single;
begin
 Result := 1 / FastCos5Term(CPiHalf32 - Value);
end;

function FastCsc5Term(const Value: Double): Double;
begin
 Result := 1 / FastCos5Term(CPiHalf64 - Value);
end;


// 6-Term: Accurate to about 7.3 decimal digits over the range [0, pi/2].

function FastCosPart6Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := CCos6Term[0] + Result *
          (CCos6Term[1] + Result *
          (CCos6Term[2] + Result *
          (CCos6Term[3] + Result *
          (CCos6Term[4] + CCos6Term[5] * Result))));
end;

function FastCosPart6Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := CCos6Term[0] + Result *
          (CCos6Term[1] + Result *
          (CCos6Term[2] + Result *
          (CCos6Term[3] + Result *
          (CCos6Term[4] + CCos6Term[5] * Result))));
end;

function FastCosPart6TermFPU(const Value: Single): Single;
asm
 fld   Value
 fmul  st(0), st(0)                // Value�
 fld   [CCos6Term + 8 * 6].Double  // CCos6Term[5], Value�
 fmul  st(0), st(1)                // Value� * CCos6Term[5], Value�
 fadd  [CCos6Term + 8 * 5].Double  // ...
 fmul  st(0), st(1)
 fadd  [CCos6Term + 8 * 4].Double
 fmul  st(0), st(1)
 fadd  [CCos6Term + 8 * 3].Double
 fmul  st(0), st(1)
 fadd  [CCos6Term + 8 * 2].Double
 fmul  st(0), st(1)
 fadd  [CCos6Term + 8 * 1].Double
 fmulp
 fadd [CCos6Term + 8].Double
end;

function FastCosPart6TermFPU(const Value: Double): Double;
asm
 fld   Value
 fmul  st(0), st(0)                // Value�
 fld   [CCos6Term + 8 * 6].Double  // CCos6Term[5], Value�
 fmul  st(0), st(1)                // Value� * CCos6Term[5], Value�
 fadd  [CCos6Term + 8 * 5].Double  // ...
 fmul  st(0), st(1)
 fadd  [CCos6Term + 8 * 4].Double
 fmul  st(0), st(1)
 fadd  [CCos6Term + 8 * 3].Double
 fmul  st(0), st(1)
 fadd  [CCos6Term + 8 * 2].Double
 fmul  st(0), st(1)
 fadd  [CCos6Term + 8 * 1].Double
 fmulp
 fadd [CCos6Term + 8].Double
end;

function FastCos6Term(const Value: Single): Single;
begin
 Result := abs(FastMod(Value, CTwoPi32));            // Get rid of values > 2 * pi
 case round(Result * CTwoDivPi32 - CHalf32) of
  0 : Result :=  FastCosPart6Term(Result);
  1 : Result := -FastCosPart6Term(Pi - Result);
  2 : Result := -FastCosPart6Term(Result - Pi);
  3 : Result :=  FastCosPart6Term(CTwoPI32 - Result);
  4 : Result :=  FastCosPart6Term(Result);
 end;
end;

function FastCos6Term(const Value: Double): Double;
begin
 Result := abs(FastMod(Value, CTwoPi64));            // Get rid of values > 2 * pi
 case round(Result * CTwoDivPi64 - CHalf64) of
  0 : Result :=  FastCosPart6Term(Result);
  1 : Result := -FastCosPart6Term(Pi - Result);
  2 : Result := -FastCosPart6Term(Result - Pi);
  3 : Result :=  FastCosPart6Term(CTwoPI64 - Result);
  4 : Result :=  FastCosPart6Term(Result);
 end;
end;

function FastSin6Term(const Value: Single): Single;
begin
  Result := FastCos6Term(CPiHalf32 - Value);
end;

function FastSin6Term(const Value: Double): Double;
begin
  Result := FastCos6Term(CPiHalf64 - Value);
end;

function FastSec6Term(const Value: Single): Single;
begin
  Result := 1 / FastCos6Term(Value);
end;

function FastSec6Term(const Value: Double): Double;
begin
  Result := 1 / FastCos6Term(Value);
end;

function FastCsc6Term(const Value: Single): Single;
begin
  Result := 1 / FastCos6Term(CPiHalf32 - Value);
end;

function FastCsc6Term(const Value: Double): Double;
begin
  Result := 1 / FastCos6Term(CPiHalf64 - Value);
end;


// 7-Term: Accurate to about 12.1 decimal digits over the range [0, pi/2].

function FastCosPart7Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := CCos7Term[0] + Result *
          (CCos7Term[1] + Result *
          (CCos7Term[2] + Result *
          (CCos7Term[3] + Result *
          (CCos7Term[4] + Result *
          (CCos7Term[5] + CCos7Term[6] * Result)))));
end;

function FastCosPart7Term(const Value: Double): Double; 
begin
 Result := sqr(Value);
 Result := CCos7Term[0] + Result *
          (CCos7Term[1] + Result *
          (CCos7Term[2] + Result *
          (CCos7Term[3] + Result *
          (CCos7Term[4] + Result *
          (CCos7Term[5] + CCos7Term[6] * Result)))));
end;

function FastCos7Term(const Value: Single): Single; 
begin
 Result := abs(FastMod(Value, CTwoPi32));            // Get rid of values > 2 * pi
 case round(Result * CTwoDivPi32 - CHalf32) of
  0 : Result :=  FastCosPart7Term(Result);
  1 : Result := -FastCosPart7Term(Pi - Result);
  2 : Result := -FastCosPart7Term(Result - Pi);
  3 : Result :=  FastCosPart7Term(CTwoPI32 - Result);
  4 : Result :=  FastCosPart7Term(Result);
 end;
end;

function FastCos7Term(const Value: Double): Double;
begin
 Result := abs(FastMod(Value, CTwoPi64));            // Get rid of values > 2 * pi
 case round(Result * CTwoDivPi64 - CHalf64) of
  0 : Result :=  FastCosPart7Term(Result);
  1 : Result := -FastCosPart7Term(Pi - Result);
  2 : Result := -FastCosPart7Term(Result - Pi);
  3 : Result :=  FastCosPart7Term(CTwoPI64 - Result);
  4 : Result :=  FastCosPart7Term(Result);
 end;
end;

function FastSin7Term(const Value: Single): Single; 
begin
  Result := FastCos7Term(CPiHalf32 - Value);
end;

function FastSin7Term(const Value: Double): Double;
begin
  Result := FastCos7Term(CPiHalf64 - Value);
end;

function FastSec7Term(const Value: Single): Single;
begin
  Result := 1 / FastCos7Term(Value);
end;

function FastSec7Term(const Value: Double): Double;
begin
  Result := 1 / FastCos7Term(Value);
end;

function FastCsc7Term(const Value: Single): Single;
begin
  Result := 1 / FastCos7Term(CPiHalf32 - Value);
end;

function FastCsc7Term(const Value: Double): Double;
begin
  Result := 1 / FastCos7Term(CPiHalf64 - Value);
end;


// 2-Term: Accurate to about 3.2 decimal digits over the range [0, pi/4].

function FastTanPart2Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := Value * CTan2Term[0] / (CTan2Term[1] + Result);
end;

function FastTanPart2Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := Value * CTan2Term[0] / (CTan2Term[1] + Result);
end;

function FastTanPInv2Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := (CTan2Term[1] + Result) / (Value * CTan2Term[0]);
end;

function FastTanPInv2Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := (CTan2Term[1] + Result) / (Value * CTan2Term[0]);
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x=pi/2 and x=3*pi/2. If this is a problem
// in your application, take appropriate action.

function FastTan2Term(const Value: Single): Single;
begin
  Result := abs(FastMod(Value, CTwoPi32)); // Get rid of values > 2 * pi
  case round(Result * CFourDivPi32) of
   0 : Result :=  FastTanPart2Term(Value                    * CFourDivPi32);
   1 : Result :=  FastTanPInv2Term((CPiHalf32 - Value)      * CFourDivPi32);
   2 : Result := -FastTanPInv2Term((Value - CPiHalf32)      * CFourDivPi32);
   3 : Result := -FastTanPart2Term((Pi - Value)             * CFourDivPi32);
   4 : Result :=  FastTanPart2Term((Value - Pi)             * CFourDivPi32);
   5 : Result :=  FastTanPInv2Term((CThreeHalfPi32 - Value) * CFourDivPi32);
   6 : Result := -FastTanPInv2Term((Value - CThreeHalfPi32) * CFourDivPi32);
   7 : Result := -FastTanPart2Term((CTwo32 - Value)         * CFourDivPi32);
  end;
end;

function FastTan2Term(const Value: Double): Double;
begin
  Result := abs(FastMod(Value, CTwoPi64)); // Get rid of values > 2 * pi
  case round(Result * CFourDivPi64) of
   0 : Result :=  FastTanPart2Term(Value                    * CFourDivPi64);
   1 : Result :=  FastTanPInv2Term((CPiHalf64 - Value)      * CFourDivPi64);
   2 : Result := -FastTanPInv2Term((Value - CPiHalf64)      * CFourDivPi64);
   3 : Result := -FastTanPart2Term((Pi - Value)             * CFourDivPi64);
   4 : Result :=  FastTanPart2Term((Value - Pi)             * CFourDivPi64);
   5 : Result :=  FastTanPInv2Term((CThreeHalfPi64 - Value) * CFourDivPi64);
   6 : Result := -FastTanPInv2Term((Value - CThreeHalfPi64) * CFourDivPi64);
   7 : Result := -FastTanPart2Term((CTwo64 - Value)         * CFourDivPi64);
  end;
end;

function FastCotan2Term(const Value: Single): Single;
begin
  Result := -FastTan2Term(CPiHalf32 - Value);
end;

function FastCotan2Term(const Value: Double): Double;
begin
  Result := -FastTan2Term(CPiHalf64 - Value);
end;


// 3-Term: Accurate to about 5.6 decimal digits over the range [0, pi/4].

function FastTanPart3Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := Value * (CTan3Term[0] + CTan3Term[1] * Result) / (CTan3Term[2] + Result);
end;

function FastTanPart3Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := Value * (CTan3Term[0] + CTan3Term[1] * Result) / (CTan3Term[2] + Result);
end;

function FastTanPInv3Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := (CTan3Term[2] + Result) / (Value * (CTan3Term[0] + CTan3Term[1] * Result));
end;

function FastTanPInv3Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := (CTan3Term[2] + Result) / (Value * (CTan3Term[0] + CTan3Term[1] * Result));
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x=pi/2 and x=3*pi/2. If this is a problem
// in your application, take appropriate action.

function FastTan3Term(const Value: Single): Single;
begin
  Result := abs(FastMod(Value, CTwoPi32)); // Get rid of values > 2 * pi
  case round(Result * CFourDivPi32) of
   0 : Result :=  FastTanPart3Term(Value                    * CFourDivPi32);
   1 : Result :=  FastTanPInv3Term((CPiHalf32 - Value)      * CFourDivPi32);
   2 : Result := -FastTanPInv3Term((Value - CPiHalf32)      * CFourDivPi32);
   3 : Result := -FastTanPart3Term((Pi - Value)             * CFourDivPi32);
   4 : Result :=  FastTanPart3Term((Value - Pi)             * CFourDivPi32);
   5 : Result :=  FastTanPInv3Term((CThreeHalfPi32 - Value) * CFourDivPi32);
   6 : Result := -FastTanPInv3Term((Value - CThreeHalfPi32) * CFourDivPi32);
   7 : Result := -FastTanPart3Term((CTwo32 - Value)         * CFourDivPi32);
  end;
end;

function FastTan3Term(const Value: Double): Double;
begin
  Result := abs(FastMod(Value, CTwoPi64)); // Get rid of values > 2 * pi
  case round(Result * CFourDivPi64) of
   0 : Result :=  FastTanPart3Term(Value                    * CFourDivPi64);
   1 : Result :=  FastTanPInv3Term((CPiHalf64 - Value)      * CFourDivPi64);
   2 : Result := -FastTanPInv3Term((Value - CPiHalf64)      * CFourDivPi64);
   3 : Result := -FastTanPart3Term((Pi - Value)             * CFourDivPi64);
   4 : Result :=  FastTanPart3Term((Value - Pi)             * CFourDivPi64);
   5 : Result :=  FastTanPInv3Term((CThreeHalfPi64 - Value) * CFourDivPi64);
   6 : Result := -FastTanPInv3Term((Value - CThreeHalfPi64) * CFourDivPi64);
   7 : Result := -FastTanPart3Term((CTwo64 - Value)         * CFourDivPi64);
  end;
end;

function FastCotan3Term(const Value: Single): Single;
begin
  Result := -FastTan3Term(CPiHalf32 - Value);
end;

function FastCotan3Term(const Value: Double): Double;
begin
  Result := -FastTan3Term(CPiHalf64 - Value);
end;


// 4-Term: Accurate to about 8.2 decimal digits over the range [0, pi/4].

function FastTanPart4Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := Value * (CTan4Term[0] + CTan4Term[1] * Result) /
           (CTan4Term[2] + Result * (CTan4Term[3] + Result));
end;

function FastTanPart4Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := Value * (CTan4Term[0] + CTan4Term[1] * Result) /
           (CTan4Term[2] + Result * (CTan4Term[3] + Result));
end;

function FastTanPInv4Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := (CTan4Term[2] + Result * (CTan4Term[3] + Result)) /
           (Value * (CTan4Term[0] + CTan4Term[1] * Result));
end;

function FastTanPInv4Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := (CTan4Term[2] + Result * (CTan4Term[3] + Result)) /
           (Value * (CTan4Term[0] + CTan4Term[1] * Result));
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x=pi/2 and x=3*pi/2. If this is a problem
// in your application, take appropriate action.

function FastTan4Term(const Value: Single): Single;
begin
  Result := abs(FastMod(Value, CTwoPi32)); // Get rid of values > 2 * pi
  case round(Result * CFourDivPi32) of
   0 : Result :=  FastTanPart4Term(Value                    * CFourDivPi32);
   1 : Result :=  FastTanPInv4Term((CPiHalf32 - Value)      * CFourDivPi32);
   2 : Result := -FastTanPInv4Term((Value - CPiHalf32)      * CFourDivPi32);
   3 : Result := -FastTanPart4Term((Pi - Value)             * CFourDivPi32);
   4 : Result :=  FastTanPart4Term((Value - Pi)             * CFourDivPi32);
   5 : Result :=  FastTanPInv4Term((CThreeHalfPi32 - Value) * CFourDivPi32);
   6 : Result := -FastTanPInv4Term((Value - CThreeHalfPi32) * CFourDivPi32);
   7 : Result := -FastTanPart4Term((CTwo32 - Value)         * CFourDivPi32);
  end;
end;

function FastTan4Term(const Value: Double): Double;
begin
  Result := abs(FastMod(Value, CTwoPi64)); // Get rid of values > 2 * pi
  case round(Result * CFourDivPi64) of
   0 : Result :=  FastTanPart4Term(Value                    * CFourDivPi64);
   1 : Result :=  FastTanPInv4Term((CPiHalf64 - Value)      * CFourDivPi64);
   2 : Result := -FastTanPInv4Term((Value - CPiHalf64)      * CFourDivPi64);
   3 : Result := -FastTanPart4Term((Pi - Value)             * CFourDivPi64);
   4 : Result :=  FastTanPart4Term((Value - Pi)             * CFourDivPi64);
   5 : Result :=  FastTanPInv4Term((CThreeHalfPi64 - Value) * CFourDivPi64);
   6 : Result := -FastTanPInv4Term((Value - CThreeHalfPi64) * CFourDivPi64);
   7 : Result := -FastTanPart4Term((CTwo64 - Value)         * CFourDivPi64);
  end;
end;

function FastCotan4Term(const Value: Single): Single;
begin
  Result := -FastTan4Term(CPiHalf32 - Value);
end;

function FastCotan4Term(const Value: Double): Double;
begin
  Result := -FastTan4Term(CPiHalf64 - Value);
end;


// 6-Term: Accurate to about 14 decimal digits over the range [0, pi/4].

function FastTanPart6Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := Value * (CTan6Term[0] + Result * (CTan6Term[1] + Result * CTan6Term[2])) /
   (CTan6Term[3] + Result * (CTan6Term[4] + Result * (CTan6Term[5] + Result)));
end;

function FastTanPart6Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := Value * (CTan6Term[0] + Result * (CTan6Term[1] + Result * CTan6Term[2])) /
   (CTan6Term[3] + Result * (CTan6Term[4] + Result * (CTan6Term[5] + Result)));
end;

function FastTanPInv6Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := (CTan6Term[3] + Result * (CTan6Term[4] + Result * (CTan6Term[5] + Result))) /
   (Value * (CTan6Term[0] + Result * (CTan6Term[1] + Result * CTan6Term[2])));
end;

function FastTanPInv6Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := (CTan6Term[3] + Result * (CTan6Term[4] + Result * (CTan6Term[5] + Result))) /
   (Value * (CTan6Term[0] + Result * (CTan6Term[1] + Result * CTan6Term[2])));
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x=pi/2 and x=3*pi/2. If this is a problem
// in your application, take appropriate action.

function FastTan6Term(const Value: Single): Single;
begin
  Result := abs(FastMod(Value, CTwoPi32)); // Get rid of values > 2 * pi
  case round(Result * CFourDivPi32) of
   0 : Result :=  FastTanPart6Term(Value                    * CFourDivPi32);
   1 : Result :=  FastTanPInv6Term((CPiHalf32 - Value)      * CFourDivPi32);
   2 : Result := -FastTanPInv6Term((Value - CPiHalf32)      * CFourDivPi32);
   3 : Result := -FastTanPart6Term((Pi - Value)             * CFourDivPi32);
   4 : Result :=  FastTanPart6Term((Value - Pi)             * CFourDivPi32);
   5 : Result :=  FastTanPInv6Term((CThreeHalfPi32 - Value) * CFourDivPi32);
   6 : Result := -FastTanPInv6Term((Value - CThreeHalfPi32) * CFourDivPi32);
   7 : Result := -FastTanPart6Term((CTwo32 - Value)         * CFourDivPi32);
  end;
end;

function FastTan6Term(const Value: Double): Double;
begin
  Result := abs(FastMod(Value, CTwoPi64)); // Get rid of values > 2 * pi
  case round(Result * CFourDivPi64) of
   0 : Result :=  FastTanPart6Term(Value                    * CFourDivPi64);
   1 : Result :=  FastTanPInv6Term((CPiHalf64 - Value)      * CFourDivPi64);
   2 : Result := -FastTanPInv6Term((Value - CPiHalf64)      * CFourDivPi64);
   3 : Result := -FastTanPart6Term((Pi - Value)             * CFourDivPi64);
   4 : Result :=  FastTanPart6Term((Value - Pi)             * CFourDivPi64);
   5 : Result :=  FastTanPInv6Term((CThreeHalfPi64 - Value) * CFourDivPi64);
   6 : Result := -FastTanPInv6Term((Value - CThreeHalfPi64) * CFourDivPi64);
   7 : Result := -FastTanPart6Term((CTwo64 - Value)         * CFourDivPi64);
  end;
end;

function FastCotan6Term(const Value: Single): Single;
begin
  Result := -FastTan6Term(CPiHalf32 - Value);
end;

function FastCotan6Term(const Value: Double): Double;
begin
  Result := -FastTan6Term(CPiHalf64 - Value);
end;


// 3-Term: Accurate to about 6.6 decimal digits over the range [0, pi/12].

function FastArcTanPart3Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := Value * (CArcTan3Term[0] + CArcTan3Term[1] * Result) / (CArcTan3Term[2] + Result);
end;

function FastArcTanPart3Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := Value * (CArcTan3Term[0] + CArcTan3Term[1] * Result) / (CArcTan3Term[2] + Result);
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x=pi/2 and x=3*pi/2. If this is a problem
// in your application, take appropriate action.

function FastArcTan3Term(const Value: Single): Single;
var
  Complement : Boolean; // true if arg was >1
  Region     : Boolean; // true depending on region arg is in
  Sign       : Boolean; // true if arg was < 0
begin
 Complement := False;
 Region     := False;
 Sign       := False;
 Result     := Value;

 if Result < 0 then
  begin
   Result := -Result;
   Sign   := True;         // arctan(-x) = -arctan(x)
  end;
 if Result > 1.0 then
  begin
   Result := 1.0 / Result; // keep arg between 0 and 1
   Complement := True;
  end;
 if Result > TanTwelfthPi32 then
  begin
   Result := (Result - TanSixthPi32) / (1 + TanSixthPi32 * Result);  // reduce arg to under tan(pi/12)
   Region := True;
  end;

 Result := FastArcTanPart3Term(Result);         // run the approximation
 if Region then Result := Result + CSixthPi32; // correct for region we're in
 if Complement then Result := CHalf32 - Result; // correct for 1/x if we did that
 if Sign then Result := -Result;               // correct for negative arg
end;

function FastArcTan3Term(const Value: Double): Double;
var
  Complement : Boolean; // true if arg was >1
  Region     : Boolean; // true depending on region arg is in
  Sign       : Boolean; // true if arg was < 0
begin
 Complement := False;
 Region     := False;
 Sign       := False;
 Result     := Value;

 if Result < 0 then
  begin
   Result := -Result;
   Sign   := True;         // arctan(-x) = -arctan(x)
  end;
 if Result > 1.0 then
  begin
   Result := 1.0 / Result; // keep arg between 0 and 1
   Complement := True;
  end;
 if Result > TanTwelfthPi64 then
  begin
   Result := (Result - TanSixthPi64) / (1 + TanSixthPi64 * Result);  // reduce arg to under tan(pi/12)
   Region := True;
  end;

 Result := FastArcTanPart3Term(Result);         // run the approximation
 if Region then Result := Result + CSixthPi64; // correct for region we're in
 if Complement then Result := CHalf64 - Result; // correct for 1/x if we did that
 if Sign then Result := -Result;               // correct for negative arg
end;

function FastArcCotan3Term(const Value: Single): Single;
begin
  Result := -FastArcTan3Term(CPiHalf32 - Value);
end;

function FastArcCotan3Term(const Value: Double): Double;
begin
  Result := -FastArcTan3Term(CPiHalf64 - Value);
end;


// 3-Term: Accurate to about 6.6 decimal digits over the range [0, pi/12].

function FastArcTanPart6Term(const Value: Single): Single;
begin
 Result := sqr(Value);
 Result := Value * (CArcTan6Term[0] + Result * (CArcTan6Term[1] + Result * CArcTan6Term[2])) /
   (CArcTan6Term[3] + Result * (CArcTan6Term[4] + Result * (CArcTan6Term[5] + Result)));
end;

function FastArcTanPart6Term(const Value: Double): Double;
begin
 Result := sqr(Value);
 Result := Value * (CArcTan6Term[0] + Result * (CArcTan6Term[1] + Result * CArcTan6Term[2])) /
   (CArcTan6Term[3] + Result * (CArcTan6Term[4] + Result * (CArcTan6Term[5] + Result)));
end;

// WARNING: We do not test for the tangent approaching infinity,
// which it will at x=pi/2 and x=3*pi/2. If this is a problem
// in your application, take appropriate action.

function FastArcTan6Term(const Value: Single): Single;
var
  Complement : Boolean; // true if arg was >1
  Region     : Boolean; // true depending on region arg is in
  Sign       : Boolean; // true if arg was < 0
begin
 Complement := False;
 Region     := False;
 Sign       := False;
 Result     := Value;

 if Result < 0 then
  begin
   Result := -Result;
   Sign   := True;         // arctan(-x) = -arctan(x)
  end;
 if Result > 1.0 then
  begin
   Result := 1.0 / Result; // keep arg between 0 and 1
   Complement := True;
  end;
 if Result > TanTwelfthPi32 then
  begin
   Result := (Result - TanSixthPi32) / (1 + TanSixthPi32 * Result);  // reduce arg to under tan(pi/12)
   Region := True;
  end;

 Result := FastArcTanPart6Term(Result);         // run the approximation
 if Region then Result := Result + CSixthPi32; // correct for region we're in
 if Complement then Result := CHalf32 - Result; // correct for 1/x if we did that
 if Sign then Result := -Result;               // correct for negative arg
end;

function FastArcTan6Term(const Value: Double): Double;
var
  Complement : Boolean; // true if arg was >1
  Region     : Boolean; // true depending on region arg is in
  Sign       : Boolean; // true if arg was < 0
begin
 Complement := False;
 Region     := False;
 Sign       := False;
 Result     := Value;

 if Result < 0 then
  begin
   Result := -Result;
   Sign   := True;         // arctan(-x) = -arctan(x)
  end;
 if Result > 1.0 then
  begin
   Result := 1.0 / Result; // keep arg between 0 and 1
   Complement := True;
  end;
 if Result > TanTwelfthPi64 then
  begin
   Result := (Result - TanSixthPi64) / (1 + TanSixthPi64 * Result);  // reduce arg to under tan(pi/12)
   Region := True;
  end;

 Result := FastArcTanPart6Term(Result);         // run the approximation
 if Region then Result := Result + CSixthPi64; // correct for region we're in
 if Complement then Result := CHalf64 - Result; // correct for 1/x if we did that
 if Sign then Result := -Result;               // correct for negative arg
end;

function FastArcCotan6Term(const Value: Single): Single;
begin
  Result := -FastArcTan6Term(CPiHalf32 - Value);
end;

function FastArcCotan6Term(const Value: Double): Double;
begin
  Result := -FastArcTan6Term(CPiHalf64 - Value);
end;



function FastSinLike(const Value: Single): Single;
const
  C1 : Single = 7.61E-03;
  C2 : Single = -1.6605E-01;
{$IFDEF PUREPASCAL}
var Asqr : Double;
begin
 Asqr   := sqr(Value);
 Result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld   Value
 fmul  Value
 fld   C1
 fmul  st(0),  st(1)
 fld   C2
 faddp st(1), st(0)
 fmulp st(1), st(0)
 fld1
 faddp
 fmul  Value
{$ENDIF}
end;

function FastSinLike(const Value: Double): Double;
const
  C1 : Double = 7.61E-03;
  C2 : Double = -1.6605E-01;
{$IFDEF PUREPASCAL}
var
  Asqr : Double;
begin
 Asqr   := sqr(Value);
 Result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld   Value
 fmul  Value
 fld   C1
 fmul  st(0), st(1)
 fld   C2
 faddp st(1), st(0)
 fmulp st(1), st(0)
 fld1
 faddp
 fmul  Value
{$ENDIF}
end;

function FastCosLike(const Value: Single): Single;
const
  C1 : Single =  3.705e-02;
  C2 : Single = -4.967e-01;
{$IFDEF PUREPASCAL}
var
  Asqr : Single;
begin
 Asqr   := sqr(Value);
 Result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld   Value
 fmul  Value
 fld   C1
 fmul  st(0), st(1)
 fld   C2
 faddp st(1), st(0)
 fmulp st(1), st(0)
 fld1
 faddp
 fmul  Value
{$ENDIF}
end;

function FastCosLike(const Value: Double): Double;
const
  C1 : Double =  3.705e-02;
  C2 : Double = -4.967e-01;
{$IFDEF PUREPASCAL}
var
  Asqr : Double;
begin
 Asqr   := sqr(Value);
 Result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld   Value
 fmul  Value
 fld   C1
 fmul  st(0), st(1)
 fld   C2
 faddp st(1), st(0)
 fmulp st(1), st(0)
 fld1
 faddp
 fmul  Value
{$ENDIF}
end;

function FastArctanLike(const Value: Single): Single;
var
  VSqr : Single;
begin
 VSqr   := sqr(Value);
 Result := ((((CArcTanLike[0]  * VSqr +
               CArcTanLike[1]) * VSqr +
               CArcTanLike[2]) * VSqr +
               CArcTanLike[3]) * VSqr +
               CArcTanLike[4]) * Value;
end;

function FastArctanLike(const Value: Double): Double;
var
  VSqr : Single;
begin
 VSqr   := sqr(Value);
 Result := ((((CArcTanLike[0]  * VSqr +
               CArcTanLike[1]) * VSqr +
               CArcTanLike[2]) * VSqr +
               CArcTanLike[3]) * VSqr +
               CArcTanLike[4]) * Value;
end;

function FastFloorLn2(const Value: Single): Integer;
begin
 Result := (((Integer((@Value)^) and $7F800000) shr 23) - $7F);
end;

function FastLog2(const Value: Single): Single;
var
  IntCast : Integer absolute Value;
begin
 Result := (((IntCast and $7F800000) shr 23) - $7F) +
             (IntCast and $007FFFFF) / $800000;
end;

function FastPower2MinError2(Value: Single): Single;
var
  IntCast : Integer absolute result;
  j       : Integer;
const
  CP2MinError2 : array [0..1] of Single = (7.02679339377207945E-1,
    2.39338555345344262E-1);
begin
 j := round(Value);
 Value := Value - j;
 IntCast := ((($7F + j) shl 23) and $FF800000);
 Result := result * (1 +
            Value * (CP2MinError2[0] +
            Value * (CP2MinError2[1])));
end;

function FastPower2ContinousError2(Value: Single): Single;
var
  IntCast : Integer absolute result;
  j       : Integer;
const
  CP2ContError2 : array [0..1] of Single = (7.07990673676189286E-1,
    2.47944580878438597E-1);
begin
 j := round(Value);
 Value := Value - j;
 IntCast := ((($7F + j) shl 23) and $FF800000);
 Result := result * (1 +
            Value * (CP2ContError2[0] +
            Value * (CP2ContError2[1])));
end;

function FastPower2MinError3(Value: Single): Single;
var
  IntCast : Integer absolute result;
  j       : Integer;
const
  CP2MinError3 : array [0..2] of Single = (6.93292707161004662E-1,
    2.42162975514835621E-1, 5.48668824216034384E-2);
begin
 j := round(Value);
 Value := Value - j;
 IntCast := ((($7F + j) shl 23) and $FF800000);
 Result := result * (1 +
            Value * (CP2MinError3[0] +
            Value * (CP2MinError3[1] *
            Value * (CP2MinError3[2]))));
end;

function FastPower2ContinousError3(Value: Single): Single;
var
  IntCast : Integer absolute result;
  j       : Integer;
const
  CP2ContError3 : array [0..2] of Single = (6.93282526441610814E-1,
    2.42201488582370950E-1, 5.50043626970249666E-2);
begin
 j := round(Value);
 Value := Value - j;
 IntCast := ((($7F + j) shl 23) and $FF800000);
 Result := result * (1 +
            Value * (CP2ContError3[0] +
            Value * (CP2ContError3[1] *
            Value * (CP2ContError3[2]))));
end;

function FastPower2MinError4(Value: Single): Single;
var
  IntCast : Integer absolute result;
  j       : Integer;
const
  CP2MinError4 : array [0..3] of Single = (6.93125327471890484E-1,
    2.40243955446532431E-1, 5.58964584033713671E-2, 9.56014434382608004E-3);
begin
 j := round(Value);
 IntCast := ((($7F + j) shl 23) and $FF800000);
 Value := Value - j;
 result :=  result * (1 +
             Value * (CP2MinError4[0] +
             Value * (CP2MinError4[1] +
             Value * (CP2MinError4[2] +
             Value *  CP2MinError4[3]))));

 result := result * (1 + Error);
end;

function FastPower2ContinousError4(Value: Single): Single;
var
  IntCast : Integer absolute result;
  j       : Integer;
const
  CP2ContError4 : array [0..3] of Single = (6.93118326815805763E-1,
    2.40239617833581720E-1, 5.59538423222786241E-2, 9.60540553453761992E-3);
begin
 j := round(Value);
 IntCast := ((($7F + j) shl 23) and $FF800000);
 Value := Value - j;
 result :=  result * (1 +
             Value * (CP2ContError4[0] + Value *
                     (CP2ContError4[1] + Value *
                     (CP2ContError4[2] + Value *
                     (CP2ContError4[3])))));

 result := result * (1 + Error);
end;

function FastPower2MinError5(Value: Single): Single;
var
  IntCast : Integer absolute result;
  j       : Integer;
const
  CP2MinError5 : array [0..4] of Single = (6.93146707655684646E-1,
    2.40222758408825315E-1, 5.55115371076677494E-2, 9.66918194816469324E-3,
    1.31179782896480692E-3);
begin
 j := round(Value);
 IntCast := ((($7F + j) shl 23) and $FF800000);
 Value := Value - j;
 result :=  result * (1 +
             Value * (CP2MinError5[0] +
             Value * (CP2MinError5[1] +
             Value * (CP2MinError5[2] +
             Value * (CP2MinError5[3] +
             Value *  CP2MinError5[4])))));

 result := result * (1 + Error);
end;

function FastPower2ContinousError5(Value: Single): Single;
var
  IntCast : Integer absolute result;
  j       : Integer;
begin
 j := round(Value);
 IntCast := ((($7F + j) shl 23) and $FF800000);
 Value := Value - j;
 result :=  result * (1 +
             Value * (CP2ContError5[0] + Value *
                     (CP2ContError5[1] + Value *
                     (CP2ContError5[2] + Value *
                     (CP2ContError5[3] + Value *
                     (CP2ContError5[4]))))));

 result := result * (1 + Error);
end;

function FastLog2Laurent(Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Value;
begin
 log2   := ((x shr 23) and $FF) - $80;
 x      := x and (not ($FF shl 23)) + $7F shl 23;
 Value  := ((CMinusOneThird * Value) + CTwo32) * Value + CMinusTwoThird;
 Result := Value + log2;
end;

function FastLog2MinError2(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2MinError2 : array [0..1] of Single = (1.00055782634514956,
    4.23544952666627533E-2);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + (CL2MinError2[0] *
          Result + CL2MinError2[1]);
end;

function FastLog2ContinousError2(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2Continous2 : array [0..1] of Single = (1.00011486779516678,
    3.26835576187857176E-4);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + (CL2Continous2[0] *
           Result + CL2Continous2[1]);
end;

function FastLog2Laurent2(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2Laurent2 : array [0..1] of Single = (1.00000009294157932,
    -8.24519535454190642E-8);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + (CL2Laurent2[0] *
           Result + CL2Laurent2[1]);
end;

function FastLog2MinError3(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2MinError3 : array [0..2] of Single = (-3.45237616924014556E-1,
    2.02572339392057543, -6.75567209748426434E-1);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + ((CL2MinError3[0] *
           Result + CL2MinError3[1]) *
           Result + CL2MinError3[2]);
end;

function FastLog2ContinousError3(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2Continous3 : array [0..2] of Single = (-3.33191603749037668E-1,
    1.99957454862186501, -6.64176001948231232E-1);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + ((CL2Continous3[0] *
           Result + CL2Continous3[1]) *
           Result + CL2Continous3[2]);
end;

function FastLog2Laurent3(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2Laurent3 : array [0..2] of Single = (-3.46544729609133795E-1,
    2.03963383654773933, -6.93089382045648295E-1);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + ((CL2Laurent3[0] *
           Result + CL2Laurent3[1]) *
           Result + CL2Laurent3[2]);
end;

function FastLog2MinError4(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2MinError4 : array [0..3] of Single = (1.58518682965714114E-1,
    -1.05301320934821097, 3.04944518136610121, -1.15431731153602279);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + ((CL2MinError4[0] *
           Result + CL2MinError4[1]) *
           Result + CL2MinError4[2]) *
           Result + CL2MinError4[3];
end;

function FastLog2ContinousError4(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2Continous4 : array [0..3] of Single = (1.63659707946391092E-1,
    -1.09661081181213116, 3.14421441381158484, -1.21126297836588193);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + ((CL2Continous4[0] *
           Result + CL2Continous4[1]) *
           Result + CL2Continous4[2]) *
           Result + CL2Continous4[3];
end;

function FastLog2Laurent4(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2Laurent4 : array [0..3] of Single = (1.59220899692695511E-1,
    -1.05974853905456978, 3.06469939326067076, -1.16417164373658544);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + ((CL2Laurent4[0] *
           Result + CL2Laurent4[1]) *
           Result + CL2Laurent4[2]) *
           Result + CL2Laurent4[3];
end;

function FastLog2MinError5(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2MinError5 : array [0..4] of Single = (-8.18038640187952054E-2,
    6.46216635143615381E-1, -2.12293700635511007, 4.07217052527789480,
    -1.51355930430330177);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + (((CL2MinError5[0] *
            Result + CL2MinError5[1]) *
            Result + CL2MinError5[2]) *
            Result + CL2MinError5[3]) *
            Result + CL2MinError5[4];
end;

function FastLog2ContinousError5(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2Continous5 : array [0..4] of Single = (-8.21343513178931783E-2,
    6.49732456739820052E-1, -2.13417801862571777, 4.08642207062728868,
    -1.51984215742349793);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + (((CL2Continous5[0] *
           Result + CL2Continous5[1]) *
           Result + CL2Continous5[2]) *
           Result + CL2Continous5[3]) *
           Result + CL2Continous5[4];
end;

function FastLog2Laurent5(const Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Result;
const
  CL2Laurent5 : array [0..4] of Single = (-8.00848677328682978E-2,
    6.38108601387251673E-1, -2.11019449052551389, 4.06509622185509922,
    -1.51292537160088569);
begin
 Result := Value;
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Result := log2 + (((CL2Laurent5[0] *
           Result + CL2Laurent5[1]) *
           Result + CL2Laurent5[2]) *
           Result + CL2Laurent5[3]) *
           Result + CL2Laurent5[4];
end;


// Convert a value in dB's to a linear amplitude

function FastdBtoAmpMinError2(const Value: Single): Single;
var
  IntCast : Integer absolute Value;
begin
 if (IntCast and $FF800000) shr 23 > $3F
  then Result := FastPower2MinError2(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpMinError2(const Value: Double): Double;
begin
 if (Value > -1000.0)
  then Result := FastPower2MinError2(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpContinousError2(const Value: Single): Single;
var
  IntCast : Integer absolute Value;
begin
 if (IntCast and $FF800000) shr 23 > $3F
  then Result := FastPower2ContinousError2(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpContinousError2(const Value: Double): Double;
begin
 if (Value > -1000.0)
  then Result := FastPower2ContinousError2(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpMinError3(const Value: Single): Single;
var
  IntCast : Integer absolute Value;
begin
 if (IntCast and $FF800000) shr 23 > $3F
  then Result := FastPower2MinError3(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpMinError3(const Value: Double): Double;
begin
 if (Value > -1000.0)
  then Result := FastPower2MinError3(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpContinousError3(const Value: Single): Single;
var
  IntCast : Integer absolute Value;
begin
 if (IntCast and $FF800000) shr 23 > $3F
  then Result := FastPower2ContinousError3(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpContinousError3(const Value: Double): Double;
begin
 if (Value > -1000.0)
  then Result := FastPower2ContinousError3(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpMinError4(const Value: Single): Single;
var
  IntCast : Integer absolute Value;
begin
 if (IntCast and $FF800000) shr 23 > $3F
  then Result := FastPower2MinError4(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpMinError4(const Value: Double): Double;
begin
 if (Value > -1000.0)
  then Result := FastPower2MinError4(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpContinousError4(const Value: Single): Single;
var
  IntCast : Integer absolute Value;
begin
 if (IntCast and $FF800000) shr 23 > $3F
  then Result := FastPower2ContinousError4(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpContinousError4(const Value: Double): Double;
begin
 if (Value > -1000.0)
  then Result := FastPower2ContinousError4(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpMinError5(const Value: Single): Single;
begin
 if (Value > -1000.0)
  then Result := FastPower2MinError5(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpMinError5(const Value: Double): Double;
begin
 if (Value > -1000.0)
  then Result := FastPower2MinError5(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpContinousError5(const Value: Single): Single;
begin
 if (Value > -1000.0)
  then Result := FastPower2ContinousError5(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;

function FastdBtoAmpContinousError5(const Value: Double): Double;
begin
 if (Value > -1000.0)
  then Result := FastPower2ContinousError5(Value * CdBtoAmpExpGain32)
  else Result := 0;
end;


// Convert a value in dB's to a linear amplitude

function FastAmptodBMinError2(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2MinError2(Value);
end;

function FastAmptodBMinError2(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2MinError2(Value);
end;

function FastAmptodBContinousError2(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2ContinousError2(Value);
end;

function FastAmptodBContinousError2(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2ContinousError2(Value);
end;

function FastAmptodBLaurent2(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2Laurent2(Value);
end;

function FastAmptodBLaurent2(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2Laurent2(Value);
end;

function FastAmptodBMinError3(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2MinError3(Value);
end;

function FastAmptodBMinError3(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2MinError3(Value);
end;

function FastAmptodBContinousError3(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2ContinousError3(Value);
end;

function FastAmptodBContinousError3(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2ContinousError3(Value);
end;

function FastAmptodBLaurent3(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2Laurent3(Value);
end;

function FastAmptodBLaurent3(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2Laurent3(Value);
end;

function FastAmptodBMinError4(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2MinError4(Value);
end;

function FastAmptodBMinError4(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2MinError4(Value);
end;

function FastAmptodBContinousError4(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2ContinousError4(Value);
end;

function FastAmptodBContinousError4(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2ContinousError4(Value);
end;

function FastAmptodBLaurent4(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2Laurent4(Value);
end;

function FastAmptodBLaurent4(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2Laurent4(Value);
end;

function FastAmptodBMinError5(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2MinError5(Value);
end;

function FastAmptodBMinError5(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2MinError5(Value);
end;

function FastAmptodBContinousError5(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2ContinousError5(Value);
end;

function FastAmptodBContinousError5(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2ContinousError5(Value);
end;

function FastAmptodBLaurent5(const Value: Single): Single;
begin
 result := CFactor2IndB32 * FastLog2Laurent5(Value);
end;

function FastAmptodBLaurent5(const Value: Double): Double;
begin
 result := CFactor2IndB64 * FastLog2Laurent5(Value);
end;

function FastIntPower(i: Single; n: Integer): Single;
var
  l : Integer absolute i;
begin
 Result := (l - $3F800000) shr (n-1) + $3F800000;
end;

function FastPower(base, exp : Double): Double;
begin
 Result := Power(base, exp);
end;

function FastPower2(const Value: Single): Single;
var
  IntCast : Integer absolute result;
begin
 IntCast := ((($7F + round(Value)) shl 23) and $FF800000);
end;

function FastRoot(i: Single; n: Integer): Single;
var
  l : Integer absolute i;
begin
 Result := (l - $3F800000) shr (n-1) + $3F800000;
end;

function FastTanhOpt3Term(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b := 1.26175667589988239 + a *
    (-0.54699348440059470 + a *
    ( 2.66559097474027817));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt4Term(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  0.89690305801668457 + a *
     ( 1.89047619399687661 + a *
     (-1.35205169119085666 + a *
       1.74656303770202670));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt5Term(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  1.03971379878158321 + a *
     ( 0.54953758170495126 + a *
     ( 2.13184139104070569 + a *
     (-1.46060069227128242 + a *
     ( 0.91996358346770157))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt6Term(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  0.98516470896867081 + a *
     ( 1.21020234045009012 + a *
     (-0.22720155259481389 + a *
     ( 1.89719615102030725 + a *
     (-1.07161642656874956 + a *
     ( 0.40487405571569546)))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt7Term(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  1.00518193411912860 + a *
     ( 0.91005085146116016 + a *
     ( 1.14542500876429276 + a *
     (-0.76509890972158046 + a *
     ( 1.34808969964882519 + a *
     (-0.60147655894944263 + a *
     ( 0.15264109378548973))))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt3Term(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b := 1.26175667589988239 + a *
    (-0.54699348440059470 + a *
    ( 2.66559097474027817));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt4Term(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  0.89690305801668457 + a *
     ( 1.89047619399687661 + a *
     (-1.35205169119085666 + a *
       1.74656303770202670));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt5Term(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  1.03971379878158321 + a *
     ( 0.54953758170495126 + a *
     ( 2.13184139104070569 + a *
     (-1.46060069227128242 + a *
     ( 0.91996358346770157))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt6Term(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  0.98516470896867081 + a *
     ( 1.21020234045009012 + a *
     (-0.22720155259481389 + a *
     ( 1.89719615102030725 + a *
     (-1.07161642656874956 + a *
     ( 0.40487405571569546)))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt7Term(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  1.00518193411912860 + a *
     ( 0.91005085146116016 + a *
     ( 1.14542500876429276 + a *
     (-0.76509890972158046 + a *
     ( 1.34808969964882519 + a *
     (-0.60147655894944263 + a *
     ( 0.15264109378548973))))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt3TermFPU(const Input: Single): Single; assembler;
const
  c0 : Double =  2.66559097474027817;
  c1 : Double = -0.54699348440059470;
  c2 : Double =  1.26175667589988239;
asm
 fld Input.Single  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: b := c2 + a * (c1 + a * c0), a, Input
 fXch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt4TermFPU(const Input: Single): Single; assembler;
const
  c0 : Double =  1.74656303770202670;
  c1 : Double = -1.35205169119085666;
  c2 : Double =  1.89047619399687661;
  c3 : Double =  0.89690305801668457;
asm
 fld Input.Single  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: b := c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt5TermFPU(const Input: Single): Single; assembler;
const
  c0 : Double =  0.91996358346770157;
  c1 : Double = -1.46060069227128242;
  c2 : Double =  2.13184139104070569;
  c3 : Double =  0.54953758170495126;
  c4 : Double =  1.03971379878158321;
asm
 fld Input.Single      // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt6TermFPU(const Input: Single): Single; assembler;
const
  c0 : Double =  0.40487405571569546;
  c1 : Double = -1.07161642656874956;
  c2 : Double =  1.89719615102030725;
  c3 : Double = -0.22720155259481389;
  c4 : Double =  1.21020234045009012;
  c5 : Double =  0.98516470896867081;
asm
 fld Input.Single  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fadd c5           // Stack: b := c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt7TermFPU(const Input: Single): Single; assembler;
const
  c0 : Double =  0.152641093785489734;
  c1 : Double = -0.60147655894944263;
  c2 : Double =  1.34808969964882519;
  c3 : Double = -0.765098909721580456;
  c4 : Double =  1.14542500876429276;
  c5 : Double =  0.91005085146116016;
  c6 : Double =  1.00518193411912860;
asm
 fld Input.Single  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fadd c5           // Stack: c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fmul st(0), st(1) // Stack: a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
 fadd c6           // Stack: b := c6 + a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt3TermFPU(const Input: Double): Double; assembler;
const
  c0 : Double =  2.66559097474027817;
  c1 : Double = -0.54699348440059470;
  c2 : Double =  1.26175667589988239;
asm
 fld Input.Double  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: b := c2 + a * (c1 + a * c0), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt4TermFPU(const Input: Double): Double; assembler;
const
  c0 : Double =  1.74656303770202670;
  c1 : Double = -1.35205169119085666;
  c2 : Double =  1.89047619399687661;
  c3 : Double =  0.89690305801668457;
asm
 fld Input.Double      // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: b := c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt5TermFPU(const Input: Double): Double; assembler;
const
  c4 : Double =  0.91996358346770157;
  c3 : Double = -1.46060069227128242;
  c2 : Double =  2.13184139104070569;
  c1 : Double =  0.54953758170495126;
  c0 : Double =  1.03971379878158321;
asm
 fld Input.Double  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt6TermFPU(const Input: Double): Double; assembler;
const
  c0 : Double =  0.40487405571569546;
  c1 : Double = -1.07161642656874956;
  c2 : Double =  1.89719615102030725;
  c3 : Double = -0.22720155259481389;
  c4 : Double =  1.21020234045009012;
  c5 : Double =  0.98516470896867081;
asm
 fld Input.Double  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fadd c5           // Stack: b := c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt7TermFPU(const Input: Double): Double; assembler;
const
  c0 : Double =  0.152641093785489734;
  c1 : Double = -0.60147655894944263;
  c2 : Double =  1.34808969964882519;
  c3 : Double = -0.765098909721580456;
  c4 : Double =  1.14542500876429276;
  c5 : Double =  0.91005085146116016;
  c6 : Double =  1.00518193411912860;
asm
 fld Input.Double  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fadd c5           // Stack: c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fmul st(0), st(1) // Stack: a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
 fadd c6           // Stack: b := c6 + a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanh2Like4Term(const Input: Single): Single;
var
  a, b: Single;
begin
 a := abs(Input);
 b := 12 + a * (6 + a * (3 + a));
 Result := (Input * b) / (a * b + 24);
end;

function FastTanh2Like3Term(const Input: Single): Single;
var
  a, b: Single;
begin
 a := abs(Input);
 b := (6 + a * (3 + a));
 Result := (Input * b) / (a * b + 12);
end;

function FastTanh2Like2Term(const Input: Single): Single;
{$IFDEF PUREPASCAL}
var
  a, b: Single;
begin
 a := abs(Input);
 b := 3 + a;
 Result := (Input * b) / (a * b + 6 );
{$ELSE}
const
  c3: Single = 3;
  c6: Single = 6;
asm
 fld Input.Single;
 fabs
 fld c3
 fadd st(0),st(1)
 fld st(0)
 fmul Input.Single
 fxch st(2)
 fmulp
 fadd c6.Single
 fdiv
{$ENDIF}
end;

function FastTanh2Like1Term(const Input: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Result := Input / (abs(Input) + 3);
{$ELSE}
const c3 : Single = 3;
asm
 fld Input.Single;
 fld Input.Single;
 fabs
 fadd c3
 fdiv
{$ENDIF}
end;


procedure InitConstants;
{$IFDEF PUREPASCAL}
begin
 Ln2            := Ln(2);
 Ln2Half        := Ln2 * CHalf32;
 Ln2Rez         := 1 / ln2;
 Ln10           := Ln(10);
 TanSixthPi64   := tan(CSixthPi64);
 TanTwelfthPi64 := tan(CTwelfthPi64);
 TanSixthPi32   := TanSixthPi64;
 TanTwelfthPi32 := TanTwelfthPi64;
end;
{$ELSE}
begin
 Ln2            := ln(2); // ToDo: use ASM here!
 Ln2Half        := Ln2 * CHalf32;
 Ln2Rez         := 1 / ln2;
 Ln10           := Ln(10);
 TanSixthPi64   := tan(CSixthPi64);
 TanTwelfthPi64 := tan(CTwelfthPi64);
 TanSixthPi32   := TanSixthPi64;
 TanTwelfthPi32 := TanTwelfthPi64;
end;
{$ENDIF}

initialization
 InitConstants;

end.
