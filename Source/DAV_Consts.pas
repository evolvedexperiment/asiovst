unit DAV_Consts;

interface

const
  CDenorm32          : Single = 1E-24;
  CTwoPI32           : Single = 2 * Pi;
  CFourPI32          : Single = 4 * Pi;
  CHalf32            : Single = 0.5;
  CQuarter32         : Single = 0.25;
  CTen32             : Single = 10;
  CTwenty32          : Single = 20;
  COneTwelfth32      : Single = 1 / 12;
  CMinusOneSixteenth : Single = -0.0625;
  CTwoMulTwo2Neg32   : Single = ((2.0/$10000) / $10000);  // 2^-32

  CDenorm64      : Double = 1E-34;
  CTwoPI64       : Double = 2 * Pi;
  CFourPI64      : Double = 4 * Pi;
  CHalf64        : Double = 0.5;
  CQuarter64     : Double = 0.25;
  CTen64         : Double = 10;
  CTwenty64      : Double = 20;

  CMaxLongInt    : Integer =  $7FFFFFFF;
  CMinLongInt    : Integer = -$7FFFFFFF - 1;
  CMaxInt64      : Int64 =  9223372036854775807;
  CMinInt64      : Int64 = -9223372036854775807 - 1;
  CMaxSingle     : Single = 3.40282346638528860e+38;
  CMinusHalf32   : Single = -0.5;

  CMinusOneThird : Double = -1 / 3;
  CMinusTwoThird : Double = -2 / 3;
  CTwo32         : Single = 2;
  CTwo64         : Double = 2;
  CTwoDivPi32    : Single = 2.0 / Pi;
  CTwoDivPi64    : Double = 2.0 / Pi;
  CPiHalf32      : Single = Pi * 0.5;
  CPiHalf64      : Double = Pi * 0.5;
  CThreeHalfPi32 : Single = 1.5 * Pi; // pi times 3/2, used in tan routines
  CThreeHalfPi64 : Double = 1.5 * Pi; // pi times 3/2, used in tan routines
  CFourDivPi32   : Single = 4.0 / Pi; // 4 / pi, used in tan routines
  CFourDivPi64   : Double = 4.0 / Pi; // 4 / pi, used in tan routines
  CFourthPi32    : Single = Pi * 0.25; // pi / 4.0, used in tan routines
  CFourthPi64    : Double = Pi * 0.25; // pi / 4.0, used in tan routines
  CSixthPi32     : Single = Pi / 6.0; // pi/6.0, used in atan routines
  CSixthPi64     : Double = Pi / 6.0; // pi/6.0, used in atan routines
  CTwelfthPi32   : Single = Pi / 12.0; // pi/12.0, used in atan routines
  CTwelfthPi64   : Double = Pi / 12.0; // pi/12.0, used in atan routines

  CInv2PI  : Single = 1 / (2 * Pi);
  CInv360  : Single = 1 / 360;
  C180     : Single = 180;
  C360     : Single = 360;

implementation

end.

