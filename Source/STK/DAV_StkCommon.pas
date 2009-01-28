unit DAV_StkCommon;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common;

{$I DAV_StkConsts.inc}

type
  STK_FORMAT = LongInt;
  SINT16 = shortint;
  SINT32 = smallint;
  FLOAT32 = Single;
  FLOAT64 = Double;

type
  TStk = class
  public
    procedure SetSampleRate(const Value: Single);
  protected
    FSampleRate    : Single;
    FSampleRateInv : Single;
    procedure SampleRateChanged; virtual;
  public
    constructor Create(const SampleRate: Single = 44100); virtual;

    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

implementation

(*
const
  COne128th : Single = 0.0078125;

function PIndex(p: PSingle; i: LongInt): PSingle;
function Index(p: PSingle; i: LongInt): Single;

function pindex(p: pSingle; i: LongInt): pSingle;
begin
  Result := pSingle(LongInt(p) + sizeof(Single) * i);
end;

function index(p: pSingle; i: LongInt): Single;
var
  q: PSingle;
begin
  q := pSingle(LongInt(p) + sizeof(Single) * i);
  Result := q^;
end;

    STK_SINT8,   //*!< -128 to +127 */
    STK_SINT16,  //*!< -32768 to +32767 */
    STK_SINT32,  //*!< -2147483648 to +2147483647. */
    STK_FLOAT32, //*!< Normalized between plus/minus 1.0. */
    STK_FLOAT64: //*!< Normalized between plus/minus 1.0. */
    stk_format;

Single Stk :: srate = (Single) SRATE;
const Stk::STK_FORMAT Stk :: STK_SINT8 = 1;
const Stk::STK_FORMAT Stk :: STK_SINT16 = 2;
const Stk::STK_FORMAT Stk :: STK_SINT32 = 8;
const Stk::STK_FORMAT Stk :: STK_FLOAT32 = 16;
const Stk::STK_FORMAT Stk :: STK_FLOAT64 = 32;
*)

constructor TStk.Create(const SampleRate: Single = 44100);
begin
 FSampleRate := SampleRate;
 SampleRateChanged;
end;

procedure TStk.SampleRateChanged;
begin
 FSampleRateInv := 1 / FSampleRate;
end;

procedure TStk.SetSampleRate(const Value: Single);
begin
 if (Value > 0) then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

end.
