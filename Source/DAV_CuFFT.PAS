unit DAV_CuFft;

interface

{$I DAV_Compiler.inc}

const
  {$IFDEF Emulation}
  DLLNAME = 'CuFftEmu.dll';
  {$ELSE}
  DLLNAME = 'CuFft.dll';
  {$ENDIF}

// CuFft API function return values

type
  TCuFftResult = (
    cfrSuccess = 0,
    cfrInvalidPlan = 1,
    cfrAllocFailed = 2,
    cfrInvalidType = 3,
    cfrInvalidValue = 4,
    cfrInternalError = 5,
    cfrExecutionFailed = 6,
    cfrSetupFailed = 7,
    cfrInvalidSize = 8);

  // CuFft defines and supports the following data types

  // TCuFftHandle is a handle type used to store and access CuFft plans.
  TCuFftHandle = Cardinal;

  // TCuFftReal is a single-precision, floating-point real data type.
  TCuFftReal = Single;

  // TCuFftComplex is a single-precision, floating-point complex data type that
  // consists of interleaved real and imaginary components.
  // typedef float TCuFftComplex[2];
  TCuFftComplex = array[0..1] of Single;

  // CuFft supports the following transform types
  TCuFftType = (CCuFftR2C = $2A, CCuFftC2R = $2C, CCuFftC2C = $29);

const
  // CuFft transform directions
  CCuFftForward = -1; // Forward FFT
  CCuFftInverse = 1; // Inverse FFT

  CCuFftResultStrings : array[TCuFftResult] of String = (
    'Success',
    'Invalid Plan',
    'Alloc Failed',
    'Invalid Type',
    'Invalid Value',
    'Internal Error',
    'Execution Failed',
    'Setup Failed',
    'Invalid Size');

function CuFftPlan1d(var plan: TCuFftHandle;
                     nx: Integer;
                     atype: TCuFftType;
                     batch: Integer): TCuFftResult; stdcall ; external DLLNAME name 'cufftPlan1d';


function CuFftPlan2d(var plan: TCuFftHandle;
                     nx: Integer;
                     ny: Integer;
                     atype: TCuFftType): TCuFftResult; stdcall ; external DLLNAME name 'cufftPlan2d';


function CuFftPlan3d(var plan: TCuFftHandle;
                     nx: Integer;
                     ny: Integer;
                     nz: Integer;
                     atype: TCuFftType): TCuFftResult; stdcall ; external DLLNAME name 'cufftPlan3d';


function CuFftDestroy(plan: TCuFftHandle): TCuFftResult; stdcall ; external DLLNAME name 'cufftDestroy';


function CuFftExecC2C(plan: TCuFftHandle;
                      idata: Pointer;
                      odata: Pointer;
                      direction: Integer): TCuFftResult; stdcall ; external DLLNAME name 'cufftExecC2C';


function CuFftExecR2C(plan: TCuFftHandle;
                      idata: Pointer; // Real
                      odata: Pointer {TCuFftComplex}): TCuFftResult; stdcall ; external DLLNAME name 'cufftExecR2C';


function CuFftExecC2R(plan: THandle;
                      idata: Pointer; //TComplex;
                      odata: Pointer {TReal}): TCuFftResult; stdcall ; external DLLNAME name 'cufftExecC2R';

implementation

end.
