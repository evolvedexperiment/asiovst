unit DAV_CudaRT;

interface

const
  DLLNAME = 'CudaRt.dll';

type
  TCudaError = (
    ceSuccess = 0, 
    ceErrorMissingConfiguration,
    ceMemoryAllocation, 
    ceInitializationError, 
    ceLaunchFailure, 
    cePriorLaunchFailure,
    ceLaunchTimeout,
    ceLaunchOutOfResources,
    ceInvalidDeviceFunction,
    ceInvalidConfiguration,
    ceInvalidDevice,
    ceInvalidValue,
    ceInvalidPitchValue,
    ceInvalidSymbol,
    ceMapBufferObjectFailed,
    ceUnmapBufferObjectFailed,
    ceInvalidHostPointer,
    ceInvalidDevicePointer,
    ceInvalidTexture,
    ceInvalidTextureBinding,
    ceInvalidChannelDescriptor,
    ceInvalidMemcpyDirection,
    ceAddressOfConstant,
    ceTextureFetchFailed,
    ceTextureNotBound,
    ceSynchronizationError,
    ceInvalidFilterSetting,
    ceInvalidNormSetting,
    ceMixedDeviceExecution,
    ceCudartUnloading,
    ceUnknown,
    ceNotYetImplemented,
    ceMemoryValueTooLarge,
    ceInvalidResourceHandle,
    ceNotReady,
    ceStartupFailure = $7f,
    ceApiFailureBase = 10000);

  TCudaStream = Integer;
  TCudaEvent = Integer;

  TCudaChannelFormatKind = (ccfkSigned, ccfkUnsigned, ccfkFloat);

  PCudaChannelFormatDesc = ^TCudaChannelFormatDesc;
  TCudaChannelFormatDesc = record
    x: Integer;
    y: Integer;
    z: Integer;
    w: Integer;
    f: TCudaChannelFormatKind;
  end;

  TCudaArray = record end;//!ATTENTION foreward Declaration?)

  TCudaMemcpyKind = (cmkHostToHost = 0, cmkHostToDevice, cmkDeviceToHost,
    cmkDeviceToDevice);

  TCudaPitchedPtr = record
    ptr: Pointer;
    Pitch: Cardinal;
    xsize: Cardinal;
    ysize: Cardinal;
  end;

  TCudaExtent = record
    Width: Cardinal;
    Height: Cardinal;
    depth: Cardinal;
  end;

  TCudaPos = record
    x: Cardinal;
    y: Cardinal;
    z: Cardinal;
  end;

  TCudaMemcpy3DParms = record
    srcArray : Pointer;
    srcPos   : TCudaPos;
    srcPtr   : TCudaPitchedPtr;
    dstArray : Pointer;
    dstPos   : TCudaPos;
    dstPtr   : TCudaPitchedPtr;
    extent   : TCudaExtent;
    kind     : TCudaMemcpyKind;
  end;

  PcudaDeviceProp = ^TCudaDeviceProp;
  TCudaDeviceProp = record
    name                : array[0..256 - 1] of Char;
    totalGlobalMem      : Cardinal;
    sharedMemPerBlock   : Cardinal;
    regsPerBlock        : Integer;
    warpSize            : Integer;
    memPitch            : Cardinal;
    maxThreadsPerBlock  : Integer;
    maxThreadsDim       : array[0..3 - 1] of Integer;
    maxGridSize         : array[0..3 - 1] of Integer;
    clockRate           : Integer;
    totalConstMem       : Cardinal;
    major               : Integer;
    minor               : Integer;
    textureAlignment    : Cardinal;
    deviceOverlap       : Integer;
    multiProcessorCount : Integer;
    __cudaReserved      : array[0..40 - 1] of Integer;
  end;

function cudaMalloc(var devPtr: Pointer;
                    Size: Cardinal): TCudaError; stdcall ; external DLLNAME name 'cudaMalloc';

function cudaMallocHost(var ptr: Pointer;
                        Size: Cardinal): TCudaError; stdcall ; external DLLNAME name 'cudaMallocHost';

function cudaMallocPitch(var devPtr: Pointer;
                         var Pitch: Cardinal;
                         Width: Cardinal;
                         Height: Cardinal): TCudaError; stdcall ; external DLLNAME name 'cudaMallocPitch';

function cudaMallocArray(var aarray: Pointer;
                         var desc: TCudaChannelFormatDesc;
                         Width: Cardinal;
                         Height : Cardinal): TCudaError; stdcall ; external DLLNAME name 'cudaMallocArray';

function cudaFree(devPtr: Pointer): TCudaError; stdcall ; external DLLNAME name 'cudaFree';

function cudaFreeHost(ptr: Pointer): TCudaError; stdcall ; external DLLNAME name 'cudaFreeHost';

function cudaFreeArray(const aarray: Pointer): TCudaError; stdcall ; external DLLNAME name 'cudaFreeArray';

function cudaGetDeviceCount(var Count: Integer): TCudaError; stdcall ; external DLLNAME name 'cudaGetDeviceCount';

function cudaGetDeviceProperties(var Prop: TCudaDeviceProp;
                                 Device: Integer): TCudaError; stdcall ; external DLLNAME name 'cudaGetDeviceProperties';

function cudaChooseDevice(var Device: Integer;
                          const Prop: PcudaDeviceProp): TCudaError; stdcall ; external DLLNAME name 'cudaChooseDevice';

function cudaSetDevice(Device: Integer): TCudaError;  stdcall ; external DLLNAME name 'cudaSetDevice';

function cudaGetDevice(var Device: Integer): TCudaError;  stdcall ; external DLLNAME name 'cudaGetDevice';

const
  CCudaErrorStrings : array[0..36] of string = (
    'Success',
    'ErrorMissingConfiguration',
    'ErrorMemoryAllocation',
    'ErrorInitializationError',
    'ErrorLaunchFailure',
    'ErrorPriorLaunchFailure',
    'ErrorLaunchTimeout',
    'ErrorLaunchOutOfResources',
    'ErrorInvalidDeviceFunction',
    'ErrorInvalidConfiguration',
    'ErrorInvalidDevice',
    'ErrorInvalidValue',
    'ErrorInvalidPitchValue',
    'ErrorInvalidSymbol',
    'ErrorMapBufferObjectFailed',
    'ErrorUnmapBufferObjectFailed',
    'ErrorInvalidHostPointer',
    'ErrorInvalidDevicePointer',
    'ErrorInvalidTexture',
    'ErrorInvalidTextureBinding',
    'ErrorInvalidChannelDescriptor',
    'ErrorInvalidMemcpyDirection',
    'ErrorAddressOfConstant',
    'ErrorTextureFetchFailed',
    'ErrorTextureNotBound',
    'ErrorSynchronizationError',
    'ErrorInvalidFilterSetting',
    'ErrorInvalidNormSetting',
    'ErrorMixedDeviceExecution',
    'ErrorCudartUnloading',
    'ErrorUnknown',
    'ErrorNotYetImplemented',
    'ErrorMemoryValueTooLarge',
    'ErrorInvalidResourceHandle',
    'ErrorNotReady',
    'ErrorStartupFailure',
    'ErrorApiFailureBase');


implementation

end.
