unit DAV_MidiIO;

//****************************************************************************/
//* MIDI device classes by Adrian Meyer
//****************************************************************************/
//* V1.1 Delphi 6 Windows 2000
//****************************************************************************/
//* V1.0 First release with simple MIDI Input/Output
//* V1.1 SysEx Input Event added, refactured error handling
//* V1.2 SysEx Output procedure added, changes sysex input for multiple ports
//****************************************************************************/
//* Homepage: http://www.midimountain.com
//****************************************************************************/
//* If you get a hold of this source you may use it upon your own risk. Please
//* let me know if you have any questions: adrian.meyer@rocketmail.com.
//****************************************************************************/

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC}LCLIntf, {$ELSE} Windows, {$ENDIF} Classes, SysUtils, Math,
  Contnrs, MMSystem;

const
  // size of system exclusive buffer
  cSysExBufferSize = 2048;

type
  // event if data is received
  TOnMidiInData = procedure (const aDeviceIndex: Integer; const aStatus, aData1, aData2: Byte) of object;
  // event of system exclusive data is received
  TOnSysExData = procedure (const aDeviceIndex: Integer; const aStream: TMemoryStream) of object;

  EMidiDevices = Exception;

  // base class for MIDI devices
  TMidiDevices = class
  private
    fDevices    : TStringList;
    fMidiResult : MMResult;
    procedure SetMidiResult(const Value: MMResult);
  protected
    property MidiResult: MMResult read fMidiResult write SetMidiResult;
    function GetHandle(const aDeviceIndex: Integer): THandle;
  public
    constructor Create; virtual;                                                // create the MIDI devices
    destructor Destroy; override;                                               // whack the devices
    procedure Open(const aDeviceIndex: Integer); virtual; abstract;             // open a specific device
    procedure Close(const aDeviceIndex: Integer); virtual; abstract;            // close a specific device
    procedure CloseAll;                                                         // close all devices
    function IsOpen(aDeviceIndex: Integer): Boolean;                            // check if open

    property Devices: TStringList read fDevices;                                // THE devices
  end;

  // MIDI input devices
  TMidiInput = class(TMidiDevices)
  private
    fOnMidiData  : TOnMidiInData;
    fOnSysExData : TOnSysExData;
    fSysExData   : TObjectList;
  protected
    procedure DoSysExData(const aDeviceIndex: Integer);
  public
    constructor Create; override;                                               // create an input device
    destructor Destroy; override;                                               // what the input devices
    procedure Open(const aDeviceIndex: Integer); override;                      // open a specific input device
    procedure Close(const aDeviceIndex: Integer); override;                     // close a specific device

    property OnMidiData: TOnMidiInData read fOnMidiData write fOnMidiData;      // midi data event
    property OnSysExData: TOnSysExData read fOnSysExData write fOnSysExData;    // midi system exclusive is received
  end;

  // MIDI output devices
  TMidiOutput = class(TMidiDevices)
    constructor Create; override;
    procedure Open(const aDeviceIndex: Integer); override;                      // open a specific input device
    procedure Close(const aDeviceIndex: Integer); override;                     // close a specific device
    procedure Send(const aDeviceINdex: Integer; const aStatus, aData1, aData2: Byte);         // send some midi data to the indexed device
    procedure SendSysEx(const aDeviceIndex: Integer; const aStream: TMemoryStream); overload; // send system exclusive data to a device
    procedure SendSysEx(const aDeviceIndex: Integer; const aString: string); overload;
  end;

  function SysExStreamToStr(const aStream: TMemoryStream): string; // convert the stream into xx xx xx xx string
  procedure StrToSysExStream(const aString: string; const aStream: TMemoryStream); // fill the string in a xx xx xx xx into the stream

  function MidiInput: TMidiInput;   // MIDI input devices
  function MidiOutput: TMidiOutput; // MIDI output Devices

implementation

{ TMidiBase }
type
  TSysExBuffer = array[0..cSysExBufferSize] of Char;

  TSysExData = class
  private
    fSysExStream: TMemoryStream;
  public
    SysExHeader: {$IFDEF FPC}_midihdr{$ELSE}TMidiHdr{$ENDIF};
    SysExData: TSysExBuffer;
    constructor Create;
    destructor Destroy; override;
    property SysExStream: TMemoryStream read fSysExStream;
  end;

constructor TMidiDevices.Create;
begin
 fDevices := TStringList.create;
end;

destructor TMidiDevices.Destroy;
begin
 FreeAndNil(fDevices);
 inherited;
end;

var
  gMidiInput: TMidiInput;
  gMidiOutput: TMidiOutput;

function MidiInput: TMidiInput;
begin
 if not assigned(gMidiInput)
  then gMidiInput := TMidiInput.Create;
 Result := gMidiInput;
end;

function MidiOutput: TMidiOutput;
begin
 if not assigned(gMidiOutput)
  then gMidiOutput := TMidiOutput.Create;
 Result := gMidiOutput;
end;

{ TMidiInput }

{$IFDEF FPC}
type
  PHMIDIIN = ^HMIDIIN;
  TMidiHdr = _midihdr;
  TMidiInCapsA = _MIDIINCAPSA;
  TMidiInCapsW = _MIDIINCAPSW;
  TMidiInCaps = TMidiInCapsA;
  TMidiOutCapsA = _MIDIOUTCAPSA;
  TMidiOutCapsW = _MIDIOUTCAPSW;
  TMidiOutCaps = TMidiOutCapsA;
{$ENDIF}

procedure midiInCallback(aMidiInHandle: PHMIDIIN; aMsg: Integer; aData, aMidiData, aTimeStamp: Integer); stdcall;
begin
 case aMsg of
  MIM_DATA: begin
   if assigned(MidiInput.OnMidiData)
    then MidiInput.OnMidiData(aData, aMidiData and $000000FF,
           (aMidiData and $0000FF00) shr 8, (aMidiData and $00FF0000) shr 16);
  end;
  MIM_LONGDATA: MidiInput.DoSysExData(aData);
 end;
end;

procedure TMidiInput.Close(const aDeviceIndex: Integer);
var
  DeviceHandle : THandle;
begin
 DeviceHandle := GetHandle(aDeviceIndex);
 if DeviceHandle = 0 then Exit else
  begin
   MidiResult := midiInStop(DeviceHandle);
   MidiResult := midiInReset(DeviceHandle);
   MidiResult := midiInUnprepareHeader(DeviceHandle, @TSysExData(fSysExData[aDeviceIndex]).SysExHeader, SizeOf(TMidiHdr));
   MidiResult := midiInClose(DeviceHandle);
   fDevices.Objects[aDeviceIndex] := nil;
  end;
end;

procedure TMidiDevices.CloseAll;
var
  i: Integer;
begin
 for i := 0 to fDevices.Count - 1 do Close(i);
end;

constructor TMidiInput.Create;
var
  ll, i   : Integer;
  lInCaps : TMidiInCaps;
begin
 inherited;
 fSysExData := TObjectList.Create(true);
 try
  ll := midiInGetNumDevs;
 except
  ll := 0;
 end;
 for i := 0 to ll - 1 do
  try
   MidiResult := midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps));
   if MidiResult = MMSYSERR_NOERROR then
   begin
    fDevices.Add(StrPas(lInCaps.szPname));
    fSysExData.Add(TSysExData.Create);
   end;
  except
   // do nothing but supress the exception to the end user
  end;
end;

procedure TMidiInput.Open(const aDeviceIndex: Integer);
var
  lHandle: THandle;
  lSysExData: TSysExData;
begin
 if GetHandle(aDeviceIndex) <> 0 then Exit;
 MidiResult := midiInOpen(@lHandle, aDeviceIndex, cardinal(@midiInCallback), aDeviceIndex, CALLBACK_FUNCTION);
 fDevices.Objects[aDeviceIndex] := TObject(lHandle);
 lSysExData := TSysExData(fSysExData[aDeviceIndex]);
 lSysExData.SysExHeader.dwFlags := 0;
 MidiResult := midiInPrepareHeader(lHandle, @lSysExData.SysExHeader, SizeOf(TMidiHdr));
 MidiResult := midiInAddBuffer(lHandle, @lSysExData.SysExHeader, SizeOf(TMidiHdr));
 MidiResult := midiInStart(lHandle);
end;

procedure TMidiInput.DoSysExData(const aDeviceIndex: Integer);
var
  lSysExData: TSysExData;
begin
 lSysExData := TSysExData(fSysExData[aDeviceIndex]);
 if lSysExData.SysExHeader.dwBytesRecorded = 0 then Exit;
 lSysExData.SysExStream.Write(lSysExData.SysExData, lSysExData.SysExHeader.dwBytesRecorded);
 if lSysExData.SysExHeader.dwFlags and MHDR_DONE = MHDR_DONE then
  begin
   lSysExData.SysExStream.Position := 0;
   if assigned(fOnSysExData) then fOnSysExData(aDeviceIndex, lSysExData.SysExStream);
   lSysExData.SysExStream.Clear;
  end;
 lSysExData.SysExHeader.dwBytesRecorded := 0;
 MidiResult := midiInPrepareHeader(GetHandle(aDeviceIndex), @lSysExData.SysExHeader, SizeOf(TMidiHdr));
 MidiResult := midiInAddBuffer(GetHandle(aDeviceIndex), @lSysExData.SysExHeader, SizeOf(TMidiHdr));
end;

destructor TMidiInput.Destroy;
begin
 FreeAndNil(fSysExData);
 inherited;
end;

{ TMidiOutput }

procedure TMidiOutput.Close(const aDeviceIndex: Integer);
begin
 if GetHandle(aDeviceIndex) = 0 then Exit;
 MidiResult := midiOutClose(GetHandle(aDeviceIndex));
 fDevices.Objects[ aDeviceIndex ] := nil;
end;

constructor TMidiOutput.Create;
var
  ll, i: Integer;
  lOutCaps: TMidiOutCaps;
begin
 inherited;
 try
  ll := midiOutGetNumDevs;
 except
  ll := 0;
 end;
 for i := 0 to ll - 1 do
  begin
   MidiResult := midiOutGetDevCaps(i, @lOutCaps, SizeOf(TMidiOutCaps));
   fDevices.Add(lOutCaps.szPname);
  end;
end;

procedure TMidiOutput.Open(const aDeviceIndex: Integer);
var
  lHandle: THandle;
begin
 // device already open;
 if GetHandle(aDeviceIndex) <> 0 then Exit;
 MidiResult := midiOutOpen(@lHandle, aDeviceIndex, 0, 0, CALLBACK_NULL);
 fDevices.Objects[aDeviceIndex] := TObject(lHandle);
end;

procedure TMidiOutput.Send(const aDeviceINdex: Integer; const aStatus,
  aData1, aData2: Byte);
var
  lMsg: cardinal;
begin
 // open the device is not open
 if not assigned(fDevices.Objects[ aDeviceIndex ]) then Open(aDeviceIndex);
 lMsg := aStatus + (aData1 * $100) + (aData2 * $10000);
 MidiResult := midiOutShortMsg(GetHandle(aDeviceIndex), lMSG);
end;

procedure TMidiDevices.SetMidiResult(const Value: MMResult);
var 
  lError: array[0..MAXERRORLENGTH] of char;
begin
 fMidiResult := Value;
 if fMidiResult <> MMSYSERR_NOERROR then
  if midiInGetErrorText(fMidiResult, @lError, MAXERRORLENGTH) = MMSYSERR_NOERROR
   then raise EMidiDevices.Create(StrPas(lError));
end;

function TMidiDevices.GetHandle(const aDeviceIndex: Integer): THandle;
begin
 try
  if not ((aDeviceIndex >= 0) and (aDeviceIndex <= (fDevices.Count - 1))) then
   raise EMidiDevices.CreateFmt('%s: Device index out of bounds! (%d)', [ClassName,aDeviceIndex]);
  Result := THandle(fDevices.Objects[aDeviceIndex]);
 except
  Result := 0;
 end; 
end;

function TMidiDevices.IsOpen(aDeviceIndex: Integer): boolean;
begin
 Result := GetHandle(aDeviceIndex) <> 0;
end;

procedure TMidiOutput.SendSysEx(const aDeviceIndex: Integer;
  const aString: string);
var
  lStream: TMemoryStream;
begin
 lStream := TMemoryStream.Create;
 try
  StrToSysExStream(aString, lStream);
  SendSysEx(aDeviceIndex, lStream);
 finally
  FreeAndNil(lStream);
 end;
end;

procedure TMidiOutput.SendSysEx(const aDeviceIndex: Integer;
  const aStream: TMemoryStream);
var
  lSysExHeader: TMidiHdr;
begin
 aStream.Position := 0;
 lSysExHeader.dwBufferLength := aStream.Size;
 lSysExHeader.lpData := aStream.Memory;
 lSysExHeader.dwFlags := 0;
 MidiResult := midiOutPrepareHeader(GetHandle(aDeviceIndex), @lSysExHeader, SizeOf(TMidiHdr));
 MidiResult := midiOutLongMsg( GetHandle(aDeviceIndex), @lSysExHeader, SizeOf(TMidiHdr));
 MidiResult := midiOutUnprepareHeader(GetHandle(aDeviceIndex), @lSysExHeader, SizeOf(TMidiHdr));
end;

{ TSysExData }

constructor TSysExData.Create;
begin
 SysExHeader.dwBufferLength := cSysExBufferSize;
 SysExHeader.lpData := SysExData;
 fSysExStream := TMemoryStream.Create;
end;

destructor TSysExData.Destroy;
begin
 FreeAndNil(fSysExStream);
end;

function SysExStreamToStr(const aStream: TMemoryStream): string;
var
  i: Integer;
begin
 Result := '';
 aStream.Position := 0;
 for i := 0 to aStream.Size - 1 do
  Result := Result + Format('%.2x ', [ Byte(pchar(aStream.Memory)[i]) ]);
end;

procedure StrToSysExStream(const aString: string; const aStream: TMemoryStream);
const
  cHex = '123456789ABCDEF';
var
  i: Integer;
  lStr: string;
begin
 lStr := StringReplace(AnsiUpperCase(aString), ' ', '', [rfReplaceAll]);
 aStream.Size := Length(lStr) div 2 - 1;
 aStream.Position := 0;
 for i:=1 to aStream.Size do
  pchar(aStream.Memory)[i-1] :=
   char(AnsiPos(lStr[ i*2 - 1], cHex) shl 4 + AnsiPos(lStr[i*2], cHex));
end;

initialization
  gMidiInput := nil;
  gMidiOutput := nil;

finalization
  FreeAndNil(gMidiInput);
  FreeAndNil(gMidiOutput);

end.
