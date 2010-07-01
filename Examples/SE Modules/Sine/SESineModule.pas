unit SESineModule;

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
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
//  SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_DspLFO;

type
  // define some constants to make referencing in/outs clearer
  TSESinePins = (pinFrequency, pinOutput);

  TSESineModule = class(TSEModuleBase)
  private
    FOutputBuffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FFrequency    : Single;
    {$IFDEF PUREPASCAL}
    FSineLFO      : TSineLFO;
    {$ELSE}
    FAngle        : TComplexDouble;
    FPosition     : TComplexDouble;
    {$ENDIF}
    procedure FrequencyChanged;
  protected
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

  TSESineCosineModule = class(TSEModuleBase)
  private
    FSineBuffer   : PDAVSingleFixedArray;
    FCosineBuffer : PDAVSingleFixedArray;
    FFrequency    : Single;
    {$IFDEF PUREPASCAL}
    FSineLFO      : TSineLFO;
    {$ELSE}
    FAngle        : TComplexDouble;
    FPosition     : TComplexDouble;
    {$ENDIF}
    procedure FrequencyChanged;
  protected
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

  TSESine2Module = class(TSEModuleBase)
  private
    FSineBuffer   : PDAVSingleFixedArray;
    FSine2Buffer  : PDAVSingleFixedArray;
    FFrequency    : Single;
    {$IFDEF PUREPASCAL}
    FSineLFO      : TSineLFO;
    {$ELSE}
    FAngle        : TComplexDouble;
    FPosition     : TComplexDouble;
    {$ENDIF}
    procedure FrequencyChanged;
  protected
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

{$IFNDEF PUREPASCAL}
procedure SESineProcess(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
procedure SESineCosineProcess(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
procedure SESine2Process(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
{$ENDIF}

implementation

uses
  SysUtils, DAV_Common, DAV_Math;

constructor TSESineModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FFrequency := 440;
 {$IFDEF PUREPASCAL}
 FSineLFO := TSineLFO.Create;
 FSineLFO.SampleRate := SampleRate;
 FSineLFO.Frequency := FFrequency;
 {$ELSE}
 FPosition.Re := 0;
 FPosition.Im := -1;
 FrequencyChanged;
 {$ENDIF}
end;

destructor TSESineModule.Destroy;
begin
 {$IFDEF PUREPASCAL}
 FreeAndNil(FSineLFO);
 {$ENDIF}
 inherited;
end;

procedure TSESineModule.Open;
begin
 inherited Open;

 {$IFNDEF PUREPASCAL}
 FEffect.SubProcessPtr := SESineProcess;
 CallHost(SEAudioMasterSetProcessFunction);
 {$ELSE}
 // choose which function is used to process audio
 OnProcess := SubProcess;
 {$ENDIF}

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSESineModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if TSESinePins(CurrentPin.PinID) = pinFrequency
  then FrequencyChanged;
 inherited;
end;

procedure TSESineModule.FrequencyChanged;
begin
 {$IFDEF PUREPASCAL}
 FSineLFO.Frequency := FFrequency;
 {$ELSE}
 GetSinCos(2 * Pi * FFrequency / SampleRate, FAngle.Im, FAngle.Re);
 {$ENDIF}
end;

procedure TSESineModule.SampleRateChanged;
begin
 {$IFDEF PUREPASCAL}
 FSineLFO.SampleRate := SampleRate;
 {$ELSE}
 FrequencyChanged;
 {$ENDIF}
 inherited;
end;

// The most important part, processing the audio
procedure TSESineModule.SubProcess(const BufferOffset, SampleFrames: Integer);
{$IFDEF PUREPASCAL}
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FOutputBuffer[BufferOffset + Sample] := FPosition.Re;
   FSineLFO.CalculateNextSample;
  end;
end;
{$ELSE}
asm
 jecxz @loopend
 add edx, Self.FOutputBuffer
@loopstart:
 fld [Self.FPosition.Re].Double  // FPosition.Re
 fmul [Self.FAngle.Re].Double    // FPosition.Re * FAngle.Re
 fld [Self.FPosition.Im].Double  // FPosition.Im, FPosition.Re * FAngle.Re
 fmul [Self.FAngle.Im].Double    // FPosition.Im * FAngle.Im, FPosition.Re * FAngle.Re
 fsubp                           // FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im = New.Re

 fld [Self.FPosition.Im].Double  // FPosition.Im, New.Re
 fmul [Self.FAngle.Re].Double    // FPosition.Im * FAngle.Re, New.Re
 fld [Self.FPosition.Re].Double  // FPosition.Re, FPosition.Re * FAngle.Re, New.Re
 fmul [Self.FAngle.Im].Double    // FPosition.Re * FAngle.Im, FPosition.Re * FAngle.Re, New.Re
 faddp                           // FPosition.Re * FAngle.Re + FPosition.Im * FAngle.Im = New.Im, New.Re
 fstp [Self.FPosition.Im].Double // FPosition.Im := New.Im, New.Re
 fst [Self.FPosition.Re].Double  // FPosition.Re := New.Re
 fstp [edx].Single               // FOutputBuffer := New.Re
 add edx, 4
 loop @loopstart
@loopend:
end;
{$ENDIF}

// describe your module
class procedure TSESineModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Sine Generator';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Sine Generator';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSESineModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSESinePins(index) of
  // typical input plug (inputs are listed first)
  pinFrequency: with Properties^ do
                 begin
                  Name            := 'Frequency [Hz]';
                  VariableAddress := @FFrequency;
                  Direction       := drIn;
                  Datatype        := dtSingle;
                  DefaultValue    := '440';
                 end;

  // typical output plug
  pinOutput: with Properties^ do
              begin
               Name            := 'Output';
               VariableAddress := @FOutputBuffer;
               Direction       := drOut;
               Datatype        := dtFSample;
// Spare Plug  Flags           := [iofAutoDuplicate, iofRename,  iofSetableOutput, iofCustomisable];
              end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{$IFNDEF PUREPASCAL}
procedure SESineProcess(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
asm
 mov ecx, SampleFrames
 jecxz @loopend
 mov eax, ModuleBase
 mov edx, BufferOffset
 add edx, [eax + $A0]
@loopstart:
 fld [eax + $B8].Double  // FPosition.Re
 fmul [eax + $A8].Double // FPosition.Re * FAngle.Re
 fld [eax + $C0].Double  // FPosition.Im, FPosition.Re * FAngle.Re
 fmul [eax + $B0].Double // FPosition.Im * FAngle.Im, FPosition.Re * FAngle.Re
 fsubp                   // FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im = New.Re

 fld [eax + $C0].Double  // FPosition.Im, New.Re
 fmul [eax + $A8].Double // FPosition.Im * FAngle.Re, New.Re
 fld [eax + $B8].Double  // FPosition.Re, FPosition.Re * FAngle.Re, New.Re
 fmul [eax + $B0].Double // FPosition.Re * FAngle.Im, FPosition.Re * FAngle.Re, New.Re
 faddp                   // FPosition.Re * FAngle.Re + FPosition.Im * FAngle.Im = New.Im, New.Re
 fstp [eax + $C0].Double // FPosition.Im := New.Im, New.Re
 fst [eax + $B8].Double  // FPosition.Re := New.Re
 fstp [edx].Single       // FOutputBuffer := New.Re
 add edx, 4
 loop @loopstart
@loopend:
end;
{$ENDIF}

constructor TSESineCosineModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FFrequency := 440;
 {$IFDEF PUREPASCAL}
 FSineLFO := TSineLFO.Create;
 FSineLFO.SampleRate := SampleRate;
 FSineLFO.Frequency := FFrequency;
 {$ELSE}
 FPosition.Re := 0;
 FPosition.Im := -1;
 FrequencyChanged;
 {$ENDIF}
end;

destructor TSESineCosineModule.Destroy;
begin
 {$IFDEF PUREPASCAL}
 FreeAndNil(FSineLFO);
 {$ENDIF}
 inherited;
end;

procedure TSESineCosineModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 {$IFNDEF PUREPASCAL}
 FEffect.SubProcessPtr := SESineCosineProcess;
 CallHost(SEAudioMasterSetProcessFunction);
 {$ENDIF}

 // 'transmit' new output status to next module 'downstream'
 Pin[1].TransmitStatusChange(SampleClock, stRun);
 Pin[2].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSESineCosineModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if TSESinePins(CurrentPin.PinID) = pinFrequency
  then FrequencyChanged;
 inherited;
end;

procedure TSESineCosineModule.FrequencyChanged;
begin
 {$IFDEF PUREPASCAL}
 FSineLFO.Frequency := FFrequency;
 {$ELSE}
 GetSinCos(2 * Pi * FFrequency / SampleRate, FAngle.Im, FAngle.Re);
 FAngle.Im := -FAngle.Im;
 {$ENDIF}
end;

procedure TSESineCosineModule.SampleRateChanged;
begin
 {$IFDEF PUREPASCAL}
 FSineLFO.SampleRate := SampleRate;
 {$ELSE}
 FrequencyChanged;
 {$ENDIF}
 inherited;
end;

// The most important part, processing the audio
procedure TSESineCosineModule.SubProcess(const BufferOffset, SampleFrames: Integer);
{$IFDEF PUREPASCAL}
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FSineBuffer[BufferOffset + Sample] := FPosition.Re;
   FCosineBuffer[BufferOffset + Sample] := FPosition.Im;
   FSineLFO.CalculateNextSample;
  end;
end;
{$ELSE}
asm
 jecxz @loopend
 push ebx
 mov ebx, edx
 add edx, Self.FSineBuffer
 add ebx, Self.FCosineBuffer
@loopstart:
 fld [Self.FPosition.Re].Double  // FPosition.Re
 fmul [Self.FAngle.Re].Double    // FPosition.Re * FAngle.Re
 fld [Self.FPosition.Im].Double  // FPosition.Im, FPosition.Re * FAngle.Re
 fmul [Self.FAngle.Im].Double    // FPosition.Im * FAngle.Im, FPosition.Re * FAngle.Re
 fsubp                           // FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im = New.Re

 fld [Self.FPosition.Im].Double  // FPosition.Im, New.Re
 fmul [Self.FAngle.Re].Double    // FPosition.Im * FAngle.Re, New.Re
 fld [Self.FPosition.Re].Double  // FPosition.Re, FPosition.Re * FAngle.Re, New.Re
 fmul [Self.FAngle.Im].Double    // FPosition.Re * FAngle.Im, FPosition.Re * FAngle.Re, New.Re
 faddp                           // FPosition.Re * FAngle.Re + FPosition.Im * FAngle.Im = New.Im, New.Re
 fst [Self.FPosition.Im].Double  // FPosition.Im := New.Im, New.Re
 fstp [ebx].Single               // FCosineBuffer := New.Im, New.Re
 fst [Self.FPosition.Re].Double  // FPosition.Re := New.Re
 fstp [edx].Single               // FSineBuffer := New.Re
 add edx, 4
 add ebx, 4
 loop @loopstart
 pop ebx
@loopend:
end;
{$ENDIF}

// describe your module
class procedure TSESineCosineModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Sine/Cosine Generator';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Sine/Cosine Generator';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSESineCosineModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Frequency [Hz]';
       VariableAddress := @FFrequency;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '440';
      end;

  // typical output plug
  1: with Properties^ do
      begin
       Name            := 'Sine';
       VariableAddress := @FSineBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  2: with Properties^ do
      begin
       Name            := 'Cosine';
       VariableAddress := @FCosineBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{$IFNDEF PUREPASCAL}
procedure SESineCosineProcess(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
asm
 mov ecx, SampleFrames
 jecxz @loopend
 mov eax, ModuleBase
 push ebx
 mov edx, BufferOffset
 mov ebx, BufferOffset
 add edx, [eax + $A0]
 add ebx, [eax + $A4]
@loopstart:
 fld [eax + $C0].Double  // FPosition.Re
 fmul [eax + $B0].Double // FPosition.Re * FAngle.Re
 fld [eax + $C8].Double  // FPosition.Im, FPosition.Re * FAngle.Re
 fmul [eax + $B8].Double // FPosition.Im * FAngle.Im, FPosition.Re * FAngle.Re
 fsubp                   // FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im = New.Re

 fld [eax + $C8].Double  // FPosition.Im, New.Re
 fmul [eax + $B0].Double // FPosition.Im * FAngle.Re, New.Re
 fld [eax + $C0].Double  // FPosition.Re, FPosition.Re * FAngle.Re, New.Re
 fmul [eax + $B8].Double // FPosition.Re * FAngle.Im, FPosition.Re * FAngle.Re, New.Re
 faddp                   // FPosition.Re * FAngle.Re + FPosition.Im * FAngle.Im = New.Im, New.Re
 fst [eax + $C8].Double  // FPosition.Im := New.Im, New.Re
(*
 fmul st(0), st(0)
 fadd st(0), st(0)
 fld1
 fsubp
*)
 fstp [ebx].Single       // FOutputBuffer := New.Im, New.Re
 fst [eax + $C0].Double  // FPosition.Re := New.Re
 fstp [edx].Single       // FOutputBuffer := New.Re
 add edx, 4
 add ebx, 4
 loop @loopstart
 pop ebx
@loopend:
end;
{$ENDIF}

constructor TSESine2Module.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FFrequency := 440;
 {$IFDEF PUREPASCAL}
 FSineLFO := TSineLFO.Create;
 FSineLFO.SampleRate := SampleRate;
 FSineLFO.Frequency := FFrequency;
 {$ELSE}
 FPosition.Re := 0;
 FPosition.Im := -1;
 FrequencyChanged;
 {$ENDIF}
end;

destructor TSESine2Module.Destroy;
begin
 {$IFDEF PUREPASCAL}
 FreeAndNil(FSineLFO);
 {$ENDIF}
 inherited;
end;

procedure TSESine2Module.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 {$IFNDEF PUREPASCAL}
 FEffect.SubProcessPtr := SESine2Process;
 CallHost(SEAudioMasterSetProcessFunction);
 {$ENDIF}

 // 'transmit' new output status to next module 'downstream'
 Pin[1].TransmitStatusChange(SampleClock, stRun);
 Pin[2].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSESine2Module.PlugStateChange(const CurrentPin: TSEPin);
begin
 if TSESinePins(CurrentPin.PinID) = pinFrequency
  then FrequencyChanged;
 inherited;
end;

procedure TSESine2Module.FrequencyChanged;
begin
 {$IFDEF PUREPASCAL}
 FSineLFO.Frequency := FFrequency;
 {$ELSE}
 GetSinCos(2 * Pi * FFrequency / SampleRate, FAngle.Im, FAngle.Re);
 FAngle.Im := -FAngle.Im;
 {$ENDIF}
end;

procedure TSESine2Module.SampleRateChanged;
begin
 {$IFDEF PUREPASCAL}
 FSineLFO.SampleRate := SampleRate;
 {$ELSE}
 FrequencyChanged;
 {$ENDIF}
 inherited;
end;

// The most important part, processing the audio
procedure TSESine2Module.SubProcess(const BufferOffset, SampleFrames: Integer);
{$IFDEF PUREPASCAL}
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FSineBuffer[BufferOffset + Sample] := FPosition.Re;
   FSine2Buffer[BufferOffset + Sample] := 2 * FPosition.Re * FPosition.Im;
   FSineLFO.CalculateNextSample;
  end;
end;
{$ELSE}
asm
 jecxz @loopend
 push ebx
 mov ebx, edx
 add edx, Self.FSineBuffer
 add ebx, Self.FSine2Buffer
@loopstart:
 fld [Self.FPosition.Re].Double  // FPosition.Re
 fmul [Self.FAngle.Re].Double    // FPosition.Re * FAngle.Re
 fld [Self.FPosition.Im].Double  // FPosition.Im, FPosition.Re * FAngle.Re
 fmul [Self.FAngle.Im].Double    // FPosition.Im * FAngle.Im, FPosition.Re * FAngle.Re
 fsubp                           // FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im = New.Re

 fld [Self.FPosition.Im].Double  // FPosition.Im, New.Re
 fmul [Self.FAngle.Re].Double    // FPosition.Im * FAngle.Re, New.Re
 fld [Self.FPosition.Re].Double  // FPosition.Re, FPosition.Re * FAngle.Re, New.Re
 fmul [Self.FAngle.Im].Double    // FPosition.Re * FAngle.Im, FPosition.Re * FAngle.Re, New.Re
 faddp                           // FPosition.Re * FAngle.Re + FPosition.Im * FAngle.Im = New.Im, New.Re
 fst [Self.FPosition.Im].Double  // FPosition.Im := New.Im, New.Re
 fxch
 fst [Self.FPosition.Re].Double  // FPosition.Re := New.Re, New.Im
 fst [edx].Single                // FOutputBuffer := New.Re, New.Im
 fmulp
 fadd st(0), st(0)
 fstp [ebx].Single               // FOutputBuffer := New.Im, New.Re
 add edx, 4
 add ebx, 4
 loop @loopstart
 pop ebx
@loopend:
end;
{$ENDIF}

// describe your module
class procedure TSESine2Module.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Sine2 Generator';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Sine2 Generator';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSESine2Module.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Frequency [Hz]';
       VariableAddress := @FFrequency;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '440';
      end;

  // typical output plug
  1: with Properties^ do
      begin
       Name            := 'Sine';
       VariableAddress := @FSineBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  2: with Properties^ do
      begin
       Name            := 'Sine (2x)';
       VariableAddress := @FSine2Buffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{$IFNDEF PUREPASCAL}
procedure SESine2Process(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
asm
 mov ecx, SampleFrames
 jecxz @loopend
 mov eax, ModuleBase
 push ebx
 mov edx, BufferOffset
 mov ebx, BufferOffset
 add edx, [eax + $A0]
 add ebx, [eax + $A4]
@loopstart:
 fld [eax + $C0].Double  // FPosition.Re
 fmul [eax + $B0].Double // FPosition.Re * FAngle.Re
 fld [eax + $C8].Double  // FPosition.Im, FPosition.Re * FAngle.Re
 fmul [eax + $B8].Double // FPosition.Im * FAngle.Im, FPosition.Re * FAngle.Re
 fsubp                   // FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im = New.Re

 fld [eax + $C8].Double  // FPosition.Im, New.Re
 fmul [eax + $B0].Double // FPosition.Im * FAngle.Re, New.Re
 fld [eax + $C0].Double  // FPosition.Re, FPosition.Re * FAngle.Re, New.Re
 fmul [eax + $B8].Double // FPosition.Re * FAngle.Im, FPosition.Re * FAngle.Re, New.Re
 faddp                   // FPosition.Re * FAngle.Re + FPosition.Im * FAngle.Im = New.Im, New.Re
 fst [eax + $C8].Double  // FPosition.Im := New.Im, New.Re
 fxch
 fst [eax + $C0].Double  // FPosition.Re := New.Re, New.Im
 fst [edx].Single        // FOutputBuffer := New.Re, New.Im
 fmulp
 fadd st(0), st(0)
 fstp [ebx].Single       // FOutputBuffer := New.Im, New.Re
 add edx, 4
 add ebx, 4
 loop @loopstart
 pop ebx
@loopend:
end;
{$ENDIF}

end.
