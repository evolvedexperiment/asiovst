unit SEConvolutionModule;

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
  Windows, Classes, SysUtils, DAV_Types, DAV_SECommon, DAV_SEModule,
  DAV_Complex, DAV_DspConvolution;

type
  // define some constants to make referencing in/outs clearer
  TSEConvolutionPins = (pinInput, pinOutput, pinFileName, pinMaxIRSize,
    pinDesiredLatency, pinRealLatency);

  TSEConvolutionModule = class(TSEModuleBase)
  private
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FInputBuffer         : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer        : PDAVSingleFixedArray;

    FConvolver           : TConvolution32;

    FSemaphore           : Integer;
    FStaticCount         : Integer;
    FFileName            : PAnsiChar;
    FMaxIRSize           : Integer;
    FRealLatency         : Integer;
    FDesiredLatencyIndex : Integer;

    FContainedIRs        : TStringList;

    procedure SampleRateChanged; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
    procedure SubProcessBypass(const BufferOffset, SampleFrames: Integer);

    procedure LoadIR(FileName: TFileName); overload;
    procedure LoadIR(ID: Integer); overload;
  end;

implementation

uses
  DAV_AudioData, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

resourcestring
  RCStrSynthEditOnly = 'This module is not allowed to be embedded into a VST Plugin';
  RCNotRegistered = 'You are not a registered customer!';

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;

constructor TSEConvolutionModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FSemaphore           := 0;
 FConvolver           := TConvolution32.Create;
 FMaxIRSize           := 16384;
 FDesiredLatencyIndex := 5;

 FContainedIRs := TStringList.Create;
 EnumResourceNames(HInstance, 'IR', @EnumNamesFunc, LongWord(FContainedIRs));

 if FContainedIRs.Count > 0
  then Integer(FFileName) := 0
  else FFileName := '';
end;

destructor TSEConvolutionModule.Destroy;
begin
 FreeAndNil(FContainedIRs);
 FreeAndNil(FConvolver);
 inherited;
end;

procedure TSEConvolutionModule.Open;
{$IFDEF Use_IPPS}
var
  VSTHostParams  : TSECallVstHostParams;
  RegisteredName : array [0..99] of Char;
  ID             : Integer;
{$ENDIF}
begin
 {$IFDEF Use_IPPS}
 {$IFNDEF Registered}
 try
  FillChar(RegisteredName[0], SizeOf(RegisteredName), 0);
  CallHost(SEAudioMasterGetRegisteredName, 0, 0, @RegisteredName);
 except
  RegisteredName := '';
 end;

 if (RegisteredName <> 'Treck.de') and
    (RegisteredName <> 'Boris Kovalev') and
    (RegisteredName <> 'Victor Pavia')
  then
   begin
    ID := $29A2A826;
    if CSepMagic <> (ID shl 1)
     then raise Exception.Create(RCStrSynthEditOnly);
    VSTHostParams.Opcode := 32;

    if CallHost(SEAudioMasterCallVstHost, 0, 0, @VSTHostParams) <> -1
     then raise Exception.Create(RCStrSynthEditOnly)
   end;
 {$ENDIF}
 {$ENDIF}

 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcessBypass;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEConvolutionModule.SampleRateChanged;
begin
 inherited;
 // ignore
end;

// The most important part, processing the audio
procedure TSEConvolutionModule.SubProcessBypass(const BufferOffset, SampleFrames: Integer);
begin
 Move(FInputBuffer[BufferOffset], FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single));
end;

procedure TSEConvolutionModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEConvolutionModule.ChooseProcess;
begin
 if (FContainedIRs.Count = 0) and (not FileExists(FFileName))
  then OnProcess := SubProcessBypass
  else
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize + FConvolver.IRSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TSEConvolutionModule.GetModuleProperties(Properties : PSEModuleProperties);
var
  ContainedIRs : TStringList;
begin
 ContainedIRs := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'IR', @EnumNamesFunc, LongWord(ContainedIRs));

  with Properties^ do
   begin
    {$IFDEF Use_IPPS}
    if ContainedIRs.Count > 0 then
     begin
      Name     := 'Resource Convolution Module (IPP based)';
      ID       := 'IPP Resource Convolution Module';
     end
    else
     begin
      Name     := 'Convolution Module (IPP based)';
      ID       := 'IPP Convolution Module';
     end;
    {$ELSE}
    if ContainedIRs.Count > 0 then
     begin
      Name     := 'Resource Convolution Module';
      ID       := 'DAV Resource Convolution Module';
     end
    else
     begin
      Name     := 'Simple Convolution Module';
      ID       := 'DAV Simple Convolution Module';
     end;
    {$ENDIF}
    About      := 'by Christian-W. Budde';
    SdkVersion := CSeSdkVersion;
   end;
 finally
  FreeAndNil(ContainedIRs);
 end;
end;

// describe the pins (plugs)
function TSEConvolutionModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
var
  str : string;
begin
 Result := True;
 case TSEConvolutionPins(index) of
  // typical input plug (inputs are listed first)
  pinInput:
    with Properties^ do
     begin
      Name            := 'Input';
      VariableAddress := @FInputBuffer;
      Flags           := [iofLinearInput];
      Direction       := drIn;
      Datatype        := dtFSample;
      DefaultValue    := '0';
     end;

  // typical output plug
  pinOutput:
    with Properties^ do
     begin
      Name            := 'Output';
      VariableAddress := @FOutputBuffer;
      Direction       := drOut;
      Datatype        := dtFSample;
     end;
  pinFileName:
    with Properties^ do
     begin
      if FContainedIRs.Count <= 0 then
       begin
        Name            := 'FileName';
        VariableAddress := @FFileName;
        Flags           := [iofFilename];
        Direction       := drIn;
        DataType        := dtText;
        DefaultValue    := 'IR.wav';
       end
      else
       begin
        Name            := 'IR ID';
        VariableAddress := @FFileName;
        Direction       := drIn;
        DataType        := dtEnum;
        DefaultValue    := '0';
        str             := 'range 0,' + IntToStr(FContainedIRs.Count - 1);
        DatatypeExtra   := PChar(str);
       end;
     end;
  pinMaxIRSize:
    with Properties^ do
     begin
      Name            := 'Maximum IR Size';
      VariableAddress := @FMaxIRSize;
      Direction       := drIn;
      DataType        := dtInteger;
      DefaultValue    := '16384';
     end;
  pinDesiredLatency:
    with Properties^ do
     begin
      Name            := 'Desired Latency';
      VariableAddress := @FDesiredLatencyIndex;
      Direction       := drParameter;
      DataType        := dtEnum;
      DefaultValue    := '3';
      DatatypeExtra   := '64, 128, 256, 512, 1024, 2048, 4096, 8192';
     end;
  pinRealLatency:
    with Properties^ do
     begin
      Name            := 'Real Latency';
      VariableAddress := @FRealLatency;
      Direction       := drOut;
      DataType        := dtInteger;
     end;
  else Result := False; // host will ask for plugs 0, 1, 2, 3 etc. return false to signal when done
 end;;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEConvolutionModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEConvolutionPins(CurrentPin.PinID) of
           pinInput : begin
                       ChooseProcess;
                       Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                      end;
        pinFileName : begin
                       if FContainedIRs.Count <= 0 then
                        begin
                         if FileExists(FFileName)
                          then LoadIR(StrPas(FFileName));
                        end
                       else LoadIR(Integer(FFileName));
                       ChooseProcess;
                      end;
       pinMaxIRSize : begin
                       while FSemaphore > 0 do;
                       Inc(FSemaphore);
                       try
                        if FConvolver.IRSize > FMaxIRSize
                         then FConvolver.IRSize := FMaxIRSize;
                       finally
                        Dec(FSemaphore);
                       end;
                      end;
  pinDesiredLatency : FConvolver.FFTOrder := 6 + FDesiredLatencyIndex;
 end; inherited;
end;


procedure TSEConvolutionModule.LoadIR(FileName: TFileName);
var
  ADC : TAudioDataCollection32;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  ADC := TAudioDataCollection32.Create(nil);
  with ADC do
   try
    LoadFromFile(FileName);
    FConvolver.LoadImpulseResponse(ADC[0].ChannelDataPointer, ADC.SampleFrames);
   finally
    FreeAndNil(ADC);
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TSEConvolutionModule.LoadIR(ID: Integer);
var
  ADC : TAudioDataCollection32;
  RS  : TResourceStream;
begin
 if (ID >= 0) and (ID < FContainedIRs.Count) then
  begin
   while FSemaphore > 0 do;
   inc(FSemaphore);
   try
    ADC := TAudioDataCollection32.Create(nil);
    with ADC do
     try
      RS := TResourceStream.Create(HInstance, FContainedIRs[ID], 'IR');
      try
       LoadFromStream(RS);
      finally
       FreeAndNil(RS);
      end;
      FConvolver.LoadImpulseResponse(ADC[0].ChannelDataPointer, ADC.SampleFrames);
     finally
      FreeAndNil(ADC);
     end;
   finally
    dec(FSemaphore);
   end;
  end;
end;

procedure TSEConvolutionModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 // lock processing
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  FConvolver.ProcessBlock(PDAVSingleFixedArray(@FInputBuffer[BufferOffset]),
                          PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]),
                          SampleFrames);
 finally
  dec(FSemaphore);
 end;
end;

end.
