unit DAV_WinAmp;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  {$IFDEF FPC}LCLIntf, LCLType, {$ELSE}Windows, {$ENDIF} Classes, SysUtils, DAV_Common;

type
  TSmallIntArray = array [0..40000] of Smallint;
  PSmallIntArray = ^TSmallIntArray;
  TShortIntArray = array [0..40000] of ShortInt;
  PShortIntArray = ^TShortIntArray;
  T3Bytes = array [0..2] of Byte;
  P3Bytes = ^T3Bytes;
  T3ByteArray = array [0..40000] of T3Bytes;
  P3ByteArray = ^T3ByteArray;
  PWinampDSPModule = ^TWinampDSPModule;
  PWinAmpDSPHeader = ^TWinAmpDSPheader;

  TWAGetHeader = function: PWinAmpDSPHeader; cdecl;
  TWAGetModule = function(const Which : Integer): PWinAmpDSPModule; cdecl;
  TWAConfig = procedure(const Handle : PWinAmpDSPModule); cdecl;
  TWAInit = function(const Handle: PWinAmpDSPModule): Integer; cdecl;
  TWAQuit = procedure(const Handle: PWinAmpDSPModule); cdecl;
  TWAModifySamples = function(const Handle: PWinAmpDSPModule;
    const Samples: Pointer; const SamplesFrame, BitPerSample, ChannelCount,
    SampleRate: Integer): Integer; cdecl;

  TWinampDSPModule = record
                      Description   : PChar;
                      HwndParent    : Hwnd;
                      HDLLinstance  : Hinst;
                      Config        : TWAConfig;
                      Init          : TWAInit;
                      ModifySamples : TWAModifySamples;
                      Quit          : TWAQuit;
                      UserData      : Pointer;
                     end;

  TWinAmpDSPHeader = record
                      Version      : Integer;
                      Description  : PChar;
                      GetModule    : TWAGetModule;
                      Key          : Integer;
                     end;

  TWinAmpConvert = procedure (const Data: Pointer; const ChannelCount, SampleFrames: Integer) of object;


implementation

end.
