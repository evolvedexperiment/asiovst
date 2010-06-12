unit DAV_AsioCore32;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LclType, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, DAV_AsioInterface, DAV_Types, DAV_Asio,
  DAV_AsioConvert, DAV_AsioHostCore, DAV_BlockConvert32;

type
  TAsioInputConverter32  = procedure(Source: Pointer; Target: PSingle; SampleCount: LongInt);
  TAsioOutputConverter32 = procedure(Source: PSingle; Target: Pointer; SampleCount: LongInt);

  {$IFDEF SUPPORTS_REGION} {$region 'TAsioChannel'} {$ENDIF}
  TAsioChannelInput32 = class(TAsioChannelInput)
  protected
    FConverter : TAsioInputConverter32;
  public
    procedure UpdateChannelInfo; override;
  end;

  TAsioChannelOutput32 = class(TAsioChannelOutput)
  protected
    FConverter : TAsioOutputConverter32;
  public
    procedure UpdateChannelInfo; override;
  end;
  {$IFDEF SUPPORTS_REGION} {$endregion 'TAsioChannel'} {$ENDIF}


implementation

{ TAsioChannelInput32 }

procedure TAsioChannelInput32.UpdateChannelInfo;
begin
 inherited;
// FConverter :=
end;

{ TAsioChannelOutput32 }

procedure TAsioChannelOutput32.UpdateChannelInfo;
begin
 inherited;

end;

end.
