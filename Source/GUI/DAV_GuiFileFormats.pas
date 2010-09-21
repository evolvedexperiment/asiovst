unit DAV_GuiFileFormats;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Classes, SysUtils;

type
  IGuiFileFormats = interface
    ['{C434E656-14A8-4EB7-AE74-5314990C44AD}']
(*
    function CanLoad(const FileName: TFileName): Boolean; overload;
    function CanLoad(Stream: TStream): Boolean; overload;
*)
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);

    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

  TGuiCustomFileFormat = class(TInterfacedPersistent, IGuiFileFormats,
    IStreamPersist)
  protected
    function GetWidth: Integer; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    procedure SetWidth(const Value: Integer); virtual; abstract;
    procedure SetHeight(const Value: Integer); virtual; abstract;

    class function CanLoad(const FileName: TFileName): Boolean; overload; virtual; abstract;
    class function CanLoad(Stream: TStream): Boolean; overload; virtual; abstract;

    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    procedure LoadFromFile(Filename: TFilename); virtual; abstract;
    procedure SaveToFile(Filename: TFilename); virtual; abstract;
  end;

implementation

end.

