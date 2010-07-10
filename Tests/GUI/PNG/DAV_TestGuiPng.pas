unit DAV_TestGuiPng;

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

uses
  TestFramework, Classes, Contnrs, SysUtils, DAV_Common, DAV_ChunkClasses,
  DAV_GuiCommon, DAV_GuiPng, DAV_GuiPngTypes, DAV_GuiPngClasses,
  DAV_GuiPngChunks, DAV_TestGuiPngDisplay, pngimage;

type
  // Test methods for class TPortableNetworkGraphic
  TTestGuiPng = class(TTestCase)
  strict private
    FPngFile : TPortableNetworkGraphic;
  private
    procedure InternalTestDrawing(FileName: TFileName);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestScanning;
    procedure TestBasicWriting;
    procedure TestDrawingBasic;
    procedure TestDrawingIndex;
    procedure TestDrawingAdam7;
  end;

implementation

uses
  Dialogs, Controls;

resourcestring
  RCStrWrongDisplay = 'PNG was not displayed correctly!';
  RCStrTestFileNotFound = 'The test png file %s could not be found!';

procedure TTestGuiPng.SetUp;
begin
 FPngFile := TPortableNetworkGraphic.Create;
end;

procedure TTestGuiPng.TearDown;
begin
 FreeAndNil(FPngFile);
end;

procedure TTestGuiPng.TestScanning;
var
  SR      : TSearchRec;
  Succeed : Boolean;
begin
 if FindFirst('*.png*', faAnyFile, SR) = 0 then
  try
   repeat
    Succeed := True;
    try
     FPngFile.LoadFromFile(SR.Name)
    except
     on e: EPngError do MessageDlg(SR.Name + ': ' + e.Message, mtError, [mbOK], 0);
     else Succeed := False;
    end;
    Check(Succeed, 'Error loading file: ' + SR.Name);
   until FindNext(SR) <> 0;
  finally
   // Must free up resources used by these successful finds
   FindClose(SR);
  end;
end;

procedure TTestGuiPng.TestBasicWriting;
var
  TempStream : TMemoryStream;
begin
 TempStream := TMemoryStream.Create;
 with TempStream do
  try
   with FPngFile do
    begin
    end;
  finally
   Free;
  end;
end;

procedure TTestGuiPng.InternalTestDrawing(FileName: TFileName);
begin
 if not FileExists(FileName)
  then Fail(Format(RCStrTestFileNotFound, [FileName]));

 with TFmDisplay.Create(nil) do
  try
   with TPngImage.Create do
    try
     LoadFromFile(FileName);
     AssignTo(Reference);
    finally
     Free;
    end;

   FPngFile.LoadFromFile(FileName);

   ClientWidth := FPngFile.Width + 16;
   ClientHeight := FPngFile.Height + BtYes.Height + 20;

   Internal.Width := FPngFile.Width;
   Internal.Height := FPngFile.Height;
   Internal.Canvas.Draw(0, 0, FPngFile);

   if ShowModal <> mrYes
    then Fail(RCStrWrongDisplay);
  finally
   Free;
  end;
end;

procedure TTestGuiPng.TestDrawingBasic;
begin
 InternalTestDrawing('Test.png');
end;

procedure TTestGuiPng.TestDrawingIndex;
begin
 InternalTestDrawing('TestIndex.png');
end;

procedure TTestGuiPng.TestDrawingAdam7;
begin
 InternalTestDrawing('TestAdam7.png');
end;

initialization
  RegisterTest(TTestGuiPng.Suite);

end.
