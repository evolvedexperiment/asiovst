unit ArtProject;

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

{$I Artumes.inc}

uses
  Windows, Classes, SysUtils, Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  ComCtrls, Menus, Dialogs, ArtItemSource, ArtItemAnalysis, ArtItemDestination;

type
  TFmProject = class(TForm)
    MiDestinationCSV: TMenuItem;
    MiDestinationExcel: TMenuItem;
    MiDestinationWAV: TMenuItem;
    MiOctave: TMenuItem;
    MiSourceAddFile: TMenuItem;
    MiSourceDelete: TMenuItem;
    MiThirdOctave: TMenuItem;
    OpenDialog: TOpenDialog;
    PuAnalysis: TPopupMenu;
    PuDestination: TPopupMenu;
    PuSource: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    TvAnalysis: TTreeView;
    TvDestination: TTreeView;
    TvFilter: TTreeView;
    TvSource: TTreeView;
    TvStatistic: TTreeView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure MiSourceAddFileClick(Sender: TObject);
    procedure MiThirdOctaveClick(Sender: TObject);
    procedure MiOctaveClick(Sender: TObject);
    procedure MiDestinationExcelClick(Sender: TObject);
    procedure MiDestinationCSVClick(Sender: TObject);
    procedure MiDestinationWAVClick(Sender: TObject);
  private
    FWidthRatios : array [0..4] of Single;
    procedure ValidateNewWidthRatios;
    procedure AddSourceFile(Filename: TFilename);
    procedure AddAnalysisOctave;
    procedure AddAnalysisThirdOctave;
    procedure AddDestinationCSV;
    procedure AddDestinationWAV;
    {$IFDEF Excel}
    procedure AddDestinationExcel;
    {$ENDIF}
  protected
    procedure CustomAlignPosition(Control: TControl; var NewLeft, NewTop,
      NewWidth, NewHeight: Integer; var AlignRect: TRect;
      AlignInfo: TAlignInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(const FileName: TFileName);
    procedure Calculate;
  end;

implementation

uses
  Math, DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

{$R *.dfm}


{ TFmProject }

constructor TFmProject.Create(AOwner: TComponent);
begin
 FWidthRatios[0] := 0.2;
 FWidthRatios[1] := 0.2;
 FWidthRatios[2] := 0.2;
 FWidthRatios[3] := 0.2;
 FWidthRatios[4] := 0.2;
 inherited;
end;

procedure TFmProject.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFmProject.Calculate;
begin
 // nothing in here yet
end;

procedure TFmProject.LoadFromFile(const FileName: TFileName);
begin
 raise Exception.Create('not implemented yet');
end;

procedure TFmProject.MiDestinationCSVClick(Sender: TObject);
begin
 AddDestinationCSV;
end;

procedure TFmProject.MiDestinationExcelClick(Sender: TObject);
begin
 {$IFDEF Excel}
 AddDestinationExcel;
 {$ELSE}
 MessageDlg('Excel export is not supported in the open source edition',
   mtError, [mbOK], 0);
 {$ENDIF}
end;

procedure TFmProject.MiDestinationWAVClick(Sender: TObject);
begin
 AddDestinationWAV;
end;

procedure TFmProject.MiOctaveClick(Sender: TObject);
begin
 AddAnalysisOctave;
end;

procedure TFmProject.MiSourceAddFileClick(Sender: TObject);
begin
 with OpenDialog do
  if Execute
   then AddSourceFile(OpenDialog.FileName)
end;

procedure TFmProject.MiThirdOctaveClick(Sender: TObject);
begin
 AddAnalysisThirdOctave;
end;

procedure TFmProject.AddSourceFile(Filename: TFilename);
var
  Node : TTreeNode;
  SF   : TSourceFile;
begin
 with TvSource do
  begin
   Node := Items.AddChild(Items[0], ExtractFileName(Filename));

   // create source file
   SF := TSourceFile.Create;
   SF.FileName := Filename;

   Node.Data := SF;
  end;
end;

procedure TFmProject.AddAnalysisOctave;
var
  Node : TTreeNode;
  Ana  : TCustomAnalysis;
begin
 with TvSource do
  begin
   Node := Items.AddChild(Items[0], 'Octave');

   // create analysis
   Ana := TAnalysisOctave.Create;

   Node.Data := Ana;
  end;
end;

procedure TFmProject.AddAnalysisThirdOctave;
var
  Node : TTreeNode;
  Ana  : TCustomAnalysis;
begin
 with TvSource do
  begin
   Node := Items.AddChild(Items[0], 'Third Octave');

   // create analysis
   Ana := TAnalysisThirdOctave.Create;

   Node.Data := Ana;
  end;
end;

procedure TFmProject.AddDestinationCSV;
var
  Node : TTreeNode;
  Dest : TCustomDestination;
begin
 with TvSource do
  begin
   Node := Items.AddChild(Items[0], 'CSV');

   // create destination
   Dest := TDestinationCSV.Create;

   Node.Data := Dest;
  end;
end;

procedure TFmProject.AddDestinationWAV;
var
  Node : TTreeNode;
  Dest : TCustomDestination;
begin
 with TvSource do
  begin
   Node := Items.AddChild(Items[0], 'WAV');

   // create destination
   Dest := TDestinationWAV.Create;

   Node.Data := Dest;
  end;
end;

{$IFDEF Excel}
procedure TFmProject.AddDestinationExcel;
var
  Node : TTreeNode;
  Dest : TCustomDestination;
begin
 with TvSource do
  begin
   Node := Items.AddChild(Items[0], 'Excel');

   // create destination
   Dest := TDestinationExcel.Create;

   Node.Data := Dest;
  end;
end;
{$ENDIF}

procedure TFmProject.SplitterCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
var
  Sum : Single;
begin
 if Sender is TSplitter then
  with TSplitter(Sender) do
   begin
    Sum := FWidthRatios[Tag] + FWidthRatios[Tag + 1];
    FWidthRatios[Tag    ] := NewSize / Self.ClientWidth;
    FWidthRatios[Tag + 1] := Max(0.05, Sum - FWidthRatios[Tag]);
    ValidateNewWidthRatios;
   end;
end;

procedure TFmProject.ValidateNewWidthRatios;
var
  InvSum : Single;
begin
 InvSum := 1 / (FWidthRatios[0] + FWidthRatios[1] + FWidthRatios[2] +
   FWidthRatios[3] + FWidthRatios[4]);
 FWidthRatios[0] := FWidthRatios[0] * InvSum;
 FWidthRatios[1] := FWidthRatios[1] * InvSum;
 FWidthRatios[2] := FWidthRatios[2] * InvSum;
 FWidthRatios[3] := FWidthRatios[3] * InvSum;
 FWidthRatios[4] := FWidthRatios[4] * InvSum;
end;

procedure TFmProject.CustomAlignPosition(Control: TControl; var NewLeft,
  NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect;
  AlignInfo: TAlignInfo);
begin
 if Control = TvSource then
  begin
   NewTop := 0;
   NewLeft := 0;
   NewHeight := ClientHeight;
   NewWidth := Round(FWidthRatios[0] * ClientWidth);

   Splitter1.BoundsRect := Rect(NewWidth, 0, NewWidth +
     Splitter1.Width, ClientHeight);
  end else
 if Control = TvFilter then
  begin
   NewTop := 0;
   NewLeft := TvSource.BoundsRect.Right + 3;
   NewHeight := ClientHeight;
   NewWidth := Round(FWidthRatios[1] * ClientWidth);

   Splitter2.BoundsRect := Rect(NewLeft + NewWidth, 0,
     NewLeft + NewWidth + Splitter2.Width, ClientHeight);
  end else
 if Control = TvAnalysis then
  begin
   NewTop := 0;
   NewLeft := TvFilter.BoundsRect.Right + 3;
   NewHeight := ClientHeight;
   NewWidth := Round(FWidthRatios[2] * ClientWidth);

   Splitter3.BoundsRect := Rect(NewLeft + NewWidth, 0,
     NewLeft + NewWidth + Splitter3.Width, ClientHeight);
  end else
 if Control = TvStatistic then
  begin
   NewTop := 0;
   NewLeft := TvAnalysis.BoundsRect.Right + 3;
   NewHeight := ClientHeight;
   NewWidth := Round(FWidthRatios[3] * ClientWidth);

   Splitter4.BoundsRect := Rect(NewLeft + NewWidth, 0,
     NewLeft + NewWidth + Splitter4.Width, ClientHeight);
  end else
 if Control = TvDestination then
  begin
   NewTop := 0;
   NewLeft := TvStatistic.BoundsRect.Right + 3;
   NewHeight := ClientHeight;
   NewWidth := Round(FWidthRatios[4] * ClientWidth);
  end;
end;

end.
