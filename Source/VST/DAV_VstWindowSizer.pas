{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_VstWindowSizer;

//-------------------------------------------------------------------------------------------------------
// VST Plug-in Window Sizer
// Version 0.12
// Steven Schulze
// vstsizer@alienworks.com
//
//   This class adds the ability to resize a VST plugin GUI in most hosts.
//
//   Please  email me at the provided address if you find ways to improve the code.
//
//   To use:
//   ======
//
//   1) Create an instance of this class in your editor class
//   2) Call VstWindowSizer::SetEffect() to set the AudioEffect instance
//   3) In the AEffEditor::open function, right after you create your editor window,
//      call VstWindowSizer::SetEditorHwnd() with the newly created window handle
//
//   That is it!  You should now be able to resize the plugin window in most hosts.
//
//   Bonus functionality:
//   ===================
//
//   For any child window on your editor window (say, a fader control), you can let VstWindowSizer
//   automatically move/resize that child window by specifying one or more anchor sides.
//
//   For instance, calling...
//
//     sizer->SetAnchoredWindow(hWndSomeFader, VstWindowSizer::AnchorRight | VstWindowSizer::AnchorBottom);
//
//   ...will cause that control to retain its relative distance from the bottom and right edge while the
//   plugin GUI  is resized.  To make a child window stretch, specify both the left and right, or the top
//   and bottom sides as anchor points.
//
//
//   Delphi Adaption by Tobias Erichsen (t.erichsen@gmx.de)
//
//   This adaption uses the Delphi VST SDK from Christian Budde
//   In the file "DVstBasicModule" of that SDK, the functions "CanHostDo"
//   and "GetHostProductString" must be moved to the public section
//
//   Changelog:
//
//   2008-10-05  Tobias Erichsen
//              - HostAppVSTPluginAnalyzer added
//   2008-10-06  Tobias Erichsen
//              - SetCurrentSize added
//              - Fixed "EndTracking" - in case of "all" the HWND parameter
//                was used in SetWindowLong (being NULL) when reseting
//                the WindowProcs for the stored FrameTracking windows
//
//-------------------------------------------------------------------------------------------------------

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LCLType, {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$ELSE} Windows, Messages, {$ENDIF} Classes, Controls, Types, Contnrs,
  DAV_VSTCustomModule;

type
  TResizeFlag = (rfTrackParentSize, rfSimulateDragEdge,
    rfFillOutParents, rfFilterWMSizing, rfFilterChildWMSize,
    rfFilterWMWindowPosChanged, rfUpdateSizeOnWMWindowPosChanged);
  TResizeFlags = set of TResizeFlag;

type
  TVstWindowSizer = class;
  TWindowInfo = class
    hWindow     : HWND;
    DeltaWidth  : Integer;
    DeltaHeight : Integer;
    prevProc    : Pointer;
    pSizer      : TVstWindowSizer;
  end;

  TAnchoredWindow = class
    hWindow         : HWND;
    Anchor          : TAnchors;
    StartRect       : TRect;
    StartParentSize : TSize;
  end;

  THostApp = (haUnknown, haSONAR, haEnergyXT, haProject5,
    haFruityLoops, haCantabile, haSaviHost, haMelodyne, haCubase,
    haTracktion, haSamplitude, haVSTPluginAnalyzer);

  TVstWindowSizer = class(TObject)
  private
    procedure ConstraintsChanged(Sender: TObject);
  protected
    FAnchoredWindows     : TObjectList; // AnchoredWindow
    FEditorInfo          : TWindowInfo;
    FEffect              : TCustomVSTModule;
    FFrameHwnd           : HWND;
    FHostApp             : THostApp;
    FHostCanResize       : Boolean;
    FPreventChildSIze    : Integer;
    FRect                : TRect;
    FResizeFlags         : TResizeFlags;
    FResizeOffset        : TPoint;
    FSizingCursor        : TCursor;
    FStartMouseOffset    : TPoint;
    FTrackingInitialized : Boolean;
    FWindowsAdjust       : TObjectList; // WindowInfo
    FConstraints         : TSizeConstraints;
    function HitTest: Integer;
    function OnButtonDown: Boolean;
    function OnButtonUp: Boolean;
    function OnMouseMove: Boolean;
    procedure ApplyFrameSizeLimits(NewRect: PRECT);
    procedure ClientRectToScreen(hWindow: HWND; var Rect: TRect);
    procedure DetectHost;
    procedure EndTracking(hWindow: HWND; all: Boolean);
    procedure FixupWindowSize;
    procedure ScreenRectToClient(hWindow: HWND; var Rect: TRect);
    procedure SetEffect(Effect: TCustomVSTModule);
    procedure SetupTracking;
    procedure TrackParentWindowSize(parentFrame: HWND);
    procedure UpdateAnchoredWindow(Anchored: TAnchoredWindow);
    procedure UpdateEdgeCursor;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetCurrentSize: TSize;
    procedure SetAnchoredWindow(hWindow: HWND; Anchor: TAnchors);
    procedure SetCurrentSize(Width, Height: Integer);
    procedure SetEditorHwnd(HwndEditor: HWND);

    property Effect: TCustomVSTModule read FEffect write SetEffect;
    property Constraints: TSizeConstraints read FConstraints;
  end;

function GetSizer(hWindow: HWND): TVstWindowSizer;
function EditorWindowProc(hWindow: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function FrameWindowProc(hWindow: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function ChildWindowProc(hWindow: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

implementation

uses
  {$IFDEF DELPHI14_UP}AnsiStrings, {$ENDIF} SysUtils;

// replacement for the global vector in the original c++ code
var
  GFramesTrack: TObjectList; // TWindowInfo
  GEditors: TObjectList; // TWindowInfo


//----------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------
constructor TVstWindowSizer.Create;
begin
  // replacement for the vector in the original c++ code
  FWindowsAdjust := TObjectList.Create(True); // WindowInfo
  FAnchoredWindows := TObjectList.Create(True); // AnchoredWindow
  FConstraints := TSizeConstraints.Create(nil);

  FFrameHwnd := 0;
  FEditorInfo := nil;

  FHostApp := haUnknown;
  FTrackingInitialized := False;

  FSizingCursor := HTNOWHERE;
  FResizeFlags := [];
  FResizeOffset.x := 0;
  FResizeOffset.y := 0;
  FStartMouseOffset.x := 0;
  FStartMouseOffset.y := 0;

  FEffect := nil;
  FHostCanResize := False;
  FPreventChildSIze := 0;

  with FRect do
  begin
    left := 0;
    top := 0;
    right := 800;
    bottom := 600;
  end;

  with FConstraints do
  begin
    MinWidth := 200;
    MinHeight := 100;
    MaxWidth := 1600;
    MaxHeight := 1200;
    OnChange := ConstraintsChanged;
  end;
end;

// ----------------------------------------------------------------------
// Destructor
// ----------------------------------------------------------------------
destructor TVstWindowSizer.Destroy;
begin
  EndTracking(0, True);
  FreeAndNil(FConstraints);
  FreeAndNil(FWindowsAdjust);
  FreeAndNil(FAnchoredWindows);
  inherited;
end;

// ----------------------------------------------------------------------
// This must be called before anything else is called
// ----------------------------------------------------------------------
procedure TVstWindowSizer.ConstraintsChanged(Sender: TObject);
begin
  // nothing here to do yet...
end;

// ----------------------------------------------------------------------
// This must be called before anything else is called
// ----------------------------------------------------------------------
procedure TVstWindowSizer.SetEffect(Effect: TCustomVSTModule);
begin
  FEffect := Effect;

  if FEffect <> nil then
    FHostCanResize := FEffect.CanDo['sizeWindow'] > 0
  else
    FHostCanResize := False;
end;

// ----------------------------------------------------------------------
// Specify the main window that contains the editor GUI
// ----------------------------------------------------------------------
procedure TVstWindowSizer.SetEditorHwnd(HwndEditor: HWND);
var
  WindowInfo: TWindowInfo;
begin
  if FEditorInfo <> nil then
    EndTracking(FEditorInfo.hWindow, True);

  if IsWindow(HwndEditor) then
  begin
    // Add the editor window to the list of windows to be resized when the frame gets resized
    WindowInfo := TWindowInfo.Create;
    GEditors.Add(WindowInfo);

    FEditorInfo := WindowInfo;

    with FEditorInfo do
    begin
      hWindow := HwndEditor;
      DeltaWidth := 0;
      DeltaHeight := 0;
      pSizer := self;
      prevProc := Pointer(GetWindowLong(FEditorInfo.hWindow, GWL_WNDPROC));
    end;

    // Hook into the editor window's procedure
    SetWindowLong(FEditorInfo.hWindow, GWL_WNDPROC, Integer(@EditorWindowProc));

    // Attempt to detect the current host
    DetectHost;

    // Since we initialize tracking in the first WM_PAINT event we receive,
    // we force a repaint in case the window was already painted
    InvalidateRect(HwndEditor, nil, True);
  end
  else
    FEditorInfo := nil;
end;

// ----------------------------------------------------------------------
// Get the current size
// ----------------------------------------------------------------------
function TVstWindowSizer.GetCurrentSize: TSize;
var
  size: TSize;
begin
  size.cx := FRect.right - FRect.left;
  size.cy := FRect.bottom - FRect.top;
  Result := size;
end;

// ----------------------------------------------------------------------
// Set the current size programatically
// ----------------------------------------------------------------------
procedure TVstWindowSizer.SetCurrentSize(Width, Height: Integer);
var
  frameRect: TRect;
  NewRect: TRect;
begin
  if FFrameHwnd <> 0 then
  begin
    GetWindowRect(FFrameHwnd, frameRect);

    NewRect.left := frameRect.left;
    NewRect.top := frameRect.top;
    NewRect.right := frameRect.left + Width;
    NewRect.bottom := frameRect.top + Height;

    // Ensure we do not go over the limits set for the UI
    ApplyFrameSizeLimits(@NewRect);

    if (NewRect.left <> frameRect.left) or (NewRect.top <> frameRect.top) or
      (NewRect.right <> frameRect.right) or (NewRect.bottom <> frameRect.bottom)
    then
      MoveWindow(FFrameHwnd, NewRect.left, NewRect.top,
        NewRect.right - NewRect.left, NewRect.bottom - NewRect.top, True);
  end;
end;

// ----------------------------------------------------------------------
// Add a child window that will be automatically moved/sized during the
// main plugin GUI's sizing.  Specify VstWindowSizer::AnchorNone to remove the window
// from the sizing list
// ----------------------------------------------------------------------
procedure TVstWindowSizer.SetAnchoredWindow(hWindow: HWND; Anchor: TAnchors);
var
  idx: Integer;
  parentRect: TRect;
  anchorRect: TRect;
  parentHWnd: HWND;
  AnchoredWindow: TAnchoredWindow;
begin
  // First try to see if it was already added
  for idx := 0 to FAnchoredWindows.Count - 1 do
    if TAnchoredWindow(FAnchoredWindows.items[idx]).hWindow = hWindow then
    begin
      if Anchor = [] then
        FAnchoredWindows.Delete(idx)
      else
        TAnchoredWindow(FAnchoredWindows.items[idx]).Anchor := Anchor;
      exit;
    end;

  // Only add if it is actually anchored to any of the edges
  if Anchor = [] then
    exit;

  parentHWnd := GetParent(hWindow);

  // Retrieve initial position and size info.  Everything here on out will be
  // relative to that
  GetClientRect(parentHWnd, parentRect);
  GetWindowRect(hWindow, anchorRect);
  ScreenRectToClient(parentHWnd, anchorRect);

  // Add to the list of windows to move/resize
  AnchoredWindow := TAnchoredWindow.Create;
  FAnchoredWindows.Add(AnchoredWindow);

  AnchoredWindow.hWindow := hWindow;
  AnchoredWindow.Anchor := Anchor;
  AnchoredWindow.StartRect := anchorRect;
  AnchoredWindow.StartParentSize.cx := parentRect.right - parentRect.left;
  AnchoredWindow.StartParentSize.cy := parentRect.bottom - parentRect.top;
end;

// ----------------------------------------------------------------------
// This initializes the tracking operations
// ----------------------------------------------------------------------
procedure TVstWindowSizer.SetupTracking;
var
  wndRect: TRect;
  frameRect: TRect;
  idx: Integer;
  WindowAdjust: TWindowInfo;
  FrameTrack: TWindowInfo;
  Parent: HWND;
  style: Integer;
  oldProc: Pointer;
  frameClientRect: TRect;
  clientOffset: TPoint;
  frameClientOffset: TPoint;
begin

  // Should only do this once
  if FTrackingInitialized then
    exit;

  FTrackingInitialized := True;

  if not(rfTrackParentSize in FResizeFlags) then
    exit;

  GetWindowRect(FEditorInfo.hWindow, wndRect);

  // Add ourselves as a window to track
  WindowAdjust := TWindowInfo.Create;
  FWindowsAdjust.Add(WindowAdjust);

  with WindowAdjust do
  begin
    pSizer := self;
    hWindow := FEditorInfo.hWindow;
    prevProc := nil;
    DeltaWidth := 0;
    DeltaHeight := 0;
  end;

  // Locate all the parent windows we need to resize
  Parent := GetParent(FEditorInfo.hWindow);

  while Parent <> 0 do
  begin

    style := GetWindowLong(Parent, GWL_STYLE);
    oldProc := nil;

    // Search up the parent chain until we find the frame window
    if ((style and WS_CHILD) <> 0) and ((style and WS_CAPTION) <> WS_CAPTION)
      and not((style and (WS_THICKFRAME or WS_POPUP)) <> 0) then
    begin

      style := style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
      SetWindowLong(Parent, GWL_STYLE, style);

      if rfFilterChildWMSize in FResizeFlags then
      begin
        // Some hosts like Cubase needs special treatment here.
        // Prevent this parent from telling it's parent that it got resized
        oldProc := Pointer(GetWindowLong(Parent, GWL_WNDPROC));
        SetWindowLong(Parent, GWL_WNDPROC, Integer(@ChildWindowProc));
      end;

      WindowAdjust := TWindowInfo.Create;
      FWindowsAdjust.Add(WindowAdjust);

      WindowAdjust.pSizer := self;
      WindowAdjust.hWindow := Parent;
      WindowAdjust.prevProc := oldProc;
      WindowAdjust.DeltaWidth := 0;
      WindowAdjust.DeltaHeight := 0;
    end
    else
    begin

      // This is hopefully the frame window

      FFrameHwnd := Parent;

      // Add to the list of windows to be tracked for size changes
      FrameTrack := TWindowInfo.Create;
      GFramesTrack.Add(FrameTrack);

      FrameTrack.pSizer := self;
      FrameTrack.hWindow := Parent;
      FrameTrack.DeltaWidth := 0;
      FrameTrack.DeltaHeight := 0;
      FrameTrack.prevProc := Pointer(GetWindowLong(Parent, GWL_WNDPROC));

      // To intercept mouse events, etc, we hook into the frame window's procedure
      SetWindowLong(FFrameHwnd, GWL_WNDPROC, Integer(@FrameWindowProc));
      GetWindowRect(FFrameHwnd, frameRect);

      if rfFillOutParents in FResizeFlags then
      begin
        // Some hosts (like SONAR) puts up an ugly white border around our window plugin GUI,
        // so adjust all relevant windows to fill out the left, bottom and right edges

        GetClientRect(FFrameHwnd, frameClientRect);
        Inc(FPreventChildSIze);

        for idx := 0 to FWindowsAdjust.Count - 1 do
        begin
          GetWindowRect(TWindowInfo(FWindowsAdjust.items[idx]).hWindow,
            wndRect);

          clientOffset.x := wndRect.left;
          clientOffset.y := wndRect.top;
          ScreenToClient(GetParent(TWindowInfo(FWindowsAdjust.items[idx])
            .hWindow), clientOffset);

          frameClientOffset.x := wndRect.left;
          frameClientOffset.y := wndRect.top;
          ScreenToClient(FFrameHwnd, frameClientOffset);

          MoveWindow(TWindowInfo(FWindowsAdjust.items[idx]).hWindow, 0,
            clientOffset.y, frameClientRect.right - frameClientRect.left,
            (frameClientRect.bottom - frameClientRect.top) -
            frameClientOffset.y, True);

          // UpdateWindow(m_windowsAdjust[idx].HWND);
        end;

        Dec(FPreventChildSIze);
      end;

      // For each window we are going to resize, store the relative width/height info
      for idx := 0 to FWindowsAdjust.Count - 1 do
      begin
        GetWindowRect(TWindowInfo(FWindowsAdjust.items[idx]).hWindow, wndRect);

        TWindowInfo(FWindowsAdjust.items[idx]).DeltaWidth :=
          (wndRect.right - wndRect.left) - (frameRect.right - frameRect.left);
        TWindowInfo(FWindowsAdjust.items[idx]).DeltaHeight :=
          (wndRect.bottom - wndRect.top) - (frameRect.bottom - frameRect.top);
      end;

      exit;
    end;

    Parent := GetParent(Parent);
  end;
end;

// ----------------------------------------------------------------------
// This ends all tracking for the specified window
// ----------------------------------------------------------------------
procedure TVstWindowSizer.EndTracking(hWindow: HWND; all: Boolean);
var
  idx: Integer;
begin
  if all then
  begin
    if FEditorInfo <> nil then
      if (IsWindow(FEditorInfo.hWindow)) and (FEditorInfo.prevProc <> nil) then
        SetWindowLong(FEditorInfo.hWindow, GWL_WNDPROC,
          Integer(FEditorInfo.prevProc)); // Restore the previous window proc

    for idx := 0 to FWindowsAdjust.Count - 1 do
      if (IsWindow(TWindowInfo(FWindowsAdjust.items[idx]).hWindow)) and
        (TWindowInfo(FWindowsAdjust.items[idx]).prevProc <> nil) then
        SetWindowLong(TWindowInfo(FWindowsAdjust.items[idx]).hWindow,
          GWL_WNDPROC, Integer(TWindowInfo(FWindowsAdjust.items[idx])
          .prevProc));
    // Restore the previous window proc

    // We no longer need to resize anything
    FWindowsAdjust.Clear;
    FAnchoredWindows.Clear;

    FEditorInfo := nil;

    for idx := GEditors.Count - 1 downto 0 do
      if TWindowInfo(GEditors.items[idx]).pSizer = self then
        GEditors.Delete(idx);

    // Ensure we do a re-initialize the next time around
    FTrackingInitialized := False;

    for idx := GFramesTrack.Count - 1 downto 0 do
      if TWindowInfo(GFramesTrack.items[idx]).pSizer = self then
      begin
        // Restore the previous window proc, if any
        if (IsWindow(TWindowInfo(GFramesTrack.items[idx]).hWindow)) and
          (TWindowInfo(GFramesTrack.items[idx]).prevProc <> nil) then
          SetWindowLong(TWindowInfo(GFramesTrack.items[idx]).hWindow,
            GWL_WNDPROC, Integer(TWindowInfo(GFramesTrack.items[idx])
            .prevProc));

        GFramesTrack.Delete(idx)
      end;
  end
  else
  begin
    if (FEditorInfo <> nil) and (FEditorInfo.hWindow = hWindow) then
    begin
      // Restore the previous window proc
      if (IsWindow(FEditorInfo.hWindow)) and (FEditorInfo.prevProc <> nil) then
        SetWindowLong(FEditorInfo.hWindow, GWL_WNDPROC,
          Integer(FEditorInfo.prevProc));

      for idx := 0 to FWindowsAdjust.Count - 1 do
        if (IsWindow(TWindowInfo(FWindowsAdjust.items[idx]).hWindow)) and
          (TWindowInfo(FWindowsAdjust.items[idx]).prevProc <> nil) then
          SetWindowLong(TWindowInfo(FWindowsAdjust.items[idx]).hWindow,
            GWL_WNDPROC, Integer(TWindowInfo(FWindowsAdjust.items[idx])
            .prevProc));
      // Restore the previous window proc

      // We no longer need to resize anything
      FWindowsAdjust.Clear;
      FAnchoredWindows.Clear;

      FEditorInfo := nil;

      for idx := 0 to GEditors.Count - 1 do
        if TWindowInfo(GEditors.items[idx]).hWindow = hWindow then
        begin
          GEditors.Delete(idx);
          Break;
        end;

      // Ensure we do a re-initialize the next time around
      FTrackingInitialized := False;
    end;

    for idx := 0 to GFramesTrack.Count - 1 do
      if TWindowInfo(GFramesTrack.items[idx]).hWindow = hWindow then
      begin
        // Restore the previous window proc, if any
        if (IsWindow(TWindowInfo(GFramesTrack.items[idx]).hWindow)) and
          (TWindowInfo(GFramesTrack.items[idx]).prevProc <> nil) then
          SetWindowLong(hWindow, GWL_WNDPROC,
            Integer(TWindowInfo(GFramesTrack.items[idx]).prevProc));
        GFramesTrack.Delete(idx);
        Break;
      end;
  end;
end;

// ----------------------------------------------------------------------
// This ensures that the editor window size stays in sync with the parent
// ----------------------------------------------------------------------
procedure TVstWindowSizer.TrackParentWindowSize(parentFrame: HWND);
var
  frameRect: TRect;
  idx: Integer;
  // setWidth, SetHeight: Integer;
begin

  if not(rfTrackParentSize in FResizeFlags) then
    exit;

  if FPreventChildSIze > 0 then
    exit;

  Inc(FPreventChildSIze);

  // The parent window's size changed, so adjust all window sizes here
  GetWindowRect(parentFrame, frameRect);

  for idx := 0 to FWindowsAdjust.Count - 1 do
    SetWindowPos(TWindowInfo(FWindowsAdjust.items[idx]).hWindow, 0, 0, 0,
      (frameRect.right - frameRect.left) + TWindowInfo(FWindowsAdjust.items[idx]
      ).DeltaWidth, (frameRect.bottom - frameRect.top) +
      TWindowInfo(FWindowsAdjust.items[idx]).DeltaHeight,
      SWP_NOMOVE or SWP_NOOWNERZORDER or SWP_NOZORDER)
    // if (idx == 0 || idx == 1 || idx == 2 || idx == 3)
    // if (idx == 1)
    // continue;
      ;

  Dec(FPreventChildSIze);
end;

// ----------------------------------------------------------------------
// Keep the m_rect member up to date, and resize all anchored windows
// ----------------------------------------------------------------------
procedure TVstWindowSizer.FixupWindowSize;
var
  NewRect: TRect;
  dx, dy: Integer;
  idx: Integer;
begin
  GetWindowRect(FEditorInfo.hWindow, NewRect);

  dx := (NewRect.right - NewRect.left) - (FRect.right - FRect.left);
  dy := (NewRect.bottom - NewRect.top) - (FRect.bottom - FRect.top);

  // Now allow the anchored windows to be moved/resized
  for idx := 0 to FAnchoredWindows.Count - 1 do
    UpdateAnchoredWindow(TAnchoredWindow(FAnchoredWindows.items[idx]));

  UpdateWindow(FFrameHwnd);
  UpdateWindow(FEditorInfo.hWindow);

  Inc(FRect.right, dx);
  Inc(FRect.bottom, dy);
end;

// ----------------------------------------------------------------------
// Limit the GUI size the the specified min and max values
// ----------------------------------------------------------------------
procedure TVstWindowSizer.ApplyFrameSizeLimits(NewRect: PRECT);
var
  curRect: TRect;
  newWidth, newHeight: Integer;
begin
  GetWindowRect(FFrameHwnd, curRect);

  newWidth := NewRect.right - NewRect.left;
  newHeight := NewRect.bottom - NewRect.top;

  if newWidth > FConstraints.MaxWidth then
  begin
    if (NewRect.left <> curRect.left) then
      NewRect.left := NewRect.right - FConstraints.MaxWidth
    else
      NewRect.right := NewRect.left + FConstraints.MaxWidth;
  end
  else if newWidth < FConstraints.MinWidth then
    if NewRect.left <> curRect.left then
      NewRect.left := NewRect.right - FConstraints.MinWidth
    else
      NewRect.right := NewRect.left + FConstraints.MinWidth;

  if newHeight > FConstraints.MaxHeight then
  begin
    if NewRect.top <> curRect.top then
      NewRect.top := NewRect.bottom - FConstraints.MaxHeight
    else
      NewRect.bottom := NewRect.top + FConstraints.MaxHeight;
  end
  else if newHeight < FConstraints.MinHeight then
    if NewRect.top <> curRect.top then
      NewRect.top := NewRect.bottom - FConstraints.MinHeight
    else
      NewRect.bottom := NewRect.top + FConstraints.MinHeight;
end;

// ----------------------------------------------------------------------
// Move and/or resize an anchored window, based on the initial and current parent sizes
// ----------------------------------------------------------------------
procedure TVstWindowSizer.UpdateAnchoredWindow(Anchored: TAnchoredWindow);
var
  parentRect: TRect;
  NewRect: TRect;
  dx, dy: Integer;
begin
  NewRect := Anchored.StartRect;

  GetClientRect(GetParent(Anchored.hWindow), parentRect);

  dx := (parentRect.right - parentRect.left) - Anchored.StartParentSize.cx;
  dy := (parentRect.bottom - parentRect.top) - Anchored.StartParentSize.cy;

  if (akLeft in Anchored.Anchor) and (akRight in Anchored.Anchor) then
    Inc(NewRect.right, dx)
  else if akRight in Anchored.Anchor then
  begin
    Inc(NewRect.right, dx);
    Inc(NewRect.left, dx);
  end;

  if (akTop in Anchored.Anchor) and (akBottom in Anchored.Anchor) then
    Inc(NewRect.bottom, dy)
  else if akBottom in Anchored.Anchor then
  begin
    Inc(NewRect.top, dy);
    Inc(NewRect.bottom, dy);
  end;

  // Not sure if this saves any CPU time or not, but do a check and early exit
  // if nothing changed
  if (NewRect.left = Anchored.StartRect.left) and
    (NewRect.top = Anchored.StartRect.top) and
    (NewRect.right = Anchored.StartRect.right) and
    (NewRect.bottom = Anchored.StartRect.bottom) then
    exit;

  MoveWindow(Anchored.hWindow, NewRect.left, NewRect.top,
    NewRect.right - NewRect.left, NewRect.bottom - NewRect.top, True);
  UpdateWindow(Anchored.hWindow);
end;

// ----------------------------------------------------------------------
// Detect the current host
// ----------------------------------------------------------------------
procedure TVstWindowSizer.DetectHost;
var
  szProductName: array [0 .. 255] of AnsiChar;
  ProductName: AnsiString;
  curHwnd: HWND;
  curParent: HWND;
  style: Integer;
begin
  if FEditorInfo = nil then
    exit;

  FHostApp := haUnknown;
  Assert(Assigned(FEffect));
  ProductName := FEffect.HostProduct;

  // Some hosts return a string that clearly identifies them...
  if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('Cantabile', ProductName) > 0
  then
    FHostApp := haCantabile
  else if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('energyXT', ProductName) > 0
  then
    FHostApp := haEnergyXT
  else if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('SAVIHost', ProductName) > 0
  then
    FHostApp := haSaviHost
  else if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('Melodyne', ProductName) > 0
  then
    FHostApp := haMelodyne
  else if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('Cubase', ProductName) > 0
  then
    FHostApp := haCubase
  else if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('Tracktion',
    ProductName) > 0 then
    FHostApp := haTracktion
  else if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('Samplitude',
    ProductName) > 0 then
    FHostApp := haSamplitude
  else if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('VST Plugin Analyser',
    ProductName) > 0 then
    FHostApp := haVSTPluginAnalyzer
  else
  begin
    // ...others don't. SONAR, Project 5 and FL return the name of the Cakewalk VST wrapper for all 3
    curHwnd := GetParent(FEditorInfo.hWindow);

    FHostApp := haUnknown;

    while curHwnd <> 0 do
    begin
      style := GetWindowLong(curHwnd, GWL_STYLE);
      curParent := GetParent(curHwnd);
      szProductName[0] := #0;

      if (curParent = 0) or ((style and WS_CHILD) = 0) or
        ((style and WS_CAPTION) = WS_CAPTION) or
        ((style and (WS_THICKFRAME or WS_POPUP)) <> 0) then
        if GetClassNameA(curHwnd, szProductName, 256) > 0 then
        begin
          ProductName := szProductName;
          if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('Fruity',
            ProductName) > 0 then
          begin
            FHostApp := haFruityLoops;
            Break;
          end;

          if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('Project5',
            ProductName) > 0 then
          begin
            FHostApp := haProject5;
            Break;
          end;

          if {$IFDEF DELPHI14_UP}PosEx{$ELSE}Pos{$ENDIF}('SONAR', ProductName) > 0
          then
          begin
            FHostApp := haSONAR;
            Break;
          end;
        end;
      curHwnd := curParent;
    end;
  end;

  // Set various flags based on how we need to enable sizing for different hosts
  case FHostApp of

    haSONAR:
      FResizeFlags := [rfTrackParentSize, rfFillOutParents,
        rfFilterChildWMSize];

    haProject5:
      FResizeFlags := [rfTrackParentSize];

    haEnergyXT:
      FResizeFlags := [rfTrackParentSize, rfSimulateDragEdge];

    haFruityLoops:
      FResizeFlags := [rfTrackParentSize, rfSimulateDragEdge];

    haCantabile:
      FResizeFlags := [rfTrackParentSize, rfSimulateDragEdge];

    haSamplitude:
      FResizeFlags := [rfTrackParentSize, rfSimulateDragEdge];

    haSaviHost:
      FResizeFlags := [];

    haMelodyne:
      FResizeFlags := [rfTrackParentSize, rfSimulateDragEdge];

    haCubase:
      FResizeFlags := [rfTrackParentSize, rfSimulateDragEdge,
        rfFilterChildWMSize];

    haTracktion:
      FResizeFlags := [rfTrackParentSize, rfSimulateDragEdge,
        rfFilterWMWindowPosChanged, rfUpdateSizeOnWMWindowPosChanged];

    haVSTPluginAnalyzer:
      FResizeFlags := [rfTrackParentSize, rfSimulateDragEdge];

    haUnknown:
      FResizeFlags := [rfTrackParentSize, rfSimulateDragEdge];
    // This is for unknown hosts. Eventually resizing needs to be disabled completely for
    // all unknown hosts, but for now allow it in order to test this on new hosts.

  end;

  if FHostApp = haUnknown then
  // Could not detect the host.  Add a test case for this host!;
end;

// ----------------------------------------------------------------------
// Determine if the mouse is on an edge or corner that can be used for sizing
// ----------------------------------------------------------------------
function TVstWindowSizer.HitTest: Integer;
var
  frameRect: TRect;
  insideRect: TRect;
  mousePos: TPoint;
  left, top, right, bottom: Boolean;
begin
  if not(rfSimulateDragEdge in FResizeFlags) then
  begin
    Result := HTNOWHERE;
    exit;
  end;

  left := False;
  top := False;
  right := False;
  bottom := False;

  GetWindowRect(FFrameHwnd, frameRect);
  GetCursorPos(mousePos);

  insideRect := frameRect;
  InflateRect(insideRect, -6, -6);

  if (mousePos.x >= frameRect.left) and (mousePos.x <= insideRect.left) then
    left := True;

  if (mousePos.x <= frameRect.right) and (mousePos.x >= insideRect.right) then
    right := True;

  if (mousePos.y >= frameRect.top) and (mousePos.y <= insideRect.top) then
    top := True;

  if (mousePos.y <= frameRect.bottom) and (mousePos.y >= insideRect.bottom) then
    bottom := True;

  if not(left or right or bottom or top) then
  begin
    Result := HTNOWHERE;
    exit;
  end;

  if left and top then
  begin
    Result := HTTOPLEFT;
    exit;
  end;

  if left and bottom then
  begin
    Result := HTBOTTOMLEFT;
    exit;
  end;

  if right and top then
  begin
    Result := HTTOPRIGHT;
    exit;
  end;

  if right and bottom then
  begin
    Result := HTBOTTOMRIGHT;
    exit;
  end;

  if left then
  begin
    Result := HTLEFT;
    exit;
  end;

  if top then
  begin
    Result := HTTOP;
    exit;
  end;

  if right then
  begin
    Result := HTRIGHT;
    exit;
  end;

  if bottom then
  begin
    Result := HTBOTTOM;
    exit;
  end;

  Result := HTNOWHERE;
end;

// ----------------------------------------------------------------------
// Show the correct cursor, based on the detected edge the mouse is over
// ----------------------------------------------------------------------
procedure TVstWindowSizer.UpdateEdgeCursor;
begin
  if not(rfSimulateDragEdge in FResizeFlags) then
    exit;

  case FSizingCursor of
    HTTOPLEFT:
      SetCursor(LoadCursor(0, IDC_SIZENWSE));

    HTTOPRIGHT:
      SetCursor(LoadCursor(0, IDC_SIZENESW));

    HTBOTTOMLEFT:
      SetCursor(LoadCursor(0, IDC_SIZENESW));

    HTBOTTOMRIGHT:
      SetCursor(LoadCursor(0, IDC_SIZENWSE));

    HTLEFT:
      SetCursor(LoadCursor(0, IDC_SIZEWE));

    HTTOP:
      SetCursor(LoadCursor(0, IDC_SIZENS));

    HTRIGHT:
      SetCursor(LoadCursor(0, IDC_SIZEWE));

    HTBOTTOM:
      SetCursor(LoadCursor(0, IDC_SIZENS));

    HTNOWHERE:
      SetCursor(LoadCursor(0, IDC_ARROW));
  end;

end;

// ----------------------------------------------------------------------
// If the mouse was captured, resize the window based on the current edge/corner being dragged
// ----------------------------------------------------------------------
function TVstWindowSizer.OnMouseMove: Boolean;
var
  parentClientOffset: TPoint;
  frameParent: HWND;

  frameRect: TRect;
  NewRect: TRect;
  mousePos: TPoint;
begin
  if not(rfSimulateDragEdge in FResizeFlags) then
  begin
    Result := False;
    exit;
  end;

  if GetCapture() = FFrameHwnd then
  begin
    frameParent := GetParent(FFrameHwnd);

    // dxLeft := 0;
    // dxRight := 0;
    // dyTop := 0;
    // dyBottom := 0;

    GetCursorPos(mousePos);
    parentClientOffset.x := 0;
    parentClientOffset.y := 0;

    if frameParent <> 0 then
      ClientToScreen(frameParent, parentClientOffset);

    GetWindowRect(FFrameHwnd, frameRect);
    NewRect := frameRect;

    // Must resize the window here
    case FSizingCursor of

      HTLEFT:
        NewRect.left := mousePos.x - FResizeOffset.x;

      HTTOP:
        NewRect.top := mousePos.y - FResizeOffset.y;

      HTRIGHT:
        NewRect.right := mousePos.x + FResizeOffset.x;

      HTBOTTOM:
        NewRect.bottom := mousePos.y + FResizeOffset.y;

      HTTOPLEFT:
        begin
          NewRect.left := mousePos.x - FResizeOffset.x;
          NewRect.top := mousePos.y - FResizeOffset.y;
        end;

      HTTOPRIGHT:
        begin
          NewRect.right := mousePos.x + FResizeOffset.x;
          NewRect.top := mousePos.y - FResizeOffset.y;
        end;

      HTBOTTOMLEFT:
        begin
          NewRect.left := mousePos.x - FResizeOffset.x;
          NewRect.bottom := mousePos.y + FResizeOffset.y;
        end;

      HTBOTTOMRIGHT:
        begin
          NewRect.right := mousePos.x + FResizeOffset.x;
          NewRect.bottom := mousePos.y + FResizeOffset.y;
        end;
    end;

    // Ensure we do not go over the limits set for the UI
    ApplyFrameSizeLimits(@NewRect);

    if (GetWindowLong(FFrameHwnd, GWL_STYLE) and WS_POPUP) = 0 then
      ScreenRectToClient(frameParent, NewRect);

    if (NewRect.left <> frameRect.left) or (NewRect.top <> frameRect.top) or
      (NewRect.right <> frameRect.right) or (NewRect.bottom <> frameRect.bottom)
    then
      MoveWindow(FFrameHwnd, NewRect.left, NewRect.top,
        NewRect.right - NewRect.left, NewRect.bottom - NewRect.top, True);
  end
  else
    FSizingCursor := HitTest;

  UpdateEdgeCursor;

  Result := FSizingCursor <> HTNOWHERE;
end;

// ----------------------------------------------------------------------
// Enter sizing mode if the mouse is on a drag edge/corner
// ----------------------------------------------------------------------
function TVstWindowSizer.OnButtonDown: Boolean;
var
  frameRect: TRect;
  mousePos: TPoint;
begin
  if not(rfSimulateDragEdge in FResizeFlags) then
  begin
    Result := False;
    exit;
  end;

  FSizingCursor := HitTest;

  if FSizingCursor = HTNOWHERE then
  begin
    Result := False;
    exit;
  end;

  GetCursorPos(mousePos);
  GetWindowRect(FFrameHwnd, frameRect);
  SetCapture(FFrameHwnd);
  UpdateEdgeCursor;

  case FSizingCursor of
    HTTOPLEFT:
      begin
        FResizeOffset.x := mousePos.x - frameRect.left;
        FResizeOffset.y := mousePos.y - frameRect.top;
      end;
    HTTOPRIGHT:
      begin
        FResizeOffset.x := frameRect.right - mousePos.x;
        FResizeOffset.y := mousePos.y - frameRect.top;
      end;
    HTBOTTOMLEFT:
      begin
        FResizeOffset.x := mousePos.x - frameRect.left;
        FResizeOffset.y := frameRect.bottom - mousePos.y;
      end;
    HTBOTTOMRIGHT:
      begin
        FResizeOffset.x := frameRect.right - mousePos.x;
        FResizeOffset.y := frameRect.bottom - mousePos.y;
      end;
    HTLEFT:
      FResizeOffset.x := mousePos.x - frameRect.left;

    HTTOP:
      FResizeOffset.y := mousePos.y - frameRect.top;

    HTRIGHT:
      FResizeOffset.x := frameRect.right - mousePos.x;

    HTBOTTOM:
      FResizeOffset.y := frameRect.bottom - mousePos.y;

  end;

  Result := FSizingCursor <> HTNOWHERE;
end;

// ----------------------------------------------------------------------
// Exit sizing mode
// ----------------------------------------------------------------------
function TVstWindowSizer.OnButtonUp: Boolean;
begin
  if not(rfSimulateDragEdge in FResizeFlags) or (GetCapture() <> FFrameHwnd)
  then
  begin
    Result := False;
    exit;
  end;

  ReleaseCapture;
  FSizingCursor := HitTest;
  UpdateEdgeCursor;

  Result := FSizingCursor <> HTNOWHERE;
end;

// ----------------------------------------------------------------------
// Since we cannot use GWL_USERDATA with windows we do not own, instead we use a static
// list of objects to determine the VstWindowSizer instance
// ----------------------------------------------------------------------
function GetSizer(hWindow: HWND): TVstWindowSizer;
var
  idx: Integer;

begin
  for idx := 0 to GEditors.Count - 1 do
    if TWindowInfo(GEditors.items[idx]).hWindow = hWindow then
    begin
      Result := TWindowInfo(GEditors.items[idx]).pSizer;
      exit;
    end;
  Result := nil;
end;

// ----------------------------------------------------------------------
// Convert a client rect to a screen rect
// ----------------------------------------------------------------------
procedure TVstWindowSizer.ClientRectToScreen(hWindow: HWND; var Rect: TRect);
var
  clientOffset: TPoint;
begin
  clientOffset.x := 0;
  clientOffset.y := 0;
  ClientToScreen(hWindow, clientOffset);
  OffsetRect(Rect, clientOffset.x, clientOffset.y);
end;

// ----------------------------------------------------------------------
// Convert a screen rect to a client rect
// ----------------------------------------------------------------------
procedure TVstWindowSizer.ScreenRectToClient(hWindow: HWND; var Rect: TRect);
var
  screenOffset: TPoint;
begin
  screenOffset.x := 0;
  screenOffset.y := 0;
  ScreenToClient(hWindow, screenOffset);
  OffsetRect(Rect, screenOffset.x, screenOffset.y);
end;

// ----------------------------------------------------------------------
// We hook into the editor window procedure to intercept the messages we need to make this work
// ----------------------------------------------------------------------
function EditorWindowProc(hWindow: HWND; Msg: UINT; wParam: wParam;
  lParam: lParam): LRESULT; stdcall;
var
  Sizer: TVstWindowSizer;
begin
  Sizer := GetSizer(hWindow);

  case Msg of
    WM_SIZE:
      if Sizer <> nil then
        Sizer.FixupWindowSize;
    WM_LBUTTONDOWN:
      if (Sizer <> nil) and (Sizer.OnButtonDown()) then
      begin
        Result := 0;
        exit;
      end;
    WM_LBUTTONUP:
      if (Sizer <> nil) and (Sizer.OnButtonUp()) then
      begin
        Result := 0;
        exit;
      end;

    WM_MOUSEMOVE:
      if (Sizer <> nil) and (Sizer.OnMouseMove()) then
      begin
        Result := 0;
        exit;
      end;
    WM_CAPTURECHANGED:
      if Sizer <> nil then
      begin
        // We are no longer sizing, but ensure we display the correct cursor
        Sizer.FSizingCursor := Sizer.HitTest;
        Sizer.UpdateEdgeCursor;
      end;
    WM_PAINT:
      if (Sizer <> nil) and (not Sizer.FTrackingInitialized) then
      begin
        // We use the initial WM_PAINT message to set up tracking since
        // window sizes/positions have "stabilized" at this point
        Sizer.SetupTracking;
        Sizer.FTrackingInitialized := True;
      end;
    WM_DESTROY:
      if Sizer <> nil then
        Sizer.EndTracking(hWindow, False);
  end;

  if (Sizer <> nil) and (Sizer.FEditorInfo <> nil) and
    (Sizer.FEditorInfo.prevProc <> nil) then
    Result := CallWindowProc(Sizer.FEditorInfo.prevProc, hWindow, Msg,
      wParam, lParam)
  else
    Result := DefWindowProc(hWindow, Msg, wParam, lParam);
end;

// ----------------------------------------------------------------------
// We hook into the editor's frame window procedure to intercept needed messages
// ----------------------------------------------------------------------

function FrameWindowProc(hWindow: HWND; Msg: UINT; wParam: wParam;
  lParam: lParam): LRESULT; stdcall;
var
  idx: Integer;
  Track: TWindowInfo;
begin

  for idx := 0 to GFramesTrack.Count - 1 do
  begin
    Track := TWindowInfo(GFramesTrack.items[idx]);
    if Track.hWindow = hWindow then
    begin
      case Msg of
        WM_SIZING:
          begin
            Track.pSizer.ApplyFrameSizeLimits(PRECT(lParam));
            Result := 1;
            exit;
          end;
        WM_SIZE:
          begin
            Track.pSizer.TrackParentWindowSize(hWindow);
            if (rfFilterWMSizing in Track.pSizer.FResizeFlags) then
            begin
              Result := 1;
              exit;
            end;
          end;
        WM_LBUTTONDOWN, WM_NCLBUTTONDOWN:
          if Track.pSizer.OnButtonDown() then
          begin
            Result := 0;
            exit;
          end;
        WM_LBUTTONUP, WM_NCLBUTTONUP:
          if Track.pSizer.OnButtonUp() then
          begin
            Result := 0;
            exit;
          end;
        WM_MOUSEMOVE, WM_NCMOUSEMOVE:
          if Track.pSizer.OnMouseMove() then
          begin
            Result := 0;
            exit;
          end;
        WM_CLOSE:
          Track.pSizer.EndTracking(0, True);
        WM_DESTROY:
          Track.pSizer.EndTracking(0, True);

        WM_WINDOWPOSCHANGING:
          begin
            Result := 0;
            exit;
          end;

        WM_WINDOWPOSCHANGED:
          begin
            if rfUpdateSizeOnWMWindowPosChanged in Track.pSizer.FResizeFlags
            then
              Track.pSizer.TrackParentWindowSize(hWindow);
            if (rfFilterWMWindowPosChanged in Track.pSizer.FResizeFlags) then
            begin
              Result := 0;
              exit;
            end;
          end;
      end;

      Result := CallWindowProc(Track.prevProc, hWindow, Msg, wParam, lParam);
      exit;
    end;
  end;

  // Should hopefully never get here
  Result := DefWindowProc(hWindow, Msg, wParam, lParam);
end;



// ----------------------------------------------------------------------
// Some hosts like Cubase has one of the intermediate parent windows of the editor resize it's
// parent when it gets resized.  This interferes with our method here, so we stop it from doing that
// ----------------------------------------------------------------------

function ChildWindowProc(hWindow: HWND; Msg: UINT; wParam: wParam;
  lParam: lParam): LRESULT; stdcall;
var
  idx: Integer;
  childIdx: Integer;
  Track: TWindowInfo;
begin
  for idx := 0 to GFramesTrack.Count - 1 do
  begin
    Track := TWindowInfo(GFramesTrack.items[idx]);
    for childIdx := 0 to Track.pSizer.FWindowsAdjust.Count - 1 do
      if TWindowInfo(Track.pSizer.FWindowsAdjust.items[childIdx]).hWindow = hWindow
      then
      begin
        case Msg of
          WM_SIZE:
            if Track.pSizer.FPreventChildSIze > 0 then
            begin
              Result := 0;
              exit;
            end;
          WM_CLOSE:
            Track.pSizer.EndTracking(hWindow, False);
          WM_DESTROY:
            Track.pSizer.EndTracking(hWindow, False);
        end;

        if Track.prevProc <> nil then
          Result := CallWindowProc(Track.prevProc, hWindow, Msg, wParam, lParam)
        else
          Result := DefWindowProc(hWindow, Msg, wParam, lParam);
        exit;
      end;
  end;
  Result := DefWindowProc(hWindow, Msg, wParam, lParam);
end;


initialization
  GFramesTrack := TObjectList.Create(True);
  GEditors := TObjectList.Create(True);

finalization
  FreeAndNil(GFramesTrack);
  FreeAndNil(GEditors);

end.
