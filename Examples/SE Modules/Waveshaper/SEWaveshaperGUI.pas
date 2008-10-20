unit SEWaveshaperGUI;

interface

uses
  Windows, Classes, DAV_SECommon, DAV_SEModule, DAV_SEGUI, SEWaveshaperModule;

const
  pinShape     =  0;
  CDefaultValue : array [0..CWsNodeCount-1, 0..1] of Single =
  ((-5,-5), (-4,-4), (-3,-3), (-2,-2), (-1, -1), (0, 0), (1, 1), (2 ,2),
   (3, 3), (4, 4), (5, 5));

type
  PPoints = array [0..CWsNodeCount-1] of TPoint;

type
  TSEWaveshaperGui = class(TSEGUIBase)
  private
    FDragNode      : Integer;
    FPtPrev        : TSEPoint;
    FInhibitUpdate : Boolean;
    FNodes         : array [0..CWsNodeCount-1] of TPoint; // x,y co-ords of control points

    function DefaultValue: TSeSdkString;
    function GetValueS: TSeSdkString;
    procedure SetValueS(Astring: TSeSdkString);
    procedure OnValueChanged;
    procedure DrawScale(hDC: HDC; wi: PSEWndInfo);
    function Handle: THandle;
    procedure SendStringToAudio(AMsgID, ALength: Integer; AData: Pointer);
    function InvalidateControl: Integer;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
    procedure GuiPaint(hDC: HDC; wi: PSEWndInfo); override;
    procedure GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer); override;
    procedure GuiLButtonDown(wi: PSEWndInfo; nFlags: Cardinal; Pnt: TSEPoint); override;
    procedure GuiLButtonUp(wi: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint); override;
    procedure GuiMouseMove(wi: PSEWndInfo; nFlags: Cardinal; Pnt: TSEPoint); override;
    procedure Initialise(LoadedFromFile: Boolean); override;
    procedure GuiPinValueChange(Pin: TSeGuiPin); override;
    class procedure UpdateNodes(FNodes: PPoints; var AValues: TSeSdkString);
  end;

implementation

uses
  SysUtils, Math;

constructor TSEWaveshaperGui.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 inherited;
 FDragNode := -1;
 FInhibitUpdate := False;

 CallHost(seGuiHostSetWindowSize, 100, 100);
 CallHost(seGuiHostSetWindowSizeable, 1);
end;

// convert string value into segment list
class procedure TSEWaveshaperGui.UpdateNodes(FNodes: PPoints; var AValues: TSeSdkString);
var
  s          : TSeSdkString;
  i          : Integer;
  p1, p2, p3 : Integer;
  x, y       : Single;
begin
 s := AValues;

 // convert CString of numbers to array of screen co-ords
 i := 0;
 while (Length(s) > 0) and (i < CWsNodeCount) do
  begin
   p1 := Pos('(', s);
   p2 := Pos(',', s);
   p3 := Pos(')', s);
   if (p3 < 1) then exit;

   x := StrToFloat(Copy(s, p1 + 1, p2 - p1 - 1));
   y := StrToFloat(Copy(s, p2 + 1, p3 - p2 - 1));
   x := (5 + x) * 10; // convert to screen co-ords
   y := (5 - y) * 10;

   FNodes[i].x := round(x);
   FNodes[i].y := round(y);
   inc(i);

   inc(p3);
   s := copy(s, p3, Length(s) - p3); //.Right( s.length - p3 - 1);
  end;

  if (i = 0) then // if string empty, set defaults
   begin
//    UpdateNodes(FNodes, CDefaultValue);
   end;
end;

procedure TSEWaveshaperGui.Initialise(LoadedFromFile: Boolean);
begin
 inherited Initialise(LoadedFromFile);
 OnValueChanged; // initial value
end;

procedure TSEWaveshaperGui.GuiPinValueChange(Pin: TSeGuiPin);
begin
 OnValueChanged;
end;

procedure TSEWaveshaperGui.OnValueChanged;
begin
  if Length(GetValueS) = 0 then // set a default straight line
   begin
    SetValueS(DefaultValue);
    exit; // rely on recursion to re-call this routine
   end;

  if not FInhibitUpdate then
   begin
//    UpdateNodes(FNodes, GetValueS);
    InvalidateControl;
   end;
end;

function TSEWaveshaperGui.GetValueS: TSeSdkString;
begin
 result := getPin(pinShape).getValueText;
end;

procedure TSEWaveshaperGui.SetValueS(AString: TSeSdkString);
begin
  getPin(pinShape).setValueText(AString);
end;

function TSEWaveshaperGui.DefaultValue: TSeSdkString;
begin
// result := CDefaultValue;
end;

procedure TSEWaveshaperGui.DrawScale(hDC: HDC; wi: PSEWndInfo);
var
  VertScale  : Single;
  HorzScale  : Single;
  Mid        : TPoint;
  Pen        : HPEN;
  OldPen     : HGDIOBJ;
  TickWidth  : Integer;
  v          : Integer;
  x, y       : Single;
  FontHeight : Integer;
  LgFnt      : LOGFONT;
  Font       : HFONT;
  OldFont    : HGDIOBJ;
begin
 VertScale := wi.height / 2.15;
 HorzScale := wi.width / 2.15;
 Mid.x   := wi.width div 2;
 Mid.y   := wi.height div 2;

  // create a green pen
  pen := CreatePen(PS_SOLID, 1, RGB(0, 128, 0)); // dark green

  // 'select' it
  OldPen := SelectObject(hDC, pen);

  // BACKGROUND LINES

  // horizontal line
  MoveToEx(hDC, 0, Mid.y, nil);
  LineTo(hDC, wi.width, Mid.y );

  // horiz center line
  MoveToEx(hDC, Mid.x, 0, nil);
  LineTo(hDC, Mid.x, wi.height );

  // vertical center line
  MoveToEx(hDC, Mid.x,0, nil);
  LineTo(hDC, Mid.x, wi.height);

  // diagonal
  MoveToEx(hDC, 0, wi.height, nil);
  LineTo(hDC, wi.width,0);

 v := -10;
 while v <= 10 do
  begin
   y := v * VertScale * 0.1;
   x := v * HorzScale * 0.1;

   if v mod 5 = 0
    then TickWidth := 4
    else TickWidth := 2;

   // X-Axis ticks
   MoveToEx(hDC, Mid.x - TickWidth, round(Mid.y + y), nil);
   LineTo(hDC, Mid.x + TickWidth, round(Mid.y + y));

   // Y-Axis ticks
   MoveToEx(hDC, round(Mid.x + x), Mid.y - TickWidth, nil);
   LineTo(hDC, round(Mid.x + x), Mid.y + TickWidth );
   inc(v, 2);
  end;

 // cleanup
 SelectObject(hDC, OldPen);
 DeleteObject(pen);

 // labels
 if (wi.height > 30) then
  begin
   // Set up the Font

//    DcFontInfo oldfont(p_child.Skin,&pDC, _T('tty'));
   FontHeight := 10; // p_child.Skin.getFontDescription(_T('tty')).AverageCharSize.cy;

    FillChar(LgFnt, SizeOf(LOGFONT), 0);   // Clear out structure.

    StrCopy(LgFnt.lfFaceName, 'Terminal');    // face name
    LgFnt.lfHeight := -FontHeight;

    Font := CreateFontIndirect(LgFnt);
    OldFont := SelectObject(hDC, Font);

    SetTextColor( hDC, RGB(0,250,0) );
    SetBkMode( hDC, TRANSPARENT );
    SetTextAlign( hDC, TA_LEFT );

(*
    char txt[10];
    // Y-Axis text
    for( Single fv = -5 ; fv < 5.1 ; fv += 2.0 )
     begin
      Single y = fv * VertScale / 5.f;
      if( fv != -1.f )
       begin
        sprintf( txt, '%2.0f', fv );
        TextOut( hDC, Mid.x + TickWidth, Mid.y - (Integer) y - FontHeight/2, txt, strlen(txt) );
       end;
     end;

    Integer orig_ta = SetTextAlign( hDC, TA_CENTER );

    // X-Axis text
    for( Single fv = -4 ; fv < 4.1 ; fv += 2.0 )
     begin
      Single y = fv * HorzScale / 5.f;
      if( fv != -1.f )
       begin
        sprintf( txt, '%2.0f', fv );
        TextOut( hDC, Mid.x + (Integer)y, Mid.y + TickWidth, txt, strlen(txt) );
       end;
     end;

    // cleanup
    SelectObject( hDC, OldFont );
    DeleteObject(Font);
    SetTextAlign( hDC, orig_ta );
 *)
  end;
end;

procedure TSEWaveshaperGui.GuiPaint(hDC: HDC; wi :PSEWndInfo);
var
  CtlWidth, i      : Integer;
  CtlHeight, x, y  : Integer;
  dx, dy           : Double;
  VertScale        : Single;
  HorzScale        : Single;
  Mid              : TPoint;
  BackgroundBrush  : HBRUSH;
  Rct              : TRect;
  Pen              : HPEN;
  OldPen, OldFont : HGDIOBJ;
  LgFnt            : LOGFONT;
  Font             : HFONT;
  pt                : TPoint;
  txt              : string;
  pts              : array [0..4] of TSEPoint; // holds square node points

const
  FontHeight : Integer = 10;

begin
 CtlWidth  := wi.width;
 CtlHeight := wi.height;
 VertScale := CtlHeight * 0.01;
 HorzScale := CtlWidth * 0.01;
 Mid.x     := CtlWidth div 2;
 Mid.y     := CtlHeight div 2;

 // Fill in solid background black
 BackgroundBrush := CreateSolidBrush( RGB(0,0,0) );

 Rct.top    := 0;
 Rct.left   := 0;
 Rct.right  := wi.width + 1;
 Rct.bottom := wi.height + 1;
 FillRect(hDC, Rct, BackgroundBrush);

 // cleanup objects
 DeleteObject(BackgroundBrush);

 // draw scale markings
 DrawScale(hDC, wi);

 // create a green pen
 pen := CreatePen(PS_SOLID, 1, RGB(0, 255, 0)); // light green

  // 'select' it
  OldPen := SelectObject(hDC, pen);

  i := CWsNodeCount - 1;

  MoveToEx(hDC, round(FNodes[i].x * HorzScale + 0.5), round(FNodes[i].y * VertScale + 0.5), nil);
  Dec(i);

  while i >= 0 do
   begin
    LineTo(hDC, round(FNodes[i].x * HorzScale + 0.5), round(FNodes[i].y * VertScale + 0.5));
    Dec(i);
   end;

  // Nodes
  for i := CWsNodeCount - 1 downto 0 do
   begin
    x := round(FNodes[i].x * HorzScale - CNodeSize * 0.5 + 0.5);
    y := round(FNodes[i].y * VertScale - CNodeSize * 0.5 + 0.5);
    pts[0] := Point(x, y);
    pts[1] := Point(x + CNodeSize,y);
    pts[2] := Point(x + CNodeSize,y + CNodeSize);
    pts[3] := Point(x ,y + CNodeSize);
    pts[4] := pts[0];
    Polyline(hDC, pts, 5);
   end;

  // display drag node co-ords
    if FDragNode > -1 then
     begin
      FillChar(LgFnt, SizeOf(LOGFONT), 0);   // Clear out structure.

      StrCopy(LgFnt.lfFaceName, 'Terminal');    // face name
      LgFnt.lfHeight := -FontHeight;

      Font := CreateFontIndirect(&LgFnt);
      OldFont := SelectObject( hDC, Font );

      SetTextColor( hDC, RGB(0,250,0) );
      SetBkMode( hDC, TRANSPARENT );
      SetTextAlign( hDC, TA_LEFT );

      pt := FNodes[FDragNode];
      dx := pt.x * 0.1 - 5.0;
      dy := 5.0 - pt.y * 0.1;
      txt := FloatToStrF(dx, ffFixed, 3, 1) + ', ' +
             FloatToStrF(dy, ffFixed, 3, 1);

      TextOut(hDC, 0, 0, @txt[1], Length(txt));

      // cleanup
      SelectObject(hDC, OldFont);
      DeleteObject(Font);
     end;

  // cleanup
  SelectObject(hDC, OldPen);
  DeleteObject(pen);
end;

procedure TSEWaveshaperGui.GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer);
(*
var
  Msg    : TChunkName;
  Size   : Integer;
  Handle : THandle;
*)
begin
(*
 assert(ALength = SizeOf(values));
 Move(Values, AData, ALength);

 // aknowledge
 h    := Handle;
 msg  := 'ack';
 size := 3;

 ////////////// EXPERIMENTAL ////////////
 SendStringToAudio(4, @Handle);
 SendStringToAudio(4, @Size);
 SendStringToAudio(Size, Msg);
 ///////////////////////////////////////
*)

 InvalidateControl;
end;

procedure TSEWaveshaperGui.GuiLButtonDown(wi: PSEWndInfo; nFlags: Cardinal; Pnt: TSEPoint);
var
  i         : Integer;
  CtlWidth  : Integer;
  CtlHeight : Integer;
  VertScale : Single;
  HorzScale : Single;
  pt        : TPoint;
  rct       : TRect;
begin
  (* testing pop up menu...
  // get this module's window handle
  HWND hw = (HWND) CallHost( seGuiHostGetWindowHandle, wi.context_handle );

  RECT Rct;
  // find it's location (so we know where to draw pop up menu)
  GetWindowRect(hw, &Rct);

  // create a pop-up menu
  HMENU hm = CreatePopupMenu;

  // add some items to it..
  AppendMenu( hm, MF_STRING , 1, 'Cat' );
  AppendMenu( hm, MF_STRING , 2, 'Dog' );

  // show the menu
  Integer selection = TrackPopupMenu( hm, TPM_LEFTALIGN|TPM_NONOTIFY|TPM_RETURNCMD,Rct.left ,Rct.top,0, hw, 0);

  // clean up
  DestroyMenu(hm);
  return;
*)
  CtlWidth  := wi.width;
  CtlHeight := wi.height;

  VertScale     := CtlHeight * 0.01;
  HorzScale     := CtlWidth * 0.01;

  FDragNode := -1;

  for i := CWsNodeCount - 1 downto 0 do
   begin
    pt := Point(round(FNodes[i].x * HorzScale), round(FNodes[i].y * VertScale));
    rct.top    := pt.y - CNodeSize div 2;
    rct.bottom := pt.y + CNodeSize div 2;
    rct.left   := pt.x - CNodeSize div 2;
    rct.right  := pt.x + CNodeSize div 2;
    if PtInRect(rct, Pnt) then
     begin
      FDragNode := i;
      FPtPrev   := Pnt;
      SetCapture(wi); // get mouse moves
      break;
     end;
   end;
end;

procedure TSEWaveshaperGui.GuiMouseMove(wi: PSEWndInfo; nFlags: Cardinal; Pnt: TSEPoint);
var
  i         : Integer;
  CtlWidth  : Integer;
  CtlHeight : Integer;

  VertScale : Single;
  HorzScale : Single;
  Left      : Single;
  Right     : Single;
  pt        : TPoint;
  v         : TSeSdkString;
begin
  if not GetCapture(wi) then exit;

  if FDragNode > -1 then
   begin
    CtlWidth  := wi.width;
    CtlHeight := wi.height;
    VertScale     := CtlHeight * 0.01;
    HorzScale     := CtlWidth * 0.01;

    Left  := 0;
    Right := 100;

    if FDragNode > 0
     then Left := FNodes[FDragNode - 1].x + 1;

    if FDragNode < CWsNodeCount - 1
     then right := FNodes[FDragNode + 1].x - 1;

    if FDragNode = 0
     then Right := 0;

    if FDragNode = CWsNodeCount - 1
     then left := 100;

    pt := FNodes[FDragNode];
    pt.x := round(pt.x + (Pnt.x - FPtPrev.x) / HorzScale);
    pt.y := round(pt.y + (Pnt.y - FPtPrev.y) / VertScale);

    // constain
    pt.x := round(max(pt.x, left));
    pt.x := round(min(pt.x, right));
    pt.y := round(max(pt.y, 0));
    pt.y := round(min(pt.y, 100));

    FNodes[FDragNode] := pt;
    FPtPrev := Pnt;

    for i := 0 to CWsNodeCount - 1 do
     begin
(*
      pt := FNodes[i];
      char pt[20];
      double x = pt.x/10.0 - 5.0;
      double y = 5.0 - pt.y/10.0;
      sprintf(pt,'(%3.1f,%3.1f)',x,y);
      strcat(v, pt);
      assert(Length(v) < 280);
*)
     end;

    FInhibitUpdate := True; // prevent jitter due to Single.text.Single conversion
    SetValueS(v); // should default to current patch
    FInhibitUpdate := False;
    InvalidateControl;
  end;

  //TODO
//  SetValueString(CurrentPatch, v); // should default to current patch
end;

procedure TSEWaveshaperGui.GuiLButtonUp(wi: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint);
begin
 if (not GetCapture(wi))
  then exit;

 ReleaseCapture(wi); // don't want further mouse move events
 FDragNode := -1;

 // clear on-screen drag co-ords
 InvalidateControl;
end;

procedure TSEWaveshaperGui.SendStringToAudio(AMsgID, ALength: Integer; AData: Pointer);
begin
 CallHost(seGuiHostSendStringToAudio, ALength, AMsgID, AData);
end;

function TSEWaveshaperGui.Handle: THandle;
begin
 result := CallHost(seGuiHostGetHandle);
end;

function TSEWaveshaperGui.InvalidateControl: Integer;
begin
 result := CallHost(seGuiHostRequestRepaint);
end;

end.
