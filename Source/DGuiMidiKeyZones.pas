unit DGuiMidiKeyZones;

interface

uses Classes, Graphics;

type
  TGuiKeyZoneItem = class(TCollectionItem)
  protected
    fDefaultBrushColor:  TColor;
    fDefaultBrushStyle:  TBrushStyle;
    fDefaultBorderColor: TColor;
    fDefaultBorderWidth: Integer;
    fDefaultBorderStyle: TPenStyle;
    
    fHoverBrushColor:  TColor;
    fHoverBrushStyle:  TBrushStyle;
    fHoverBorderColor: TColor;
    fHoverBorderWidth: Integer;
    fHoverBorderStyle: TPenStyle;
    
    fSelectedBrushColor:  TColor;
    fSelectedBrushStyle:  TBrushStyle;
    fSelectedBorderColor: TColor;
    fSelectedBorderWidth: Integer;
    fSelectedBorderStyle: TPenStyle;

    fDisplayName: String;
    fVisible:     Boolean;
    fSelected:    Boolean;
    fIsMouseOver: Boolean;
    
    fLowestZoneKey: Byte;
    fHighestZoneKey: Byte;

    fTag: Integer;

    procedure SetLowestZoneKey(Value: Byte);
    procedure SetHighestZoneKey(Value: Byte);
    procedure SetVisible(Value: Boolean);
    procedure SetDefaultBrushColor(Value: TColor);
    procedure SetDefaultBrushStyle(Value: TBrushStyle);
    procedure SetDefaultBorderColor(Value: TColor);
    procedure SetDefaultBorderWidth(Value: Integer);
    procedure SetDefaultBorderStyle(Value: TPenStyle);
    
    procedure SetHoverBrushColor(Value: TColor);
    procedure SetHoverBrushStyle(Value: TBrushStyle);
    procedure SetHoverBorderColor(Value: TColor);
    procedure SetHoverBorderWidth(Value: Integer);
    procedure SetHoverBorderStyle(Value: TPenStyle);
    
    procedure SetSelectedBrushColor(Value: TColor);
    procedure SetSelectedBrushStyle(Value: TBrushStyle);
    procedure SetSelectedBorderColor(Value: TColor);
    procedure SetSelectedBorderWidth(Value: Integer);
    procedure SetSelectedBorderStyle(Value: TPenStyle);

    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    procedure SetIndex(Value: Integer); override;
    procedure MoveZoneZ(NewIndex: Integer);
    procedure MoveZone(MoveAmount: Integer);
    procedure BringToFront;
    procedure SendToBack;
    procedure Select(doUpdate: Boolean = true);
    procedure SetMouseOver(MouseOverState: Boolean = true; doUpdate: Boolean = true);
    procedure UnSelect(doUpdate: Boolean = true);
    procedure SetBorders(Key1, Key2: Byte; doUpdate: Boolean = true);
    function  KeyInZone(KeyNr: Byte): boolean;
  published
    property DisplayName;
    property LowestZoneKey: Byte read fLowestZoneKey write SetLowestZoneKey default 48;
    property HighestZoneKey: Byte read fHighestZoneKey write SetHighestZoneKey default 59;

    property Visible: Boolean read fVisible write SetVisible default true;

    property DefaultBrushColor: TColor read fDefaultBrushColor write SetDefaultBrushColor default clSkyBlue;
    property DefaultBrushStyle: TBrushStyle read fDefaultBrushStyle write SetDefaultBrushStyle default bsSolid;
    property DefaultBorderColor: TColor read fDefaultBorderColor write SetDefaultBorderColor default clBlack;
    property DefaultBorderWidth: Integer read fDefaultBorderWidth write SetDefaultBorderWidth default 1;
    property DefaultBorderStyle: TPenStyle read fDefaultBorderStyle write SetDefaultBorderStyle default psSolid;

    property HoverBrushColor: TColor read fHoverBrushColor write SetHoverBrushColor default $FFDAB6;
    property HoverBrushStyle: TBrushStyle read fHoverBrushStyle write SetHoverBrushStyle default bsSolid;
    property HoverBorderColor: TColor read fHoverBorderColor write SetHoverBorderColor default clBlack;
    property HoverBorderWidth: Integer read fHoverBorderWidth write SetHoverBorderWidth default 1;
    property HoverBorderStyle: TPenStyle read fHoverBorderStyle write SetHoverBorderStyle default psSolid;

    property SelectedBrushColor: TColor read fSelectedBrushColor write SetSelectedBrushColor default $000099;
    property SelectedBrushStyle: TBrushStyle read fSelectedBrushStyle write SetSelectedBrushStyle default bsSolid;
    property SelectedBorderColor: TColor read fSelectedBorderColor write SetSelectedBorderColor default clBlack;
    property SelectedBorderWidth: Integer read fSelectedBorderWidth write SetSelectedBorderWidth default 1;
    property SelectedBorderStyle: TPenStyle read fSelectedBorderStyle write SetSelectedBorderStyle default psSolid;

    property Tag: Integer read fTag write fTag;
    property Selected: Boolean read fSelected;
    property IsMouseOver: Boolean read fIsMouseOver;
  end;



  TGuiKeyZoneCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer):TGuiKeyZoneItem;
    procedure SetItem(Index: Integer; Value: TGuiKeyZoneItem);
  protected
    fAllowUpdate: Boolean;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    procedure ClipZones;
    function Add: TGuiKeyZoneItem;
    function Selected: TGuiKeyZoneItem;
    function Insert(Index: Integer): TCollectionItem;
    procedure UpdateOwner;
    procedure UnselectAll(doUpdate: Boolean = true);
    function ZoneByKey(KeyNr: Byte): TGuiKeyZoneItem;
    procedure DeleteSelected;
    property Items[Index: Integer]: TGuiKeyZoneItem read GetItem write SetItem; default;
  end;

implementation

uses DGuiMidiKeys, Math;


constructor TGuiKeyZoneItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fDisplayName    := ClassName;
  fLowestZoneKey  := 48;
  fHighestZoneKey := 59;
  fVisible        := true;

  fDefaultBrushColor  := $F0CAA6;   fHoverBrushColor  := $FFDAB6;             fSelectedBrushColor  := $000099;
  fDefaultBrushStyle  := bsSolid;   fHoverBrushStyle  := fDefaultBrushStyle;  fSelectedBrushStyle  := fDefaultBrushStyle;
  fDefaultBorderWidth := 1;         fHoverBorderWidth := fDefaultBorderWidth; fSelectedBorderWidth := fDefaultBorderWidth;
  fDefaultBorderColor := clBlack;   fHoverBorderColor := fDefaultBorderColor; fSelectedBorderColor := fDefaultBorderColor;
  fDefaultBorderStyle := psSolid;   fHoverBorderStyle := fDefaultBorderStyle; fSelectedBorderStyle := fDefaultBorderStyle;
end;

procedure TGuiKeyZoneItem.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
end;

function TGuiKeyZoneItem.GetDisplayName: string;
begin
  Result := fDisplayName;
end;

procedure TGuiKeyZoneItem.SetDisplayName(const Value: string);
begin
  fDisplayName := Value;
end;





procedure TGuiKeyZoneItem.SetDefaultBrushColor(Value: TColor);
begin
  if fDefaultBrushColor<>Value then
  begin
    fDefaultBrushColor:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetDefaultBrushStyle(Value: TBrushStyle);
begin
  if fDefaultBrushStyle<>Value then
  begin
    fDefaultBrushStyle:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetDefaultBorderColor(Value: TColor);
begin
  if fDefaultBorderColor<>Value then
  begin
    fDefaultBorderColor:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetDefaultBorderWidth(Value: Integer);
begin
  if fDefaultBorderWidth<>Value then
  begin
    fDefaultBorderWidth:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetDefaultBorderStyle(Value: TPenStyle);
begin
  if fDefaultBorderStyle<>Value then
  begin
    fDefaultBorderStyle:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;




procedure TGuiKeyZoneItem.SetHoverBrushColor(Value: TColor);
begin
  if fHoverBrushColor<>Value then
  begin
    fHoverBrushColor:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetHoverBrushStyle(Value: TBrushStyle);
begin
  if fHoverBrushStyle<>Value then
  begin
    fHoverBrushStyle:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetHoverBorderColor(Value: TColor);
begin
  if fHoverBorderColor<>Value then
  begin
    fHoverBorderColor:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetHoverBorderWidth(Value: Integer);
begin
  if fHoverBorderWidth<>Value then
  begin
    fHoverBorderWidth:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetHoverBorderStyle(Value: TPenStyle);
begin
  if fHoverBorderStyle<>Value then
  begin
    fHoverBorderStyle:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;




procedure TGuiKeyZoneItem.SetSelectedBrushColor(Value: TColor);
begin
  if fSelectedBrushColor<>Value then
  begin
    fSelectedBrushColor:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetSelectedBrushStyle(Value: TBrushStyle);
begin
  if fSelectedBrushStyle<>Value then
  begin
    fSelectedBrushStyle:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetSelectedBorderColor(Value: TColor);
begin
  if fSelectedBorderColor<>Value then
  begin
    fSelectedBorderColor:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetSelectedBorderWidth(Value: Integer);
begin
  if fSelectedBorderWidth<>Value then
  begin
    fSelectedBorderWidth:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetSelectedBorderStyle(Value: TPenStyle);
begin
  if fSelectedBorderStyle<>Value then
  begin
    fSelectedBorderStyle:=Value;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;




procedure TGuiKeyZoneItem.SetVisible(Value: Boolean);
begin
  if fVisible<>Value then
  begin
    fVisible:=Value;
    (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetLowestZoneKey(Value: Byte);
begin
   if fLowestZoneKey<>Value then
  begin
    fLowestZoneKey:=Value;
    if fHighestZoneKey<fLowestZoneKey then fHighestZoneKey:=fLowestZoneKey;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetHighestZoneKey(Value: Byte);
begin
  if fHighestZoneKey<>Value then
  begin
    fHighestZoneKey:=Value;
    if fHighestZoneKey<fLowestZoneKey then fLowestZoneKey:=fHighestZoneKey;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetMouseOver(MouseOverState, doUpdate: Boolean);
var i:integer;
begin
  if MouseOverState<>fIsMouseOver then
  begin
    if MouseOverState then with (Collection as TGuiKeyZoneCollection) do
    begin
      for i:=Count-1 downto 0 do Items[i].SetMouseOver(false, false);
    end;
    fIsMouseOver:=MouseOverState;
    if doUpdate then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;


procedure TGuiKeyZoneItem.Select(doUpdate: Boolean = true);
begin
  if not fSelected then
  begin
    (Collection as TGuiKeyZoneCollection).UnSelectAll(false);
    fSelected:=true;
    if doUpdate then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.UnSelect(doUpdate: Boolean = true);
begin
  if fSelected then
  begin
    fSelected:=false;
    if doUpdate then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetBorders(Key1, Key2: Byte; doUpdate: Boolean);
var tmp: Byte;
begin
  if Key2<Key1 then begin tmp:=Key1; Key1:=Key2; Key2:=tmp; end; // flip
  fLowestZoneKey:=Key1;
  fHighestZoneKey:=Key2;
  if doUpdate then (Collection as TGuiKeyZoneCollection).UpdateOwner;
end;


procedure TGuiKeyZoneItem.MoveZoneZ(NewIndex: Integer);
begin
  SetIndex(NewIndex);
  if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
end;

procedure TGuiKeyZoneItem.BringToFront;
begin
  MoveZoneZ(Collection.Count-1);
end;

procedure TGuiKeyZoneItem.SendToBack;
begin
  MoveZoneZ(0);
end;

procedure TGuiKeyZoneItem.MoveZone(MoveAmount: Integer);
begin
  if MoveAmount=0 then exit;

  MoveAmount:=max(-fLowestZoneKey, MoveAmount);
  MoveAmount:=min(GUI_KB_HIGHESTKEY-fHighestZoneKey, MoveAmount);

  fLowestZoneKey  := fLowestZoneKey+MoveAmount;
  fHighestZoneKey := fHighestZoneKey+MoveAmount;

  if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
end;

function TGuiKeyZoneItem.KeyInZone(KeyNr: Byte): boolean;
begin
  result:=(KeyNr>=fLowestZoneKey) and (KeyNr<=fHighestZoneKey);
end;






constructor TGuiKeyZoneCollection.create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TGuiKeyZoneItem);
  fAllowUpdate := true;
end;

procedure TGuiKeyZoneCollection.UpdateOwner;
begin
  if fAllowUpdate then (Owner as TGuiMidiKeys).RedrawBuffer(true);
end;

procedure TGuiKeyZoneCollection.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  UpdateOwner;
end;

procedure TGuiKeyZoneCollection.Update(Item: TCollectionItem);
begin
  UpdateOwner
end;

function TGuiKeyZoneCollection.GetItem(Index: Integer):TGuiKeyZoneItem;
begin
  Result := TGuiKeyZoneItem(inherited GetItem(Index));
end;

procedure TGuiKeyZoneCollection.SetItem(Index: Integer; Value: TGuiKeyZoneItem);
begin
   inherited SetItem(Index, Value);
end;

function TGuiKeyZoneCollection.Add: TGuiKeyZoneItem;
begin
  Result := TGuiKeyZoneItem(inherited Add);
end;

function TGuiKeyZoneCollection.Insert(Index: Integer): TCollectionItem;
begin
  Result := TGuiKeyZoneItem(inherited Insert(Index));
end;

function TGuiKeyZoneCollection.ZoneByKey(KeyNr: Byte): TGuiKeyZoneItem;
var i:integer;
begin
  result:=nil;
  if Count=0 then exit;

  for i:=Count-1 downto 0 do
    if items[i].KeyInZone(KeyNr) then
    begin
      result:=items[i];
      exit;
    end;
end;

procedure TGuiKeyZoneCollection.ClipZones;

    procedure ClipZoneX(clipIndex, OverlayIndex: Integer);
    var mink, maxk: byte;
    begin
       if not items[OverlayIndex].Visible then Exit;

       mink := items[OverlayIndex].LowestZoneKey;
       maxk := items[OverlayIndex].HighestZoneKey;
       with items[clipIndex] do
       begin
         // check for no overlay
         if (HighestZoneKey<mink) and (LowestZoneKey<mink) then exit;
         if (LowestZoneKey>maxk)  and (HighestZoneKey>maxk) then exit;

         // check for complete overlay (underlaying layer is invisible)
         if  (LowestZoneKey>=mink) and (HighestZoneKey<=maxk) then
         begin          
           Visible:=false;
           LowestZoneKey :=0;
           HighestZoneKey:=0;
           exit;
         end;

         // check for partial overlay
         if (HighestZoneKey>=mink) and (HighestZoneKey<=maxk) then begin
           if LowestZoneKey>maxk then HighestZoneKey:=maxk+1 else HighestZoneKey:=mink-1;
         end else if (LowestZoneKey>=mink) and (LowestZoneKey<=maxk) then begin
           if HighestZoneKey>maxk then LowestZoneKey:=maxk+1 else LowestZoneKey:=mink-1;
         end else begin
           // complete overlay (underlaying Layer is visible)
           HighestZoneKey:=mink-1;
         end;
       end;
    end;

var i, j: integer;
begin
  if Count<2 then exit;

  fAllowUpdate := false;

  for j:=count-1 downto 1 do
    for i:=j-1 downto 0 do
      ClipZoneX(i,j);

  fAllowUpdate := true;
  UpdateOwner;
end;

procedure TGuiKeyZoneCollection.UnSelectAll(doUpdate: Boolean = true);
var i: integer;
begin
  if Count<1 then exit;

  for i:=Count-1 downto 0 do Items[i].UnSelect(false);

  if doUpdate then UpdateOwner;
end;

function TGuiKeyZoneCollection.Selected: TGuiKeyZoneItem;
var i: integer;
begin
  Result:=nil;
  if Count<1 then exit;

  for i:=Count-1 downto 0 do
    if Items[i].Selected then
    begin
      Result:=Items[i];
    end;
end;

procedure TGuiKeyZoneCollection.DeleteSelected;
begin
  Delete(Selected.Index);
end;

end.
