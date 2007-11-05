unit DVSTParameters;

interface

{$I ASIOVST.INC}

uses classes, sysutils, Windows, Forms, DVSTEffect, DAVDCommon, DVSTBasicModule;

type
  TCurveType = (ctLinear, ctLogarithmic, ctExponential, ctFrequencyScale);

  TParameterChangeEvent        = procedure(Sender: TObject; const Index: Integer; var Value: Single) of object;
  TCustomParameterLabelEvent   = procedure(Sender: TObject; const Index: Integer; var PreDefined: string) of object;
  TCustomParameterDisplayEvent = procedure(Sender: TObject; const Index: Integer; var PreDefined: string) of object;


  TCustomVstParameterProperty = class(TCollectionItem)
  private
    FSmoothStates     : T2SingleArray;
    FMin, fMax        : Single;
    FCurve            : TCurveType;
    FCurveFactor      : Single;
    FDisplayName      : string;
    FUnits            : string;
    FSmoothingFactor  : Single;
    FCanBeAutomated   : Boolean;
    FV2Properties     : Boolean;
    FStepFloat        : Single;
    FSmallStepFloat   : Single;
    FLargeStepFloat   : Single;
    FFlags            : TVstParameterPropertiesFlags;
    FMinInteger       : Integer;
    FMaxInteger       : Integer;
    FStepInteger      : Integer;
    FLargeStepInteger : Integer;
    FCC               : Integer;
    FShortLabel       : string[7];

    FVSTModule        : TBasicVSTModule;
    FOnSPC            : TParameterChangeEvent;
    FOnCPL            : TCustomParameterLabelEvent;
    FOnCPD            : TCustomParameterDisplayEvent;

    
    function GetShortLabel: string;
    procedure SetShortLabel(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
    procedure SetUnits(AUnits: string);
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
    function Smooth(i: Single): Single;
  published
    property Min: Single read FMin write FMin;
    property Max: Single read FMax write FMax;
    property CC: Integer read FCC write FCC default -1;
    property Curve: TCurveType read FCurve write FCurve;
    property DisplayName{$IFNDEF FPC}: string read FDisplayName write SetDisplayName{$ENDIF};
    property Units: string read FUnits write SetUnits;
    property CurveFactor: Single read FCurveFactor write FCurveFactor;
    property SmoothingFactor: Single read FSmoothingFactor write FSmoothingFactor;
    property CanBeAutomated: Boolean read FCanBeAutomated write FCanBeAutomated default true;
    property ReportVST2Properties: Boolean read FV2Properties write FV2Properties default false;
    property StepFloat: Single read FStepFloat write FStepFloat;
    property SmallStepFloat: Single read FSmallStepFloat write FSmallStepFloat;
    property LargeStepFloat: Single read FLargeStepFloat write FLargeStepFloat;
    property Flags: TVstParameterPropertiesFlags read FFlags write FFlags default [];
    property MinInteger: Integer read FMinInteger write FMinInteger default 0;
    property MaxInteger: Integer read FMaxInteger write FMaxInteger default 100;
    property StepInteger: Integer read FStepInteger write FStepInteger default 1;
    property LargeStepInteger: Integer read FLargeStepInteger write FLargeStepInteger default 10;
    property ShortLabel: string read GetShortLabel write SetShortLabel;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
    property OnParameterChange: TParameterChangeEvent read FOnSPC write FOnSPC;
    property OnCustomParameterLabel: TCustomParameterLabelEvent read FOnCPL write FOnCPL;
    property OnCustomParameterDisplay: TCustomParameterDisplayEvent read FOnCPD write FOnCPD;
  end;

  TCustomVstParameterProperties = class(TOwnedCollection)
  private
    FVSTModule: TBasicVSTModule;
  protected
    function GetItem(Index: Integer): TCustomVstParameterProperty; virtual;
    procedure SetItem(Index: Integer; const Value: TCustomVstParameterProperty); virtual;
    property Items[Index: Integer]: TCustomVstParameterProperty read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure WriteVSTXML; overload;
    procedure WriteVSTXML(FileName : TFileName); overload;
    function Add: TCustomVstParameterProperty;
    function Insert(Index: Integer): TCustomVstParameterProperty;
    procedure Delete(Index: Integer);
    property Count;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;

  TVstParameterProperty = TCustomVstParameterProperty;
  TVstParameterProperties = TCustomVstParameterProperties;

implementation

uses DVSTModuleWithPrograms, Math;

{$IFDEF FPC}
constructor TCustomVstParameterProperty.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstParameterProperty.Create(Collection: TCollection);
{$ENDIF}
var i: Integer;
begin
  inherited;
  FMin         := 0;
  FMax         := 1;
  FMinInteger  := 0;
  FMaxInteger  := 100;
  FStepInteger := 1;
  FCC          := -1;
  FCurve       := ctLinear;
  FFlags       := [];
  FCurveFactor := 1;
  FLargeStepInteger := 10;
  FSmoothingFactor  := 1;
  FCanBeAutomated   := True;
  FV2Properties     := False;
  FDisplayName := 'Parameter '+IntTostr(Collection.Count);

  FVSTModule := (Collection As TCustomVstParameterProperties).VSTModule;
  with FVSTModule as TVSTModuleWithPrograms do
  try
    Effect^.numParams := Collection.Count;
    if not (effFlagsProgramChunks in Effect^.EffectFlags) then
      if (Effect^.numPrograms>0) then
        for i := 0 to Effect^.numPrograms-1 do
          Programs[i].SetParameterCount(Collection.Count)
      else SetParameterCount(Collection.Count);
  except
  end;
end;

destructor TCustomVstParameterProperty.Destroy;
var i: Integer;
begin
  try
    if assigned(VSTModule) then
      with FVSTModule as TVSTModuleWithPrograms do
      begin
        if not (effFlagsProgramChunks in Effect^.EffectFlags) then
          if Effect^.numPrograms>0 then
            for i := 0 to Effect^.numPrograms-1 do
              Programs[i].SetParameterCount(Collection.Count - 1)
          else SetParameterCount(Collection.Count - 1);

        if (HostProduct <> 'Cubase VST') and (HostProduct <> 'Unknown') and (HostProduct <> '') then
          Effect^.numParams := Collection.Count - 1;
    end;
  except
  end;

  inherited;
end;

function TCustomVstParameterProperty.Smooth(i: Single): Single;
begin
  FSmoothStates[0] := FSmoothStates[0] + SmoothingFactor * (i - FSmoothStates[0]);
  FSmoothStates[1] := FSmoothStates[1] + SmoothingFactor * (FSmoothStates[0] - FSmoothStates[1]);
  Result := FSmoothStates[1];
end;

procedure TCustomVstParameterProperty.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomVstParameterProperty then
    with TCustomVstParameterProperty(Dest) do
    try
      Units := Self.Units;
      DisplayName := Self.DisplayName;
    except
      inherited;
    end
  else inherited;
end;

procedure TCustomVstParameterProperty.SetDisplayName(const AValue: string);
begin
  FDisplayName := Copy(AValue,1,Math.Min(30,Length(AValue)));
end;

function TCustomVstParameterProperty.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

procedure TCustomVstParameterProperty.SetUnits(AUnits: string);
begin
  FUnits := AUnits;
end;

function TCustomVstParameterProperty.GetShortLabel: string;
begin
  Result := fShortLabel;
end;

procedure TCustomVstParameterProperty.SetShortLabel(const Value: string);
begin
  fShortLabel := Value;
end;

{ TCustomVstParameterProperties }

constructor TCustomVstParameterProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCustomVstParameterProperty);
  FVSTModule := TVSTModuleWithPrograms(AOwner);
end;

destructor TCustomVstParameterProperties.Destroy;
begin
  while Count>0 do Delete(0);
  inherited;
end;

procedure TCustomVstParameterProperties.WriteVSTXML;
{$IFNDEF FPC}
var s : string;
    b : PChar;
{$ENDIF}
begin
  {$IFNDEF FPC}
  GetMem(b,255); GetModuleFileName(Application.Handle,b,255); FreeMem(b);
  s := b; WriteVSTXML(Copy(s,1,Pos('.dll',s)-1)+'.VSTXML');
  {$ENDIF}
end;

procedure TCustomVstParameterProperties.WriteVSTXML(FileName: TFileName);
var i : Integer;
begin
  with TStringlist.Create do
  try
    SaveToFile(FileName);
    Add('<!-- =========================================================== -->');
    Add('<!-- XML definition of VST parameters ========================== -->');
    Add('<!-- Draft 0.1================================================== -->');
    Add('<!-- Date: '+DateToStr(Now)+'=========================================== -->');
    Add('<!-- =========================================================== -->');
    Add('<VSTPluginProperties>');
    Add('');
    Add(#9+'<VSTParametersStructure>');
    Add(#9+#9+'<!--  Create Global Params================================== -->');
    for i := 0 to Count-1 do
    begin
      Add(#9+#9+'<Param name="'+Items[i].FDisplayName+'"'+#9+
                 'shortName="'+Items[i].fShortLabel+'"'+#9+
                 'id="'+IntToStr(i)+'"/>');
    end;
    Add(#9+'</VSTParametersStructure>');
    Add('</VSTPluginProperties>');
  finally
    Free;
  end;
end;

function TCustomVstParameterProperties.Add: TCustomVstParameterProperty;
begin
  Result := TCustomVstParameterProperty(inherited Add);
end;

function TCustomVstParameterProperties.GetItem(Index: Integer): TCustomVstParameterProperty;
begin
  Result := TCustomVstParameterProperty(inherited GetItem(Index));
end;

function TCustomVstParameterProperties.Insert(Index: Integer): TCustomVstParameterProperty;
begin
  Result := TCustomVstParameterProperty(inherited Insert(Index));
end;

procedure TCustomVstParameterProperties.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TCustomVstParameterProperties.SetItem(Index: Integer; const Value: TCustomVstParameterProperty);
begin
  inherited SetItem(Index, Value);
end;

end.
