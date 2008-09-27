unit PMmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  Menus, Spin, ExtCtrls, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiGroup, DAV_ChunkClasses, DAV_ChunkPluginGUI, ComCtrls;

type
  TFmPluginMerger = class(TForm)
    MainMenu1: TMainMenu;
    MIAdd: TMenuItem;
    MIClear: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIPlugin: TMenuItem;
    MISaveasVST: TMenuItem;
    PageControl1: TPageControl;
    TSMergedPlugins: TTabSheet;
    TabSheet1: TTabSheet;
    LbKnob: TLabel;
    LbBackgroundColor: TLabel;
    ShBackgroundColor: TShape;
    LbKnobsPerRow: TLabel;
    EdKnob: TEdit;
    SEKnobsPerRow: TSpinEdit;
    GBPreview: TGuiGroup;
    DialPreview: TGuiDial;
    LbTest: TGuiLabel;
    LBFont: TLabel;
    ShFontColor: TShape;
    LbFontSize: TLabel;
    SEFontSize: TSpinEdit;
    LbFontAA: TLabel;
    RBAAnone: TRadioButton;
    RBAA4: TRadioButton;
    LBPlugins: TListBox;
    RBAA2: TRadioButton;
    RBAA8: TRadioButton;
    procedure MIExitClick(Sender: TObject);
    procedure MIAddClick(Sender: TObject);
    procedure MISaveasVSTClick(Sender: TObject);
    procedure MIClearClick(Sender: TObject);
    procedure EdKnobClick(Sender: TObject);
    procedure LBPluginsClick(Sender: TObject);
    procedure ShBackgroundColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EdKnobChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBAAnoneClick(Sender: TObject);
    procedure RBAA2Click(Sender: TObject);
    procedure RBAA4Click(Sender: TObject);
    procedure RBAA8Click(Sender: TObject);
    procedure SEFontSizeChange(Sender: TObject);
  end;

var
  FmPluginMerger: TFmPluginMerger;

implementation

uses
  Graphics, IniFiles, PNGImage, DAV_DLLResources;

{$R *.dfm}

procedure TFmPluginMerger.EdKnobChange(Sender: TObject);
var
  i, j   : Integer;
  Aspect : Single;
begin
 if FileExists(EdKnob.Text) then
  begin
   with TPNGObject.Create do
    try
     i := 0;
     LoadFromFile(EdKnob.Text);
     DialPreview.DialBitmap.SetSize(0, 0);
     AssignTo(DialPreview.DialBitmap);
     with DialPreview.DialBitmap do
      if Width > Height then
       begin
        DialPreview.StitchKind := skHorizontal;
        j := Width div Height;
        while True do
         begin
          Aspect := Width / (j + i);
          if (Aspect >= 1) and (abs(Aspect - round(Aspect)) < 1E-24)
           then break;
          Aspect := Width / (j - i);
          if (Aspect > 0) and (abs(Aspect - round(Aspect)) < 1E-24)
           then break
           else inc(i);
         end;
       end
      else
       begin
        DialPreview.StitchKind := skVertical;
        j := Height div Width;
        while True do
         begin
          Aspect := Height / (j + i);
          if (Aspect >= 1) and (abs(Aspect - round(Aspect)) < 1E-24)
           then break;
          Aspect := Height / (j - i);
          if (Aspect > 0) and (abs(Aspect - round(Aspect)) < 1E-24)
           then break
           else inc(i);
         end;
       end;
     DialPreview.NumGlyphs := j + i;
    finally
     Free;
    end;
  end;
end;

procedure TFmPluginMerger.EdKnobClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'png';
   Filter := 'PNG Image (*.png)|*.png';
   Title := 'Choose a stitched knob png';
   Options := Options + [ofFileMustExist];
   if Execute then
    begin
     EdKnob.Text := FileName;
    end;
  finally
   Free;
  end;
end;

procedure TFmPluginMerger.FormCreate(Sender: TObject);
var
  SL : TStringList;
  i  : Integer;
begin
 with TIniFile.Create('PluginMerger.ini') do
  try
   EdKnob.Text := ReadString('Last State','Knob', EdKnob.Text);
   Left := ReadInteger('Last State','Left', Left);
   Top  := ReadInteger('Last State','Top', Top);
   SL := TStringList.Create;
   try
    ReadSectionValues('Merged Plugins', SL);
    for i := 0 to SL.Count - 1 do
     if FileExists(SL.ValueFromIndex[i])
      then LBPlugins.Items.Add(SL.ValueFromIndex[i]);
    MISaveasVST.Enabled := LBPlugins.Items.Count > 0;
   finally
    FreeAndNil(SL);
   end;
   EraseSection('Merged Plugins');
  finally
   Free;
  end;
end;

procedure TFmPluginMerger.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
 with TIniFile.Create('PluginMerger.ini') do
  try
   WriteString('Last State','Knob', EdKnob.Text);
   WriteInteger('Last State','Left', Left);
   WriteInteger('Last State','Top', Top);
   for i := 0 to LBPlugins.Count - 1
    do WriteString('Merged Plugins', 'VST' + IntToStr(i + 1), LBPlugins.Items[i]);
  finally
   Free;
  end;
end;

procedure TFmPluginMerger.LBPluginsClick(Sender: TObject);
begin
 if LBPlugins.Count = 0
  then MIAddClick(Sender);
end;

procedure TFmPluginMerger.MIAddClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Title := 'Choose a VST DLL';
   Options := Options + [ofFileMustExist];
   if Execute then
    begin
     LBPlugins.Items.Add(FileName);
     MISaveasVST.Enabled := True;
    end;
  finally
   Free;
  end;
end;

procedure TFmPluginMerger.MIClearClick(Sender: TObject);
begin
 LBPlugins.Clear;
 MISaveasVST.Enabled := False;
end;

procedure TFmPluginMerger.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmPluginMerger.MISaveasVSTClick(Sender: TObject);
var
  RS  : TResourceStream;
  RM  : TPEResourceModule;
  RD  : TResourceDetails;
  i   : Integer;
  MS  : TMemoryStream;
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Title := 'Save As VST DLL';
   if Execute then
    begin
     RS := TResourceStream.Create(HInstance, 'CustomWrapper', 'DLL');
     try
      RM := TPEResourceModule.Create;
      with RM do
       try
        LoadFromStream(RS);

        // store VST Plugins
        for i := 0 to LBPlugins.Count - 1 do
         begin
          with TMemoryStream.Create do
           try
            LoadFromFile(LBPlugins.Items[i]);
            RD := TResourceDetails.CreateResourceDetails(RM, 0, 'VST' + IntToStr(i + 1), 'DLL', Size, Memory);
           finally
            Free;
           end;
          AddResource(RD);
         end;

        // store knob image
        if FileExists(EdKnob.Text) then
         with TMemoryStream.Create do
          try
           LoadFromFile(EdKnob.Text);
           RD := TResourceDetails.CreateResourceDetails(RM, 0, 'KNOB', 'PNG', Size, Memory);
           AddResource(RD);
          finally
           Free;
          end;

        // store gui information
        with TDAVPluginGuiChunk.Create do
         try
          BackgroundColor := ShBackgroundColor.Brush.Color;
          KnobsPerRow     := SEKnobsPerRow.Value;
          FontSize        := SEFontSize.Value;
          FontColor       := ShFontColor.Brush.Color;
          if RBAAnone.Checked then FontAntiAliasing := gaaNone else
          if RBAA2.Checked then FontAntiAliasing := gaaLinear2x else
          if RBAA4.Checked then FontAntiAliasing := gaaLinear4x else
          if RBAA8.Checked then FontAntiAliasing := gaaLinear8x
           else FontAntiAliasing := gaaLinear16x;
          FontSize := LbTest.Font.Size;
          MS := TMemoryStream.Create;
          try
           SaveToStream(MS);
           MS.Position := 0;
           RD := TResourceDetails.CreateResourceDetails(RM, 0, 'PLUGINGUI', '10', MS.Size, MS.Memory);
           AddResource(RD);
          finally
           FreeAndNil(MS);
          end;
         finally
          Free;
         end;

        SortResources;
        SaveToFile(FileName);
       finally
        FreeAndNil(RM);
       end;
     finally
      FreeAndNil(RS);
     end;
    end;
  finally
   Free;
  end;
end;

procedure TFmPluginMerger.RBAA2Click(Sender: TObject);
begin
 LbTest.AntiAlias := gaaLinear2x;
end;

procedure TFmPluginMerger.RBAA4Click(Sender: TObject);
begin
 LbTest.AntiAlias := gaaLinear4x;
end;

procedure TFmPluginMerger.RBAA8Click(Sender: TObject);
begin
 LbTest.AntiAlias := gaaLinear8x;
end;

procedure TFmPluginMerger.RBAAnoneClick(Sender: TObject);
begin
 LbTest.AntiAlias := gaaNone;
end;

procedure TFmPluginMerger.SEFontSizeChange(Sender: TObject);
begin
 LbTest.Font.Size := SEFontSize.Value;
end;

procedure TFmPluginMerger.ShBackgroundColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 with TColorDialog.Create(Self) do
  if Execute then
   begin
    ShBackgroundColor.Brush.Color := Color;
    GBPreview.Color := Color;
   end;
end;

end.
