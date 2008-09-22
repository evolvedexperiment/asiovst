unit PMmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  Menus, Spin, ExtCtrls, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiGroup, DAV_ChunkClasses, DAV_ChunkPluginGUI;

type
  TFmPluginMerger = class(TForm)
    CBAntialiasedFont: TCheckBox;
    DialPreview: TGuiDial;
    EdKnob: TEdit;
    GBPreview: TGuiGroup;
    LbBackgroundColor: TLabel;
    LbKnob: TLabel;
    LbKnobsPerRow: TLabel;
    LbMergedVSTPlugins: TLabel;
    LBPlugins: TListBox;
    LbTest: TGuiLabel;
    MainMenu1: TMainMenu;
    MIAdd: TMenuItem;
    MIClear: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIPlugin: TMenuItem;
    MISaveasVST: TMenuItem;
    SEKnobsPerRow: TSpinEdit;
    ShBackgroundColor: TShape;
    procedure MIExitClick(Sender: TObject);
    procedure MIAddClick(Sender: TObject);
    procedure MISaveasVSTClick(Sender: TObject);
    procedure MIClearClick(Sender: TObject);
    procedure EdKnobClick(Sender: TObject);
    procedure LBPluginsClick(Sender: TObject);
    procedure ShBackgroundColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CBAntialiasedFontClick(Sender: TObject);
    procedure EdKnobChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  FmPluginMerger: TFmPluginMerger;

implementation

uses
  Graphics, IniFiles, PNGImage, DAV_DLLResources;

{$R *.dfm}

procedure TFmPluginMerger.CBAntialiasedFontClick(Sender: TObject);
begin
 if CBAntialiasedFont.Checked
  then LbTest.AntiAlias := gaaLinear4x
  else LbTest.AntiAlias := gaaNone;
end;

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
     with TResourceStream.Create(HInstance, 'CustomWrapper', 'DLL') do
      try
       SaveToFile(FileName);
      finally
       Free;
      end;

     RM := TPEResourceModule.Create;
     with RM do
      try
       LoadFromFile(FileName);

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
         if CBAntialiasedFont.Checked
          then FontAntiAliasing := gaaLinear4x
          else FontAntiAliasing := gaaNone;
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
    end;
  finally
   Free;
  end;
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
