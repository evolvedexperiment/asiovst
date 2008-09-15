unit WADSPVST;

interface

{$I ASIOVST.INC}

uses
  {$IFDEF FPC} LCLIntf, LResources, Windows, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Forms, SysUtils, Registry, Menus, ExtCtrls, Graphics, Dialogs,
  StdCtrls, Controls, DAV_Common, DAV_VstHost;

type
  TSmallIntArray = array [0..20000] of Smallint;
  PSmallIntArray = ^TSmallIntArray;
  TShortIntArray = array [0..20000] of ShortInt;
  PShortIntArray = ^TShortIntArray;
  T3Bytes = array [0..2] of Byte;
  P3Bytes = ^T3Bytes;
  T3ByteArray = array [0..20000] of T3Bytes;
  P3ByteArray = ^T3ByteArray;
  PWinampDSPModule = ^TWinampDSPModule;
  PWinAmpDSPHeader = ^TWinAmpDSPheader;
  PFmWinAmpVST = ^TFmWinAmpVST;

  TWAGetHeader = function : PWinAmpDSPHeader; cdecl;
  TWAGetModule = function(Which : Integer ) : PWinAmpDSPModule; cdecl;
  TWAConfig = procedure(This_Mod : PWinAmpDSPModule ); cdecl;
  TWAInit = function(This_Mod : PWinAmpDSPModule ) : Integer; cdecl;
  TWAQuit = procedure(This_Mod : PWinAmpDSPModule ); cdecl;
  TWAModifySamples = function(This_Mod : PWinAmpDSPModule; Samples : Pointer; NumSamples, BitPerSample, nCh, sRate : Integer) : Integer; cdecl;

  TWinampDSPModule = record
                      Description   : Pchar;
                      HwndParent    : THandle;
                      hDLLinstance  : THandle;
                      Config        : TWAConfig;
                      Init          : TWAInit;
                      ModifySamples : TWAModifySamples;
                      Quit          : TWAQuit;
                      UserData      : PFmWinAmpVST;
                     end;

  TWinAmpDSPHeader = record
                      Version      : Integer;
                      Description  : PChar;
                      GetModule    : TWAGetModule;
                     end;

  TFmWinAmpVST = class(TForm)
    CBPreset: TComboBox;
    EdVSTName: TEdit;
    LbPlugin: TLabel;
    LbProgram: TLabel;
    MILoadDLL: TMenuItem;
    MIRecent: TMenuItem;
    MIReset: TMenuItem;
    MIResetDll: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Panel1: TPanel;
    PanelControl: TPanel;
    PnGUI: TPanel;
    PUVSTPlugin: TPopupMenu;
    Timer: TTimer;
    VstHost: TVstHost;
    procedure FormCreate(Sender: TObject);
    procedure MILoadDLLClick(Sender: TObject);
    procedure EdVSTNameClick(Sender: TObject);
    procedure CBPresetChange(Sender: TObject);
    procedure PUVSTPluginPopup(Sender: TObject);
    procedure MILoadDLLDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure MIResetClick(Sender: TObject);
    procedure MIResetDllClick(Sender: TObject);
    procedure VstHostVstPlugIns0AudioMasterUpdateDisplay(Sender: TObject);
    procedure CallEditIdle(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbPluginClick(Sender: TObject);
  private
    fColDetected   : Boolean;
    fColorBack     : TColor;
    fColorEdit     : TColor;
    fColorBorder   : TColor;
    fBypass        : Boolean;
    fRegistryEntry : string;
    fTmpData       : TAVDArrayOfSingleDynArray;
    fNrChannels    : Integer;
    fSampleRate    : Integer;
    fNumSamples    : Integer;
    procedure SetScheme;
    procedure LoadVSTDLL(VSTDLL: TFileName);
    procedure LoadRecent(Sender: TObject);
    procedure ResizeChannelArray(NewChannelNumber: Integer);
  protected
    procedure ClosePlugin;
  public
  end;

function winampDSPGetHeader2 : PWinAmpDSPHeader; cdecl; export;
function GetModule(Which : Integer) : PWinAmpDSPModule; cdecl;
procedure Config(This_Mod : PWinAmpDSPModule); cdecl;
function Init(This_Mod : PWinAmpDSPModule) : Integer; cdecl;
function ModifySamples(This_Mod : PWinAmpDSPModule; Samples : Pointer; NumSamples, BitPerSample, nCh, sRate : Integer) : Integer; cdecl;
procedure Quit(This_Mod : PWinAmpDSPModule); cdecl;

implementation

uses
  Math, SyncObjs, DAV_VSTEffect;

var
  WADSPHeader      : TWinAmpDSPheader =
                     (Version : $20;
                      Description : 'VST Host DSP v1.0 for WinAmp';
                      GetModule : GetModule);

  WADSPModule      : TWinAmpDSPModule =
                      (Description : 'VST Host DSP v1.0 for WinAmp';
                       HwndParent : 0;
                       hDLLinstance : 0;
                       Config : Config;
                       Init : Init;
                       ModifySamples : ModifySamples;
                       Quit : Quit;
                       UserData : nil);
  FmWinAmpVST      : TFmWinAmpVST = nil;
  CriticalSection  : TCriticalSection;

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

function winampDSPGetHeader2 : PWinAmpDSPHeader; cdecl;
begin
 try
  Result := @WADSPHeader;
 except
  Result := nil;
 end;
end;

function GetModule(Which : Integer) : PWinAmpDSPModule;
begin
 case Which of
   0 : Result := @WADSPModule;
 else
  Result := nil;
 end;
end;

function Init(This_Mod : PWinAmpDSPModule) : Integer;
//var tab: array[0..2] of Integer;
begin
 CriticalSection := TCriticalSection.Create;
 CriticalSection.Enter;
 if not Assigned(FmWinAmpVST) then
  begin
   FmWinAmpVST := TFmWinAmpVST.Create(Application);
   This_Mod^.UserData := @FmWinAmpVST;
  end
 else
  begin
   This_Mod^.UserData^.Timer.OnTimer := This_Mod^.UserData^.CallEditIdle;
   WADSPModule.UserData^.Visible := True;
  end;
 CriticalSection.Leave;
 Result := 0;
end;

procedure Config(This_Mod : PWinAmpDSPModule);
begin
 if Assigned(This_Mod^.UserData^)
  then This_Mod^.UserData^.Show;
end;

function ModifySamples(This_Mod : PWinAmpDSPModule; Samples : Pointer;
                       NumSamples, BitPerSample, nCh, sRate : Integer) : Integer;
var
  i, j, ch : Integer;
  Temp     : Integer;
const
  DivFak16 : Single  = 1 / $8000;   MulFak16 : Single = $7FFF;
  DivFak24 : Single  = 1 / $800000; MulFak24 : Single = $7FFFFF;
  MulFak16D : Single = $7FFE;
begin
 CriticalSection.Enter;
 try
  if Assigned(This_Mod^.UserData^) then
   with This_Mod^.UserData^ do if Assigned(VstHost) then
    if VstHost[0].Active and not fBypass then
     begin
(*
      if Length(fPDCBuffer)<nCh then SetLength(fPDCBuffer,nCh);
      for i := 0 to nCh-1 do
       if Length(fPDCBuffer[i])<>VstHost[0].InitialDelay
        then SetLength(fPDCBuffer[i],VstHost[0].InitialDelay);
*)

      if sRate <> fSampleRate then
       begin
        fSampleRate := sRate;
        VstHost[0].SetSampleRate(sRate);
       end;
      ch := max(VstHost[0].numInputs, VstHost[0].numOutputs);
      ch := max(nCh, ch);

      if ch <> fNrChannels
       then ResizeChannelArray(ch);

      case BitPerSample of
       16: begin
            for i := 0 to ch - 1 do
             begin
              SetLength(fTmpData[i], NumSamples);
              if i >= nCh then Break;
              for j := 0 to NumSamples - 1
               do fTmpData[i,j] := PSmallIntArray(Samples)^[j * nCh + i] * DivFak16;
             end;
            VstHost[0].ProcessReplacing(@fTmpData[0], @fTmpData[0], NumSamples);
            for i := 0 to ch - 1 do
             begin
              if i >= nCh then Break;
              for j := 0 to NumSamples - 1
               do PSmallIntArray(Samples)^[j * nCh + i] := Round(f_Limit(fTmpData[i, j]) * MulFak16)
             end;
           end;
       24: begin
            for i := 0 to ch - 1 do
             begin
              SetLength(fTmpData[i], NumSamples);
              if i < nCh then
               for j := 0 to NumSamples - 1
                do fTmpData[i, j] := ((ShortInt(P3ByteArray(Samples)^[j*nCh+i][2]) shl 16) +
                                      (P3ByteArray(Samples)^[j*nCh+i][1] shl 8)  +
                                      (P3ByteArray(Samples)^[j*nCh+i][0] )) * DivFak24;
             end;
            VstHost[0].ProcessReplacing(@fTmpData[0], @fTmpData[0], NumSamples);
            for i := 0 to ch-1 do if i < nCh then
             for j := 0 to NumSamples - 1 do
              begin
               Temp := Round(f_Limit(fTmpData[i, j]) * MulFak24);
               P3ByteArray(Samples)^[j * nCh + i][2] := (Temp shr 16) and $FF;
               P3ByteArray(Samples)^[j * nCh + i][1] := (Temp shr 8 ) and $FF;
               P3ByteArray(Samples)^[j * nCh + i][0] := (Temp       ) and $FF;
              end;
           end;
      end;
     end;
  Result := NumSamples;
 finally
  CriticalSection.Leave;
 end;
end;

procedure Quit(This_Mod : PWinAmpDSPModule);
begin
 CriticalSection.Enter;
 with This_Mod^ do
  try
   UserData^.fBypass := True; Sleep(5);
   UserData^.ClosePlugin;
   try FreeAndNil(UserData^); finally FmWinAmpVST := nil; end;
  finally
   CriticalSection.Leave;
   FreeAndNil(CriticalSection);
  end;
end;

{ TFmWinAmpVST }

procedure TFmWinAmpVST.FormCreate(Sender: TObject);
var
  s : string;
  b : PChar;
begin
 fSampleRate := 44100; fNumSamples := 0; fNrChannels := 0;
 GetMem(b, 255); GetModuleFileName(WADSPModule.hDLLinstance, b, 255);
 s := b; fRegistryEntry := ExtractFileName(s); FreeMem(b);
 if fRegistryEntry = 'dsp_vst.dll'
  then fRegistryEntry := 'Software\WinAmp\VST Host DSP Plugin'
  else fRegistryEntry := 'Software\WinAmp\' + Copy(fRegistryEntry, 1, Pos('.dll', fRegistryEntry) - 1);
 s := ExpandUNCFileName(Copy(s, 1, Pos('.dll', s) - 1) + '.fxp');
 fBypass := False;
 with TRegistry.Create do
  try
   if OpenKeyReadOnly(fRegistryEntry) then
    begin
     if ValueExists('Visible') then if ReadBool('Visible') then Show;
     if ValueExists('Left') then Left := ReadInteger('Left');
     if ValueExists('Top') then Top := ReadInteger('Top');
     if ValueExists('Timer') then Timer.Interval := ReadInteger('Timer');
     if ValueExists('Last Plugin') then
      begin
       LoadVSTDLL(ReadString('Last Plugin'));
       if FileExists(s) then VstHost[0].LoadPreset(s);
      end;
    end;
  finally
   CloseKey;
   Free;
  end;
end;

procedure TFmWinAmpVST.FormShow(Sender: TObject);
begin
 //
end;

procedure TFmWinAmpVST.ClosePlugin;
var
  s : string;
  b : PChar;
begin
 with TRegistry.Create do
  try
   if OpenKey(fRegistryEntry,True) then
    begin
     WriteBool('Visible', Visible);
     WriteInteger('Left', Left);
     WriteInteger('Top', Top);
    end;
  finally
   CloseKey;
   Free;
  end;
 Timer.Enabled := False;
 Timer.OnTimer := nil; Sleep(5);
 try
  GetMem(b,255); GetModuleFileName(WADSPModule.hDLLinstance, b, 255);
  s := b;
  s := Copy(s, 1, Pos('.dll', s) - 1) + '.fxp';
  FreeMem(b);
  VstHost[0].SavePreset(s);
  VstHost[0].CloseEdit;
  Sleep(5);
  VstHost[0].Active := False;
  Sleep(5);
  VstHost[0].Unload;
  Sleep(5);
 except
 end;
end;

procedure TFmWinAmpVST.ResizeChannelArray(NewChannelNumber: Integer);
begin
 SetLength(fTmpData, NewChannelNumber);
 fNrChannels := NewChannelNumber;
end;

procedure TFmWinAmpVST.SetScheme;
var
  col1, col2, col3, col4 : TColor;
  c, c1, c2, c3, c4      : Integer;
  r1, r2, r3, r4         : Integer;
  g1, g2, g3, g4         : Integer;
  b1, b2, b3, b4         : Integer;
var
  DC: HDC;
begin
 DC := GetDC(0);
 try
  Col1 := GetPixel(dc, Left + PnGUI.Left + 17, Top + PanelControl.Height + 32);
  Col2 := GetPixel(dc, Left + PnGUI.Left + 40, Top + PanelControl.Height + 48);
  Col3 := GetPixel(dc, Left + PnGUI.Left + 16, Top + PanelControl.Height + 62);
  Col4 := GetPixel(dc, Left + PnGUI.Left + 38, Top + PanelControl.Height + 68);
  r1 := GetRValue(col1); r2 := GetRValue(col2); r3 := GetRValue(col3); r4 := GetRValue(col4);
  g1 := GetGValue(col1); g2 := GetGValue(col2); g3 := GetGValue(col3); g4 := GetGValue(col4);
  b1 := GetBValue(col1); b2 := GetBValue(col2); b3 := GetBValue(col3); b4 := GetBValue(col4);
  c1 := abs(r1 - r2) + abs(g1 - g2) + abs(b1 - b2) + abs(r1 - r3) + abs(g1 - g3) + abs(b1 - b3) + abs(r1 - r4) + abs(g1 - g4) + abs(b1 - b4);
  c2 := abs(r2 - r1) + abs(g2 - g1) + abs(b2 - b1) + abs(r2 - r3) + abs(g2 - g3) + abs(b2 - b3) + abs(r2 - r4) + abs(g2 - g4) + abs(b2 - b4);
  c3 := abs(r3 - r1) + abs(g3 - g1) + abs(b3 - b1) + abs(r3 - r2) + abs(g3 - g2) + abs(b3 - b2) + abs(r3 - r4) + abs(g3 - g4) + abs(b3 - b4);
  c4 := abs(r4 - r1) + abs(g4 - g1) + abs(b4 - b1) + abs(r4 - r2) + abs(g4 - g2) + abs(b4 - b2) + abs(r4 - r3) + abs(g4 - g3) + abs(b4 - b3);
  if (c1 <= c2) and (c1 <= c3) and (c1 <= c4) then c := col1 else
  if (c2 <= c1) and (c2 <= c3) and (c2 <= c4) then c := col2 else
  if (c3 <= c1) and (c4 <= c2) and (c4 <= c4) then c := col3 else c := col4;
  c1 := GetRValue(c) + GetGValue(c) + GetBValue(c);
  if c1 < 3*$30 then c := $303030;
   while c1 > 3*$80 do
    begin
     r1 := GetRValue(c); if r1 > 0 then dec(r1);
     g1 := GetGValue(c); if g1 > 0 then dec(g1);
     b1 := GetBValue(c); if b1 > 0 then dec(b1);
     c := rgb(r1, g1, b1);//$c0c0c0;
     c1 := GetRValue(c) + GetGValue(c) + GetBValue(c);
   end;

  fColorEdit := rgb(GetRValue(c) div 2, GetGValue(c) div 2, GetBValue(c) div 2);
//  Font.Color :=
  fColorBack        := c;
  fColorBorder      := fColorBack + $00101010;
  EdVSTName.Color   := fColorEdit;
  CBPreset.Color    := fColorEdit;
  FmWinAmpVST.Color := fColorBack;
 finally
  fColDetected := True;
  ReleaseDC(0, DC);
 end;
end;

procedure TFmWinAmpVST.LoadVSTDLL(VSTDLL : TFileName);
var
  rct  : ERect;
  i    : Integer;
  s    : string;
  temp : pchar;
begin
 Timer.Enabled := False;
 with VstHost[0] do
  try
   CriticalSection.Enter;
   try CloseEdit; except end;
   Active := False;
   sleep(10);
   try Unload; except end;
   sleep(10);
   DLLFileName := VSTDLL;
//   fRealDelay := 0;
   Active := True;
   try
    ShowEdit(TForm(PnGUI));
    Idle;
    EditIdle;
   except
     raise
   end;
   if GetVendorString = '' then
    if GetEffectName = ''
     then Caption := ExtractFileName(VSTDLL)
     else Caption := GetEffectName
    else Caption := GetVendorString + ' - '  + GetEffectName;
  finally
   CriticalSection.Leave;
  end;

 CBPreset.Clear; if not VSTHost[0].Active then exit;
 getmem(temp, 25);
 try
  for i := 0 to VSTHost[0].numPrograms - 1 do
   begin
    VSTHost[0].GetProgramNameIndexed(-1, i, temp);
    s := IntToStr(i);
    if i < 10 then s := '00' + s else
    if i < 100 then s := '0' + s;
    s := s+' - '+StrPas(temp);
    CBPreset.Items.Add(s)
   end;
  CBPreset.ItemIndex := 0;
 finally
  Freemem(temp);
 end;

 try
  s := VSTHost[0].GetProgramName;
  s := IntToStr(CBPreset.ItemIndex) + ' - ' + s;
  if CBPreset.ItemIndex < 10 then s := '00' + s else
  if CBPreset.ItemIndex < 100 then s := '0' + s;
  if (CBPreset.Text <> s) then
   begin
    CBPreset.Text := s;
    for i := 0 to VSTHost[0].numPrograms - 1 do
     begin
      VSTHost[0].ProgramNr := i;
      s := VSTHost[0].GetProgramName;
      s := IntToStr(i) + ' - ' + s;
      if i < 10 then s := '00' + s else
      if i < 100 then s := '0' + s;
      CBPreset.Items[i] := s;
     end;
    VSTHost[0].ProgramNr := 0;
    CBPreset.ItemIndex := 0;
   end;
 except
 end;

 try
  if effFlagsHasEditor in VSTHost[0].EffectOptions then
   begin
    rct := VSTHost[0].EditGetRect;
    if (rct.right <> 0) and (rct.Bottom <> 0) then
     begin
      ClientWidth := max(rct.right - rct.left, 60);
      ClientHeight := rct.bottom - rct.Top + PanelControl.Height;
     end;
    if ClientWidth > 300 then
     if ClientWidth < 400
      then CBPreset.Width := ClientWidth - 200
      else CBPreset.Width := 200
     else CBPreset.Width := 99;
    fColDetected := False; Timer.Enabled := True;
   end
  else
   begin
    ClientHeight := 112;
    ClientWidth := 300;
    Timer.Enabled := False;
   end;
 except
 end;

 if VSTHost[0].GetEffectName = '' then
  begin
   EdVSTName.text := ExtractFileName(VSTDLL);
   EdVSTName.Text := Copy(EdVSTName.Text, 0, Pos('.dll', EdVSTName.Text) - 1);
  end
 else EdVSTName.text := VSTHost[0].GetEffectName;
end;

procedure TFmWinAmpVST.MILoadDLLClick(Sender: TObject);
var
  i : Integer;
begin
 with TOpenDialog.Create(Self) do
  try
   InitialDir := VSTHost.PlugInDir;
   DefaultExt := '.dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Load VST Plugin DLL';
   if Execute then
    begin
     LoadVSTDLL(FileName);
     with TRegistry.Create do
      try
       if OpenKey(fRegistryEntry, True) then
        begin
         WriteString('Last Plugin', FileName);
         i := 1;
         while i < 10 do
          begin
           if not ValueExists('Recent ' + IntToStr(i)) then break;
           if ReadString('Recent ' + IntToStr(i)) = Filename then break;
           inc(i);
          end;
         if i <= 10 then
          begin
           while i > 1 do
            begin
             WriteString('Recent ' + IntToStr(i), ReadString('Recent ' + IntToStr(i - 1)));
             dec(i);
            end;
           WriteString('Recent 1', FileName);
          end;
        end;
      finally
       CloseKey; Free;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmWinAmpVST.EdVSTNameClick(Sender: TObject);
begin
 PUVSTPlugin.Popup(Mouse.CursorPos.X,
                   Mouse.CursorPos.Y);
end;

procedure TFmWinAmpVST.CBPresetChange(Sender: TObject);
begin
 with VSTHost[0] do
  begin
   ProgramNr := CBPreset.ItemIndex;
   EditIdle; Idle;
  end;
end;

procedure TFmWinAmpVST.PUVSTPluginPopup(Sender: TObject);
var
  i  : Integer;
  MI : TMenuItem;
begin
 while MIRecent.Count > 2 do MIRecent.Delete(0);
 with TRegistry.Create do
  try
   if OpenKey(fRegistryEntry, True) then
    begin
     i := 1;
     while ValueExists('Recent ' + IntToStr(i)) and (i < 10) do
      try
       MI := TMenuItem.Create(MIRecent);
       MI.Caption := ExtractFileName(ReadString('Recent ' + IntToStr(i)));
       MI.Tag := i; MI.OnClick := LoadRecent;
       MIRecent.Insert(i - 1, MI);
      finally
       inc(i);
      end;
    end;
  finally
   CloseKey;
   Free;
  end;
end;

procedure TFmWinAmpVST.LoadRecent(Sender: TObject);
var
  str : string;
  i   : Integer;
begin
 with TRegistry.Create do
  try
   if OpenKey(fRegistryEntry, True) then
     begin
      str := ReadString('Recent ' + IntToStr(TMenuItem(Sender).Tag));
      if FileExists(str) then
       try
        LoadVSTDLL(str);
       finally
        i := TMenuItem(Sender).Tag;
        while i > 1 do
         begin
          WriteString('Recent ' + IntToStr(i), ReadString('Recent ' + IntToStr(i - 1)));
          dec(i);
         end;
        WriteString('Recent 1', str);
       end;
      WriteString('Last Plugin', str);
     end;
  finally
   CloseKey;
   Free;
  end;
end;

procedure TFmWinAmpVST.MILoadDLLDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
// ACanvas.Brush.color := $00585858;
// ACanvas.FillRect(ARect);
// ACanvas.TextOut(ARect.Left + 2, ARect.top, (Sender As TMenuItem).Caption);
end;

procedure TFmWinAmpVST.MIResetClick(Sender: TObject);
var
  i : Integer;
begin
 with TRegistry.Create do
  try
   if OpenKey(fRegistryEntry, False) then
    for i := 0 to 9 do if ValueExists('Recent ' + IntToStr(i))
     then DeleteValue('Recent ' + IntToStr(i));
  finally
   CloseKey; Free;
  end;
end;

procedure TFmWinAmpVST.MIResetDllClick(Sender: TObject);
begin
 with VstHost[0] do
  try
   CloseEdit;
   Active := False;
   Unload;
   DLLFileName := '';
  except
  end;
 EdVSTName.Text := '';
 with TRegistry.Create do
  try
   if OpenKey(fRegistryEntry,false)
    then DeleteValue('Last Plugin');
  finally
   CloseKey; Free;
  end;
 ClientHeight := 29;
 ClientWidth := 148;
end;

procedure TFmWinAmpVST.VstHostVstPlugIns0AudioMasterUpdateDisplay(Sender: TObject);
begin
 CBPreset.ItemIndex := VstHost[0].ProgramNr;
end;

procedure TFmWinAmpVST.CallEditIdle(Sender: TObject);
var
  rct  : ERect;
begin
 try
  VstHost[0].Idle;
  VstHost[0].EditIdle;
  if effFlagsHasEditor in VSTHost[0].EffectOptions then
   try
    rct := VSTHost[0].EditGetRect;
    if (rct.right - rct.Left <> 0) and
       (rct.bottom - rct.Top <> 0) then
     begin
      if ClientWidth <> rct.right - rct.Left
       then ClientWidth := max(rct.right - rct.left, 60);
      if ClientHeight <> rct.bottom - rct.Top + PanelControl.Height
       then ClientHeight := rct.bottom - rct.Top + PanelControl.Height;
     end;
   except
   end;
  if not fColDetected
   then SetScheme;
 except
 end;
end;

procedure TFmWinAmpVST.LbPluginClick(Sender: TObject);
begin
 fBypass := not fBypass;
 if fBypass
  then PanelControl.Font.Color := $00C8C8C8
  else PanelControl.Font.Color := clWhite;
 EdVSTName.Font.Color := PanelControl.Font.Color;
 CBPreset.Font.Color := PanelControl.Font.Color;
end;

initialization
  {$IFDEF FPC}
  {$i WADSPVST.lrs}
  {$ENDIF}

end.
