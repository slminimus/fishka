{-----------------------------------------------------------------------------
 Unit Name: CornDefs
 Author:    sl.minimus@gmail.com
 Date:      23-авг-2016
 Purpose:   Глобальные определения. Константы, типы, сообщения...
 History:
-----------------------------------------------------------------------------}
unit CornDefs;
interface

uses Windows, Classes, Messages, DB, Menus, Forms, Controls, Dialogs, Registry,
     MultiMon, ComCtrls, strUtils, SysUtils, UITypes, Math, usTools;

const
  REG_APP_NAME = 'Fish';
  REG_ROOT     = '\Software\'+ REG_APP_NAME +'\';
  regEntryKey: string = REG_ROOT;

type
  TGuidString = string[36];

  TConfirm      = (cfIns = 0, cfEdit = 1, cfDel = 2);
  TConfirms     = set of TConfirm;

  TExitAppConfirm = (
    eacConfirm      = 0, // спрашивать "Вы уверены?" при закрытии приложения
    eacModifiedOnly = 1, // проверять на "сохранить/отменить"
    ecForce         = 2  // закрывать, не спрашивая
  );

//  TxTreeView = TcxTreeView;
//  TxPageControl = TcxPageControl;
//  TxTabSheet    = TcxTabSheet;
  TxPageControl = TPageControl;
  TxTabSheet    = TTabSheet;

  TVwrModalResult  = (vmrCanceled, vmrInserted, vmrUpdated);
  TVwrModalMode    = (vmmNone, vmmInsert, vmmEdit);

  TWinControlHelper = class helper for TWinControl
    function CanFocusEx: boolean; // слямзено с TcxControl.CanFocusEx
  end;

  TPageControlHelper = class helper for TxPageControl
    function FirstPage: TxTabSheet;
    function LastPage: TxTabSheet;
  end;

//  TcxTreeViewHelper = class helper for TcxTreeView
//    function RootNode: TTreeNode; overload;
//    function RootNode(out Node: TTreeNode): boolean; overload;
//    procedure SelectRootNode;
//  end;

  TFormHelper = class helper for TForm
    procedure InsureVisible;  // MakeFullyVisible и его аналог, если форма MDIChild
    procedure SaveBounds(const Section: string);
    function  LoadBounds(const Section: string): boolean;
  end;

  TExRegIni = class(TRegistryIniFile)
  public
    function  ReadBinary(const Section, Ident: string; var Buffer; BufSize: Integer): Integer;
    procedure WriteBinary(const Section, Ident: string; var Buffer; BufSize: Integer);
  end;

  Cfg = class
  private class var
    fCfg : TExRegIni;
  private
    class constructor Create;
    class destructor Destroy;
  public
    class function  Active: boolean;
    class procedure EraseSection(const Section: string);
    class function  ReadBinary(const Section, Ident: string; var Buffer; BufSize: Integer): Integer;
    class function  ReadInteger(const Section, Ident: string; Default: Longint): Longint;
    class function  ReadString(const Section, Ident, Default: string): string;
    class function  ReadBool(const Section, Ident: string; Default: boolean): boolean;
    class procedure WriteInteger(const Section, Ident: string; Value: Longint);
    class procedure WriteString(const Section, Ident: string; Value: string);
    class procedure WriteBinary(const Section, Ident: string; var Buffer; BufSize: Integer);
    class procedure WriteBool(const Section, Ident: string; Value: Boolean);
    class procedure ReadSection(const Section: string; Strings: TStrings);
    class procedure ReadSectionValues(const Section: string; Strings: TStrings);
    class procedure DeleteKey(const Section, Ident: string);
    class function  ValueExists(const Section, Ident: string): Boolean;
  end;

  TVerRec = record
    Major: Integer;
    Minor: Integer;
    Release: Integer;
    Build: Integer;
    constructor Init(const aStr: string);
  end;

  TVerInfo = record
    FileVersion      : string;
    FileDescription  : string;
    InternalName     : string;
    OriginalFilename : string;
    CompanyName      : string;
    ProductVersion   : string;
    ProductName      : string;
    LegalCopyright   : string;
    LanguageInfo     : string;
    Comments         : string;
  public
    constructor Init(FileName: string);
    function VerRecFile   : TVerRec;
    function VerRecProduct: TVerRec;
  end;

  TAppInfo = record
    AppName: string;
    TmpPath: string;
    AppDataPath: string;
    MyDocsPath: string;
    ExePath: string;
  //---
    DbVersion: string;
    DbPath: string;
    DbName: string;
    UserID: string;
    Login: string;
    UserName: string;
    ExpDate: TDate;
    IsAdmin: boolean;
    IsDBA: boolean;
  //---
    function AppDataFolder: string;
    function MyDocsFolder: string;
  end;

  TBarCodeType = (btProduct, btDiscont);

var
  HelpChm: string = ''; // Имя файла помощи *.chm

  VerInfo: TVerInfo;
  AppInfo: TAppInfo;


procedure CheckCRC;
function  ShowHelpContext: boolean;
procedure ShowHelpCommon;

// Найти первый Parent ctl'а, который is TxTabSheet.
function ParentSheet(ctl: TControl): TxTabSheet; overload;
function ParentSheet(ctl: TControl; out Sheet: TxTabSheet): boolean; overload;

// Вернуть WC.Controls[0], если таковой имеется и он is NeedClass.
function ChildOf(WC: TWinControl; NeedClass: TControlClass): TControl; overload;
function ChildOf(WC: TWinControl; NeedClass: TControlClass; out ctl): boolean; overload;

//procedure vgStatesToValue(Sender: TcxDBEditorRow;
//                      const ACheckStates: TcxCheckStates; out AValue: Variant);
//
//procedure vgValueToStates(Sender: TcxDBEditorRow;
//                    const AValue: Variant; var ACheckStates: TcxCheckStates);

// у форм, которые позиционируются при помощи этих функций,
// DefaultMonitor должен быть dmDesktop
// RestrictOverlap: контрол, который, по возможности, не надо перекрывать,
// диалог пытаться расположить сначала под RestrictOverlap, при чём
// Rect.Left = RestrictOverlap.Left
procedure AlignDlgToWA(var Rect: TRect; RestrictOverlap: TControl = nil); overload;
procedure AlignDlgToWA(Dlg: TCustomForm; RestrictOverlap: TControl = nil); overload;

function MsgBox(const Mess: string; Style: word): integer; overload;
function MsgBox(const Title, Mess: string; Style: word): integer; overload;

procedure ShowError(const Mess: string); overload;
procedure ShowError(const Mess: string; Params: array of const); overload;

procedure RaiseError(const Mess: string); overload;
procedure RaiseError(const Mess: string; Params: array of const); overload;

function IsClientard(const BarCode: string): boolean;
function IsMyBarCode(const BarCode: string): boolean;

implementation
uses Variants;

function ParentSheet(ctl: TControl): TxTabSheet;
begin
  if not ParentSheet(ctl, result) then
    result:= nil;
end;

function ParentSheet(ctl: TControl; out Sheet: TxTabSheet): boolean;
var wc: TWinControl;
begin
  result:= false;
  if ctl = nil then exit;
  wc:= ctl.Parent;
  while assigned(wc) do begin
    if wc is TxTabSheet then begin
      Sheet:= TxTabSheet(wc);
      exit(true);
    end;
    wc:= wc.Parent;
  end;
end;

function ChildOf(WC: TWinControl; NeedClass: TControlClass): TControl;
begin
  if not ChildOf(WC, NeedClass, result) then
    result:= nil;
end;

function ChildOf(WC: TWinControl; NeedClass: TControlClass; out ctl): boolean;
begin
  result:= Assigned(WC) and (WC.ControlCount > 0);
  if not result then exit;
  if not(WC.Controls[0] is NeedClass) then exit(false);
  TControl(ctl):= WC.Controls[0];
end;

procedure CheckCRC;
const MSG = 'Исполняемый файл повреждён.'#13#10+
            {$IFDEF DEBUG}'Режим отладки.'
            {$ELSE}'Приложение будет завершено.'
            {$ENDIF};
begin
  if PE_CRC_Check(Application.ExeName) then exit;
  MessageDlg(MSG, mtError, [mbOK], 0);
{$IFNDEF DEBUG}
  Halt(17);
{$ENDIF}
end;

//procedure vgStatesToValue(Sender: TcxDBEditorRow;
//                      const ACheckStates: TcxCheckStates; out AValue: Variant);
//var v: integer;
//    i: integer;
//  msk: integer;
//  grp: TcxCheckGroupProperties;
//begin
//  grp:= TcxCheckGroupProperties(Sender.Properties.EditProperties);
//  v:= 0;
//  msk:= 0;
//  for i:= 0 to grp.Items.Count -1 do begin
//    msk:= msk or grp.Items[i].Tag;
//    v:= v or ifthen(ACheckStates[i] = cbsChecked, grp.Items[i].Tag, 0);
//  end;
//  AValue:= Sender.Properties.DataBinding.Field.AsInteger and not msk or v;
//end;
//
//procedure vgValueToStates(Sender: TcxDBEditorRow;
//                    const AValue: Variant; var ACheckStates: TcxCheckStates);
//var v: integer;
//    i: integer;
//  grp: TcxCheckGroupProperties;
//begin
//  grp:= TcxCheckGroupProperties(Sender.Properties.EditProperties);
//  if not VarIsNumeric(AValue) then
//    exit;
//  v:= AValue;
//  for i:= 0 to grp.Items.Count -1 do
//    ACheckStates[i]:= CxChecked[v and grp.Items[i].Tag  <> 0];
//end;


function MonWorkArea(hMonitor: HMONITOR): TRect;
var MonInfo: TMonitorInfo;
begin
  MonInfo.cbSize:= SizeOf(MonInfo);
  GetMonitorInfo(hMonitor, @MonInfo);
  Result:= MonInfo.rcWork;
end;

procedure AlignDlgToWA(var Rect: TRect; RestrictOverlap: TControl = nil);
var R,C: TRect;
  dx,dy: integer;
    Mon: HMonitor;
begin
  dx:= 0; dy:= 0;
  if RestrictOverlap <> nil then begin
    C:= RestrictOverlap.BoundsRect;
    C.TopLeft:= RestrictOverlap.Parent.ClientToScreen(C.TopLeft);
    C.BottomRight:= RestrictOverlap.Parent.ClientToScreen(C.BottomRight);
    OffsetRect(Rect, C.Left - Rect.Left, C.Bottom - Rect.Top);
  end;
  Mon:= MonitorFromRect(@Rect, MONITOR_DEFAULTTONEAREST);
  R:= MonWorkArea(Mon);
  if Rect.Left < R.Left then
    dx:= R.Left - Rect.Left
  else if Rect.Right > R.Right then
    dx:= R.Right - Rect.Right;
  if Rect.Top < R.Top then
    dy:= R.Top - Rect.Top
  else if Rect.Bottom > R.Bottom then
    dy:= R.Bottom - Rect.Bottom;
  OffsetRect(Rect, dx, dy);
  if RestrictOverlap = nil then exit;
  if not IntersectRect(R, C, Rect) then exit;
  OffsetRect(Rect, 0, C.Top - Rect.Bottom);
  AlignDlgToWA(Rect, nil);
end;

procedure AlignDlgToWA(Dlg: TCustomForm; RestrictOverlap: TControl = nil);
var R: TRect;
begin
  R:= Dlg.BoundsRect;
  AlignDlgToWA(R, RestrictOverlap);
  Dlg.BoundsRect:= R;
end;

procedure RaiseError(const Mess: string);
begin
  RaiseError(Mess, []);
end;

procedure RaiseError(const Mess: string; Params: array of const);
begin
  ShowError(Mess, Params);
  Abort;
end;

procedure ShowError(const Mess: string);
begin
  MsgBox(Mess, MB_OK or MB_ICONERROR);
end;

procedure ShowError(const Mess: string; Params: array of const);
begin
  MsgBox(Format(Mess, Params), MB_OK or MB_ICONERROR);
end;

function MsgBox(const Mess: string; Style: word): integer;
begin
  result:= MsgBox(Application.Title, Mess, Style);
end;

function MsgBox(const Title, Mess: string; Style: word): integer;
begin
  result:= MB_OK;
  if Length(Mess) = 0 then exit;
  result:= Application.MessageBox(PChar(Mess), PChar(Title), Style);
end;

//**********************************************************************
//**********************************************************************
//**********************************************************************

{ TPageControlHelper }

function TPageControlHelper.FirstPage: TxTabSheet;
begin
  result:= nil;
  if PageCount > 0 then
    result:= Pages[0];
end;

function TPageControlHelper.LastPage: TxTabSheet;
begin
  result:= nil;
  if PageCount > 0 then
    result:= Pages[PageCount -1];
end;

function GetCurrentHelpKeyword(out Keyword: string): boolean;
var ctl: TControl;
begin
  result:= true;
  ctl:= Screen.ActiveControl;
  while assigned(ctl) do begin
    Keyword:= ctl.HelpKeyword;
    if Keyword <> '' then exit;
    ctl:= ctl.Parent;
  end;
  result:= false;
end;

function ShowHelpContext: boolean;
var hha: HH_AKLINK;
    Keyword: string;
begin
  result:= GetCurrentHelpKeyword(Keyword);
  if not result then exit;
  FillChar(hha,sizeof(hha),#0);
  hha.cbStruct:= sizeof(hha);
  hha.fReserved:= false;
  hha.pszKeywords:= PChar(Keyword);
  hha.fIndexOnFail:= true;
  HtmlHelp(0, PChar(HelpChm), HH_KEYWORD_LOOKUP, DWORD(@hha));
end;

procedure ShowHelpCommon;
begin
  HtmlHelp(0, PChar(HelpChm), HH_DISPLAY_TOC, 0);
end;

procedure CloseAllViewers;
var i: integer;
    n: integer;
    f: TCustomForm;
begin
  for i:= Screen.FormCount -1 downto 0 do begin
    f:= Screen.Forms[i];
    if not (f.ClassNameIs('TExpl') and (f.Parent = nil)) then continue;
    if not f.CloseQuery then exit;
    n:= Screen.FormCount;
    f.Close;
    while Screen.FormCount >= n do begin
      Application.ProcessMessages;
      Sleep(25);
    end;
  end;
end;

{ TcxTreeViewHelper

function TcxTreeViewHelper.RootNode: TTreeNode;
begin
  result:= Items.GetFirstNode;
end;

function TcxTreeViewHelper.RootNode(out Node: TTreeNode): boolean;
var N: TTreeNode;
begin
  if Self = nil then Exit(false);
  N:= Items.GetFirstNode;
  result:= N <> nil;
  if result then
    Node:= N;
end;

procedure TcxTreeViewHelper.SelectRootNode;
var Node: TTreeNode;
begin
  if RootNode(Node) then
    Selected:= Node;
end;

{ TWinControlHelper }

function TWinControlHelper.CanFocusEx: boolean;
var AParentForm: TCustomForm;
begin
  AParentForm:= GetParentForm(Self);
  Result:= CanFocus and ((AParentForm = nil) or
    AParentForm.CanFocus and AParentForm.Enabled and AParentForm.Visible);
end;

{ TFormHelper }

procedure TFormHelper.InsureVisible;
  //---
  procedure _Move;
  var R: TRect;
    ALeft: Integer;
    ATop: Integer;
    i: integer;
  begin
    R:= Application.MainForm.ClientRect;
    for i:= 0 to Application.MainForm.ControlCount -1 do begin
      if Application.MainForm.Controls[i].Align <> alClient then Continue;
      R:= Application.MainForm.Controls[i].ClientRect;
      break;
    end;
    ALeft:= Left;
    ATop:= Top;
    if Left + Width > R.Right then
      ALeft:= R.Right - Width;
    if ALeft < R.Left then
      ALeft:= R.Left;
    if Top + Height > R.Bottom then
      ATop:= R.Bottom - Height;
    if ATop < R.Top then
      ATop:= R.Top;
    SetBounds(ALeft, ATop, Width, Height);
  end;
  //---
begin
  if FormStyle = fsMDIChild then
    _Move
  else
    MakeFullyVisible;
  Show;
end;

function TFormHelper.LoadBounds(const Section: string): boolean;
var FP : TWindowPlacement;
    i  : integer;
    b  : boolean;
begin
  result:= false;
  if not HandleAllocated or not Cfg.Active then exit;
  i:= Cfg.ReadBinary(Section, 'Placement', FP, SizeOf(FP));
  if i <> SizeOf(FP) then exit;
  result:= true;
  FP.length:= SizeOf(FP);
  b:= FP.showCmd = SW_MAXIMIZE;
  if (Self <> Application.MainForm) and (Application.MainForm <> nil) then
    FP.showCmd:= SW_HIDE;//SW_SHOWNA;//

  if FormStyle = fsMDIChild then
    SetBounds(FP.rcNormalPosition.Left,
              FP.rcNormalPosition.Top,
              FP.rcNormalPosition.Width,
              FP.rcNormalPosition.Height
    )
  else
   SetWindowPlacement(Handle, @FP);
  if b then
    PostMessage(Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0)
//    WindowState:= wsMaximized
  else WindowState:= wsNormal;
end;

procedure TFormHelper.SaveBounds(const Section: string);
var FP: TWindowPlacement;
begin
  if not HandleAllocated or not Cfg.Active then exit;
  FP.length:= SizeOf(FP);
  GetWindowPlacement(Handle, @FP);
  Cfg.WriteBinary(Section, 'Placement', FP, SizeOf(FP));
end;

{ TExRegIni }

type THRI = class(TRegIniFile) end;

function TExRegIni.ReadBinary(const Section, Ident: string; var Buffer; BufSize: Integer): Integer;
var Key, OldKey: HKEY;
begin
  with THRI(RegIniFile) do begin
    Result:= 0;
    Key:= GetKey(Section);
    if Key = 0 then exit;
    try
      OldKey:= CurrentKey;
      SetCurrentKey(Key);
      try
        if ValueExists(Ident) then
          Result:= ReadBinaryData(Ident, Buffer, BufSize);
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end;
  end;
end;

procedure TExRegIni.WriteBinary(const Section, Ident: string; var Buffer; BufSize: Integer);
var Key, OldKey: HKEY;
begin
  with THRI(RegIniFile) do begin
    CreateKey(Section);
    Key := GetKey(Section);
    if Key = 0 then exit;
    try
      OldKey:= CurrentKey;
      SetCurrentKey(Key);
      try
        WriteBinaryData(Ident, Buffer, BufSize);
      finally
        SetCurrentKey(OldKey);
      end;
    finally
      RegCloseKey(Key);
    end;
  end;
end;

{ Cfg }

class constructor Cfg.Create;
var CfgFileName: string;
begin
  CfgFileName:= regEntryKey + AppInfo.AppName;
  if not FindCmdLineSwitch('NoCfg') then
    fCfg:= TExRegIni.Create(CfgFileName);
end;

class destructor Cfg.Destroy;
begin
  FreeAndNil(fCfg);
end;

class function Cfg.Active: boolean;
begin
  result:= Assigned(fCfg);
end;

class procedure Cfg.DeleteKey(const Section, Ident: string);
begin
  if Assigned(fCfg) then
    fCfg.DeleteKey(Section, Ident);
end;

class procedure Cfg.EraseSection(const Section: string);
begin
  if Assigned(fCfg) then
    fCfg.EraseSection(Section);
end;

class function Cfg.ReadBinary(const Section, Ident: string; var Buffer;
                                             BufSize: Integer): Integer;
begin
  result:= 0;
  if Assigned(fCfg) then
    result:= fCfg.ReadBinary(Section, Ident, Buffer, BufSize);
end;

class function Cfg.ReadBool(const Section, Ident: string; Default: boolean): boolean;
begin
  result:= Default;
  if Assigned(fCfg) then
    result:= fCfg.ReadBool(Section, Ident, Default);
end;

class function Cfg.ReadInteger(const Section, Ident: string; Default: Longint): Longint;
begin
  result:= Default;
  if Assigned(fCfg) then
    result:= fCfg.ReadInteger(Section, Ident, Default);
end;

class procedure Cfg.ReadSection(const Section: string; Strings: TStrings);
begin
  if Assigned(fCfg) then
    fCfg.ReadSection(Section, Strings);
end;

class procedure Cfg.ReadSectionValues(const Section: string; Strings: TStrings);
begin
  if Assigned(fCfg) then
    fCfg.ReadSectionValues(Section, Strings);
end;

class function Cfg.ReadString(const Section, Ident, Default: string): string;
begin
  result:= Default;
  if Assigned(fCfg) then
    result:= fCfg.ReadString(Section, Ident, Default);
end;

class function Cfg.ValueExists(const Section, Ident: string): Boolean;
begin
  result:= false;
  if Assigned(fCfg) then
    result:= fCfg.ValueExists(Section, Ident);
end;

class procedure Cfg.WriteBinary(const Section, Ident: string; var Buffer;
                                                              BufSize: Integer);
begin
  if Assigned(fCfg) then
    fCfg.WriteBinary(Section, Ident, Buffer, BufSize);
end;

class procedure Cfg.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  if Assigned(fCfg) then
    fCfg.WriteBool(Section, Ident, Value);
end;

class procedure Cfg.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  if Assigned(fCfg) then
    fCfg.WriteInteger(Section, Ident, Value);
end;

class procedure Cfg.WriteString(const Section, Ident: string; Value: string);
begin
  if Assigned(fCfg) then
    fCfg.WriteString(Section, Ident, Value);
end;

{ TVerRec }

constructor TVerRec.Init(const aStr: string);
var a: TArray<string>;
begin
  a:= aStr.Split(['.']);
  Major  := ifthen(Length(a) > 0, StrToIntDef(a[0], 0), 0);
  Minor  := ifthen(Length(a) > 1, StrToIntDef(a[1], 0), 0);
  Release:= ifthen(Length(a) > 2, StrToIntDef(a[2], 0), 0);
  Build  := ifthen(Length(a) > 3, StrToIntDef(a[3], 0), 0);
end;

{ TVerInfo }

constructor TVerInfo.Init(FileName: string);
type
  tVarFileInfoTranslation = record
      Language :WORD;
      CodePage :WORD;
    end;
var
  VISize: Cardinal;
  VIBuff: Pointer;
  trans: Pointer;
  buffsize: Cardinal;
  str: PChar;
  //---
  function GetStringValue(const From: string): string;
  begin
    VerQueryValue(VIBuff,pchar('\StringFileInfo\'+LanguageInfo+'\'+From), Pointer(str),
                  buffsize);
    if buffsize > 0 then
      Result := str
    else
      Result := 'n/a';
  end;
  //---
begin
  if FileName = '' then
    FileName:= ParamStr(0);
  VIBuff:= nil;
  if not FileExists(Filename) then
    raise EFilerError.Create('TVerInfo:: File not found: '+ FileName);
  VISize := GetFileVersionInfoSize(pchar(Filename), buffsize);
  if VISize < 1 then
    raise EReadError.Create('TVerInfo:: Invalid version info record in file '+ FileName);
  VIBuff := AllocMem(VISize);
  try
    GetFileVersionInfo(pchar(FileName), cardinal(0), VISize, VIBuff);

    VerQueryValue(VIBuff, '\VarFileInfo\Translation', Trans, buffsize);
    if buffsize < 4 then
      EReadError.Create('Invalid language info in file '+ FileName);

    with  tVarFileInfoTranslation(Trans^) do
      LanguageInfo := IntToHex(MakeLong(CodePage,Language),8);

    CompanyName := GetStringValue('CompanyName');
    FileDescription := GetStringValue('FileDescription');
    FileVersion := GetStringValue('FileVersion');
    InternalName := GetStringValue('InternalName');
    LegalCopyright := GetStringValue('LegalCopyright');
    OriginalFilename := GetStringValue('OriginalFilename');
    ProductName := GetStringValue('ProductName');
    ProductVersion := GetStringValue('ProductVersion');
    Comments := GetStringValue('Comments');
  finally
    FreeMem(VIBuff,VISize);
  end;
End;

function TVerInfo.VerRecFile: TVerRec;
begin
  result.Init(FileVersion);
end;

function TVerInfo.VerRecProduct: TVerRec;
begin
  result.Init(ProductVersion);
end;

{ TAppInfo }

function TAppInfo.AppDataFolder: string;
begin
  result:= AppDataPath + REG_APP_NAME +'\'+ AppName +'\';
end;

function TAppInfo.MyDocsFolder: string;
begin
  result:= MyDocsPath + REG_APP_NAME +'\'+ AppName +'\';
end;

procedure InitGlobals;
const
  regShell   = 'Software\Microsoft\Windows\CurrentVersion\Explorer';
  ShellFldr  = 'Shell Folders';
  regAppData = 'AppData';
  regMyDocs  = 'Personal';
var
  buf: array[0..MAX_PATH] of Char;
  reg: TRegIniFile;
  s  : string;
begin
  HelpChm:= ChangeFileExt(ParamStr(0), '.chm');
  VerInfo.Init('');
  AppInfo.ExePath:= ExtractFilePath(ParamStr(0));
  AppInfo.AppName:= ChangeFileExt(ExtractFileName(ParamStr(0)),'');
//  ExitAppConfirm:= CheckExitConfirmMode;
  GetTempPath(MAX_PATH, buf);
  AppInfo.TmpPath:= IncludeTrailingPathDelimiter(buf);
  reg:= TRegIniFile.Create(regShell);
  try
    AppInfo.AppDataPath:= IncludeTrailingPathDelimiter(reg.ReadString(ShellFldr, regAppData, AppInfo.TmpPath));
    AppInfo.MyDocsPath:= IncludeTrailingPathDelimiter(reg.ReadString(ShellFldr, regMyDocs, AppInfo.TmpPath));
  finally
    reg.Free;
  end;
  s:= ExcludeTrailingPathDelimiter(AppInfo.AppDataFolder);
  if not DirectoryExists(s) then
    ForceDirectories(s);
  s:= ExcludeTrailingPathDelimiter(AppInfo.MyDocsFolder);
  if not DirectoryExists(s) then
    ForceDirectories(s);
end;

function IsClientard(const BarCode: string): boolean;
begin
  result:= (Length(BarCode) = 13) and BarCode.StartsWith('23');
end;

function IsMyBarCode(const BarCode: string): boolean;
begin
  result:= (Length(BarCode) = 13) and BarCode.StartsWith('2');
end;

initialization
  Randomize;
  InitGlobals;

end.
