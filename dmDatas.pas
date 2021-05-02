unit dmDatas;

interface

uses
  SysUtils, Classes, Types, Uitypes, Windows, Messages, Forms, dLoginDlg,
  JvComponentBase, JvAppStorage, JvAppIniStorage, Vcl.AppEvnts, Variants,
  System.Actions, Vcl.ActnList, CornDefs, usTools, usIntfs, usClasses,
  Generics.Collections, uDBProvider, Vcl.ExtCtrls, Data.DB, MemDS;

const
  W_ROLE           = 'RWORKER';
  PTH_LAST_CONNECT = 'Common\LastConnect';
  PTH_LAST_USER    = 'Common\LastUser';

type
  TDmDb = class(TDataModule)
    JvAppStore: TJvAppIniFileStorage;
    AppEvents: TApplicationEvents;
    procedure DataModuleCreate(Sender: TObject);
    procedure AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fWasConnected: boolean;
    FLoginParams: TLoginParams;
    fUsers: TDictStr<string>;
    procedure DoConnect(Params: TLoginParams);
  public
    function  Connected: boolean;
    function  Reconnect: boolean;
    procedure Disconnect;
    procedure InitDB;
    function  DecodeUser(const ID: string): string;
    function  AltPassDlg: boolean;
  public
    property DbPath: string read FLoginParams.Alias;
    property RtPath: string read FLoginParams.RtPath;
    property User: string read FLoginParams.Login;
  end;

var
  DmDb: TDmDb;

implementation
{$R *.dfm}
{$R SPLASH.res}
uses _Splash;//, dgAltPass;

{ TDmDb }

procedure TDmDb.DataModuleCreate(Sender: TObject);
begin
  fUsers:= TDictStr<string>.Create;
  JvAppStore.FileName:= ChangeFileExt(ExtractFileName(ParamStr(0)), '.sav');
  FLoginParams.Alias:= JvAppStore.ReadString(PTH_LAST_CONNECT);
  FLoginParams.Login:= JvAppStore.ReadString(PTH_LAST_USER);
  if not Reconnect then
    Halt(77);
end;

procedure TDmDb.DataModuleDestroy(Sender: TObject);
begin
  fUsers.Free;
end;

function TDmDb.DecodeUser(const ID: string): string;
begin
  if not fUsers.TryGetValue(ID, result) then
    result:= ID;
end;

procedure TDmDb.AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);
begin
  if Application.MainForm = nil then exit;
  if not Application.MainForm.Visible then exit;
  HideSplash;
  AppEvents.OnMessage:= nil;
end;

function TDmDb.Connected: boolean;
begin
  result:= DBProvider.IsConnected;
end;

procedure TDmDb.Disconnect;
begin
  DBProvider.Disconnect;
end;

procedure TDmDb.DoConnect(Params: TLoginParams);
begin
  PushCursor;
  if Connected then
    Disconnect;
  DBProvider.Connect(Params.RtPath, Params.Login, Params.Pass, W_ROLE);
  AppInfo.DbPath:= Params.RtPath;
  AppInfo.Login:= Params.Login;
end;

function TDmDb.Reconnect: boolean;
begin
  result:= true;
  FLoginParams.CheckConnectParams(true);
  if not LoginLoop(FLoginParams,
    procedure(Stage: TLoginStage; const Params: TLoginParams;
                    var DlgPos: TPoint; var Disables: TDisables)
    begin
      Disables:= [];
      case Stage of
        lsGetParams:
          begin
            ShowSplash(Params.RtPath);
            DlgPos.Y:= SplashBottom;
          end;
        lsDbChanged:
          SetSplashText(Params.RtPath);
        lsOK: begin
          PushCursor;
          FLoginParams:= Params;
          DoConnect(FLoginParams);
        end;
      end;
    end
  ) then
    exit(false);
  try
    InitDB;
  except
    try Disconnect; except end;
    Application.HandleException(Application);
    Halt(4);
  end;
  fWasConnected:= true;
  JvAppStore.WriteString(PTH_LAST_CONNECT, FLoginParams.Alias);
  JvAppStore.WriteString(PTH_LAST_USER, FLoginParams.Login);
end;

procedure TDmDb.InitDB;
begin
end;

function TDmDb.AltPassDlg: boolean;
//const
//  SQL = 'execute procedure AltPass(:LOGIN,:PASS)';
//var
//  psw: string;
begin
  result:= false;
//  result:= TdlgAltPass.Exec(psw);
//  if not result then exit;
//  PushCursor;
//  QPrepareWR(SQL).SetParams([User, psw]).Invoke;
//  Application.MessageBox('Пароль изменен', '', MB_OK + MB_ICONINFORMATION);
end;

end.
