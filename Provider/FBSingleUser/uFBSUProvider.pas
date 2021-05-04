unit uFBSUProvider;

interface
uses Classes, SysUtils, uFBProvider, uDBProvider;

type
  TSingleUserConnection = class(TConnection)
  const
   START_USER   = 'EMPTY';
   START_ROLE   = 'REMPTY';
   START_PASS   = 'empty';
   W_ROLE       = 'RWORKER';
   W_USER       = 'WORKER';
   W_PASS       = 'SuperJob';
  private
    fPreConnect: boolean;
  protected
    procedure ConnectError; override;
  public
    procedure DoConnect(const URL, aUser, aPassword, aRole: string); override;
  end;

implementation
uses usTools, Fib, Math, StrUtils, uifProvider, Hash;

{ TSingleUserConnection }

procedure TSingleUserConnection.ConnectError;
const
  E_BADLOGIN  = 335544472;
  E_EXCEPTION = 335544517;
  S_BADLOGIN  = 'Неправильный пароль или имя пользователя';
  S_PRELOGIN  = 'Нарушена система проверки привилегий'#13#10;
var E: EFIBError;
begin
  if not (ExceptObject is EFIBError) then exit;
  E:= EFIBError(ExceptObject);
  case E.IBErrorCode of
    E_BADLOGIN : E.Message:= ifthen(fPreConnect, S_PRELOGIN + S_BADLOGIN, S_BADLOGIN);
    E_EXCEPTION: E.Message:= ExUnknownMsg(E.Message);
    else         E.Message:= E.IBMessage;
  end;
end;

procedure TSingleUserConnection.DoConnect(const URL, aUser, aPassword, aRole: string);
const
  S_KEY  = #$0D#$4E#$30#$1E#$2B#$34#$49#$32#$28#$45#$78#$6D#$34#$6E#$3D#$0D;
  SQL_0  = 'select MAGIC from GetMagic';
  SQL_1  = 'execute procedure PreLogin(:AUSER,:APASS)';
  SQL_2  = 'execute procedure CHECKLOGIN';
  //---
  function NewQueryWR: IDBQuery;
  begin
    result:= NewQuery(NewTRS(false));
  end;
  //---
  function FormatGuid(const S: string): string;
  begin
    result:= UpperCase(
        copy(S, 1, 8) +'-'
      + copy(S, 9, 4) +'-'+ copy(S, 13, 4) +'-'+ copy(S, 17, 4) +'-'
      + copy(S, 21, 12)
    );
  end;
var
  Magic: string;
  HKey: string;
  Pass: string;
  Role: string;
begin
  fPreConnect:= true;
  inherited DoConnect(URL, START_USER, START_PASS, START_ROLE);
  try
    fPreConnect:= false;
    Magic:= coalesce(NewQueryWR.Prepare(SQL_0).Open.GetColData(0), '');
    if Magic = '' then
      raise Exception.Create('Ошибка системы авторизации');
    HKey:= FormatGuid(THashMD5.GetHashString(AnsiUpperCase(aUser) + S_KEY + aPassword));
    Pass:= FormatGuid(UpperCase(THashMD5.GetHashString(HKey + Magic)));
    NewQueryWR.Prepare(SQL_1).SetParams([aUser, Pass]).Invoke;
    Role:= aRole;
    if Role = '' then
      Role:= W_ROLE;
    if aUser = 'SYSDBA' then begin
      inherited DoConnect(URL, aUser, aPassword, '');
      NewQueryWR.Prepare(SQL_2).Invoke;
    end else
      inherited DoConnect(URL, W_USER, W_PASS, Role);
  except
    ConnectError;
  end;
end;

function NewConnect(const URL, aUser, aPassword: string; const aRole: string = ''): IConnection;
begin
  result:= TSingleUserConnection.NewConnect(URL, aUser, aPassword, aRole);
end;

initialization
  uDBProvider.NewConnectionProc:= NewConnect;

end.
