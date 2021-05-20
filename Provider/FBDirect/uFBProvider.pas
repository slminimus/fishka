{-----------------------------------------------------------------------------
 Unit Name: uFBProvider
 Author:    sl
 Date:      14-дек-2019
 Purpose:
     Реализация интерфейсов uifProvider; СУБД FireBird 2.5 - 3.0

-----------------------------------------------------------------------------}

unit uFBProvider;

interface
uses Classes, Types, SysUtils, usIntfs, usTools, Generics.Collections, DB,
   Variants, pFIBDatabase, pFIBDataSet, pFIBQuery, FIBQuery, Math, Windows,
   UsClasses, DBConsts, IBase, FIBDataSet, pFIBFieldsDescr, pFIBProps, FibTypes,
   uifProvider
;
type
  TTransaction = class;

  TConnection = class(TInterfacedObject, IConnection)
  private
    function  ReadTRS: ITransaction;
    function  QPrepare(const SQL: string; trs: ITransaction = nil): IDBQuery;
    function  QPrepareWR(const SQL: string): IDBQuery;
    function  QPrepareBatch(const SQL: string): IDBQuery;
    function  IsConnected: boolean;
  protected
    fDB: TpFIBDatabase;
    FReadTRS: ITransaction;
    procedure ConnectError; virtual;
    function  NewQuery(trs: ITransaction = nil): IDBQuery;
    function  NewTRS(AReadOnly: boolean = true): ITransaction;
  public
    class function NewConnect(const URL, aUser, aPassword: string; const aRole: string = ''): IConnection;
    constructor Create;
    destructor Destroy; override;
    procedure DoConnect(const URL, aUser, aPassword, aRole: string); virtual;
  end;

  TTransaction = class(TInterfacedObject, ITransaction)
  private
    function  IsActive: Boolean;
    function  Connection: IConnection;
    function  Start: ITransaction;
    procedure Commit;
    procedure Rollback;
    function  ReadOnly: boolean;
  private
    fBody: TpFIBTransaction;
    fConnection: TConnection;
    fReadOnly: boolean;
    procedure CommitRetaining;
  public
    constructor Create(aConnection: TConnection; AReadOnly: boolean);
    destructor Destroy; override;
  end;

  TDBQuery = class(TInterfacedObject, IDBQuery, IUsData)
  private // IDBQuery
    function  GetParamValue(const ParamName: string): Variant; overload;
    procedure SetParamValue(const ParamName: string; const aValue: Variant); overload;
    function  GetParamValue(ParamIndex: integer): Variant; overload;
    procedure SetParamValue(ParamIndex: integer; const aValue: Variant); overload;
    function  Connection: IConnection;
    function  Transaction: ITransaction;
    function  ParamCount: Integer;
    function  ParamName(index: integer): string;
    function  ParamsDebug (Mode: integer = 3): IDBQuery;
    function  Prepare: IDBQuery; overload;
    function  Prepare  (const SQL: string): IDBQuery; overload;
    function  SetParam (const ParamName: string; const ParamValue: Variant): IDBQuery; overload;
    function  LoadParam(const ParamName: string; Stream: TStream) :IDBQuery; overload;
    function  LoadParam(const ParamName, FileName: string) :IDBQuery; overload;
    function  LoadParam(index: integer; Stream: TStream) :IDBQuery; overload;
    function  LoadParam(index: integer; const FileName: string) :IDBQuery; overload;
    function  LoadParam(index: integer; Src: IDBQuery; SrcIndex: integer) :IDBQuery; overload;
    function  SetParams(const Params: Variant): IDBQuery; overload;
    function  SetParams(const Params: array of Variant): IDBQuery; overload;
    function  SetParams(const Params: array of const): IDBQuery; overload;
    // параметр _OPER[ATION] или AOPERATION (если в DS нет такого Fied) получает значение
    // 0: если DS.State = dsInsert
    // 1: если DS.State = dsEdit
    // 2: если DS.State = dsBrowse
    // иначе игнорируется
    function  SetParams(const DS: TDataSet; const Params: string = '') :IDBQuery; overload;
    function  SetParams(const us: IUsData; const Params: string = '') :IDBQuery; overload;
    function  SetConnection(Value: IConnection): IDBQuery;
    function  SetNewTransaction: IDBQuery;
    function  SetTransaction(Value: ITransaction = nil): IDBQuery;
    function  SQL: string;
    function  Open: IDBQuery;
    function  Close: IDBQuery;
    // выполняет запрос, даже если уже Active. Для Batch - процессов.
    // Если Src указан - выполнить запрос для каждой его строки,
    // заполняя параметры из одноименных столбцов Src
    function Exec(Src: IUsData = nil): IDBQuery;
    // Если транзакция пишущая, финализирует её сразу; результата нет (Open.Close)
    function  Invoke: IDBQuery;
    // Commit свою транзакцию, если она активна
    function Commit: IDBQuery;

    function  Eof: Boolean;
    function  IsActive: Boolean;
    function  IsEmpty: Boolean;
    procedure Next;
    function  FieldCount: integer;
    function  FieldIndex(const aFieldName: string; RaiseIf: boolean = true): integer;
    function  FieldName(index: integer): string;
    function  FieldValue(const FieldName: string): Variant; overload;
    function  FieldValue(aFieldIndex: integer): Variant; overload;
    function  IsBlob(aFieldIndex: integer): boolean;
    function  BlobRead(const FieldName: string; Dst: TStream): IDBQuery; overload;
    function  BlobRead(aFieldIndex: integer; Dst: TStream): IDBQuery; overload;
    function  BlobWrite(const FieldName: string; Src: TStream): IDBQuery; overload;
    function  BlobWrite(aFieldIndex: integer; Src: TStream): IDBQuery; overload;
  private // IUsData
    function  ColCount: Cardinal;
    function  DescribeColumn(index: Cardinal): TUsColDef;
    function  GetColData(index: Cardinal): Variant;
    function  GetRowData(const RowBuf: array of PVariant): boolean;
    function  Start(StartRow: Integer = -1): IUsData;
  private type
    TStrIntDict = class(TDictStr<integer>)
    private
      fWhat: string;
    public
      constructor Create(aWhat: string);
      function ByName(const Name: string): integer;
    end;
  private
    fSQL: TSQL;
    fConnection  :IConnection;
    fTransaction :ITransaction;
    fNdxFields: TStrIntDict;
    fNdxParams: TStrIntDict;
    fTrsOwner: boolean;
    fAutoCommit: boolean;
    procedure OnSQLChanging(Sender: TObject);
    procedure MakeParamsIndex;
    procedure MakeFieldsIndex;
    procedure _Open(Retaining: boolean; ForceExec: boolean);
    procedure ClearParamValues;
    function  FindField(const FieldName: string) :TSQLVAR;
  public
    constructor Create(AutoCommit: boolean = false);
    destructor Destroy; override;
  end;

  TOnSql = procedure(After: boolean);

function ExUnknownMsg(const Msg: string): string;

implementation
uses uDBProvider, Fib, StrUtils;

const
  OCTETS_CHARSET_ID = 1;

type
  TUUIDField = class (TFIBStringField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetSize: Integer; override;
    procedure SetSize(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

//#########################################################################

function ExUnknownMsg(const Msg: string): string;
var P: PChar;
  W,Q: PChar;
begin
  result:= Msg;
  P:= PChar(Msg);
  while P^ <> #0 do begin
    if not NextLine(P).StartsWith('Exception ', true) then Continue;
    NextLine(P);
    break;
  end;
  if P^ = #0 then exit;
  W:= P;
  Q:= P;
  while P^ <> #0 do begin
    Q:= P;
    if NextLine(P).StartsWith('At ', true) then break;
  end;
  SetString(result, W, Q - W);
  result:= Trim(result);
end;

// Если это exception FireBird, то оставляет в E.Message
// только сообщение самого exception
procedure CheckFIBError;
const
  E_BADLOGIN  = 335544472;
  E_EXCEPTION = 335544517;
  S_BADLOGIN  = 'Неправильный пароль или имя пользователя';
var E: EFIBError;
begin
  if not (ExceptObject is EFIBError) then exit;
  E:= EFIBError(ExceptObject);
  case E.IBErrorCode of
    E_BADLOGIN : E.Message:= S_BADLOGIN;
    E_EXCEPTION: E.Message:= ExUnknownMsg(E.Message);
    else         E.Message:= E.IBMessage;
  end;
end;

class procedure TUUIDField.CheckTypeSize(Value: Integer);
begin
  if not (Value in [16,36]) then
    DatabaseError(SInvalidFieldSize);
end;

constructor TUUIDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Size:= 36;
end;

function TUUIDField.GetSize: Integer;
begin
  Result:= 36;
end;

procedure TUUIDField.SetSize(Value: Integer);
begin
  //
end;

procedure uuid_to_char(Buf: Pointer);
type
  tBufSrc = array [0..15] of Byte;
  tBufDst = array [0..36] of AnsiChar;
  pBufSrc = ^tBufSrc;
  pBufDst = ^tBufDst;
const
  HexTable :array[0..15] of AnsiChar = '0123456789ABCDEF';
begin
  pBufDst(Buf)[36] := #0;
  pBufDst(Buf)[35] := HexTable[pBufSrc(Buf)[15] and $F];
  pBufDst(Buf)[34] := HexTable[pBufSrc(Buf)[15] shr  4];
  pBufDst(Buf)[33] := HexTable[pBufSrc(Buf)[14] and $F];
  pBufDst(Buf)[32] := HexTable[pBufSrc(Buf)[14] shr  4];
  pBufDst(Buf)[31] := HexTable[pBufSrc(Buf)[13] and $F];
  pBufDst(Buf)[30] := HexTable[pBufSrc(Buf)[13] shr  4];
  pBufDst(Buf)[29] := HexTable[pBufSrc(Buf)[12] and $F];
  pBufDst(Buf)[28] := HexTable[pBufSrc(Buf)[12] shr  4];
  pBufDst(Buf)[27] := HexTable[pBufSrc(Buf)[11] and $F];
  pBufDst(Buf)[26] := HexTable[pBufSrc(Buf)[11] shr  4];
  pBufDst(Buf)[25] := HexTable[pBufSrc(Buf)[10] and $F];
  pBufDst(Buf)[24] := HexTable[pBufSrc(Buf)[10] shr  4];
  pBufDst(Buf)[23] := '-';
  pBufDst(Buf)[22] := HexTable[pBufSrc(Buf)[ 9] and $F];
  pBufDst(Buf)[21] := HexTable[pBufSrc(Buf)[ 9] shr  4];
  pBufDst(Buf)[20] := HexTable[pBufSrc(Buf)[ 8] and $F];
  pBufDst(Buf)[19] := HexTable[pBufSrc(Buf)[ 8] shr  4];
  pBufDst(Buf)[18] := '-';
  pBufDst(Buf)[17] := HexTable[pBufSrc(Buf)[ 7] and $F];
  pBufDst(Buf)[16] := HexTable[pBufSrc(Buf)[ 7] shr  4];
  pBufDst(Buf)[15] := HexTable[pBufSrc(Buf)[ 6] and $F];
  pBufDst(Buf)[14] := HexTable[pBufSrc(Buf)[ 6] shr  4];
  pBufDst(Buf)[13] := '-';
  pBufDst(Buf)[12] := HexTable[pBufSrc(Buf)[ 5] and $F];
  pBufDst(Buf)[11] := HexTable[pBufSrc(Buf)[ 5] shr  4];
  pBufDst(Buf)[10] := HexTable[pBufSrc(Buf)[ 4] and $F];
  pBufDst(Buf)[ 9] := HexTable[pBufSrc(Buf)[ 4] shr  4];
  pBufDst(Buf)[ 8] := '-';
  pBufDst(Buf)[ 7] := HexTable[pBufSrc(Buf)[ 3] and $F];
  pBufDst(Buf)[ 6] := HexTable[pBufSrc(Buf)[ 3] shr  4];
  pBufDst(Buf)[ 5] := HexTable[pBufSrc(Buf)[ 2] and $F];
  pBufDst(Buf)[ 4] := HexTable[pBufSrc(Buf)[ 2] shr  4];
  pBufDst(Buf)[ 3] := HexTable[pBufSrc(Buf)[ 1] and $F];
  pBufDst(Buf)[ 2] := HexTable[pBufSrc(Buf)[ 1] shr  4];
  pBufDst(Buf)[ 1] := HexTable[pBufSrc(Buf)[ 0] and $F];
  pBufDst(Buf)[ 0] := HexTable[pBufSrc(Buf)[ 0] shr  4];
end;

procedure char_to_uuid(pSrc: pAnsiChar; Len: Integer; pDst: pByte); overload;
  //---
  procedure Error;
  begin
    // поскольку pSrc может указывать на буфер без завершающего #0, надо брать строку через Copy
    raise EConvertError.CreateFmt('''%s'' недопустимое для Interbase строковое представление UUID-значения',[Copy(pSrc,1,Len)]);
  end;
  //---
var
  p :pAnsiChar;
  e :pAnsiChar;
begin
  if  Len <> 36  then
    Error;
  p := pSrc-1;
  e := pSrc+35;
  repeat
    Inc(p);
    case  p^  of
      '0'..'9': pDst^ := (Ord(p^) - Ord('0')    ) shl 4;
      'A'..'F': pDst^ := (Ord(p^) - Ord('A') +10) shl 4;
      'a'..'f': pDst^ := (Ord(p^) - Ord('a') +10) shl 4;
      '-'     : if (p-pSrc) in [8,13,18,23] then continue else Error;
      else Error;
    end;

    Inc(p);
    case  p^  of
      '0'..'9': pDst^ := (Ord(p^) - Ord('0')    ) or pDst^;
      'A'..'F': pDst^ := (Ord(p^) - Ord('A') +10) or pDst^;
      'a'..'f': pDst^ := (Ord(p^) - Ord('a') +10) or pDst^;
      else Error;
    end;

    Inc(pDst);
  until p >= e ;
end;

{ TConnection }

class function TConnection.NewConnect(const URL, aUser, aPassword,
                                                   aRole: string): IConnection;
begin
  result:= Create;
 (result as TConnection).DoConnect(URL, aUser, aPassword, aRole);
end;

constructor TConnection.Create;
begin
  fDB:= TpFIBDatabase.Create(nil);
  FDB.DBParams.Clear;
  FDB.DBParams.Add('lc_ctype=WIN1251');
  FDB.UseLoginPrompt:= False;
  FDB.SQLDialect:= 3;
  FDB.LibraryName:= 'fbclient.dll';
  FDB.DBParams.Add('user_name=');
  FDB.DBParams.Add('password=');
  FDB.DBParams.Add('sql_role_name=');
  FReadTRS:= TTransaction.Create(Self, true);
  FDB.DefaultTransaction:= (FReadTRS as TTransaction).fBody;
end;

procedure TConnection.ConnectError;
begin
  CheckFIBError;
  raise ExceptObject;
end;

procedure TConnection.DoConnect(const URL, aUser, aPassword, aRole: string);
begin
  try
    if FDB.Connected then FDB.ForceClose;
    FDB.DBParams.Values['user_name']:= aUser;
    FDB.DBParams.Values['password']:= aPassword;
    FDB.DBParams.Values['sql_role_name']:= aRole;
    FDB.DBName:= URL;
    FDB.Open;
    FReadTRS.Start;
  except
    ConnectError;
  end;
end;

destructor TConnection.Destroy;
begin
  fDB.ForceClose;
  FReadTRS:= nil;
  FreeAndNil(fDB);
  inherited;
end;

function TConnection.IsConnected: boolean;
begin
  result:= assigned(fDB) and fDB.Connected;
end;

function TConnection.NewQuery(trs: ITransaction): IDBQuery;
begin
  if trs = nil then
    trs:= FReadTRS;
  result:= TDBQuery.Create;
  result.SetConnection(Self).SetTransaction(trs);
end;

function TConnection.QPrepare(const SQL: string; trs: ITransaction): IDBQuery;
begin
  result:= NewQuery(trs).Prepare(SQL);
end;

function TConnection.QPrepareWR(const SQL: string): IDBQuery;
begin
  result:= NewQuery(NewTRS(false)).Prepare(SQL);
end;

function TConnection.QPrepareBatch(const SQL: string): IDBQuery;
begin
  result:= TDBQuery.Create(true);
  result.SetConnection(Self).SetTransaction(NewTRS(false).Start).Prepare(SQL);
end;

function TConnection.NewTRS(AReadOnly: boolean): ITransaction;
var trs: TTransaction;
begin
  try
    trs:= TTransaction.Create(Self, AReadOnly);
  except
    CheckFIBError;
    raise;
  end;
  result:= trs;
end;

function TConnection.ReadTRS: ITransaction;
begin
  result:= FReadTRS;
end;

{ TTransaction }

constructor TTransaction.Create(aConnection: TConnection; AReadOnly: boolean);
var s: string;
begin
  fConnection:= aConnection;
  fBody:= TpFIBTransaction.Create(nil);
  fReadOnly:= AReadOnly;
  if AReadOnly then
    s:= 'read,';
  s:= s + 'read_committed,rec_version,nowait';
  fBody.TRParams.CommaText:= s;
  fBody.AddDatabase(aConnection.fDB);
end;

destructor TTransaction.Destroy;
begin
  FreeAndNil(fBody);
  inherited;
end;

procedure TTransaction.Commit;
begin
  fBody.Commit;
end;

procedure TTransaction.CommitRetaining;
begin
  fBody.CommitRetaining;
end;

function TTransaction.Connection: IConnection;
begin
  result:= fConnection;
end;

function TTransaction.IsActive: Boolean;
begin
  result:= fBody.Active;
end;

function TTransaction.ReadOnly: boolean;
begin
  result:= fReadOnly;
end;

procedure TTransaction.Rollback;
begin
  if fBody.Active then
    fBody.Rollback;
end;

function  TTransaction.Start: ITransaction;
begin
  result:= self;
  fBody.StartTransaction;
end;


{ TDBQuery.TStrIntDict }

function TDBQuery.TStrIntDict.ByName(const Name: string): integer;
const
  MSG = '%s not found: "%s"';
begin
  if not TryGetValue(Name, result) then
    raise Exception.CreateFmt(MSG, [fWhat, Name]);
end;

constructor TDBQuery.TStrIntDict.Create(aWhat: string);
begin
  inherited Create;
  fWhat:= aWhat;
end;

{ TDBQuery }

constructor TDBQuery.Create(AutoCommit: boolean);
begin
  fAutoCommit:= AutoCommit;
  fSQL:= TSQL.Create(nil);
  fNdxFields:= TStrIntDict.Create('Field');
  fNdxParams:= TStrIntDict.Create('Parameter');
  fSQL.OnSQLChanging:= OnSQLChanging;
end;

destructor TDBQuery.Destroy;
begin
  if fTrsOwner or fAutoCommit then
    if Assigned(fTransaction) and fTransaction.IsActive then
    try
      fTransaction.Rollback;
    except
      OutputDebugString('TDBQuery.BeforeDestruction Exception');
    end;
  fTransaction:= nil;
  fConnection:= nil;
  FreeAndNil(fNdxFields);
  FreeAndNil(fNdxParams);
  FreeAndNil(fSQL);
  inherited;
end;

function TDBQuery.DescribeColumn(index: Cardinal): TUsColDef;
var f: TFIBXSQLVAR;
   dt: TUsDataType;
   sz: integer;
begin
  f:= fSQL.Fields[index];
  case f.SQLType and $FFFFFFFE of
    SQL_VARYING    : dt:= ustVariant;
    SQL_TEXT       : dt:= ustString;
    SQL_DOUBLE     : dt:= ustFloat;
    SQL_FLOAT      : dt:= ustFloat;
    SQL_LONG       : dt:= ustLong;
    SQL_SHORT      : dt:= ustShortInt;
    SQL_TIMESTAMP  : dt:= ustDateTime;
    SQL_BLOB       : dt:= ustString;
    SQL_D_FLOAT    : dt:= ustFloat;
    SQL_ARRAY      : dt:= ustHexBinary;
    SQL_QUAD       : dt:= ustQInt;
    SQL_TYPE_TIME  : dt:= ustTime;
    SQL_TYPE_DATE  : dt:= ustDate;
    SQL_INT64      : dt:= ustQInt;
    SQL_BOOLEAN,
    FB3_SQL_BOOLEAN: dt:= ustBoolean;
    else raise Exception.CreateFmt('Unknown SQL type: ', [f.SQLType]);
  end;
  sz:= 0;
  if dt in ustVarLen then
    sz:= f.Size;
  result:= TUsColDef.Create(f.Name, dt, sz);
end;

function TDBQuery.BlobRead(aFieldIndex: integer; Dst: TStream): IDBQuery;
begin
  result:= Self;
  fSQL.Fields[aFieldIndex].SaveToStream(Dst);
end;

function TDBQuery.BlobRead(const FieldName: string; Dst: TStream): IDBQuery;
begin
  result:= Self;
  MakeFieldsIndex;
  BlobRead(fNdxFields[FieldName], Dst);
end;

function TDBQuery.BlobWrite(aFieldIndex: integer; Src: TStream): IDBQuery;
begin
  result:= Self;
  fSQL.Fields[aFieldIndex].LoadFromStream(Src);
end;

function TDBQuery.BlobWrite(const FieldName: string; Src: TStream): IDBQuery;
begin
  result:= Self;
  MakeFieldsIndex;
  BlobWrite(fNdxFields[FieldName], Src);
end;

procedure TDBQuery.ClearParamValues;
var i :Integer;
begin
  for i:= 0 to fSQL.Params.Count -1 do
    fSQL.Params[i].Value:= null;
end;

function TDBQuery.Close: IDBQuery;
var b: boolean;
begin
  Result:= Self;
  b:= fTrsOwner;
  fTrsOwner:= false;
  try
    if b and fTransaction.IsActive then
      fTransaction.Commit;
    fSQL.Close;
  finally
    SetTransaction;
  end;
end;

function TDBQuery.ColCount: Cardinal;
begin
  result:= fSQL.FieldCount;
end;

function TDBQuery.Commit: IDBQuery;
begin
  Result:= Self;
  if Transaction.IsActive then
    Transaction.Commit;
end;

function TDBQuery.FindField(const FieldName: string): TSQLVAR;
var i: integer;
begin
  result:= nil;
  MakeFieldsIndex;
  if fNdxFields.TryGetValue(FieldName, i) then
    result:= fSQL.Fields[i];
end;

function TDBQuery.Connection: IConnection;
begin
  Result:= fConnection;
end;

function TDBQuery.Eof: Boolean;
begin
  Result:= fSQL.Eof;
end;

function TDBQuery.FieldCount: integer;
begin
  Result:= fSQL.FieldCount;
end;

function TDBQuery.FieldIndex(const aFieldName: string; RaiseIf: boolean): integer;
begin
  MakeFieldsIndex;
  if fNdxFields.TryGetValue(aFieldName, result) then exit;
  if RaiseIf then
    raise Exception.CreateFmt('IDBQuery: Field not found [%s]', [aFieldName]);
  result:= -1;
end;

function TDBQuery.FieldName(index: integer): string;
begin
  Result:= fSQL.Fields[Index].Name;
end;

function TDBQuery.FieldValue(aFieldIndex: integer): Variant;
begin
  Result:= fSQL.Fields[aFieldIndex].Value;
end;

function TDBQuery.GetColData(index: Cardinal): Variant;
begin
  with fSQL.Fields[index] do
    if SQLType and $FFFFFFFE = SQL_BLOB then
      result:= AsString
    else
      result:= AsVariant;
end;

function TDBQuery.GetParamValue(const ParamName: string): Variant;
begin
  MakeParamsIndex;
  result:= fSQL.Params[fNdxParams[ParamName]].Value;
end;

function TDBQuery.GetParamValue(ParamIndex: integer): Variant;
begin
  result:= fSQL.Params[ParamIndex].Value;
end;

function TDBQuery.GetRowData(const RowBuf: array of PVariant): boolean;
var i: Cardinal;
    P: PVariant;
begin
  if not fSQL.Open then
    Open;
  result:= not Eof;
  for i:= 0 to min(ColCount, Length(RowBuf)) -1 do begin
    P:= RowBuf[i];
    if Assigned(P) then
      P^:= GetColData(i);
  end;
end;

function TDBQuery.FieldValue(const FieldName: string): Variant;
begin
  MakeFieldsIndex;
  Result:= fSQL.Fields[fNdxFields[FieldName]].Value;
end;

function TDBQuery.Invoke: IDBQuery;
begin
  result:= Self;
  try
    _Open(false, true);
  finally
    Close;
  end;
end;

function TDBQuery.IsActive: Boolean;
begin
  Result:= fSQL.Open;
end;

function TDBQuery.IsBlob(aFieldIndex: integer): boolean;
begin
  result:= fSQL.Fields[aFieldIndex].IsBlob;
end;

function TDBQuery.IsEmpty: Boolean;
begin
  Result:= fSQL.Bof and fSQL.Eof;
end;

procedure TDBQuery.MakeFieldsIndex;
var i: integer;
begin
  if fNdxFields.Count = FieldCount then exit;
  fNdxFields.Clear;
  for i:= 0 to fSQL.FieldCount -1 do
    fNdxFields.AddOrSetValue(fSQL.Fields[i].Name, i);
end;

procedure TDBQuery.MakeParamsIndex;
var
  P: TSQLVAR;
  i: integer;
begin
  if fNdxParams.Count = ParamCount then exit;
  fNdxParams.Clear;
  for i:= 0 to fSQL.ParamCount -1 do begin
    P:= fSQL.Params[i];
    if not fNdxParams.ContainsKey(P.Name) then
      fNdxParams.Add(P.Name, i)
    else if P.IsGuid then
      fNdxParams[P.Name]:= i;
  end;
end;

procedure TDBQuery.Next;
begin
  fSQL.Next;
end;

procedure TDBQuery.OnSQLChanging(Sender: TObject);
begin
  fNdxFields.Clear;
  fNdxParams.Clear;
end;

function TDBQuery.Open: IDBQuery;
begin
  result:= Self;
  _Open(true, false);
end;

function TDBQuery.Exec(Src: IUsData): IDBQuery;
var
  a: TArray<TSmallPoint>;
  i: integer;
  n: integer;
  p: integer;
begin
  result:= Self;
  if Assigned(Src) then begin
    Src.Start;
    if Src.EOF then exit;
    MakeParamsIndex;
    SetLength(a, fNdxParams.Count);
    n:= 0;
    for i:= 0 to Src.ColCount -1 do begin
      if not fNdxParams.TryGetValue(Src.DescribeColumn(i).Name, p) then
        Continue;
      a[n]:= SmallPoint(p, i);
      inc(n);
    end;
    SetLength(a, n);

    while not Src.EOF do begin
      for i:= 0 to High(a) do
        with a[i] do
          fSQL.Params[x].Value:= Src.Values[y];
      _Open(true, true);
      Src.Next;
    end;
  end else
    _Open(true, true);
end;

function TDBQuery.ParamCount: Integer;
begin
  result:= fSQL.ParamCount;
end;

function TDBQuery.ParamName(index: integer): string;
begin
  result:= fSQL.Params[index].Name;
end;

function ValueAsSqlStr(const P: TSQLVAR): string;
const A: array[boolean] of Char = ('0', '1');
var V: Variant;
begin
  V:= P.Value;
  if VarIsNothing(V) or P.IsNull then begin
    result:= 'null';
    exit;
  end;
  case P.SQLType and $FFFFFFFE of
    SQL_VARYING,
    SQL_BLOB      : result:= AnsiQuotedStr(P.AsString, '''');
    SQL_TEXT      :
        if P.IsGuid then
          result:= 'x'''+ StringReplace(P.AsString, '-', '', [rfReplaceAll]) + ''''
        else
          result:= AnsiQuotedStr(P.AsString, '''');
    SQL_DOUBLE,
    SQL_FLOAT,
    SQL_D_FLOAT   : result:= FloatToStr(V, HtFormats);
//    SQL_LONG,
//    SQL_SHORT,
//    SQL_QUAD,
//    SQL_INT64     : result:= P.AsString;
//    SQL_TYPE_TIME : result:= P.AsString;
    SQL_TYPE_DATE : result:= AnsiQuotedStr(FormatDateTime('dd.mm.yyyy', VarToDateTime(V)), '''');
    SQL_TIMESTAMP : result:= AnsiQuotedStr(FormatDateTime('dd.mm.yyyy hh:nn:ss', VarToDateTime(V)), '''');
//    SQL_ARRAY     : result:= P.AsString;
    SQL_BOOLEAN   : result:= A[P.AsBoolean];
    else            result:= P.AsString;
  end;
end;

function SqlTypeName(const P: TSQLVAR): string;
const A: array[boolean] of string = ('TEXT', 'DGUID');
begin
  case P.SQLType and $FFFFFFFE of
    SQL_VARYING    : result:= 'TEXT';
    SQL_TEXT       : result:= A[P.IsGuid];
    SQL_DOUBLE     : result:= 'double precision';
    SQL_FLOAT      : result:= 'float';
    SQL_D_FLOAT    : result:= 'double precision';
    SQL_LONG       : result:= 'integer';
    SQL_SHORT      : result:= 'smallint';
    SQL_QUAD       : result:= 'bigint';
    SQL_INT64      : result:= 'bigint';
    SQL_TYPE_TIME  : result:= 'time';
    SQL_TYPE_DATE  : result:= 'date';
    SQL_TIMESTAMP  : result:= 'dtimestamp';
    SQL_BLOB       : result:= 'BLOB';
//    SQL_ARRAY     : result:= '';
    SQL_BOOLEAN,
    FB3_SQL_BOOLEAN: result:= 'boolean';
    else             result:= '??';
  end;
end;

function Indent(const List: TStrings; Value: integer): string;
var i: integer;
begin
  result:= '';
  if List.Count = 0 then exit;
  for i:= 0 to List.Count -2 do
    result:= result + DupeString(' ', Value) + List[i] + #13#10;
  result:= result + DupeString(' ', Value) + List[List.Count -1];
end;

function TDBQuery.ParamsDebug(Mode: integer): IDBQuery;
  //---
  procedure Mode1;
  var i: integer;
      s: string;
  begin
    s:= '';
    for i:= 0 to ParamCount -1 do
      s:= s + format('%s: %s', [fSQL.Params[i].Name, VarToStrDef(fSQL.Params[i].Value, 'null')]) + #13#10;
    if s = '' then
      s:= '<no params>';
    OutputDebugString(PChar(s));
  end;
  //---
  procedure Mode2;
  begin
    OutputDebugString(PChar(fSQL.SQL.Text));
    Mode1;
  end;
  //---
  procedure Mode3;
  const
    CBLOCK =
       #13#10'execute block'
      +'%s'
      +      'as'
      +#13#10'%s'
      +      'begin'
      +#13#10'%s%s'
      +#13#10'end'
      +#13#10
    ;
    COUT  = '  ,%-*s %s'#13#10;
    CVAR  = '  declare variable %-*s %s;'#13#10;
    CVAL  = '  %-*s = %s;'#13#10;
  var i,n: integer;
        m: integer;
    block: string;
     vars: string;
     outs: string;
     vals: string;
     body: string;
        s: string;
     lst : TList;
  begin
    n:= 0; // считаем в n макс. длину имени среди выходных параметров
    for i:= 0 to fSQL.FieldCount -1 do
      n:= max(n, Length(fSQL.Fields[i].Name));
    outs:= '';
    for i:= 0 to fSQL.FieldCount -1 do
      outs:= outs + format(COUT, [n, fSQL.Fields[i].Name, SqlTypeName(TSQLVAR(fSQL.Fields[i]))]);
    if outs <> '' then begin
      outs[3]:= ' '; // убрать запятую в первой строке
      outs:= #13#10'returns('#13#10 + outs + ')';
    end;

    lst:= TList.Create;
    try
      n:= 0; // считаем в n макс. длину имени среди входных параметров
      m:= 0;
      for i:= 0 to ParamCount -1 do begin
        s:= fSQL.Params[i].Name;
        m:= max(m, Length(s));
        if FindField(s) <> nil then Continue;
        n:= max(n, Length(s));
        lst.Add(Pointer(i));
      end;
      //для входных параметров, для которых нет одноименных выходных, нужны декларации переменных
      vars:= '';
      for i:= 0 to lst.Count -1 do
        vars:= vars + format(CVAR, [n, fSQL.Params[integer(lst[i])].Name, SqlTypeName(fSQL.Params[integer(lst[i])])]);
    finally
      lst.Free;
    end;

    vals:= '';
    for i:= 0 to ParamCount -1 do
      vals:= vals + format(CVAL, [m, fSQL.Params[i].Name, ValueAsSqlStr(TSQLVAR(fSQL.Params[i]))]);
    if vals <> '' then
      vals:= vals + '-----'#13#10;

    s:= ''; // перечень выходных параметров для оператора into
    for i:= 0 to fSQL.FieldCount -1 do
      s:= s + fSQL.Fields[i].Name + ',';
    SetLength(s, Length(s) -1);

    body:= TrimRight(Indent(fSQL.SQL, ifthen(fSQL.FieldCount > 0, 3, 1)));
    if (length(body) > 0) and (body[length(body)] = ';') then
      SetLength(body, length(body) -1);
    if fSQL.FieldCount = 0 then
      body:= body + ';'
    else
      body:= '  for'#13#10+ body +#13#10'    into '+ s +#13#10'  do suspend;';
    block:= format(CBLOCK, [outs, vars, vals, body]);
    OutputDebugString(PChar(block));
  end;
  //---
begin
  result:= Self;
  if ParamCount = 0 then
    Mode:= 0;
  case Mode of
    0:   OutputDebugString(PChar(fSQL.SQL.Text));
    1:   Mode1;
    2:   Mode2;
    else Mode3;
  end;
end;

function TDBQuery.Prepare(const SQL: string): IDBQuery;
begin
  Result:= Self;
  fSQL.SQL.Text:= SQL;
  fTrsOwner:= not fTransaction.IsActive;
  if not fTrsOwner then
    Prepare()
  else begin
    fTransaction.Start;
    try
      Prepare();
    except
      fTrsOwner:= false;
      fTransaction.Rollback;
      CheckFIBError;
      raise;
    end;
  end;
end;

function TDBQuery.Prepare: IDBQuery;
begin
  Result:= Self;
  fNdxFields.Clear;
  fNdxParams.Clear;
  fSQL.Prepare;
  MakeFieldsIndex;
end;

function TDBQuery.SetConnection(Value: IConnection): IDBQuery;
begin
  Result:= Self;
  if  not Assigned(Value)  then
    raise Exception.Create('Неверные параметры вызова (nil)!');
  fConnection:= Value;
  fSQL.Database:= (Value as TConnection).fDB;
end;

function TDBQuery.SetNewTransaction: IDBQuery;
begin
  Result:= SetTransaction(fConnection.NewTRS);
end;

function TDBQuery.LoadParam(const ParamName, FileName: string) :IDBQuery;
begin
  Result := Self;
  MakeParamsIndex;
  fSQL.Params[fNdxParams[ParamName]].LoadFromFile(FileName);
end;

function TDBQuery.LoadParam(const ParamName: string; Stream: TStream) :IDBQuery;
begin
  Result := Self;
  MakeParamsIndex;
  fSQL.Params[fNdxParams[ParamName]].LoadFromStream(Stream);
end;

function TDBQuery.LoadParam(index: integer; const FileName: string): IDBQuery;
begin
  Result := Self;
  fSQL.Params[index].LoadFromFile(FileName);
end;

function TDBQuery.LoadParam(index: integer; Stream: TStream): IDBQuery;
begin
  Result := Self;
  fSQL.Params[index].LoadFromStream(Stream);
end;

function TDBQuery.LoadParam(index: integer; Src: IDBQuery; SrcIndex: integer) :IDBQuery;
const
  E1 = 'IDBQuery.LoadParam: Param[%s] is not BLOB type';
  E2 = 'IDBQuery.LoadParam: Src.Field[%s] is not BLOB type';
var stm: TMemoryStream;
begin
  if not fSQL.Params[index].IsBlob then
    raise Exception.CreateFmt(E1, [ParamName(index)]);
  if not Src.IsBlob(SrcIndex) then
    raise Exception.CreateFmt(E2, [Src.FieldName(SrcIndex)]);
  stm:= TMemoryStream.Create;
  try
    Src.BlobRead(SrcIndex, stm);
    stm.Seek(0, soFromBeginning);
    LoadParam(index, stm);
  finally
    stm.Free;
  end;
end;

function TDBQuery.SetParam(const ParamName: string;
                                 const ParamValue: Variant): IDBQuery;
begin
  Result:= Self;
  MakeParamsIndex;
  fSQL.Params[fNdxParams[ParamName]].Value:= ParamValue;
end;

function TDBQuery.SetParams(const Params: array of const): IDBQuery;
var i,n: Integer;
   dTmp: Double;
  _SqlType: integer;
begin
  Result:= Self;
  ClearParamValues;
  Assert(Low(Params)=0);
  n:= min(High(Params), fSQL.ParamCount -1);
  for i := 0 to n do with Params[i], fSQL.Params[i] do begin
    _SqlType:= SQLType and $FFFFFFFE;
    case VType of
      vtInteger   : AsInteger := VInteger;
      vtInt64     : AsInt64 := VInt64^;
      vtBoolean   : AsBoolean := VBoolean;

      {$IF CompilerVersion >= 20} // 2009+
      vtPChar     : AsAnsiString:= VPChar;
      vtChar      : AsAnsiString:= VChar;
      vtWideChar  : AsString    := VWideChar;
      vtAnsiString: AsAnsiString:= PAnsiChar(VAnsiString);
      vtUnicodeString: AsString := string(VUnicodeString);
      {$ELSE} // до Delphi 2009
      vtPChar     : AsString:= VPChar;
      vtChar      : AsString:= VChar;
      vtWideChar  : AsString:= UTF8Encode(WideString(VWideChar)); // SAP: Предполагаю что перекодировка в UTF8 - это ошибка!
      vtAnsiString: AsString:= PAnsiChar(VAnsiString);
      {$IFEND}
      vtString    : AsString:= String(VString^);
      vtWideString: AsString:= WideCharToString(VWideString);

      vtVariant   : Value:= Variant(VVariant^);
      vtCurrency  : if _SqlType <> SQL_TIMESTAMP then
                      AsCurrency:= VCurrency^
                    else
                      AsDateTime:= TDateTime( VCurrency^ );
      vtExtended  : if _SqlType <> SQL_TIMESTAMP then
                      AsExtended:= VExtended^
                    else begin
                      dTmp:= Extended(VExtended^);
                      AsDateTime:= TDateTime( dTmp )
                    end;
      vtObject    : if not Assigned(VObject) then
                      Value:= Null
                    else if VObject is TStream then begin
                      TStream(VObject).Position := 0;
                      LoadFromStream(TStream(VObject));
                      end
                    else
                      raise Exception.Create('Не совместимый объект ClassName=' + VObject.ClassName);
      else
        raise Exception.Create('Не совместимый формат данных VType=' + IntToStr(VType));
    end;
  end;
end;

function TDBQuery.SetTransaction(Value: ITransaction): IDBQuery;
begin
  Result:= Self;
  if  not Assigned(Value) then begin
    if  not Assigned(fConnection) then
      raise Exception.Create('TDBQuery.SetTransaction: Не назначен Connection');
    Value:= fConnection.ReadTRS;
  end;
  fTransaction:= Value;

  fSQL.Database:= TTransaction(fTransaction).fBody.DefaultDatabase;
  fSQL.Transaction:= TTransaction(fTransaction).fBody;
end;

function TDBQuery.SetParams(const Params: Variant): IDBQuery;
var i: Integer;
begin
  Result:= Self;
  MakeParamsIndex;
  ClearParamValues;
  if  VarIsNull(Params)  then Exit;
  if  VarIsEmpty(Params) then Exit;
  if not VarIsArray(Params) then
    fSQL.Params[0].Value:= Params
  else
    for i:= 0 to VarArrayHighBound(Params, 1) - VarArrayLowBound(Params, 1) do
      fSQL.Params[i].Value:= Params[VarArrayLowBound(Params,1) + i];
end;

function TDBQuery.SetParams(const Params: array of Variant): IDBQuery;
var i: Integer;
begin
  Result:= Self;
  MakeParamsIndex;
  ClearParamValues;
  for i:= 0 to High(Params) do
    fSQL.Params[i].Value:= Params[i];
end;

function TDBQuery.SetParams(const us: IUsData; const Params: string): IDBQuery;
var
  ndx: TStringList;
  //---
  procedure Step(const P: TSQLVAR);
  var f: integer;
  begin
    f:= ndx.IndexOf(P.Name);
    if f < 0 then exit;
    f:= IntPtr(ndx.Objects[f]);
    P.AsVariant:= us.GetColData(f);
  end;
  //---
var i: Integer;
    s: string;
    A: TArray<string>;
begin
  result:= self;
  MakeParamsIndex;
  us.Start;
  ndx:= TStringList.Create;
  try
    UsColDefsIndex(us, ndx);
    ndx.Sort;
    if Params <> '' then begin
      A:= Params.Split([',', ' ', ';', #9, #13]);
      for s in A do
        Step(fSQL.Params[fNdxParams[s]]);
    end
    else
      for i:= 0 to fSQL.Params.Count -1 do
        Step(fSQL.Params[i]);
  finally
    ndx.Free;
  end;
end;

function TDBQuery.SetParams(const DS: TDataSet; const Params: string): IDBQuery;
  //---
  procedure Step(const P: TSQLVAR);
  var F: TField;
     op: integer;
  begin
    F:= DS.FindField(P.Name);
    if Assigned(F) then
      P.AsVariant:= F.AsVariant
    else begin
      case DS.State of
        dsInsert: op:= 0;
        dsEdit  : op:= 1;
        dsBrowse: op:= 2;
        else exit;
      end;
      if SameText(P.Name, '_OPER') or SameText(P.Name, '_OPERATION')
                                   or SameText(P.Name, 'AOPERATION')
      then
        P.Value:= op;
    end;
  end;
  //---
var i: Integer;
    s: string;
    A: TArray<string>;
begin
  result:= self;
  MakeParamsIndex;
  if Params <> '' then begin
    A:= Params.Split([',', ' ', ';', #9, #13]);
    for s in A do
      Step(fSQL.Params[fNdxParams[s]]);
  end
  else
    for i:= 0 to fSQL.Params.Count -1 do
      Step(fSQL.Params[i]);
end;

procedure TDBQuery.SetParamValue(const ParamName: string;
                                                  const aValue: Variant);
begin
  MakeParamsIndex;
  fSQL.Params[fNdxParams[ParamName]].Value:= aValue;
end;

procedure TDBQuery.SetParamValue(ParamIndex: integer; const aValue: Variant);
begin
  fSQL.Params[ParamIndex].Value:= aValue;
end;

function TDBQuery.SQL: string;
begin
  Result:= fSQL.SQL.Text;
end;

function TDBQuery.Start(StartRow: Integer): IUsData;
begin
  result:= Self;
  Open;
  while StartRow > 0 do
    Next;
end;

function TDBQuery.Transaction: ITransaction;
begin
  Result:= fTransaction;
end;

procedure TDBQuery._Open(Retaining, ForceExec: boolean);
begin
  if IsActive and not ForceExec then
    Exit;
//  if SqlDebugMode >= 0 then
//    ParamsDebug(SqlDebugMode);
  if fTransaction.IsActive and not fTrsOwner then
    fSQL.ExecQuery
  else begin
    if not fTransaction.IsActive then
      fTransaction.Start;
    try
      if not fTrsOwner then
        fTrsOwner:= Retaining;
      fSQL.ExecQuery;
      if not Retaining then
        fTransaction.Commit;
    except
      fTrsOwner:= false;
      fTransaction.Rollback;
      CheckFIBError;
      raise;
    end
  end;
  fNdxFields.Clear;
end;

function NewConnect(const URL, aUser, aPassword: string; const aRole: string = ''): IConnection;
begin
  result:= TConnection.NewConnect(URL, aUser, aPassword, aRole);
end;

initialization
  uDBProvider.NewConnectionProc:= NewConnect;

end.
