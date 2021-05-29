{-----------------------------------------------------------------------------
 Unit Name: uDataServiceFB
 Author:    sl
 Date:      14-апр-2021
 Purpose:
   DataService, реализация FireBird, двузвенка.
Достаточно включить этот юнит в проект, он себя зарегистрирует в
модуле uEntities (см. раздел initialization).

-----------------------------------------------------------------------------}

unit uDataServiceFB;

interface
uses Classes, Types, SysUtils, usIntfs, slTools, slfTools, Variants, DB, usDb,
     uifProvider, uDBProvider, uFBProvider, uEntities;

type
  EPostError = class(Exception);

  TDataService = class(TInterfacedObject, IDataService)
  private
    procedure Connect(const aURL, aLogin, aPass: string);
    procedure Disconnect;
    function  Connected: boolean;
    function  CreateOpMethod(const aEntityID: TEntityID; const aMethod: TPrivOper;
                              Alter: boolean; RaiseIf: boolean = true): IOpMethod;
    function  StartTRS: ITransaction;
  public
  end;

  TTransaction = class(TInterfacedObject, ITransaction)
  private
    fTRS: uifProvider.ITransaction;
    procedure Commit;
    procedure Rollback;
  public
    constructor Create;
  end;

  TOpMethod = class(TInterfacedObject, IOpMethod)
  protected
    fEntityID: TEntityID;
    fOperType: TOperType;
    fOper: TPrivOper;
    fAlter: boolean;
    fQry: IDBQuery;
    function  ParamCount: integer;
    function  GetParam(index: integer): Variant; overload;
    function  SetParam(index: integer; const Value: Variant): IOpMethod; overload;
    procedure Set_Param(index: integer; const Value: Variant); overload;
    function  GetParam(const ParamName: string): Variant; overload;
    function  SetParam(const ParamName: string; const Value: Variant): IOpMethod; overload;
    procedure Set_Param(const ParamName: string; const Value: Variant); overload;
    function  SetParams(const DataRow: IUsData): IOpMethod; overload;
    function  SetParams(const DataSet: TDataSet): IOpMethod; overload;

    procedure Invoke(Proc: TProc<IUsData> = nil); overload;
    procedure Invoke(TRS: ITransaction; Proc: TProc<IUsData> = nil); overload;
    procedure Invoke(Receiver: IUsReceiver); overload;
    procedure Invoke(TRS: ITransaction; Receiver: IUsReceiver); overload;
    procedure Invoke(DataSet: TDataSet); overload;
    procedure Invoke(TRS: ITransaction; DataSet: TDataSet); overload;
  public
    constructor Create(const aEntityID: TEntityID; aOperType: TOperType;
                 const aMethod: TPrivOper; Alter: boolean; const aSql: string);
  end;

  TOpMethodClass = class of TOpMethod;

  TSqlDict = TDictStr<string>;

var
  //cmd line param -dbg:[0..4]; 0: no debug else IDBQuery.ParamsDebug(SqlDebug -1)
  SqlDebug: integer = 0;

implementation
uses StrUtils, usClasses, TypInfo;

var
  fConnection: IConnection = nil;

function GetSql(const aEntityID: TEntityID;
         const aMethod: TPrivOper; Alter: boolean; out Sql: string): TOperType;
const
  QRY = 'select OPER,OPTYPE,SQL from GET_ENTITIES_INFO(:ENTITY,:OPER)';
  ALT = 'select first 1 OPER,OPTYPE,SQL from GET_ENTITIES_INFO(:ENTITY)'
       +' where OPER in(:OPSEL, :OPGET) order by OPER';
var
  us: IUsData;
begin
  Sql:= '';
  us:= QPrepare(ifthen(Alter and SameText(aMethod, OP_SELECT), ALT, QRY))
       .SetParams([aEntityID, UpperCase(aMethod), OP_GETROW])
       .Open
  ;
  if us.EOF then
    exit(optSelect);
  sql:= us.Values[2];
  // Если нужен OP_GETROW, а такового нет, предполагается, что в теле запроса
  // OP_SELECT есть нужный where (и доп. поля) под комментарием /*/ ... /*/
  if Alter and SameText(us.Values[0], OP_SELECT) then
    sql:= StringReplace(sql, '/*/', '', [rfReplaceAll]);
  result:= TOperType(us.Values[1]);
end;

{ TOpMethod }

constructor TOpMethod.Create(const aEntityID: TEntityID; aOperType: TOperType;
                const aMethod: TPrivOper; Alter: boolean; const aSql: string);
begin
  fEntityID:= aEntityID;
  fOper:= UpperCase(aMethod);
  fOperType:= aOperType;
  fAlter:= Alter;
  fQry:= fConnection.QPrepare(aSql);
end;

function TOpMethod.GetParam(index: integer): Variant;
begin
  result:= fQry.GetParamValue(index);
end;

function TOpMethod.GetParam(const ParamName: string): Variant;
begin
  result:= fQry.GetParamValue(ParamName);
end;

procedure TOpMethod.Invoke(Receiver: IUsReceiver);
begin
  Invoke(nil, Receiver);
end;

procedure TOpMethod.Invoke(TRS: ITransaction; Receiver: IUsReceiver);
begin
  Invoke(TRS, procedure(us: IUsData)
    begin
      Receiver.CopyData(us);
    end
  );
end;

function OpType(const Oper: TPrivOper): TOperType;
const //    01...5..8.0....5....0.2
  STD_OPS = 'SELECT,INSERT,DELETE,UPDATE,';
begin
  case Pos(Oper + ',', STD_OPS) of
     1: result:= optSelect;
     8: result:= optInsert;
    15: result:= optDelete;
    22: result:= optUpdate;
    else
      raise Exception.Create('Альтернативный режим допустим только для стандвртных методов.');
  end;
end;

type
  TPostError = (bpeOK, bpeNoDeleted, bpeBadState, bpeIsEmpty, bpeTooMany);

procedure PostError(Code: TPostError);
const MSG = 'Ошибка редактирования. Обратитесь к разработчику.'#13#10' %s';
begin
  raise EPostError.CreateFmt(MSG, [
                Copy(GetEnumName(TypeInfo(TPostError), ord(Code)), 4, MaxInt)
  ]);
end;

procedure TOpMethod.Invoke(TRS: ITransaction; Proc: TProc<IUsData>);
  //--
  function CheckDelete: IUsData;
  var id: Variant;
     sql: string;
       q: IDBQuery;
  begin
    id:= null;
    if fQry.EOF then
      PostError(bpeIsEmpty);
    id:= fQry.Values[0];
    GetSql(fEntityID, OP_SELECT, true, sql);
    q:= QPrepare(sql, fQry.Transaction).SetParams(fQry);
    fQry.Next;
    if not fQry.EOF then
      PostError(bpeTooMany);
    if VarIsNull(id) then
      PostError(bpeIsEmpty);
    q.Exec;
    if not q.EOF then
      PostError(bpeNoDeleted);
    result:= q;
  end;
  //--
  function CheckPost: IUsData;
  var id: Variant;
     sql: string;
       q: IDBQuery;
       c: IUsDataCache;
  begin
    id:= null;
    if fQry.EOF then
      PostError(bpeIsEmpty);
    id:= fQry.Values[0];
    GetSql(fEntityID, OP_SELECT, true, sql);
    q:= QPrepare(sql, fQry.Transaction).SetParams(fQry);
    fQry.Next;
    if not fQry.EOF then
      PostError(bpeTooMany);
    if VarIsNull(id) then
      PostError(bpeIsEmpty);
    q.Exec;
    if q.EOF then
      PostError(bpeIsEmpty);
    c:= NewUsDataCache(q, 1);
    if not q.EOF then
      PostError(bpeTooMany);
    result:= c;
  end;
  //--
var qSel: IUsData;
begin
  try
    if Assigned(TRS) then
      fQry.SetTransaction((TRS as TTransaction).fTRS)
    else if (fOperType <> optSelect) and fQry.Transaction.ReadOnly then
      fQry.SetNewTransaction;  // заменить транзакцию на пишущую
    PushCursor;
    if SqlDebug > 0 then
      fQry.ParamsDebug(SqlDebug - 1);
    fQry.Exec;

    qSel:= fQry;
    if fAlter then begin
      case OpType(fOper) of
        optSelect: ;
        optDelete: qSel:= CheckDelete;
        else       qSel:= CheckPost;
      end;
    end;

    if Assigned(Proc) then
      Proc(qSel);
    fQry.Close;
  except
    fQry.Transaction.Rollback;
    raise;
  end;
end;

procedure TOpMethod.Invoke(Proc: TProc<IUsData>);
begin
  Invoke(nil, Proc);
end;

procedure TOpMethod.Invoke(TRS: ITransaction; DataSet: TDataSet);
begin
  Invoke(TRS, procedure(us: IUsData)
    begin
      UsCopyData(DataSet, us);
    end
  );
end;

procedure TOpMethod.Invoke(DataSet: TDataSet);
begin
  Invoke(nil, DataSet);
end;

function TOpMethod.ParamCount: integer;
begin
  result:= fQry.ParamCount;
end;

function TOpMethod.SetParam(index: integer; const Value: Variant): IOpMethod;
begin
  result:= self;
  fQry.ParamValues[index]:= Value;
end;

function TOpMethod.SetParam(const ParamName: string; const Value: Variant): IOpMethod;
begin
  result:= self;
  fQry.SetParamValue(ParamName, Value);
end;

function TOpMethod.SetParams(const DataSet: TDataSet): IOpMethod;
begin
  result:= self;
  fQry.SetParams(DataSet);
end;

function TOpMethod.SetParams(const DataRow: IUsData): IOpMethod;
begin
  result:= self;
  fQry.SetParams(DataRow);
end;

procedure TOpMethod.Set_Param(const ParamName: string; const Value: Variant);
begin
  fQry.SetParamValue(ParamName, Value);
end;

procedure TOpMethod.Set_Param(index: integer; const Value: Variant);
begin
  fQry.ParamValues[index]:= Value;
end;

{ TDataService }

procedure TDataService.Connect(const aURL, aLogin, aPass: string);
begin
  try
    fConnection:= DBProvider.Connect(aURL, aLogin, aPass);
//    QPrepare(SQL).Exec;
  except
    fConnection:= nil;
    raise;
  end;
end;

function TDataService.Connected: boolean;
begin
  result:= Assigned(fConnection);
end;

function TDataService.CreateOpMethod(const aEntityID: TEntityID;
                 const aMethod: TPrivOper; Alter, RaiseIf: boolean): IOpMethod;
const
  ERR = 'Сущность "%s": метод "%s" не определен или нет доступа';
var
  OperType: TOperType;
  Sql: string;
begin
  OperType:= GetSql(aEntityID, aMethod, Alter, Sql);
  if Sql = '' then
    if RaiseIf then
      raise Exception.CreateFmt(ERR, [aEntityID, aMethod])
    else Exit(nil);
  result:= TOpMethod.Create(aEntityID, OperType, aMethod, Alter, sql);
end;

procedure TDataService.Disconnect;
begin
  fConnection:= nil;
end;

function TDataService.StartTRS: ITransaction;
begin
  result:= TTransaction.Create;
end;

{ TTransaction }

constructor TTransaction.Create;
begin
  fTRS:= fConnection.NewTRS;
end;

procedure TTransaction.Commit;
begin
  fTRS.Commit;
end;

procedure TTransaction.Rollback;
begin
  fTRS.Rollback;
end;

procedure CheckDbg;
var s: string;
begin
  FindCmdLineSwitch('dbg', s);
  SqlDebug:= StrToIntDef(s, 0);
end;

initialization
  DataService:= TDataService.Create;
  CheckDbg;

finalization
  DataService:= nil;

end.
