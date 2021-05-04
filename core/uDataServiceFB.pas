{-----------------------------------------------------------------------------
 Unit Name: uDataServiceFB
 Author:    sl
 Date:      14-���-2021
 Purpose:
   DataService, ���������� FireBird, ���������.
���������� �������� ���� ���� � ������, �� ���� �������������� �
������ uEntities (��. ������ initialization).

-----------------------------------------------------------------------------}

unit uDataServiceFB;

interface
uses Classes, Types, SysUtils, usIntfs, usTools, Variants, DB, usDb,
     uifProvider, uDBProvider, uFBProvider, uEntities;

type
  TDataService = class(TInterfacedObject, IDataService)
  private
    procedure Connect(const aURL, aLogin, aPass: string);
    procedure Disconnect;
    function  Connected: boolean;
    function  CreateOpMethod(const EntityID: TEntityID; const MAttr: OpMethodAttribute): IOpMethod;
    function  StartTRS: ITransaction;
    procedure ClearCache;
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
    fOper: string;
    fName: string;
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
    constructor Create(const aEntityID: TEntityID; const MAttr: OpMethodAttribute);
  end;

  TSqlDict = TDictStr<string>;

implementation

var
  fConnection: IConnection = nil;

  SqlDict: TSqlDict = nil; // ��� ��������

function FindSql(const EntityID: TEntityID; const Oper: TPrivOper): string;
const
  SQL = 'select OPER_TYPE,OPER,NAME,SQL from SQLS$VW where ID = :ID and OPER = :OPER';
  SQL_UNKNOWN = '������ �� ������ (%s)'#13#10'ID %s';
var
  v: Variant;
  id: string;
begin
  id:= EntityID +':'+ Oper;
  if SqlDict.TryGetValue(id, result) then exit;
  PushCursor;
  v:= QPrepare(SQL).SetParams([EntityID, Oper]).Open.FieldValue('SQL');
  if VarIsNull(v) then
    raise Exception.CreateFmt(SQL_UNKNOWN, [Oper, EntityID]);
  result:= v;
  SqlDict.Add(id, result);
end;

{ TOpMethod }

constructor TOpMethod.Create(const aEntityID: TEntityID; const MAttr: OpMethodAttribute);
var sql: string;
begin
  fEntityID:= aEntityID;
  fOper:= MAttr.Oper;
  fOperType:= MAttr.OperType;
  fName:= MAttr.Name;
  sql:= FindSql(fEntityID, fOper);
  if fOperType = optSelect then
    fQry:= fConnection.QPrepare(sql)
  else
    fQry:= fConnection.QPrepareWR(sql);
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

procedure TOpMethod.Invoke(TRS: ITransaction; Proc: TProc<IUsData>);
begin
  try
    if Assigned(TRS) then
      fQry.SetTransaction((TRS as TTransaction).fTRS);
    PushCursor;
//fQry.ParamsDebug(3);
    fQry.Exec;
    if Assigned(Proc) then
      Proc(fQry);
  finally
    fQry.Close;
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
      DataSet.CopyData(us);
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

procedure TDataService.ClearCache;
begin
  SqlDict.Clear;
end;

procedure TDataService.Connect(const aURL, aLogin, aPass: string);
const � = #13#10;
  SQL =
       'select'
    +�+'   rdb$set_context(''USER_SESSION'',''ID_USER'',x''2ED08DF89E2E48D8BDA8C99176CC4A02'')'
    +�+'  ,rdb$set_context(''USER_SESSION'',''USER_NAME'',''SYSDBA'')'
    +�+'  ,rdb$set_context(''USER_SESSION'',''KODFIL'',1000)'
    +�+'from RDB$DATABASE'
  ;

begin
  try
    fConnection:= DBProvider.Connect(aURL, aLogin, aPass);
    QPrepare(SQL).Exec;
  except
    fConnection:= nil;
    raise;
  end;
end;

function TDataService.Connected: boolean;
begin
  result:= Assigned(fConnection);
end;

function TDataService.CreateOpMethod(const EntityID: TEntityID; const MAttr: OpMethodAttribute): IOpMethod;
begin
  result:= TOpMethod.Create(EntityID, MAttr);
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

initialization
  DataService:= TDataService.Create;
  SqlDict:= TSqlDict.Create;

finalization
  FreeAndNil(SqlDict);

end.
