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
uses Classes, Types, SysUtils, usIntfs, usTools, Variants, DB, usDb,
     uifProvider, uDBProvider, uFBProvider, uEntities;

type
  TDataService = class(TInterfacedObject, IDataService)
  private
    procedure Connect(const aURL, aLogin, aPass: string);
    procedure Disconnect;
    function  Connected: boolean;
    function  CreateOpMethod(const aEntityID: TEntityID; const aMethod: string;
                                           RaiseIf: boolean = true): IOpMethod;
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
    constructor Create(const aEntityID: TEntityID; aOperType: TOperType;
                       const aMethod, aSql: string);
  end;

  TSqlDict = TDictStr<string>;

implementation

var
  fConnection: IConnection = nil;

{ TOpMethod }

constructor TOpMethod.Create(const aEntityID: TEntityID; aOperType: TOperType;
                                                 const aMethod, aSql: string);
begin
  fEntityID:= aEntityID;
  fOper:= aMethod;
  fOperType:= aOperType;
  if fOperType = optSelect then
    fQry:= fConnection.QPrepare(aSql)
  else
    fQry:= fConnection.QPrepareWR(aSql);
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

procedure TDataService.Connect(const aURL, aLogin, aPass: string);
const я = #13#10;
  SQL =
       'select'
    +я+'   rdb$set_context(''USER_SESSION'',''ID_USER'',x''2ED08DF89E2E48D8BDA8C99176CC4A02'')'
    +я+'  ,rdb$set_context(''USER_SESSION'',''USER_NAME'',''SYSDBA'')'
    +я+'  ,rdb$set_context(''USER_SESSION'',''KODFIL'',1000)'
    +я+'from RDB$DATABASE'
  ;

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
                          const aMethod: string; RaiseIf: boolean): IOpMethod;
const
  ERR = 'Сущность "%s": метод "%s" не определен или нет доступа';
  SQL = 'select OPTYPE,SQL from GET_ENTITIES_INFO(:ENTITY,:OPER)';
var
  us: IUsData;
begin
  us:= QPrepare(SQL).SetParams([aEntityID, aMethod]).Open;
  if us.EOF then
    if RaiseIf then
      raise Exception.CreateFmt(ERR, [aEntityID, aMethod])
    else exit(nil);
  result:= TOpMethod.Create(aEntityID, us.Values[0], aMethod, us.Values[1]);
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

finalization
  DataService:= nil;

end.
