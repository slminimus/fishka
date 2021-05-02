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
uses Classes, Types, SysUtils, usIntfs, usTools,
     uifProvider, uDBProvider, uFBProvider, uEntities;

type
  TDataService = class(TInterfacedObject, IDataService)
  private
    procedure Connect(const aURL, aLogin, aPass: string);
    procedure Disconnect;
    function  Connected: boolean;
    function  CreateMethod(const MAttr: MethodAttribute): IMethod;
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

  TMethod = class(TInterfacedObject, IMethod)
  protected
    fID: string;
    fOperType: TOperType;
    fOper: string;
    fName: string;
    fQry: IDBQuery;
    function  ParamCount: integer;
    function  GetParam(index: integer): Variant; overload;
    function  SetParam(index: integer; const Value: Variant): IMethod; overload;
    procedure Set_Param(index: integer; const Value: Variant); overload;
    function  GetParam(const ParamName: string): Variant; overload;
    function  SetParam(const ParamName: string; const Value: Variant): IMethod; overload;
    procedure Set_Param(const ParamName: string; const Value: Variant); overload;
    function  SetParams(const DataRow: IUsData): IMethod;

    procedure Invoke(Proc: TProc<IUsData> = nil); overload;
    procedure Invoke(TRS: ITransaction; Proc: TProc<IUsData> = nil); overload;
  public
    constructor Create(const MAttr: MethodAttribute);
  end;

  TSqlDict = TDictStr<string>;

implementation

var
  fConnection: IConnection = nil;

  SqlDict: TSqlDict = nil; // Кэш скриптов

function QueryByID(const aID: string): string;
const
  SQL = 'select OPER_TYPE,OPER,NAME,SQL from SQLS$VW where ID = :ID';
begin
  if SqlDict.TryGetValue(aID, result) then exit;
  PushCursor;
  result:= QPrepare(SQL).SetParams([aID]).Open.FieldValue('SQL');
  SqlDict.Add(aID, result);
end;

{ TMethod }

constructor TMethod.Create(const MAttr: MethodAttribute);
begin
  fID:= MAttr.ID;
  fOper:= MAttr.Oper;
  fOperType:= MAttr.OperType;
  fName:= MAttr.Name;
  fQry:= fConnection.QPrepare(QueryByID(fID));
end;

function TMethod.GetParam(index: integer): Variant;
begin
  result:= fQry.GetParamValue(index);
end;

function TMethod.GetParam(const ParamName: string): Variant;
begin
  result:= fQry.GetParamValue(ParamName);
end;

procedure TMethod.Invoke(TRS: ITransaction; Proc: TProc<IUsData>);
begin
  try
    if Assigned(TRS) then
      fQry.SetTransaction((TRS as TTransaction).fTRS);
    PushCursor;
    fQry.Exec;
    if Assigned(Proc) then
      Proc(fQry);
  finally
    fQry.Close;
  end;
end;

procedure TMethod.Invoke(Proc: TProc<IUsData>);
begin
  Invoke(nil, Proc);
end;

function TMethod.ParamCount: integer;
begin
  result:= fQry.ParamCount;
end;

function TMethod.SetParam(index: integer; const Value: Variant): IMethod;
begin
  result:= self;
  fQry.ParamValues[index]:= Value;
end;

function TMethod.SetParam(const ParamName: string; const Value: Variant): IMethod;
begin
  result:= self;
  fQry.SetParamValue(ParamName, Value);
end;

function TMethod.SetParams(const DataRow: IUsData): IMethod;
begin
  result:= self;
  fQry.SetParams(DataRow);
end;

procedure TMethod.Set_Param(const ParamName: string; const Value: Variant);
begin
  fQry.SetParamValue(ParamName, Value);
end;

procedure TMethod.Set_Param(index: integer; const Value: Variant);
begin
  fQry.ParamValues[index]:= Value;
end;

{ TDataService }

procedure TDataService.ClearCache;
begin
  SqlDict.Clear;
end;

procedure TDataService.Connect(const aURL, aLogin, aPass: string);
begin
  fConnection:= DBProvider.Connect(aURL, aLogin, aPass);
end;

function TDataService.Connected: boolean;
begin
  result:= Assigned(fConnection);
end;

function TDataService.CreateMethod(const MAttr: MethodAttribute): IMethod;
begin
  result:= TMethod.Create(MAttr);
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
