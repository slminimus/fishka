unit uEntities;

interface
uses RTTI, Classes, Types, SysUtils, DB, usTools, usIntfs;

type
  TEntityID = string; // GUID 36
  TPrivOper = string;
const // OP_ - privileged operation; DS_ - DESCRIPTION
  OP_SELECT  = 'Select';
  DS_SELECT  = 'Просмотр';
  OP_INSERT  = 'Insert';
  DS_INSERT  = 'Создать';
  OP_DELETE  = 'Delete';
  DS_DELETE  = 'Удалить';
  OP_UPDATE  = 'Update';
  DS_UPDATE  = 'Исправить';
  OP_GETROW  = 'GetRow';
  DS_GETROW  = 'Получить строку для редактирования';
// OP_GETROW: select одна строка (по ID); в отбор должны попасть все поля
//             OP_SELECT и все поля для редактирования (в карточке).
// OP_SELECT: select для отображения списком.
// И OP_SELECT, и OP_GETROW должны селектировать превым столбцом ID строки,
// вторым - основной Human Readeble текст (для отображения в LookupCombo).
// Прочие операции должны вызывать Exception в случае ошибки.

type
  TOperType = (optSelect, optInsert, optUpdate, optDelete);

  PropsAttribute = class(TCustomAttribute)
  private
    fProps: string;
  public
    constructor Create(const aProps: string);
    property Props: string read fProps; // CSV list of pairs PropName=PropValue
  end;

  TPrivAttribute = class(TCustomAttribute)
  private
    fID: string;
    fName: string;
  public
    constructor Create(const aID: string; const aName: string = '');
    property ID: string read fID;
    property Name: string read fName;
  end;

  EntityAttribute = class(TPrivAttribute);

  OpMethodAttribute = class(TCustomAttribute)
  private
    fOperType: TOperType;
    fOper: string;
    fName: string;
  public
    constructor Create(aOperType: TOperType; const aName, aOper: string);
    property Oper: string read fOper;
    property OperType: TOperType read fOperType;
    property Name: string read fName;
  end;

  SelectAttribute = class(OpMethodAttribute)
  public
    constructor Create(const aName: string = DS_SELECT;
                       const aOper: string = OP_SELECT);
  end;

  InsertAttribute = class(OpMethodAttribute)
  public
    constructor Create(const aName: string = DS_INSERT;
                       const aOper: string = OP_INSERT);
  end;

  DeleteAttribute = class(OpMethodAttribute)
  public
    constructor Create(const aName: string = DS_DELETE;
                       const aOper: string = OP_DELETE);
  end;

  UpdateAttribute = class(OpMethodAttribute)
  public
    constructor Create(const aName: string = DS_UPDATE;
                       const aOper: string = OP_UPDATE);
  end;

  GetRowAttribute = class(OpMethodAttribute)
  public
    constructor Create(const aName: string = DS_GETROW;
                       const aOper: string = OP_GETROW);
  end;

  ITransaction = interface
    ['{2832B665-55FF-4DC9-8465-409A855753EF}']
    procedure Commit;
    procedure Rollback;
  end;

  IUsReceiver = interface
    ['{9097FDB6-4EB3-4F56-9C9E-77A184E43E18}']
    procedure CopyData(Src: IUsData);
  end;

  IOpMethod = interface
    ['{3AA82F5B-5971-486B-8EB2-17E04D232BB7}']
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

    property  Params[index: integer]: Variant read GetParam write Set_Param;
    property  Params[const ParamName: string]: Variant read GetParam write Set_Param;
  end;

  IDataService = interface
    ['{4BD35CBD-6CB9-4542-B1EA-AB194588AC3C}']
    procedure Connect(const aURL, aLogin, aPass: string);
    procedure Disconnect;
    function  Connected: boolean;
    function  CreateOpMethod(const EntityID: TEntityID; const MAttr: OpMethodAttribute): IOpMethod;
    function  StartTRS: ITransaction;
    procedure ClearCache;
  end;

  TEntity = class(TCustomAttribute)
  protected
    class procedure GetInfo(out aID, aName: string);
  public
    class function _FindOpMethod(const Oper: TPrivOper; out Value): boolean; overload;
    class function _GetOpMethod(const Oper: TPrivOper): IOpMethod; overload;
    class function GetID: string;
    class function GetName: string;
  end;

  TEntObjHelper = class helper(TAttrObjHelper) for TObject
  public
     // пары (Key=Value) из Props всех PropsAttribute
    class function AttrProps: TArray<string>;
    class function ObjMainEntityID: TEntityID;
    class function FindOpMethod(const EntityID: TEntityID; const Oper: TPrivOper; out Value): boolean; overload;
    class function GetOpMethod(const EntityID: TEntityID; const Oper: TPrivOper): IOpMethod; overload;
    // первый подходящий атрибут OpMethodAttribute из всех TEntity
    class function FindOpMethod(const Oper: TPrivOper; out Value): boolean; overload;
    class function GetOpMethod(const Oper: TPrivOper): IOpMethod; overload;
  end;

var
  DataService: IDataService = nil;

implementation

{ TPrivAttribute }

constructor TPrivAttribute.Create(const aID, aName: string);
begin
  fID:= aID;
  fName:= aName;
end;

{ OpMethodAttribute }

constructor OpMethodAttribute.Create(aOperType: TOperType; const aName, aOper: string);
begin
  fOperType:= aOperType;
  fOper:= aOper;
  fName:= aName;
end;

{ SelectAttribute }

constructor SelectAttribute.Create(const aName, aOper: string);
begin
  inherited Create(optSelect, aName, aOper);
end;

{ InsertAttribute }

constructor InsertAttribute.Create(const aName, aOper: string);
begin
  inherited Create(optInsert, aName, aOper);
end;

{ DeleteAttribute }

constructor DeleteAttribute.Create(const aName, aOper: string);
begin
  inherited Create(optDelete, aName, aOper);
end;

{ UpdateAttribute }

constructor UpdateAttribute.Create(const aName, aOper: string);
begin
  inherited Create(optUpdate, aName, aOper);
end;

{ GetRowAttribute }

constructor GetRowAttribute.Create(const aName, aOper: string);
begin
  inherited Create(optSelect, aName, aOper);
end;

const
  ER_METHOD_NOT_FOUND = 'Метод не найден: %s.%s';

class procedure TEntity.GetInfo(out aID, aName: string);
var _id, _name: string;
begin
  _id:= '';
  _name:= '';
  EnumAttrs<EntityAttribute>(
    procedure(Entity: EntityAttribute; var Stop: boolean)
    begin
      Stop:= true;
      _id:= Entity.ID;
      _name:= Entity.Name;
    end
  );
  aID:= _id;
  aName:= _name;
end;

class function TEntity.GetID: string;
var dummy: string;
begin
  GetInfo(result, dummy);
end;

class function TEntity.GetName: string;
var dummy: string;
begin
  GetInfo(dummy, result);
end;

class function TEntity._FindOpMethod(const Oper: TPrivOper; out Value): boolean;
var v: IOpMethod;
begin
  v:= nil;
  result:= EnumAttrs<OpMethodAttribute>(
    procedure(OM: OpMethodAttribute; var Stop: boolean)
    begin
      if not SameText(Oper, OM.Oper) then exit;
      Stop:= true;
      v:= DataService.CreateOpMethod(GetID, OM);
    end
  );
  if Assigned(v) then
    IOpMethod(Value):= v;
end;

class function TEntity._GetOpMethod(const Oper: TPrivOper): IOpMethod;
begin
  if not _FindOpMethod(Oper, result) then
    raise Exception.CreateFmt('%s Method not found: %s', [ClassName, Oper]);
end;

{ TEntObjHelper }

class function TEntObjHelper.ObjMainEntityID: TEntityID;
var id: TEntityID;
begin
  id:= '';
  EnumAttrs<TEntity>(
    procedure(Entity: TEntity; var Stop: boolean)
    begin
      Stop:= true;
      id:= Entity.GetID;
    end
  );
  result:= UpperCase(id);
end;

class function TEntObjHelper.FindOpMethod(const EntityID: TEntityID; const Oper: TPrivOper; out Value): boolean;
var v: IOpMethod;
begin
  v:= nil;
  EnumAttrs<TEntity>(
    procedure(Entity: TEntity; var Stop: boolean)
    var vv: IOpMethod;
    begin
      if EntityID <> '' then
        if not SameText(Entity.GetID, EntityID) then exit;
      vv:= nil;
      Stop:= Entity._FindOpMethod(Oper, vv);
      if Stop then
        v:= vv
      else
        Stop:= EntityID <> '';
    end
  );
  result:= Assigned(v);
  if result then
    IOpMethod(Value):= v;
end;

class function TEntObjHelper.AttrProps: TArray<string>;
var
  lst: TStringList;
begin
  lst:= TStringList.Create;
  try
    EnumAttrs<PropsAttribute>(
      procedure(PA: PropsAttribute; var Stop: boolean)
      var a: TArray<string>;
          s: string;
      begin
        for s in PA.Props.Split([',']) do begin
          a:= s.Split(['=']);
          if Length(a) <> 2 then Continue;
          Lst.Values[a[0]]:= a[1];
        end;
      end
    );
    result:= lst.ToStringArray;
  finally
    lst.Free;
  end;
end;

class function TEntObjHelper.FindOpMethod(const Oper: TPrivOper; out Value): boolean;
begin
  result:= FindOpMethod('', Oper, Value);
end;

class function TEntObjHelper.GetOpMethod(const EntityID: TEntityID;
                                             const Oper: TPrivOper): IOpMethod;
begin
  if not FindOpMethod(EntityID, Oper, result) then
    raise Exception.CreateFmt('%s Method not found: %s', [ClassName, Oper]);
end;

class function TEntObjHelper.GetOpMethod(const Oper: TPrivOper): IOpMethod;
begin
  if not FindOpMethod(Oper, result) then
    raise Exception.CreateFmt('%s Method not found: %s', [ClassName, Oper]);
end;

{ PropsAttribute }

constructor PropsAttribute.Create(const aProps: string);
begin
  fProps:= aProps;
end;

end.
