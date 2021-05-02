unit uEntities;

interface
uses RTTI, Classes, Types, SysUtils, usTools, usIntfs;

const // OP_ - privileged operation; DS_ - DESCRIPTION
  OP_SELECT = 'Select';
  DS_SELECT = 'Просмотр';
  OP_INSERT = 'Insert';
  DS_INSERT = 'Создать';
  OP_DELETE = 'Delete';
  DS_DELETE = 'Удалить';
  OP_UPDATE = 'Update';
  DS_UPDATE = 'Изменить';

type
  TOperType = (optSelect, optInsert, optDelete, optUpdate);

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

  MethodAttribute = class(TPrivAttribute)
  private
    fOperType: TOperType;
    fOper: string;
  public
    constructor Create(aOperType: TOperType; const aID, aName, aOper: string);
    property Oper: string read fOper;
    property OperType: TOperType read fOperType;
  end;

  SelectAttribute = class(MethodAttribute)
  public
    constructor Create(const aID: string; const aName: string = DS_SELECT;
                                          const aOper: string = OP_SELECT);
  end;

  InsertAttribute = class(MethodAttribute)
  public
    constructor Create(const aID: string; const aName: string = DS_INSERT;
                                          const aOper: string = OP_INSERT);
  end;

  DeleteAttribute = class(MethodAttribute)
  public
    constructor Create(const aID: string; const aName: string = DS_DELETE;
                                          const aOper: string = OP_DELETE);
  end;

  UpdateAttribute = class(MethodAttribute)
  public
    constructor Create(const aID: string; const aName: string = DS_UPDATE;
                                          const aOper: string = OP_UPDATE);
  end;

  ITransaction = interface
    ['{2832B665-55FF-4DC9-8465-409A855753EF}']
    procedure Commit;
    procedure Rollback;
  end;

  IMethod = interface
    ['{3AA82F5B-5971-486B-8EB2-17E04D232BB7}']
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

    property  Params[index: integer]: Variant read GetParam write Set_Param;
    property  Params[const ParamName: string]: Variant read GetParam write Set_Param;
  end;

  IDataService = interface
    ['{4BD35CBD-6CB9-4542-B1EA-AB194588AC3C}']
    procedure Connect(const aURL, aLogin, aPass: string);
    procedure Disconnect;
    function  Connected: boolean;
    function  CreateMethod(const MAttr: MethodAttribute): IMethod;
    function  StartTRS: ITransaction;
    procedure ClearCache;
  end;

  TEntity = class(TCustomAttribute)
  protected
    procedure GetInfo(out aID, aName: string);
  public
    function GetID: string;
    function GetName: string;
  end;

  TEntObjHelper = class helper(TAttrObjHelper) for TObject
  public
    // только свои атрибуты MethodAttribute
    class function Find_Method(ByID: boolean; const OperOrID: string; out Value): boolean;
    // первый подходящий атрибут MethodAttribute из всех TEntity
    class function Find_Methods(ByID: boolean; const OperOrID: string; out Value): boolean;
    class function Get_Method(ByID: boolean; const OperOrID: string): IMethod;
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

{ MethodAttribute }

constructor MethodAttribute.Create(aOperType: TOperType; const aID, aName, aOper: string);
begin
  inherited Create(aID, aName);
  fOperType:= aOperType;
  fOper:= aOper;
end;

{ SelectAttribute }

constructor SelectAttribute.Create(const aID, aName, aOper: string);
begin
  inherited Create(optSelect, aID, aName, aOper);
end;

{ InsertAttribute }

constructor InsertAttribute.Create(const aID, aName, aOper: string);
begin
  inherited Create(optInsert, aID, aName, aOper);
end;

{ DeleteAttribute }

constructor DeleteAttribute.Create(const aID, aName, aOper: string);
begin
  inherited Create(optDelete, aID, aName, aOper);
end;

{ UpdateAttribute }

constructor UpdateAttribute.Create(const aID, aName, aOper: string);
begin
  inherited Create(optUpdate, aID, aName, aOper);
end;

const
  ER_METHOD_NOT_FOUND = 'Метод не найден: %s.%s';

procedure TEntity.GetInfo(out aID, aName: string);
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

function TEntity.GetID: string;
var dummy: string;
begin
  GetInfo(result, dummy);
end;

function TEntity.GetName: string;
var dummy: string;
begin
  GetInfo(dummy, result);
end;

{ TEntObjHelper }

class function TEntObjHelper.Find_Method(ByID: boolean; const OperOrID: string; out Value): boolean;
var v: IMethod;
begin
  v:= nil;
  result:= EnumAttrs<MethodAttribute>(
    procedure(MAttr: MethodAttribute; var Stop: boolean)
    begin
      if ByID then
        Stop:= SameText(OperOrID, MAttr.ID)
      else
        Stop:= SameText(OperOrID, MAttr.Oper);
      if Stop then
        v:= DataService.CreateMethod(MAttr);
    end
  );
  if Assigned(v) then
    IMethod(Value):= v;
end;

class function TEntObjHelper.Find_Methods(ByID: boolean; const OperOrID: string; out Value): boolean;
var v: IMethod;
begin
  v:= nil;
  result:= EnumAttrs<TEntity>(
    procedure(Entity: TEntity; var Stop: boolean)
    var vv: IMethod;
    begin
      vv:= nil;
      Stop:= Entity.Find_Method(ByID, OperOrID, vv);
      if Stop then
        v:= vv;
    end
  );
  if Assigned(v) then
    IMethod(Value):= v;
end;

class function TEntObjHelper.Get_Method(ByID: boolean;
                                              const OperOrID: string): IMethod;
begin
  if not Find_Methods(ByID, OperOrID, result) then
    raise Exception.CreateFmt('%s Method not found: %s', [ClassName, OperOrID]);
end;

end.
