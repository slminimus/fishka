unit uEntities;

interface
uses RTTI, Classes, Types, SysUtils, DB, slTools, usIntfs;

type
  TEntityID = string; // GUID 36
  TPrivOper = string;
const
//OP_ - operation;              DS_ - DESCRIPTION
  OP_SELECT  = 'SELECT';        DS_SELECT  = '��������';
  OP_INSERT  = 'INSERT';        DS_INSERT  = '�������';
  OP_DELETE  = 'DELETE';        DS_DELETE  = '�������';
  OP_UPDATE  = 'UPDATE';        DS_UPDATE  = '���������';
  OP_GETROW  = 'GETROW';        DS_GETROW  = '�������� ������ ��� ��������������';
// OP_GETROW: select ����� ������ (�� primary key); � ����� ������ ������� ���
//            ������� OP_SELECT � ��� ������� ��� �������������� (� ��������).
// OP_SELECT: select ��� ����������� �������.
// � OP_SELECT, � OP_GETROW ������ �������� ������ �������� primary key ������,
// ������ - �������� Human Readeble ����� (��� ����������� � LookupCombo).
// �������� ������ �������� Exception � ������ ������.

// OP_GETROW �������� �� ����������, ��� �������� ������ � ENTITY, ���� ������
// OP_SELECT � �������������� ������. ���� OP_GETROW � ������� ENTITY ��
// ���������, ������������ OP_SELECT � �������������, ��� ���. ���� �������
// � ������� ������ ������������ ������ ��������� ��� ������������� /*/ ... /*/,
// ������� � ���� ������ �� ���� ������� ���������.

// �������������� �����: �������� Alter � ������� TEntObjHelper; ���������
// �� ������ OP_SELECT..OP_UPDATE. � ������� ������ ��������������, ���
// ������ �������������� (OP_INSERT,OP_DELETE,OP_UPDATE) ���������� � usData
// ���� �������� (���� �������, ���� ������): primary key ������������ ������.
// � �������������� ������ ����������� �������� (� ������� ������), �����
// (� ��� �� ����������) ����������� OP_GETROW ��� ����������� primary key ��
// �������� ��������������. � ������ OP_DELETE ������ ���������� ������
// ���������, ��� OP_INSERT � OP_UPDATE - ���� � ������ ���� ������, �����
// ����������� Exception � ����� ����������. ������������ ����� ������ usData
// �� OP_GETROW ��� ���������� ������ � ���������� ������������.

type
  TOperType = (optSelect, optInsert, optUpdate, optDelete);

  PropsAttribute = class(TCustomAttribute)
  private
    fProps: string;
  public
    constructor Create(const aProps: string);
    property Props: string read fProps; // CSV list of pairs PropName=PropValue
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
    function  CreateOpMethod(const aEntityID: TEntityID; const aMethod: TPrivOper;
                              Alter: boolean; RaiseIf: boolean = true): IOpMethod;
    function  StartTRS: ITransaction;
  end;

  EntityAttribute = class(TCustomAttribute)
  private
    fID: string;
  public
    constructor Create(const aID: string);
    property ID: string read fID;
  end;

  CardAttribute = class(EntityAttribute);

  TAttrObjHelper = class helper for TObject
  public type
    TEnumProc<T> = reference to procedure(Attr: T; var Stop: boolean);
  public
    class function EnumAttrs<T: TCustomAttribute>(Proc: TEnumProc<T>): boolean;
  end;

  TEntObjHelper = class helper for TComponent
  public
     // ���� (Key=Value) �� Props ���� PropsAttribute
    class function AttrProps: TArray<string>;
    class function ObjMainEntityID: TEntityID;
    class function FindOpMethod(const EntityID: TEntityID; const Oper: TPrivOper;
                                   Alter: boolean; out Value): boolean; overload;
    class function GetOpMethod(const EntityID: TEntityID; const Oper: TPrivOper;
                                   Alter: boolean): IOpMethod; overload;
    // ������ ���������� ������� OpMethodAttribute �� ���� TEntity
    class function FindOpMethod(const Oper: TPrivOper; Alter: boolean;
                                                  out Value): boolean; overload;
    class function GetOpMethod(const Oper: TPrivOper; Alter: boolean): IOpMethod; overload;
  end;

var
  DataService: IDataService = nil;

implementation


const
  ER_METHOD_NOT_FOUND = '����� �� ������: %s.%s';


{ PropsAttribute }

constructor PropsAttribute.Create(const aProps: string);
begin
  fProps:= aProps;
end;

{ EntityAttribute }

constructor EntityAttribute.Create(const aID: string);
begin
  fID:= aID;
end;

{ TAttrObjHelper }

class function TAttrObjHelper.EnumAttrs<T>(Proc: TEnumProc<T>): boolean;
var
  rc: TRttiContext;
   a: TCustomAttribute;
   stop: boolean;
begin
  result:= false;
  rc:= TRttiContext.Create;
  try
    stop:= false;
    for a in rc.GetType(Self).GetAttributes do begin
      if not a.InheritsFrom(T) then Continue;
      Proc(T(a), stop);
      if not stop then Continue;
      result:= true;
      break;
    end;
  finally
    rc.Free;
  end;
end;

{ TEntObjHelper }

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

class function TEntObjHelper.ObjMainEntityID: TEntityID;
var id: TEntityID;
begin
  id:= '';
  EnumAttrs<EntityAttribute>(
    procedure(Entity: EntityAttribute; var Stop: boolean)
    begin
      Stop:= true;
      id:= Entity.ID;
    end
  );
  result:= UpperCase(id);
end;

class function TEntObjHelper.FindOpMethod(const EntityID: TEntityID;
               const Oper: TPrivOper; Alter: boolean; out Value): boolean;
var v: IOpMethod;
begin
  v:= nil;
  EnumAttrs<EntityAttribute>(
    procedure(Entity: EntityAttribute; var Stop: boolean)
    var vv: IOpMethod;
    begin
      if EntityID <> '' then
        if not SameText(Entity.ID, EntityID) then exit;
      vv:= DataService.CreateOpMethod(Entity.ID, Oper, Alter, false);
      Stop:= Assigned(vv);
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

class function TEntObjHelper.FindOpMethod(const Oper: TPrivOper; Alter: boolean;
                                                            out Value): boolean;
begin
  result:= FindOpMethod('', Oper, Alter, Value);
end;

class function TEntObjHelper.GetOpMethod(const EntityID: TEntityID;
                              const Oper: TPrivOper; Alter: boolean): IOpMethod;
begin
  if not FindOpMethod(EntityID, Oper, Alter, result) then
    raise Exception.CreateFmt('%s Method not found: %s', [ClassName, Oper]);
end;

class function TEntObjHelper.GetOpMethod(const Oper: TPrivOper; Alter: boolean): IOpMethod;
begin
  if not FindOpMethod(Oper, Alter, result) then
    raise Exception.CreateFmt('%s Method not found: %s', [ClassName, Oper]);
end;

end.
