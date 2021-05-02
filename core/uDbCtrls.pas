unit uDbCtrls;

interface

uses
  System.SysUtils, System.Classes, uControllers, DB, Variants, UsIntfs;

type
  IDbController = interface(IController)
  ['{ED272725-E3ED-41D4-B1CF-0831BAA239C0}']
    // false, ���� Proc �� ���� �� ��������� (��� ������).
    function  GetData(Proc: TProc<IUsData>): boolean;
    function  ExecOper(const aOper: TPrivOper; const aData: IUsData): IUsData;
    // ������ �� �������� DBC
    function  Master(out Value: IDbController): boolean;
    procedure SetMaster(const Value: IDbController);
    // ������ ����� ��������� PrimaryKey � Master'� ��� ����������� � ��������� �������
    function  PrimaryKey: Variant;
    function  MainText: string;
  end;

  TDbController = class(TController, IController, IDbController)
  private
    fMaster: IDbController;
    procedure _SetMaster(const Value: IDbController);
  protected //-- IDbController
    function  GetData(Proc: TProc<IUsData>): boolean; virtual;
    // ������� (�� RTTI) public ����� ����������� (��� ������) � ������
    // 'op' + aOper � �������� ��� � ���������� (aData)
    // ������: public function opInsert(aData: IUsData): IUsData;
    function  ExecOper(const aOper: TPrivOper; const aData: IUsData): IUsData; virtual;
    function  Master: IDbController; overload;
    function  Master(out Value: IDbController): boolean; overload;
    procedure SetMaster(const Value: IDbController);
  protected
    procedure DoCreate; override;
    property _Master: IDbController read fMaster write _SetMaster;
  public
    // ������ ����� ��������� PrimaryKey � Master'� ��� ����������� � ��������� �������
    // �� ��������� PrimaryKey - ������ ���� DataSet, MainText - ������ ����
    function PrimaryKey: Variant; virtual;
    function MainText: string; virtual;
  end;

  TDbControllerClass = class of TDbController;

  MainDBCAttribute = class(_RequiredAttribute)
  private
    function GetCtl: TDbControllerClass;
  public
    constructor Create(Ctl: TDbControllerClass); overload;
    constructor Create(const CtlName: string); overload;
    property Ctl: TDbControllerClass read GetCtl;
  end;

implementation
{$R *.dfm}
{%CLASSGROUP 'Vcl.Controls.TControl'}
uses RTTI, TypInfo, usClasses;

{ TDbController }

procedure TDbController.DoCreate;
begin
  inherited;
  if not IsMortal then
    RefreshData(false);
end;

function TDbController.PrimaryKey: Variant;
begin
  result:= null;
end;

function TDbController.GetData(Proc: TProc<IUsData>): boolean;
begin
  result:= false;
end;

function TDbController.ExecOper(const aOper: TPrivOper; const aData: IUsData): IUsData;
const
  MSG = '%s.%s: �������� �� ����������';
var
  rc: TRttiContext;
   a: TArray<TRttiMethod>;
   v: TValue;
begin
  result:= nil;
  CheckEnabled(aOper);
  rc:= TRttiContext.Create;
  try
    a:= rc.GetType(ClassType).GetMethods('op'+ aOper);
    if Length(a) = 0 then
      raise Exception.CreateFmt(MSG, [ClassName, aOper]);
    TValue.Make<IUsData>(aData, v);
    result:= a[0].Invoke(Self, [v]).AsType<IUsData>;
  finally
    rc.Free;
  end;
end;

function TDbController.MainText: string;
begin
  result:= '';
end;

procedure TDbController._SetMaster(const Value: IDbController);
begin
  fMaster := Value;
end;

function TDbController.Master: IDbController;
begin
  if not Supports(_Master, IDbController, result) then
    result:= nil;
end;

function TDbController.Master(out Value: IDbController): boolean;
begin
  result:= Supports(_Master, IDbController, Value);
end;

procedure TDbController.SetMaster(const Value: IDbController);
begin
  _SetMaster(Value);
end;

{ MainDBCAttribute }

constructor MainDBCAttribute.Create(const CtlName: string);
const
  MSG1 = 'MainDBCAttribute.Create: ����� �� ������: "%s"';
  MSG2 = 'MainDBCAttribute.Create: ����� "%s" �� �������� ����������� �� TDbControllerClass';
var cc: TControllerClass;
begin
  if CtlName = '' then  exit;
  if not TController.FindCtlClass(CtlName, cc) then
    raise Exception.CreateFmt(MSG1, [CtlName]);
  if not cc.InheritsFrom(TDbController) then
    raise Exception.CreateFmt(MSG2, [CtlName]);
  Create(TDbControllerClass(cc));
end;

function MainDBCAttribute.GetCtl: TDbControllerClass;
begin
  result:= TDbControllerClass(fCtrls[0]);
end;

constructor MainDBCAttribute.Create(Ctl: TDbControllerClass);
begin
  SetLength(fCtrls, 1);
  fCtrls[0]:= Ctl;
end;

end.
