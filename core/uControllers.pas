{-----------------------------------------------------------------------------
 Unit Name: uControllers
 Author:    sl.minimus@gmail.com
 Date:      01-12-2016
 Purpose:   Управляемые TDataModule с вариантами "времени жизни".
 History:
-----------------------------------------------------------------------------}

unit uControllers;
interface
{$REGION 'Info'}
(* Приложение часто требует наличия в памяти кэшированных справочников, к
которым будут иметь доступ многие модули, например, через DbLookup-компоненты.
Естественное решение - создать DataModule, оснастить его, скажем,
ClientDataSet'ами, начитать данные и связать с этим DataModule формы-клиенты.
Остается вопрос управления временем жизни таких справочных DataModule.
Инициализировать их сразу при запуске приложения может быть накладно и
бессмысленно, т.к. пользователь может и не открыть те формы, что используют
справочники. Логично возложить контроль за созданием/разрушением справочных
DataModules (назовем их Controller'ами) на самих клиентов.

В настоящей реализации используются механизм атрибутов (TCustomAttribute), RTTI
и возможности класса System.Classes.TReader, который при создании модуля
(TForm или TDatModule) читает и интерпретирует его *.dfm.

Как это выглядит для пользователя:
---------------------------------

Для создания нового контроллера выполняйте визуальное наследование от
TСontroller (или его потомка) и добавьте новому классу атрибут MController,
указав желаемый LifeMode. Если атрибут не указывать, LifeMode считается
lmMortal.

Аналогично, формы, использующие такие контроллеры, наследуются от TBaseForm
(модуль uBaseForms).
Затем в дизайнере компоненты связываются обычным порядком; например, в качестве
TDataSet у TDataSource формы назначаем TClientDataSet контроллера.
Если форма не имеет явных ссылок на компоненты контроллера, но контроллер к ней
создавать все равно надо, форме можно добавить атрибут MRequired, указав ему
в параметрах необходимые классы контроллеров (или их имена).

!!! Все контроллеры необходимо убрать из Autocreate !!!

В работающем приложении контроллеры создаются автоматически при первой
необходимости, разрушаются в соответствии с их LifeMode:

 - lmMortal    : instance создается для каждого клиента, разрушается вместе с
                 клиентом;
 - lmSingleton : instance создается при создании первого его клиента, все
                 клиенты ссылаются на этот единственный instance, разрушается
                 вместе с последним клиентом;
 - lmPersistent: instance создается при создании первого его клиента, все
                 клиенты ссылаются на этот единственный instance, разрушается
                 при завершении приложения (разрушении Application).

Если TBaseForm в качестве предка почему-то не устраивает, нужную
функциональность легко добавить в любую форму, просто скопировав метод
TCustView.ReadState и добавив поле fCtrls.

ВАЖНО:
-----------------
Если класс никак явно не используется в коде приложения, оптимизатор его
выбросит, и через RTTI к нему будет не добраться. Вариантов борьбы несколько:
  - {$STRONGLINKTYPES ON} в модуле проекта; минус один и большой: в исполняемый
    файл попадает RTTI вообще всех классов, что заметно раздувает EXE;
  - всегда перечислять Controller'ы модуля в его атрибутах MRequired, причем
    именно типы, а не имена. Минусы очевидны.
  - в каждом модуле контроллера в секции Initialization вызывать метод
    TController.Register, который выполнит регистрацию класса модуля.
Ну и подождать, пока Embarcadero допилит опцию {M+}, что б она таки заработала, 
как полагается. На этот случай имеется class constructor TController.ClassCreate
(пока закомментаренный), который (используя RTTI) регистрирует всех наследников
TController, объявленных в проекте.

Как это работает:
-----------------
Для загрузки формы из *.dfm в VCL существует класс TReader. У TReader есть
Event, в обработчике которого можно организовать свой алгоритм восстановления
ссылок компонент создаваемого модуля на компоненты другого модуля. Наш
TFormReader ищет класс искомого модуля (FindClass), читает его LifeMode из
атрибута MController и вызывает метод класса Reborn, который или создает
новый Instance, или находит существующий: в соответствии со значением LifeMode.
Дальше работают штатные методы VCL, выполняя связывание.
TFormReader также заполняет массив fCtrls, передаваемый ему в конструкторе,
интерфейсными ссылками (IController) на те контроллеры, на которые есть ссылки
из компонент клиента, и указанные в атрибуте(-ах) MRequired. Список
fCtrls уникализирован.
Следовательно, эти контроллеры будут "жить", как минимум, пока "жив" их
последний клиент. Поскольку контроллер с LifeMode = lmMortal создается для
каждого клиента, на него имеется только одна ссылка, и он "умрет" вместе с
клиентом. Если LifeMode = lmSingleton, все его клиенты получают ссылку на один
и тот же модуль, поэтому lmSingleton живет "до последнего клиента".
Контроллер с LifeMode = lmPersistent отличается от lmSingleton только тем, что
при создании вызывает собственный _AddRef, чем обеспечивает себе "бессмертие";
а при разрушении Application вызывает собственный _Release для самоубийства.
*)
{$ENDREGION 'Info'}

uses Classes, Types, UITypes, SysUtils, RTTI, Generics.Collections,
     //Windows, //<-- нужен, если использовать OutputDebugStr
     CornDefs, uEntities, Forms;

type
  TLifeMode = (
      lmPrivate,   // каждому владельцу свой экземпляр
      lmShared,    // один экземпляр на всех; разрушается с обнулением последней ссылки
      lmPersistent // один экземпляр на всех; живет до завершения приложения
  );

  TPrivOper = type string;

  TAccessID  = TGuidString;  // TD объекта доступа (то, на что раздаются права)
  TPrivOpers = array of TPrivOper;

  TOperationsHelper = record helper for TPrivOpers
    function Contains(const aOperation: TPrivOper): boolean;
    function AsString: string;
    class function FromString(const s: string): TPrivOpers; static;
  end;

  TController = class;
  TControllerClass = class of TController;
  TCustomAttributeClass = class of TCustomAttribute;

  IController = interface
  ['{E22C237F-3FC3-4E2E-BE13-23FD9E18D8AC}']
    function  Instance: TController; overload;
    function  Instance(CtlType: TControllerClass; out V): boolean; overload;
    function  Instance(const CtlClassName: string; out V): boolean; overload;
    function  IsEnabled(const aOper: TPrivOper): boolean;
    function  IsMortal: boolean;  // для удобства, эквивалент LifeMode = lmMortal
    function  LifeMode: TLifeMode;
    function  Opers: TPrivOpers;
    // force: false - только если не активен; true - безусловно.
    procedure RefreshData(force: boolean); overload;
    procedure RefreshData(const aAccessID: TAccessID); overload; // force = true
    //-- like properties
    function  AccessID: TAccessID;
  end;

//  TReqDict = TDictionary<TControllerClass, IController>;
  TCtlClassArray = TArray<TControllerClass>;
  TControllerArray = TArray<IController>;
  TCtrls = TControllerArray;

  TCtrlsHelper = record helper for TCtrls
      // обе функции ищут строго указанный тип
    function FindCtrl(const aClassName: string; out ctl): boolean; overload;
    function FindCtrl(const CtrlClassType: TControllerClass; out ctl): boolean; overload;
  end;

  TObjEntHelper = class helper for TObject
  protected
  public
    // вызывать Proc для каждого атрибута класса T, но не более Count раз. (Count < 0 - неограничено)
    class procedure _Attrs<T: TCustomAttribute>(Count: integer; Proc: TProc<T>); overload;
    class procedure _Attrs<T: TCustomAttribute>(Proc: TProc<T>); overload; // Count = -1
    // Создать или найти контроллеры из Ctrls каждого атрибута MRequired,
    // заполнить результат ссылками на интерфейсы этих контроллеров
    procedure _GetCtrls(var Ctrls: TCtrls);
  end;

  // Атрибуты контроллера
  ControllerAttribute = class(TCustomAttribute)
  protected
    fLifeMode: TLifeMode;
  public
    constructor Create; overload;
    constructor Create(aLifeMode: TLifeMode); overload;
    function IsMortal: boolean; inline;
  public
    property LifeMode: TLifeMode read fLifeMode;
  end;

  _RequiredAttribute = class(TCustomAttribute)
  protected
    fCtrls: TCtlClassArray;
  end;

  // Привязка контроллера(-ов) к View
  RequiredAttribute = class(_RequiredAttribute)
  public
    constructor Create(Ctrl0: TControllerClass; Ctrl1: TControllerClass = nil;
                 Ctrl2: TControllerClass = nil; Ctrl3: TControllerClass = nil;
                 Ctrl4: TControllerClass = nil; Ctrl5: TControllerClass = nil;
                 Ctrl6: TControllerClass = nil; Ctrl7: TControllerClass = nil;
                 Ctrl8: TControllerClass = nil; Ctrl9: TControllerClass = nil
    ); overload;
    constructor Create(const Ctrl0: string;      const Ctrl1: string = '';
                       const Ctrl2: string = ''; const Ctrl3: string = '';
                       const Ctrl4: string = ''; const Ctrl5: string = '';
                       const Ctrl6: string = ''; const Ctrl7: string = '';
                       const Ctrl8: string = ''; const Ctrl9: string = ''
    ); overload;
  end;

  TController = class(TDataModule, IInterface, IController)
  private
  public
    function  IsMortal: boolean;
    function  Instance: TController; overload;
    function  Instance(CtlType: TControllerClass; out V): boolean; overload;
    function  Instance(const CtlClassName: string; out V): boolean; overload;
    function  IsEnabled(const aOper: TPrivOper): boolean;
    function  LifeMode: TLifeMode;
    function  Opers: TPrivOpers;
    procedure RefreshData(force: boolean); overload; virtual;
    procedure RefreshData(const aAccessID: TAccessID); overload; virtual;
    procedure CheckEnabled(const aOper: TPrivOper); // raise exception if not enabled
    //-- like properties
    function  AccessID: TAccessID;
  private
    FRefCount: Integer;
  private
    class procedure __MarkDestroying(const Obj); static; inline;
  protected
    fAccessID: TAccessID;
    fCtrls : TCtrls;
    fLifeMode: TLifeMode;
    fOpers: TPrivOpers; // разрешенные операции
    function _AddRef: Integer; stdcall;   // не убирать из protected!
    function _Release: Integer; stdcall;
    procedure ReadState(Reader: TReader); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
//    class constructor ClassCreate;
    constructor Create(AOwner: TComponent); override;
    class function  ClassLifeMode: TLifeMode;
    class function  Reborn: TController;
    // найти первый Instance класса Self
    class function  FindInstance(out Ctl: TController): boolean;
    class function  FindCtlClass(const aClassName: string;
                                           out cc: TControllerClass): boolean;
    class procedure Register;
  end;

  TCustView = class(TForm)
  protected
    fCtrls: TCtrls;
    procedure ReadState(Reader: TReader); override;
  end;

  TFormReader = class
  private
    fRoot: TComponent;
    fReader: TReader;
    fCtrls: ^TCtrls;
    sav: TFindComponentInstanceEvent;
    procedure FindCompInst(Reader: TReader; const Name: string;
                                              var Instance: Pointer);
  public
    constructor Create(Root: TComponent; aReader: TReader; var aCtrls: TCtrls);
    destructor Destroy; override;
  end;

resourcestring
  SePersistentCtl   = 'Persistent Controller "%s" already created';

implementation
{$R *.dfm}
uses Math;

{ TFormReader }

constructor TFormReader.Create(Root: TComponent; aReader: TReader; var aCtrls: TCtrls);
begin
  if csDesigning in Root.ComponentState then exit;
  fReader:= aReader;
  sav:= fReader.OnFindComponentInstance;
  fReader.OnFindComponentInstance:= FindCompInst;
  fRoot:= Root;
  fRoot._GetCtrls(aCtrls);
  fCtrls:= @aCtrls;
end;

destructor TFormReader.Destroy;
begin
  if Assigned(fRoot) then
    fReader.OnFindComponentInstance:= sav;
  inherited;
end;

procedure TFormReader.FindCompInst(Reader: TReader; const Name: string;
                                                      var Instance: Pointer);
var A: TArray<string>;
  ctl: TController;
   cc: TControllerClass;
    c: TComponent absolute ctl;
    i: integer;
begin
  Instance:= nil;

  A:= Name.Split(['.'], TStringSplitOptions.ExcludeEmpty);
  if Length(A) < 2 then exit;

  if not fCtrls^.FindCtrl('T' + A[0], ctl) then begin
    if not TController.FindCtlClass('T' + A[0], cc) then exit;
    ctl:= cc.Reborn;
    i:= Length(fCtrls^);
    SetLength(fCtrls^, i + 1);
    fCtrls^[i]:= ctl;
  end;
  for i:= 1 to High(A) do begin
    c:= c.FindComponent(A[i]);
    if c <> nil then Continue;
    exit;
  end;
  Instance:= c;
  if Instance <> nil then
    exit;
//  OutputDebugString(format('Ссылка "%s" не найдена в %s.Ctrls', [Name, fRoot.Name]);
  if assigned(sav) then
    sav(Reader, Name, Instance);
end;

{ RequiredAttribute }

constructor RequiredAttribute.Create(Ctrl0, Ctrl1, Ctrl2, Ctrl3, Ctrl4, Ctrl5,
                             Ctrl6, Ctrl7, Ctrl8, Ctrl9: TControllerClass);
var n: integer;
//---
  procedure Proc(const cc: TControllerClass);
  begin
    fCtrls[n]:= cc;
    inc(n, ord(Assigned(cc)));
  end;
//---
begin
  SetLength(fCtrls, 10);
  n:= 0;
  Proc(Ctrl0);
  Proc(Ctrl1);
  Proc(Ctrl2);
  Proc(Ctrl3);
  Proc(Ctrl4);
  Proc(Ctrl5);
  Proc(Ctrl6);
  Proc(Ctrl7);
  Proc(Ctrl8);
  Proc(Ctrl9);
  SetLength(fCtrls, n);
end;

constructor RequiredAttribute.Create(const Ctrl0, Ctrl1, Ctrl2, Ctrl3, Ctrl4,
                                   Ctrl5, Ctrl6, Ctrl7, Ctrl8, Ctrl9: string);
var n: integer;
//---
  procedure Proc(const cn: string);
  const MSG = 'MRequired.Create: класс не найден: "%s"';
  var cc: TControllerClass;
  begin
    if cn = '' then  exit;
    if not TController.FindCtlClass(cn, cc) then
      raise Exception.CreateFmt(MSG, [cn]);
    fCtrls[n]:= cc;
    inc(n);
  end;
//---
begin
  SetLength(fCtrls, 10);
  n:= 0;
  Proc(Ctrl0);
  Proc(Ctrl1);
  Proc(Ctrl2);
  Proc(Ctrl3);
  Proc(Ctrl4);
  Proc(Ctrl5);
  Proc(Ctrl6);
  Proc(Ctrl7);
  Proc(Ctrl8);
  Proc(Ctrl9);
  SetLength(fCtrls, n);
end;

{ TCtrlsHelper }

function TCtrlsHelper.FindCtrl(const aClassName: string; out ctl): boolean;
var i: IController;
begin
  result:= false;
  for i in Self do
    if Assigned(i) and i.Instance(aClassName, ctl) then
      exit(true);
end;

function TCtrlsHelper.FindCtrl(const CtrlClassType: TControllerClass; out ctl): boolean;
var i: IController;
begin
  result:= false;
  for i in Self do
    if Assigned(i) and i.Instance(CtrlClassType, ctl) then
      exit(true);
end;

{ TObjEntHelper }

class procedure TObjEntHelper._Attrs<T>(Proc: TProc<T>);
begin
  _Attrs<T>(-1, Proc);
end;

class procedure TObjEntHelper._Attrs<T>(Count: integer; Proc: TProc<T>);
var a: TCustomAttribute;
   rc: TRttiContext;
begin
  if Count = 0 then exit;
  if Count < 0 then Count:= MaxInt;
  rc:= TRttiContext.Create;
  try
    for a in rc.GetType(Self).GetAttributes do begin
      if not(a is T) then Continue;
      Proc(T(a));
      dec(Count);
      if Count <= 0 then break;
    end;
  finally
    rc.Free;
  end;
end;

procedure TObjEntHelper._GetCtrls(var Ctrls: TCtrls);
var n: integer;
    i: integer;
  dct: TDictionary<TControllerClass, integer>; // уникализация с сохранением очередности
  itm: TPair<TControllerClass, integer>;
   cc: TControllerClass;
   ic: IController;
begin
  dct:= TDictionary<TControllerClass, integer>.Create;
  try
  //-- уникализировать и поджать Ctrls
    for i:= 0 to High(Ctrls) do begin
      ic:= Ctrls[i];
      if not Assigned(ic) then Continue;
      cc:= TControllerClass(ic.Instance.ClassType);
      if dct.ContainsKey(cc) then Continue;
      if i <> dct.Count then
        Ctrls[dct.Count]:= ic;
      dct.Add(cc, dct.Count);
    end;
    n:= dct.Count;
  //-- собрать все Ctrls атрибутов и уникализировать в словаре
    _Attrs<_RequiredAttribute>(
      procedure(M: _RequiredAttribute)
      var c: TControllerClass;
      begin
        for c in M.fCtrls do begin
          if not dct.ContainsKey(c) then
            dct.Add(c, dct.Count);
        end;
      end
    );
  //-- добавить в Ctrls то, что взято из атрибутов
    SetLength(Ctrls, dct.Count);
    for itm in dct do
      if itm.Value >= n then
        Ctrls[itm.Value]:= itm.Key.Reborn;
  finally
    dct.Free;
  end;
end;

{ TOperationsHelper }

function TOperationsHelper.AsString: string;
var s: TPrivOper;
begin
  result:= '';
  for s in Self do
    result:= result + s + ',';
  SetLength(result, max(0, Length(result) -1));
end;

function TOperationsHelper.Contains(const aOperation: TPrivOper): boolean;
var op, s: TPrivOper;
begin
  op:= UpperCase(aOperation);
  for s in Self do
    if UpperCase(s) = op then
      exit(true);
  result:= false;
end;

class function TOperationsHelper.FromString(const s: string): TPrivOpers;
begin
  result:= TPrivOpers(s.Split([',', ';', ' ', #9], TStringSplitOptions.ExcludeEmpty));
end;

{ ControllerAttribute }

constructor ControllerAttribute.Create;
begin
  Create(lmPrivate);
end;

constructor ControllerAttribute.Create(aLifeMode: TLifeMode);
begin
  fLifeMode:= aLifeMode;
end;

function ControllerAttribute.IsMortal: boolean;
begin
  result:= LifeMode = lmPrivate;
end;

{ TController }

constructor TController.Create(AOwner: TComponent);
var ctl: TController;
begin
  fLifeMode:= lmPrivate;
  if not (csDesigning in ComponentState) then begin // в дизайнере это обычный DataModule
    fLifeMode:= ClassLifeMode;
    AOwner:= nil;
  end;
  if (LifeMode <> lmPrivate) and FindInstance(ctl) then
    raise Exception.CreateFmt(SePersistentCtl, [ClassName]);
  inherited Create(AOwner);
  if LifeMode <> lmPersistent then exit;
  // Если LifeMode = lmPersistent, лишний _AddRef уберегает от разрушения,
  // а при завершении Application сработает Notification
  Application.FreeNotification(Self);
  _AddRef;
end;

class function TController.Reborn: TController;
begin
  result:= nil;
  if (ClassLifeMode = lmPrivate) or not FindInstance(Result) then
    result:= Create(nil);
end;

procedure TController.RefreshData(force: boolean);
begin
end;

procedure TController.RefreshData(const aAccessID: TAccessID);
var force: boolean;
begin
  force:= true;
  if aAccessID <> Default(TAccessID) then
    fAccessID:= aAccessID
  else
    force:= aAccessID <> fAccessID;
  RefreshData(force);
end;

(* Оптимизатор компилятора выкидывает классы, которые не используются явно.
   Когда будет возможность управлять оптимизацией, вместо регистрации
   модулей в секции initialization их можно будет регистрировать в этом
   конструкторе.

class constructor TController.ClassCreate;
var rc: TRttiContext;
    rt: TRttiType;
    rs: TRttiInstanceType absolute rt;
     a: TCustomAttribute;
     m: ControllerAttribute absolute a;
begin
  if DesignTime then
    exit;
  rc:= TRttiContext.Create;
  try
    for rt in rc.GetTypes do
      if (rt.TypeKind = tkClass) and rs.MetaclassType.InheritsFrom(TController) then
        RegisterClass(TPersistentClass(rs.MetaclassType));
  finally
    rc.Free;
  end;
end;
*)

class procedure TController.Register;
begin
  RegisterClass(Self);
end;

class function TController.FindCtlClass(const aClassName: string;
                                           out cc: TControllerClass): boolean;
var
  pc: TPersistentClass;
  mc: TControllerClass absolute pc;
begin
  pc:= FindClass(aClassName);
  result:= Assigned(pc) and pc.InheritsFrom(TController);
  if result then
    cc:= mc;
end;

class function TController.FindInstance(out Ctl: TController): boolean;
var i: integer;
begin
  for i:= 0 to Screen.DataModuleCount -1 do begin
    if not (Screen.DataModules[i] is Self) then Continue;
    ctl:= TController(Screen.DataModules[i]);
    exit(true);
  end;
  result:= false;
end;

procedure TController.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  // У LifeMode = lmPersistent при создании был выполнен дополнительный
  // _AddRef - для бессмертия. При завершении Application таки пора разбить
  // яйцо и сломать иглу.
  if
    (AComponent = Application)  and
    (Operation  = opRemove)     and
    (LifeMode   = lmPersistent)
  then
    _Release;
end;

function TController.Opers: TPrivOpers;
begin
  result:= fOpers;
end;

function TController._AddRef: Integer;
begin
  if csDesigning in ComponentState then Exit(-1);
  Result := AtomicIncrement(FRefCount);
end;

function TController._Release: Integer;
begin
  if csDesigning in ComponentState then Exit(-1);
  Result:= AtomicDecrement(FRefCount);
  if Result <> 0 then exit;
  __MarkDestroying(Self);
  Destroy;
end;

class procedure TController.__MarkDestroying(const Obj);
const objDestroyingFlag = Integer($80000000);
var LRef: Integer;
begin
  repeat
    LRef := TController(Obj).FRefCount;
  until AtomicCmpExchange(TController(Obj).FRefCount, LRef or objDestroyingFlag, LRef) = LRef;
end;

function TController.Instance(CtlType: TControllerClass; out V): boolean;
begin
  result:= ClassType = CtlType;
  if result then
    TController(V):= Self;
end;

function TController.Instance(const CtlClassName: string; out V): boolean;
begin
  result:= ClassNameIs(CtlClassName);
  if result then
    TController(V):= Self;
end;

function TController.Instance: TController;
begin
  result:= Self;
end;

function TController.IsEnabled(const aOper: TPrivOper): boolean;
begin
  result:= fOpers.Contains(aOper);
end;

function TController.IsMortal: boolean;
begin
  if csDesigning in ComponentState then exit(true);
  result:= fLifeMode = lmPrivate;
end;

function TController.AccessID: TAccessID;
begin
  result:= fAccessID;
end;

procedure TController.CheckEnabled(const aOper: TPrivOper);
const MSG = '%s.%s: нет прав на выполнение операции';
begin
  if not IsEnabled(aOper) then
    raise Exception.CreateFmt(MSG, [ClassName, aOper]);
end;

class function TController.ClassLifeMode: TLifeMode;
var rc: TRttiContext;
    rt: TRttiType;
     a: TCustomAttribute;
     m: ControllerAttribute absolute a;
begin
  result:= lmPrivate;
  rc:= TRttiContext.Create;
  try
    rt:= rc.GetType(Self);
    for a in rt.GetAttributes do begin
      if not(a is ControllerAttribute) then Continue;
      result:= m.LifeMode;
      break;
    end;
  finally
    rc.Free;
  end;
end;

procedure TController.ReadState(Reader: TReader);
var R: TFormReader;
begin
  R:= TFormReader.Create(Self, Reader, fCtrls);
  try
    inherited ReadState(Reader);
  finally
    R.Free;
  end;
end;

 // Если TFormReader почему-либо не сможет связать компонент с одним из Ctrls,
 // нужно обезопаситься от стандартного связывания по именам.
procedure TController.SetName(const NewName: TComponentName);
begin
  if csDesigning in ComponentState then
    inherited SetName(NewName)
  else
    inherited SetName('');//'_' + NewName);
end;

function TController.LifeMode: TLifeMode;
begin
  result:= fLifeMode;
end;

{ TCustView }

procedure TCustView.ReadState(Reader: TReader);
var R: TFormReader;
begin
  R:= TFormReader.Create(Self, Reader, fCtrls);
  try
    inherited ReadState(Reader);
  finally
    R.Free;
  end;
end;

end.
