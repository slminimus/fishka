unit uifProvider;

interface
uses Classes, usIntfs, usTools, Generics.Collections, DB;

type
  IConnection = interface;
  IDBQuery    = interface;

  TNewConnectionProc = function(const URL, aUser, aPassword: string; const aRole: string = ''): IConnection;

  ITransaction = interface
  ['{F39EC817-3B7D-44E1-B190-C77647B96CE0}']
    function  IsActive: Boolean;
    function  Connection: IConnection;
    function  Start: ITransaction;
    procedure Commit;
    procedure Rollback;
  end;

  IConnection = interface
  ['{7351B362-A990-4CF2-A892-D2998C84DF64}']
    function  NewTRS(Writeble: boolean = true): ITransaction;
    function  ReadTRS: ITransaction;
    function  QPrepare(const SQL: string; trs: ITransaction = nil): IDBQuery;
    function  QPrepareWR(const SQL: string): IDBQuery;
    function  QPrepareBatch(const SQL: string): IDBQuery;
    function  IsConnected: boolean;
  end;

  IDBProvider = interface
  ['{2F97713B-1096-4484-9EF9-45FA61C6AA9F}']
    function  Connection: IConnection;
    function  Connect(const URL, aUser, aPassword: string; const aRole: string = ''): IConnection;
    procedure Disconnect;
    function  IsConnected: boolean;
  end;

  TParam = TPair<string, variant>;
  TParamArray = TArray<TParam>;
  TParamsDict = TDictStr<Variant>;
  TDPEnumerator = TEnumerator<TPair<string, Variant>>;
  TEnumReqParam = TEnumerable<TParam>;

  IParams = interface
    function  get_Values(const ParamName: string): Variant;
    procedure set_Values(const ParamName: string; const aValue: Variant);
    function  GetEnumerator: TDPEnumerator; // если написать TParamsDict.TPairEnumerator, у компилятора едет крыша
    //---
    function  Count: integer;
    function  HasParam(const Name: string): boolean;
    function  TryGetValue(const Name: string; out Value: Variant): boolean;
    function  Add(const Name: string; const Value: Variant): IParams; overload;
    procedure Add(const Collection: TEnumerable<TParam>); overload;
    procedure Add(const Collection: TArray<TParam>); overload;
    function  AsDict: TParamsDict;
    function  Merge(aParams: IParams): boolean; // true, если все пары aParams имеются в fData (даже если fData больше)
    procedure Remove(const Name: string);
    function  SameAs(aParams: IParams): boolean;
    function  Serialize: string;
    function  Deserialize(const aValues: string): IParams;
    function  Clone: IParams;
    function  ToArray: TArray<TParam>;
    property Values[const ParamName: string]: Variant read get_Values write set_Values; default;
  end;

  IDBQuery = interface(IUsData)
  ['{A257536A-56E5-499C-8AD5-E84324968096}']
    function  GetParamValue(const ParamName: string): Variant; overload;
    procedure SetParamValue(const ParamName: string; const aValue: Variant); overload;
    function  GetParamValue(ParamIndex: integer): Variant; overload;
    procedure SetParamValue(ParamIndex: integer; const aValue: Variant); overload;
    function  Connection: IConnection;
    function  Transaction: ITransaction;
    function  ParamCount: Integer;
    function  ParamName(index: integer): string;
    function  Prepare: IDBQuery; overload;
    function  Prepare(const SQL: string): IDBQuery; overload;
    function  SetParam(const ParamName: string; const ParamValue: Variant): IDBQuery; overload;
    function  LoadParam(const ParamName: string; Stream: TStream) :IDBQuery; overload;
    function  LoadParam(const ParamName, FileName: string) :IDBQuery; overload;
    function  LoadParam(index: integer; Stream: TStream) :IDBQuery; overload;
    function  LoadParam(index: integer; const FileName: string) :IDBQuery; overload;
    function  LoadParam(index: integer; Src: IDBQuery; SrcIndex: integer) :IDBQuery; overload;
    function  SetParams(const Params: Variant): IDBQuery; overload;
    function  SetParams(const Params: array of Variant): IDBQuery; overload;
    function  SetParams(const Params: array of const): IDBQuery; overload;
    // параметр _OPER[ATION] (если в DS нет такого Fied) получает значение
    // 0: если DS.State = dsInsert
    // 1: если DS.State = dsEdit
    // 2: если DS.State = dsBrowse
    // иначе игнорируется
    function SetParams(const DS: TDataSet; const Params: string = '') :IDBQuery; overload;
    function SetParams(const us: IUsData; const Params: string = '') :IDBQuery; overload;
    function SetConnection(Value: IConnection): IDBQuery;
    function SetNewTransaction: IDBQuery;
    function SetTransaction(Value: ITransaction = nil): IDBQuery;
    function SQL: string;
    // выводит результат PrepareSql в OutputDebugString.
    //  0: тело prepared SQL;
    //  1: все параметры и их значения;
    //  2: и SQL, и параметры со значениями;
    //  3: если параметров нет - тело SQL, иначе готовый к исполнению "Execute Block"
    function ParamsDebug (Mode: integer = 3): IDBQuery;
    // если уже Active, ничего не делает
    function Open: IDBQuery;
    function Close: IDBQuery;
    // выполняет запрос, даже если уже Active. Для Batch - процессов.
    // Если Src указан - выполнить запрос для каждой его строки,
    // заполняя параметры из одноименных столбцов Src
    function Exec(Src: IUsData = nil): IDBQuery;
    // Если транзакция пишущая, финализирует её сразу; результата нет (Open.Close)
    function Invoke: IDBQuery;
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

    property ParamValues[const ParamName: string]: Variant read GetParamValue write SetParamValue;
    property ParamValues[ParamIndex: integer]: Variant read GetParamValue write SetParamValue;
  end;

implementation

end.
