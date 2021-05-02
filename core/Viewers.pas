{-----------------------------------------------------------------------------
 Unit Name: Viewers
 Author:    vyacheslav.miniyarov
 Date:      22-ноя-2016
 Purpose:
 History:
-----------------------------------------------------------------------------}
unit Viewers;

interface
uses
  Windows, Classes, SysUtils, Generics.Collections, DB, Messages, usTools,
  UITypes, System.Variants, Dialogs, TypInfo, Controls, ComCtrls, Forms,
  cxPC, cxGraphics, cxControls,
  CornDefs, usIntfs, Vwrs, UsDb;

const
  WMC_ACTIVE_CONTROL = WM_AIS + 1;
  HCM_COMPLEXTREE    = WM_AIS + 2;  // заполнить комплексное дерево. No params.

  IMG_TREE_ROOT       = 1;
  IMG_TREE_DOCUMENT   = 2;
  IMG_TREE_FOLDCLOSED = 3;
  IMG_TREE_FOLDOPENED = 4;

type
  TVNode  = class;
  TVNodeClass  = class of TVNode;
  TDNode  = class;


  TViewer = class;
  TViewerClass = class of TViewer;

  {$M+}
  TLastShow = class(TObject)
  private
    fID     : TNodeID;
    fID_OWN : TNodeID;
    fRootID : TNodeID;
    fName   : string;
    fOptions: integer;
    fViewer : TViewerID;
    fTag    : integer;
    fOpers  : integer;
    fIsValid: boolean;
  public
    procedure Assign(aNode: TVNode); overload;
    procedure Assign(NodeState: TLastShow); overload;
    procedure AssignUSOrder(us: IUsData; MaxCols: integer = -1);
    procedure ClearProps;
    function IsSubtree: boolean;
    function IsComplex: boolean;
    function CanComplex: boolean;
    function IsShortCut: boolean;
    function IsSystem: boolean;
    function HasOpts(ofFlags: integer): boolean;
    function CanOper(BitOper: integer): boolean;
    function ViewerClass: TViewerClass;
  published
    property ID       : TNodeID   read fID write fID;
    property Name     : string    read fName write fName;
    property Options  : integer   read fOptions write fOptions;
    property Viewer   : TViewerID read fViewer write fViewer;
    property Tag      : integer   read fTag write fTag;
    property Opers    : integer   read fOpers write fOpers;
    property RootID   : TNodeID   read fRootID;
    property ID_OWN   : TNodeID   read fID_OWN;
    property IsValid  : boolean   read fIsValid;
  end;

  TViewer = class(T_Viewer, IViewer)
  public //-- IViewer
    function  ivGetExpl(out Value: IExpl): boolean;
    procedure ivFillComplexTree(TreeView: TxTreeView);
    procedure ieStopModal(aResult: TVwrModalResult);
  private
    fPageIndex: integer;
    FComplexChanging: boolean; // Viewer в процессе переключения Complex состояния
    FPageChanging: boolean;    // Viewer в процессе переключения TabSheet
    FIsComplex: boolean;
    FPageEdit: TxTabSheet;
    FPageMain: TxTabSheet;
    FPageCard: TxTabSheet;
    procedure CMComplexTree(var mess: TMessage); message HCM_COMPLEXTREE;
    procedure SetIsComplex(const Value: boolean);
    function  NotifyPageChange(Page: TxTabSheet; Before: boolean; Force: boolean = false): boolean;
  protected
    procedure PageChanged(OldPage: TxTabSheet); virtual;
    procedure PageChanging(NewPage: TxTabSheet; var Allow: boolean); virtual;
  private
    fLastShow: TLastShow;
    function GetVParam(const ParamName: string): string;
  protected
    fParams: IAsCommaText;
    fAutoPage: boolean; // ActivePage переключилась автоматически при изменении состояния (StateChanged)
    fCardComplex: boolean;
    fTree: TTreeView;
    FExpl: IExpl;
    fMyTree: TxTreeView;
    FComplexTree: TxTreeView;
    FOldPage: TxTabSheet;
    function  GetComplexNode(out Node: TDNode): boolean;
      // заполнить свойства корневого комплексного узла сразу после создания
    procedure AssignComplexRoot(Node: TDNode); virtual;
    procedure ComplexChanged; virtual; // перекрыть, если нужна обработка смены режима IsComplex
    procedure _Edit; override;
    procedure _Insert(AsCopy: boolean); override;
    // Direct: true, если пользователь явно переключается на карточку;
    //         false, если переключение происходит из-за смены режима (напр., Insert)
    function  SetPage(Page: TxTabSheet; Direct: boolean = false): boolean; virtual;
    function  InternalBeginComplex: boolean; virtual;
    function  InternalDoneComplex: boolean; virtual;
    procedure FillComplexTree; virtual;
    procedure UpdateActions; override;
    procedure StateChanged; override;
    procedure InternalCancel; override;
    procedure InternalDelete; override;
    procedure LastShowChanged; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetLastNode(Node: TVNode); virtual;
    procedure SetLastState(NodeState: TLastShow); virtual;
    function  _ExecAsNB(ls: TLastShow;  Params: IAsCommaText; OKProc: TProc;
                                 RestrictOverlap: TControl = nil): boolean;
    function ShowModalNB(OKProc: TProc): boolean; virtual;
  public
    constructor CreateViewer(aExpl: IExpl; aMaster: T_Viewer = nil); overload; virtual;
    destructor Destroy; override;
    class procedure Register(Alias: string = ''; ID: integer = VIEWER_NULLID);
    class function ViewerID: TViewerID;
    class procedure Lookup<T: TViewer>(const ID: TNodeID; OKProc: TProc<T>;
                             RestrictOverlap: TControl = nil); overload; static;
    class procedure Lookup<T: TViewer>(ls: TLastShow;  OKProc: TProc<T>;
                             RestrictOverlap: TControl = nil); overload; static;
    class procedure Lookup(const ID: TNodeID; OKProc: TProc<TViewer>;
                             RestrictOverlap: TControl = nil); overload; static;
    class procedure Lookup(const ID: TNodeID; Params: IAsCommaText;
                             OKProc: TProc<TViewer>;
                             RestrictOverlap: TControl = nil); overload; static;
    procedure AfterConstruction; override;
    function  Childs<T: T_Viewer>: TArray<T>; // Details, которые IsChild
    function  BeginComplex: boolean; // перейти в режим отображения структуры документа
    function  IsChild: boolean; inline; // true, если отображается на подузле комплексного дерева
    function  DoneComplex: boolean;  // выйти из режима отображения структуры документа
    function  GetPage: TxTabSheet; virtual;
    procedure DoDown; virtual; // например, реакция на DoubleClick в гриде: SetPage(PageCard) или Expl.DoDown
    // выйти из режима карточки или попытаться сделать активным LastShow.Parent
    procedure GoUp(AndClose: boolean); virtual;
    class function NodeClass(ForComplex: boolean): TVNodeClass; virtual;
    // если IsComplex = true, сделать активным корневой узел ComplexTree,
    // что приведет к закрытию Detail-вьювера. Вернет true, если операция удалась.
    function SetRootComplex: boolean;
     // IsTop = true, если это основной вьювер (главный в TfrmExpl или свободный).
     // Проверка создания второго экз. выполняется только для вьюверов с IsTop = true
    function  IsTop: boolean; override;
    procedure Post; override;
    procedure CancelClose; override;
    function  CloseQuery: Boolean; override;
    function  CanComplex: boolean;
    function  ComplexDetail: TViewer;
    function  CheckChildsDoneEdit(FreeChilds: boolean): boolean;
    function  RefreshData(Node: TVNode; force: boolean = false): TViewer; overload;
    function  RefreshData(NodeState: TLastShow; force: boolean = false): TViewer; overload;
    function  MyNode: TVNode; overload; // Node, по которому был последний Refresh
    function  MyNode(out Node: TVNode): boolean; overload;
    function  GetCanCard: boolean; virtual;
    function  GetCanDelete: boolean; override;
    function  GetCanEdit(IgnoreState: boolean = false): boolean; override;
    function  GetCanInsert(IgnoreState: boolean = false): boolean; override;
    procedure Undock(Maximize: boolean = false);
  public
    property ActivePage: TxTabSheet read GetPage;
      // Viewer - составной документ и находится в режиме отображения структуры.
    property IsComplex: boolean read FIsComplex write SetIsComplex;
      // если IsComplex = true, ComplexTree ссылается на дерево структуры документа.
    property ComplexTree: TxTreeView read FComplexTree;
    property LastShow: TLastShow read fLastShow;
    property MyTree: TxTreeView read fMyTree;
    property Expl: IExpl read FExpl;
    property vParams[const ParamName: string]: string read GetVParam;
    // Автоматически переходить в режим IsComplex при ActivePage = PageCard и
    // автоматически переходить на PageCard в режиме IsComplex (если, конечно, xxPage <> nil).
    property CardComplex: boolean read fCardComplex write fCardComplex default false;
      // Если не nil, то этот TabSheet будет текущим при переходе в режим "Карточка".
    property PageCard: TxTabSheet read FPageCard write FPageCard;
      // Если не nil, то этот TabSheet будет текущим для vState in [vsInsert, vsEdit].
    property PageEdit: TxTabSheet read FPageEdit write FPageEdit;
      // Если не nil, то этот TabSheet будет текущим для vState in [vsInactive, vsActive].
    property PageMain: TxTabSheet read FPageMain write FPageMain;
  end;

  TVwrInfo = record
    VwrClass: TViewerClass;
    ID: TViewerID;
    Alias: string;
    constructor Create(aClass: TViewerClass; const aID: TViewerID; const aAlias: string);
    function SetID(const aID: TViewerID): TVwrInfo;
  end;

  TViewersDict = class
  private type
    TIdDict   = TDictionary<TViewerID, integer>;
    TNameDict = TDictStr<integer>;
    TInfoList = class(TList<TVwrInfo>)
    protected
      procedure Notify(const Item: TVwrInfo; Action: TCollectionNotification); override;
    end;
  private class var
    fList: TInfoList;
    fByID: TIdDict;
    fByName: TNameDict;
    class procedure _RegisterViewer(ViewerClass: TViewerClass;
                             Alias: string = ''; ID: integer = VIEWER_NULLID);
  private
    class function _ByID: TIdDict;
  protected
    class procedure Init;
    class procedure Done;
  public
    class procedure Clean;
    class procedure IdFromDB(const ID: TViewerID; const aName: string);
    class function  HasID(const ID: TViewerID): boolean;
    class function  ByID(const ID: TViewerID; out Info: TVwrInfo): boolean; overload;
    class function  ByID(const ID: TViewerID): TVwrInfo; overload;
    class function  TryByID(const ID: TViewerID): TVwrInfo;
    class function  ByName(const Name: string; out Info: TVwrInfo): boolean; overload;
    class function  ByName(const Name: string): TVwrInfo; overload;
  end;

  EVNodeAssign = class(Exception);

  TVNode = class(TTreeNode)
  private type
    PInfo = ^TInfo;
    TInfo = record
      fID: TNodeID;
      fViewerID: TViewerID;
      fViewerClass: TViewerClass;
      fNodeState: integer;
      fNodeTag: integer;
      fOptions: integer;
      fOpers: integer;
    end;
  private
    function  GetIsDetail: boolean;
    function  GetImage: integer; inline;
    procedure SetImage(const Value: integer);
    procedure SetViewerID(const Value: TViewerID);
    procedure SetID(const Value: TNodeID);
    function  GetID: TNodeID; inline;
    function  GetState: integer; inline;
    function  GetTag: integer; inline;
    function  GetOptions: integer; inline;
    function  GetOpers: integer;
    function  GetViewerClass: TViewerClass; inline;
    function  GetViewerID: TViewerID; inline;
    procedure SetState(const Value: integer);
    procedure SetTag(const Value: integer);
    procedure SetOptions(const Value: integer);
    procedure SetOpers(const Value: integer);
    procedure SetViewerClass(const Value: TViewerClass);
    procedure _AssignDSOrder(DS: TDataSet; MaxCols: integer);
    procedure _AssignUSOrder(us: IUsData; MaxCols: integer);
  protected
    fAssigning: boolean;
    procedure AfterAssign; virtual;
  protected
    function  Info: PInfo;
  public
    procedure AssignDS(DS: TDataSet); overload;
    procedure AssignUS(us: IUsData);
    procedure AssignDSOrder(DS: TDataSet; MaxCols: integer = -1);
    procedure AssignUSOrder(us: IUsData; MaxCols: integer = -1);
    procedure AssignLS(const LS: TLastShow);
    function  AddChild: TVNode;
    function  AddChildFirst: TVNode;
    procedure ClearProps;
    function  FillSubTree: integer; virtual;
    function  FindByID(const ID: TNodeID; out Node: TVNode; ForceRefresh: boolean = false): boolean;
    procedure RefreshChilds(force: boolean);
    function  RootNode: TVNode;
    function  ParentVNode: TVNode;
    procedure FreeInfo;
    function  UsChilds: IUsData; // список подузлов (CornDefs.VNODE_COLUMNS)
  public
    property ViewerClass: TViewerClass read GetViewerClass write SetViewerClass;
    function XTreeView: TxTreeView;
  public
    property IsDetail: boolean read GetIsDetail;
    property Image: integer read GetImage write SetImage;
    property State: integer read GetState write SetState;
  published
    property ID: TNodeID read GetID write SetID;
    property Text;
    property Options: integer read GetOptions write SetOptions;
    property Viewer: TViewerID read GetViewerID write SetViewerID;
    property Tag: integer read GetTag write SetTag;
    property Opers: integer read GetOpers write SetOpers;
  end;

  TDNode = class(TVNode)
  private
    fRootID: TNodeID;
    function GetRootID: TNodeID;
  public
    function FillSubTree: integer; override;
    property RootID: TNodeID read GetRootID write fRootID;
  end;

  TDNodeClass = class of TDNode;

  TxTreeViewHelper = class helper(TcxTreeViewHelper) for TxTreeView
    function FindNode(const ID: TNodeID; out Node): boolean;
  end;

  // заполнить свойства Node из БД по ID
  TReadNodeState = procedure(NodeState: TLastShow; const ID: TNodeID);

function FindViewer(const NodeID: TNodeID; out vwr: TViewer; DoPopup: boolean = true): boolean;

procedure PushCursor;

var
  ViewerClassDummy: TViewerClass = nil;

  ReadNodeState: TReadNodeState = nil;

implementation
uses Math, usClasses;


type
  TUsVNode = class(TInterfacedObject, IUsData)
  private // IUsData
    function  ColCount: Cardinal;
    function  DescribeColumn(index: Cardinal): TUsColDef; overload;
    function  EOF: boolean;
    function  GetColData(index: Cardinal): Variant; overload;
    function  GetRowData(const RowBuf: array of PVariant): boolean;
    function  Start(StartRow: Integer = -1): IUsData;
    procedure Next;
  private
    fNode: TTreeNode;
    fDefs: TArray<TUsColDef>;
  public
    constructor Create(Node: TVNode; const Props: string);
  end;

function FindViewer(const NodeID: TNodeID; out vwr: TViewer; DoPopup: boolean = true): boolean;
var i: integer;
    f: TForm;
begin
  result:= false;
  vwr:= nil;
  for i:= Screen.FormCount -1 downto 0 do begin
    f:= Screen.Forms[i];
    if not f.InheritsFrom(TViewer) then Continue;
    vwr:= TViewer(f);
    if not vwr.IsTop then Continue;
    if vwr.LastShow.ID <> NodeID then Continue;
    if DoPopup then
      vwr.PopupForm;
    Exit(true);
  end;
end;

{ TUsVNode }

constructor TUsVNode.Create(Node: TVNode; const Props: string);
var
  a: TArray<string>;
  i: integer;
begin
  Node.RefreshChilds(false);
  fNode:= Node.getFirstChild;
  a:= Props.Split([',']);
  SetLength(fDefs, Length(a));
  for i:= 0 to High(a) do
    fDefs[i]:= TUsColDef.Create(a[i], ustVariant);
end;

function TUsVNode.ColCount: Cardinal;
begin
  result:= Length(fDefs);
end;

function TUsVNode.DescribeColumn(index: Cardinal): TUsColDef;
begin
  result:= fDefs[index];
end;

function TUsVNode.EOF: boolean;
begin
  result:= fNode = nil;
end;

function TUsVNode.GetColData(index: Cardinal): Variant;
begin
  if Assigned(fNode) then
    result:= GetPropValue(fNode, fDefs[index].Name, false)
  else
    result:= null;
end;

function TUsVNode.GetRowData(const RowBuf: array of PVariant): boolean;
var i: integer;
begin
  result:= not Eof;
  for i:= 0 to High(RowBuf) do
    if RowBuf[i] <> nil then
      RowBuf[i]^:= GetColData(i);
end;

procedure TUsVNode.Next;
begin
  if Assigned(fNode) then
    fNode:= fNode.getNextSibling;
end;

function TUsVNode.Start(StartRow: Integer): IUsData;
begin
  result:= Self;
end;

{ TVNode }

procedure TVNode.FreeInfo;
var P: PInfo;
begin
  P:= Data;
  Data:= nil;
  Dispose(P);
end;

function TVNode.XTreeView: TxTreeView;
var wc: TWinControl;
begin
  wc:= TreeView;
  repeat
    if wc is TxTreeView then
      Exit(TxTreeView(wc));
    wc:= wc.Parent;
  until wc = nil;
  result:= nil;
end;

function TVNode.AddChild: TVNode;
begin
  result:= TTreeView(TreeView).Items.AddChild(Self, '') as TVNode;
end;

function TVNode.AddChildFirst: TVNode;
begin
  result:= TTreeView(TreeView).Items.AddChildFirst(Self, '') as TVNode;
end;

procedure TVNode.AfterAssign;
begin
  if RootNode = Self then
    Image:= IMG_TREE_ROOT;
  if Image = 0 then
    Image:= IMG_TREE_FOLDCLOSED;
end;

procedure TVNode.AssignDS(DS: TDataSet);
begin
  fAssigning:= true;
  try
    Assign_DS(DS);
  finally
    fAssigning:= false;
    AfterAssign;
  end;
end;

procedure TVNode.AssignUS(us: IUsData);
begin
  fAssigning:= true;
  try
    Assign_US(us);
  finally
    fAssigning:= false;
    AfterAssign;
  end;
end;

procedure TVNode.AssignDSOrder(DS: TDataSet; MaxCols: integer);
begin
  fAssigning:= true;
  try
    _AssignDSOrder(DS, MaxCols);
  finally
    fAssigning:= false;
    AfterAssign;
  end;
end;

procedure TVNode.AssignLS(const LS: TLastShow);
begin
  fAssigning:= true;
  try
    ID      := LS.ID;
    Text    := LS.Name;
    Options := LS.Options;
    Viewer  := LS.Viewer;
    Tag     := LS.Tag;
    Opers   := LS.Opers;
  finally
    fAssigning:= false;
    AfterAssign;
  end;
end;

procedure TVNode.AssignUSOrder(us: IUsData; MaxCols: integer);
begin
  fAssigning:= true;
  try
    _AssignUSOrder(us, MaxCols);
  finally
    fAssigning:= false;
    AfterAssign;
  end;
end;

procedure TVNode.ClearProps;
begin
  ID:= Default(TNodeID);
  Text:= '';
  Options:= 0;
  Viewer:= Default(TViewerID);
  Tag:= 0;
  State:= 0;
end;

procedure TVNode._AssignDSOrder(DS: TDataSet; MaxCols: integer);
var  No : integer;
  PropInfo: PPropInfo;
begin
  No:= 0;
  if MaxCols < 0 then MaxCols:= DS.FieldCount;
  for PropInfo in GetPropArray(Self) do begin
    if PropInfo.SetProc = nil then Continue;
    if No >= MaxCols then break;
    SetPropValue(Self, PropInfo, DS.Fields[No].AsVariant);
    inc(No);
  end;
end;

procedure TVNode._AssignUSOrder(us: IUsData; MaxCols: integer);
var No : integer;
  PropInfo: PPropInfo;
begin
  No:= 0;
  us.Start;
  if us.EOF then
    ClearProps
  else begin
    if MaxCols < 0 then MaxCols:= us.ColCount;
    for PropInfo in GetPropArray(Self) do begin
      if PropInfo.SetProc = nil then Continue;
      if No >= MaxCols then break;
      SetPropValue(Self, PropInfo, us.Values[No]);
      inc(No);
    end;
  end;
end;

function TVNode.FillSubTree: integer;
begin
  result:= 0;
end;

function TVNode.FindByID(const ID: TNodeID; out Node: TVNode;
                                      ForceRefresh: boolean = false): boolean;
var N: TVNode;
begin
  result:= false;
//  RefreshChilds(ForceRefresh);
  if ForceRefresh then
    RefreshChilds(true);
  N:= TVNode(getFirstChild);
  while assigned(N) do begin
    if N.ID = ID then begin
      Node:= N;
      Exit(true);
    end;
    result:= N.FindByID(ID, Node, ForceRefresh);
    if result then exit;
    N:= TVNode(N.getNextSibling);
  end;
end;

function TVNode.GetID: TNodeID;
begin
  result:= Info.fID;
end;

function TVNode.GetImage: integer;
begin
  result:= ImageIndex;
end;

function TVNode.GetIsDetail: boolean;
begin
  result:= XTreeView.Tag <> 0;
end;

function TVNode.GetState: integer;
begin
  result:= Info.fNodeState;
end;

function TVNode.GetTag: integer;
begin
  result:= Info.fNodeTag;
end;

function TVNode.GetOpers: integer;
begin
  result:= Info.fOpers;
end;

function TVNode.GetOptions: integer;
begin
  result:= Info.fOptions;
end;

function TVNode.GetViewerClass: TViewerClass;
begin
  result:= Info.fViewerClass;
end;

function TVNode.GetViewerID: TViewerID;
begin
  result:= Info.fViewerID;
end;

function TVNode.Info: PInfo;
begin
  result:= Data;
  if result <> nil then exit;
  New(result);
  Data:= result;
end;

function TVNode.ParentVNode: TVNode;
begin
  result:= nil;
  if Parent is TVNode then
    result:= TVNode(Parent);
end;

procedure TVNode.RefreshChilds(Force: boolean);
begin
  if Force or (Info.fOptions and ofSubtree <> 0) and (getFirstChild() = nil) then
    HasChildren:= FillSubTree > 0;
end;

function TVNode.RootNode: TVNode;
var N: TTreeNode;
begin
  result:= nil;
  N:= TTreeView(TreeView).Items.GetFirstNode;
  if N is TVNode then
    result:= TVNode(N);
end;

procedure TVNode.SetID(const Value: TNodeID);
begin
  Info.fID:= Value;
end;

procedure TVNode.SetImage(const Value: integer);
begin
  ImageIndex:= Value;
  if Value = IMG_TREE_FOLDCLOSED then
    SelectedIndex:= IMG_TREE_FOLDOPENED
  else
    SelectedIndex:= Value;
end;

procedure TVNode.SetState(const Value: integer);
begin
  Info.fNodeState:= Value;
end;

procedure TVNode.SetTag(const Value: integer);
begin
  Info.fNodeTag:= Value;
end;

procedure TVNode.SetOpers(const Value: integer);
begin
  Info.fOpers:= Value;
end;

procedure TVNode.SetOptions(const Value: integer);
begin
  Info.fOptions:= Value;
  HasChildren:= (Info.fOptions and ofSubtree <> 0) or (getFirstChild <> nil);
end;

procedure TVNode.SetViewerClass(const Value: TViewerClass);
begin
  Info.fViewerClass:= Value;
end;

procedure TVNode.SetViewerID(const Value: TViewerID);
var VInfo: TvwrInfo;
begin
  Info.fViewerID:= Value;
  Info.fViewerClass:= nil;
  if TViewersDict.ByID(Value, VInfo) then
    Info.fViewerClass:= vInfo.VwrClass;
end;

function TVNode.UsChilds: IUsData;
begin
  result:= TUsVNode.Create(Self, VNODE_COLUMNS);
end;

{ TDNode }

function TDNode.FillSubTree: integer;
//var m: MView;
begin
  result:= 0;
  DeleteChildren;


//  for m in View.SubViews do begin
//    if not m.Model.Enabled then Continue;
//    TVNode(TTreeView(TreeView).Items.AddChild(Self, m.Model.PrefName)).View:= m;
//    inc(result);
//  end;
end;


function TDNode.GetRootID: TNodeID;
begin
  Result:= fRootID;
  if RootNode is TDNode then
    Result:= TDNode(RootNode).fRootID;
end;

{ TVwrInfo }

constructor TVwrInfo.Create(aClass: TViewerClass; const aID: TViewerID;
                                                  const aAlias: string);
begin
  VwrClass:= aClass;
  ID:= aID;
  Alias:= aAlias;
end;

function TVwrInfo.SetID(const aID: TViewerID): TVwrInfo;
begin
  result.VwrClass:= VwrClass;
  result.ID:= aID;
  result.Alias:= Alias;
end;

{ TViewersDict.TInfoList }

procedure TViewersDict.TInfoList.Notify(const Item: TVwrInfo;
                                              Action: TCollectionNotification);
begin
  inherited;
  TViewersDict.fByID.Clear;
end;

{ TViewersDict }

class procedure TViewersDict.Init;
begin
  fList:= TInfoList.Create;
  fByID:= TIdDict.Create;
  fByName:= TNameDict.Create;
end;

class procedure TViewersDict.Done;
begin
  FreeAndNil(fList);
  FreeAndNil(fByID);
  FreeAndNil(fByName);
end;

class procedure TViewersDict.Clean;
var i: integer;
  Info: TVwrInfo;
begin
  for i:= fList.Count -1 downto 0 do begin
    Info:= fList[i];
    if Info.ID <> VIEWER_NULLID then Continue;
    fByName.Remove(Info.Alias);
    fList.Delete(i);
  end;
end;

class function TViewersDict._ByID: TIdDict;
var i,n: integer;
begin
  result:= fByID;
  if fByID.Count > 0 then exit;
  for i:= 0 to fList.Count -1 do begin
    n:= fList[i].ID;
    if n = 0 then Continue;
    if fByID.ContainsKey(n) then
      ShowError('Повторная регистрация м. просмотра %d', [n])
    else
      fByID.Add(n, i);
  end;
end;

class procedure TViewersDict._RegisterViewer(ViewerClass: TViewerClass;
                                                  Alias: string; ID: integer);
var i: integer;
begin
  if Alias = '' then
    Alias:= ViewerClass.ClassName;
  if fByName.ContainsKey(Alias) then
    raise Exception.CreateFmt('Viewer "%s" is already registered', [Alias]);
  i:= fList.Add(TVwrInfo.Create(ViewerClass, ID, Alias));
  fByName.Add(Alias, i);
end;

class procedure TViewersDict.IdFromDB(const ID: TViewerID;
                                                const aName: string);
var i: integer;
begin
  if not fByName.TryGetValue(aName, i) then exit;
  fList.Items[i]:= fList.Items[i].SetID(ID);
end;

class function TViewersDict.HasID(const ID: TViewerID): boolean;
begin
  result:= _ByID.ContainsKey(ID);
end;

class function TViewersDict.ByID(const ID: TViewerID; out Info: TVwrInfo): boolean;
var i: integer;
begin
  result:= _ByID.TryGetValue(ID, i);
  if result then
    Info:= fList[i];
end;

const E_VWR_NOT_FOUND = 'Модуль просмотра не зарегистрирован [%s]';

class function TViewersDict.ByID(const ID: TViewerID): TVwrInfo;
var
  v: Variant;
  s: string;
begin
  if ByID(ID, result) then exit;
  v:= ID;
  s:= VarToStrDef(ID, '??');
  raise Exception.CreateFmt(E_VWR_NOT_FOUND, [s]);
end;

class function TViewersDict.TryByID(const ID: TViewerID): TVwrInfo;
begin
  if not ByID(ID, result) then
    result.VwrClass:= nil;
end;

class function TViewersDict.ByName(const Name: string;
                                               out Info: TVwrInfo): boolean;
var i: integer;
begin
  result:= fByName.TryGetValue(Name, i);
  if result then
    Info:= fList[i];
end;

class function TViewersDict.ByName(const Name: string): TVwrInfo;
begin
  if not ByName(Name, result) then
    raise Exception.CreateFmt(E_VWR_NOT_FOUND, [Name]);
end;

{ TLastShow }

procedure TLastShow.Assign(aNode: TVNode);
begin
  fID      := aNode.ID;
  fRootID  := '';
  if aNode is TDNode then
    fRootID:= TDNode(aNode).RootID;
  fID_OWN  := '';
  if aNode.ParentVNode <> nil then
    fID_OWN := aNode.ParentVNode.ID;
  fName    := aNode.Text;
  fOptions := aNode.Options;
  fViewer  := aNode.Viewer;
  fTag     := aNode.Tag;
  fOpers   := aNode.Opers;
  fIsValid := GuidIsValid(fID);
end;

function TLastShow.HasOpts(ofFlags: integer): boolean;
begin
  result:= Options and ofFlags <> 0;
end;

procedure TLastShow.Assign(NodeState: TLastShow);
begin
  fID      := NodeState.ID;
  fRootID  := NodeState.RootID;
  fID_OWN  := NodeState.ID_OWN;
  fName    := NodeState.Name;
  fOptions := NodeState.Options;
  fViewer  := NodeState.Viewer;
  fTag     := NodeState.Tag;
  fOpers   := NodeState.Opers;
  fIsValid := GuidIsValid(fID);
end;

procedure TLastShow.AssignUSOrder(us: IUsData; MaxCols: integer);
var No : integer;
  PropInfo: PPropInfo;
begin
  No:= 0;
  us.Start;
  if us.EOF then
    ClearProps
  else begin
    if MaxCols < 0 then MaxCols:= us.ColCount;
    for PropInfo in GetPropArray(Self) do begin
      if PropInfo.SetProc = nil then Continue;
      if No >= MaxCols then break;
      SetPropValue(Self, PropInfo, us.Values[No]);
      inc(No);
    end;
  end;
end;

function TLastShow.CanComplex: boolean;
begin
  result:= Options and ofCanComplex <> 0;
end;

function TLastShow.CanOper(BitOper: integer): boolean;
begin
  result:= BitGet(@fOpers, BitOper);
end;

procedure TLastShow.ClearProps;
begin
  fID:= Default(TNodeID);
  fName:= '';
  fOptions:= 0;
  fViewer:= Default(TViewerID);
  fTag:= 0;
  fOpers:= 0;
  fIsValid:= false;
end;

function TLastShow.IsComplex: boolean;
begin
  result:= Options and ofComplex <> 0;
end;

function TLastShow.IsShortCut: boolean;
begin
  result:= Options and ofShortCut <> 0;
end;

function TLastShow.IsSubtree: boolean;
begin
  result:= Options and ofSubtree <> 0;
end;

function TLastShow.IsSystem: boolean;
begin
  result:= Options and ofSystem <> 0;
end;

function TLastShow.ViewerClass: TViewerClass;
begin
  result:= TViewersDict.ByID(Viewer).VwrClass;
end;

{ TViewer }

constructor TViewer.CreateViewer(aExpl: IExpl; aMaster: T_Viewer = nil);
begin
  FExpl:= aExpl;
  inherited CreateViewer(aMaster);
  if PageMain <> nil then
    PageMain.PageControl.ActivePage:= PageMain;
end;

class procedure TViewer.Lookup(const ID: TNodeID; OKProc: TProc<TViewer>;
                                                  RestrictOverlap: TControl);
begin
  Lookup(ID, nil, OKProc, RestrictOverlap);
end;

class procedure TViewer.Lookup(const ID: TNodeID; Params: IAsCommaText;
                          OKProc: TProc<TViewer>; RestrictOverlap: TControl);
var
   ls: TLastShow;
  vwr: TViewer;
begin
  ls:= TLastShow.Create;
  try
    try
      PushCursor;
      ReadNodeState(ls, ID);
      vwr:= ls.ViewerClass.CreateViewer(nil);
      vwr._ExecAsNB(ls, Params,
          procedure begin OKProc(vwr); end
         ,RestrictOverlap
      );
    except
      FreeAndNil(vwr);
      raise;
    end;
  finally
    ls.Free;
  end;
end;

class procedure TViewer.Lookup<T>(const ID: TNodeID; OKProc: TProc<T>;
                                                   RestrictOverlap: TControl);
var s: string;
   vc: TViewerClass;
  vwr: TViewer;
begin
  vwr:= T.CreateViewer(nil);
  try
    PushCursor;
    if ID <> '' then begin
      ReadNodeState(vwr.LastShow, ID);
      vc:= vwr.LastShow.ViewerClass;
      if (vc <> TViewer) and (vc <> T) then
        RaiseError('Модуль не соответствует узлу дерева');
    end;
    vwr._ExecAsNB(nil, nil,
        procedure begin OKProc(T(vwr)); end
       ,RestrictOverlap
    );
  except
    FreeAndNil(vwr);
    raise;
  end;
end;

class procedure TViewer.Lookup<T>(ls: TLastShow; OKProc: TProc<T>;
                                                   RestrictOverlap: TControl);
var s: string;
   vc: TViewerClass;
  vwr: TViewer;
begin
  vwr:= T.CreateViewer(nil);
  try
    vwr.SetLastState(ls);
    vwr._ExecAsNB(nil, nil,
        procedure begin OKProc(T(vwr)); end
       ,RestrictOverlap
    );
  except
    FreeAndNil(vwr);
    raise;
  end;
end;

procedure TViewer.AfterConstruction;
begin
  inherited;
  fLastShow:= TLastShow.Create;
end;

destructor TViewer.Destroy;
var Sheet: TxTabSheet;
begin
  if not(csDesigning in ComponentState) then
  try
    DisablePaint(Self);
    if Assigned(FExpl) and ParentSheet(Self, Sheet) then begin
      Parent:= nil;
      if not (csDestroying in Sheet.ComponentState) then
        Sheet.Free;
    end;
  except
    ApplicationHandleException(nil);
  end;
  FreeAndNil(fLastShow);
  inherited;
end;

procedure TViewer.InternalDelete;
begin
  SetPage(PageMain);
//  if IsComplex then
//    FillComplexTree;
  inherited;
end;

procedure TViewer._Edit;
begin
  SetPage(PageEdit);
  inherited _Edit;
end;

procedure TViewer._Insert(AsCopy: boolean);
var b: boolean;
begin
  b:= Assigned(PageEdit) and (ActivePage <> PageEdit);
  SetPage(PageEdit);
  if b and (ActivePage = PageEdit) then
    fAutoPage:= true;
  inherited _Insert(AsCopy);
end;

procedure TViewer.StateChanged;
begin
  inherited;
  if not (vState in [vsEdit, vsInsert]) then
    if (IsEmpty or (FLastOper = opDelete) or fAutoPage) and (ActivePage <> PageMain) then
      SetPage(PageMain);
  if IsComplex then begin
    PostMessage(Handle, HCM_COMPLEXTREE, 0, 0);
  end;
end;

procedure TViewer.UpdateActions;
begin
  inherited;
  if csDesigning in ComponentState then exit;
  if FOldPage <> ActivePage then begin
    FOldPage:= ActivePage;
//    if AutoCalcSize then
//      CalcPageConsraints(ActivePage.PageControl);
  end;
  if IsComplex and (vState <> vsInsert) and isEmpty then
    DoneComplex;
end;

class function TViewer.ViewerID: TViewerID;
var Info: TvwrInfo;
begin
  result:= VIEWER_NULLID;
  if TViewersDict.ByName(ClassName, Info) then
    result:= Info.ID;
end;

function TViewer.GetCanCard: boolean;
begin
  result:= Assigned(PageCard);
end;

function TViewer.GetCanDelete: boolean;
begin
  result:= LastShow.CanOper(pfDelete);
end;

function TViewer.GetCanEdit(IgnoreState: boolean): boolean;
begin
  result:= LastShow.CanOper(pfUpdate);
end;

function TViewer.GetCanInsert(IgnoreState: boolean): boolean;
begin
  result:= LastShow.CanOper(pfInsert);
end;

function TViewer.GetPage: TxTabSheet;
begin
  result:= nil;
end;

function TViewer.GetVParam(const ParamName: string): string;
begin
  if fParams = nil then exit('');
  result:= fParams[ParamName];
end;

function TViewer.IsTop: boolean;
begin
  if Parent = nil then exit(true);
  if Expl = nil then exit(false);
  result:= (Expl.ieMainViewer as T_Viewer) = Self;
end;

function TViewer.IsChild: boolean;
begin
  result:= IsDetail and Assigned(Expl);
end;

procedure TViewer.ivFillComplexTree(TreeView: TxTreeView);
begin
  FComplexTree:= TreeView;
  FillComplexTree;
end;

function TViewer.ivGetExpl(out Value: IExpl): boolean;
begin
  result:= Assigned(FExpl);
  if result then
    Value:= FExpl;
end;


procedure TViewer.GoUp(AndClose: boolean);
var Ex: IExpl;
begin
  if (ActivePage <> nil) and (ActivePage <> PageMain) then
    SetPage(PageMain)
  else
    if ivGetExpl(Ex) then
      Ex.GoUp(AndClose);
end;

procedure TViewer.ieStopModal(aResult: TVwrModalResult);
var Ex: IExpl;
begin
  if ivGetExpl(Ex) then
    Ex.ieStopModal(aResult);
end;

procedure TViewer.PageChanged(OldPage: TxTabSheet);
begin
  if fCardComplex and Assigned(FPageCard) then
    IsComplex:= ActivePage = FPageCard;
end;

procedure TViewer.PageChanging(NewPage: TxTabSheet; var Allow: boolean);
begin
  Allow:= (NewPage <> PageCard) or GetCanCard;
end;

function TViewer.GetComplexNode(out Node: TDNode): boolean;
var N: TTreeNode;
begin
  if ComplexTree = nil then Exit(false);
  N:= ComplexTree.Items.GetFirstNode;
  result:= Assigned(N) and (N is TDNode);
  if result then
    Node:= TDNode(N);
end;

procedure TViewer.Post;
var N: TDNode;
    s: TViewerState;
begin
  s:= vState;
  inherited;
  if IsComplex and (s in [vsEdit, vsInsert]) and (vState = vsActive)
               and GetComplexNode(N)
  then
    ivFillComplexTree(ComplexTree)
  else
    ieStopModal(FVwrModalResult);
end;

function TViewer.RefreshData(Node: TVNode; force: boolean): TViewer;
begin
  result:= self;
  SetLastNode(Node);
  Caption:= LastShow.Name;
  RefreshData(force);
end;

function TViewer.RefreshData(NodeState: TLastShow; force: boolean): TViewer;
begin
  result:= self;
  SetLastState(NodeState);
  Caption:= LastShow.Name;
  RefreshData(force);
end;

class procedure TViewer.Register(Alias: string; ID: integer);
begin
  TViewersDict._RegisterViewer(self, Alias, ID);
end;

procedure TViewer.CMComplexTree(var mess: TMessage);
var Node: TVNode;
begin
  if not IsComplex then exit;
  Node:= TVNode(ComplexTree.Items.GetFirstNode);
  if Node = nil then exit;
  if vState = vsInsert then begin
    Node.DeleteChildren;
    Node.Text:= MsgNewDocText;
  end else
  if Node.getFirstChild = nil then
    ivFillComplexTree(ComplexTree);
end;

class function TViewer.NodeClass(ForComplex: boolean): TVNodeClass;
begin
  result:= TVNode;
end;

procedure TViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation <> opRemove then exit;
  if AComponent = FComplexTree then
    FComplexTree:= nil;
end;

procedure TViewer.LastShowChanged;
begin
end;

function TViewer.MyNode(out Node: TVNode): boolean;
begin
{$IFDEF PACKAGE}
  result:= false;
{$ELSE}
  if MyTree = nil then exit(false);
  result:= MyTree.FindNode(LastShow.ID, Node);
{$ENDIF}
end;

function TViewer.MyNode: TVNode;
begin
  if not MyNode(result) then
    result:= nil;
end;

function TViewer.NotifyPageChange(Page: TxTabSheet; Before, Force: boolean): boolean;
var pc: TxPageControl;
begin
  result:= true;
  if not assigned(Page) then exit;
  pc:= Page.PageControl;
  if pc.ActivePage = Page then exit;
  try
    if Before then begin
      PageChanging(Page, result);
      result:= result or Force;
    end else
      PageChanged(Page);
  except
     Application.HandleException(Application);
  end;
end;

function TViewer.SetPage(Page: TxTabSheet; Direct: boolean): boolean;
var Pgs: TxPageControl;
    Pg : TxTabSheet;
begin
  result:= false;
{$IFNDEF PACKAGE}
  if FPageChanging or not assigned(Page) then exit;
  Pgs:= Page.PageControl;
  Pg:= Pgs.ActivePage;
  DisablePaint(Self);
  FPageChanging:= true;
  try
    if not NotifyPageChange(Page, true) then exit;
    if (vState in [vsEdit, vsInsert]) and (Page <> PageEdit)
                                      and not CheckDoneEdit then exit;
    Pgs.ActivePage:= Page;
    NotifyPageChange(Pg, false);
  finally
    FPageChanging:= false;
    result:= Pgs.ActivePage = Page;
    if result and not Direct and (Pg = PageMain) and (Page <> PageMain) then
      fAutoPage:= true;
  end;
  if Pgs.ActivePage = PageMain then
    fAutoPage:= false;
{$ENDIF}
end;

procedure TViewer.SetParent(AParent: TWinControl);
begin
  inherited;
  if Parent is TxTabSheet then
    TxTabSheet(Parent).Caption:= Caption;
end;

function TViewer.SetRootComplex: boolean;
var N: TTreeNode;
begin
  result:= false;
  if ComplexTree = nil then exit;
  N:= ComplexTree.Items.GetFirstNode;
  if N.Selected then exit(true);
  if not CheckDoneEdit(false) then exit;
  N.Selected:= true;
  result:= N.Selected;
end;

procedure TViewer.SetIsComplex(const Value: boolean);
begin
  if Value = IsComplex then exit;
  if Value then
    BeginComplex
  else
    DoneComplex;
end;

procedure TViewer.SetLastNode(Node: TVNode);
begin
  if fLastShow = nil then
    fLastShow:= TLastShow.Create;
  fLastShow.Assign(Node);
  fTree:= TTreeView(Node.TreeView);
  fMyTree:= Node.xTreeView;
end;

procedure TViewer.SetLastState(NodeState: TLastShow);
begin
  if fLastShow = nil then
    fLastShow:= TLastShow.Create;
  if NodeState <> nil then
    fLastShow.Assign(NodeState);
end;

procedure TViewer.InternalCancel;
begin
  ieStopModal(vmrCanceled);
  inherited;
end;

procedure TViewer.CancelClose;
begin
  if GetCanCancel then
    Cancel
  else
  if Assigned(PageCard) and (ActivePage = PageCard) and Assigned(PageMain) then
    SetPage(PageMain)
  else
    GoUp(IsChild or IsComplex);
end;

function TViewer.CanComplex: boolean;
begin
  result:= LastShow.IsComplex or LastShow.CanComplex;
end;

function TViewer.CloseQuery: Boolean;
begin
  if csDesigning in ComponentState then
    Exit(inherited CloseQuery);

  FClosing:= true;
  try
    result:= inherited CloseQuery and DoneComplex and CheckDoneEdit;
    if result then
      SaveState;
  finally
    FClosing:= false; // if Exception or not Result
  end;
end;

function TViewer.ComplexDetail: TViewer;
var f: TViewer;
begin
  result:= nil;
  for f in Details<TViewer> do
    if f.IsChild then
      exit(f);
end;

function TViewer.Childs<T>: TArray<T>;
var lst: TList<T>;
    i: integer;
begin
  lst:= TList<T>.Create;
  try
    if T.InheritsFrom(TViewer) then
      for i:= 0 to ComponentCount -1 do
        if Components[i].InheritsFrom(T) and TViewer(Components[i]).IsChild then
          lst.Add(T(Components[i]));
    result:= lst.ToArray;
  finally
    lst.Free;
  end;
end;

function TViewer.CheckChildsDoneEdit(FreeChilds: boolean): boolean;
begin
  result:= _CheckDetailDoneEdit(Childs<T_Viewer>, FreeChilds);
  if result and FreeChilds then
    ComplexTree.SelectRootNode;
end;

function TViewer.BeginComplex: boolean;
begin
  result:= true;
{$IFNDEF PACKAGE}
  if FComplexChanging then exit;
  with DisablePaint(self) do begin
    FComplexChanging:= true;
    try
      result:= InternalBeginComplex;
    finally
      FComplexChanging:= false;
    end;
    FIsComplex:= result;
    if not FIsComplex then Exit;
    ComplexChanged;
  end;
{$ENDIF}
end;

procedure TViewer.DoDown;
begin
  if Assigned(Expl) and LastShow.IsSubtree then
    Expl.DoDown(LastShow.ID)
  else
  if GetCanCard then
    SetPage(PageCard, true);
end;

function TViewer.DoneComplex: boolean;
var wc: TWinControl;
begin
  result:= true;
{$IFNDEF PACKAGE}
  if not IsComplex or FComplexChanging or (csDestroying in ComponentState) then exit;
  if not CheckChildsDoneEdit(true) then exit(false);
  wc:= nil;
  if not FComplexChanging then
    wc:= GetParentForm(Self);
  _DisablePaint(wc); // Здесь DisablePaint (без подчерка) нельзя: wc успевает протухнуть и все глючит
  try
    FComplexChanging:= true;
    try
      result:= InternalDoneComplex;
    finally
      FComplexChanging:= false;
    end;
    FIsComplex:= not result;
    if FIsComplex then
      Exit;
    FreeAndNil(FComplexTree);
    ComplexChanged;
  finally
    _EnablePaint(wc);
  end;
{$ENDIF}
//  ieStopModal(FVwrModalResult);
end;

function TViewer._ExecAsNB(ls: TLastShow; Params: IAsCommaText; OKProc: TProc;
                                     RestrictOverlap: TControl): boolean;
var b: boolean;
begin
  Caption:= LastShow.Name;
  fParams:= Params;
  b:= RefreshIfHidden;
  try
    RefreshIfHidden:= true;
    RefreshData(ls, true);
  finally
    RefreshIfHidden:= b;
  end;
  if Assigned(RestrictOverlap) then
    AlignDlgToWA(Self, RestrictOverlap)
  else
    Position:= poScreenCenter;
  result:= ShowModalNB(OKProc);
  if result then
    OKProc;
end;

function TViewer.ShowModalNB(OKProc: TProc): boolean;
  function Reccount: integer;
  var us: IUsData;
  begin
    result:= 0;
    us:= AsUsData;
    if us = nil then exit;
    us.Start(0);
    if us.Eof then exit;
    inc(result);
    us.Next;
    if not us.Eof then
    inc(result);
  end;
begin
  case Reccount of
    0: begin              // не нашли вообще, открыть с полным списком
         fParams:= nil;
         fNeedRefresh:= true;
       end;
//    1: exit(true);        // нашли единственный, не открывать форму
    else                  // нашли несколько, показать.
       fParams:= nil;     // Кнопка Refresh переищет всё
  end;
  result:= ShowModal = mrOk;
end;

procedure TViewer.ComplexChanged;
var Expl: IExpl;
begin
  if ivGetExpl(Expl) then
    Expl.ieSetComplex(Self);
  if fCardComplex then begin
    if IsComplex and Assigned(FPageCard) then SetPage(FPageCard);
    if not IsComplex and Assigned(FPageMain) then SetPage(FPageMain);
  end;
//  if Assigned(fDBC) then
//    if IsComplex then
//      fDBC.Freeze
//    else
//      fDBC.Defreeze;
end;

procedure TViewer.AssignComplexRoot(Node: TDNode);
begin
  Node.ID:= fLastShow.ID;
  Node.RootID:= fLastShow.ID;
  Node.Options:= ofComplexRoot;
  Node.Viewer:= ViewerID;
  Node.Tag:= 0;
  Node.Image:= IMG_TREE_FOLDCLOSED;
  Node.HasChildren:= true;
  if vState = vsInsert then
    Node.Text:= MsgNewDocText;
end;

procedure TViewer.FillComplexTree;
var Root: TDNode;
begin
  FComplexTree.Items.BeginUpdate;
  try
    FComplexTree.Items.Clear;
    Root:= FComplexTree.Items.AddChild(nil, Caption) as TDNode;
    Root.HasChildren:= vState <> vsInsert;
    AssignComplexRoot(Root);
    Root.Expand(false);
    Root.Focused:= true;
    Root.Selected:= true;
  finally
    FComplexTree.Items.EndUpdate;
  end;
end;

function TViewer.InternalBeginComplex: boolean;
begin
  SetPage(PageCard);
  result:= ActivePage = PageCard;
end;

function TViewer.InternalDoneComplex: boolean;
begin
  result:= true;
end;

procedure TViewer.Undock(Maximize: boolean);
var P: TPoint;
  R: TRect;
begin
  if not(Parent is TcxTabSheet) then exit;
  fPageIndex:= TcxTabSheet(Parent).PageIndex;
  GetCursorPos(P);
  R.TopLeft:= P;
  R.Bottom:= GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CYCAPTION);
  dec(R.Top, R.Bottom div 3 * 2);
  dec(R.Left, 48);
  R.Width:= UndockWidth;
  R.Height:= UndockHeight;
  if Maximize then
    WindowState:= wsMaximized;
  ManualFloat(R);
  MakeFullyVisible(Screen.MonitorFromPoint(P));
end;

{ TxTreeViewHelper }

function TxTreeViewHelper.FindNode(const ID: TNodeID; out Node): boolean;
var R: TTreeNode;
    N: TVNode;
begin
  result:= true;
  R:= Items.GetFirstNode;
  if not (R is TVNode) then exit(false);
  if TVNode(R).ID = ID then
    N:= TVNode(R)
  else
    result:= TVNode(R).FindByID(ID, N);
  if result then
    TVNode(Node):= N;
end;


initialization
  TViewersDict.Init;

finalization
  TViewersDict.Done;

end.
