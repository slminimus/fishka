{-----------------------------------------------------------------------------
 Unit Name: frExpl
 Author:    vyacheslav.miniyarov
 Date:      03-окт-2016
 Purpose:
 History:
-----------------------------------------------------------------------------}
unit frExpl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  CommCtrl, Dialogs, ExtCtrls, ComCtrls, ToolWin, Vcl.Menus, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxBarBuiltInMenu,
  cxContainer, cxEdit, cxTreeView, cxPC, cxClasses, dxBar, dxSkinsCore,
  dxSkinsDefaultPainters, usTools, usIntfs, BaseForms, CornDefs, Vwrs, Viewers;

const
  CWM_DOWN         = WM_AIS + 111;
  CWM_SET_CPX_ROOT = WM_AIS + 112;

  CL_COMPLEXTREE = clCream;

type
  TExpl = class(TxForm, IExpl)
    TreeView: TcxTreeView;
    Splitter: TSplitter;
    PgsMain: TcxPageControl;
    tsPlace: TcxTabSheet;
    PgsTree: TcxPageControl;
    tsTree: TcxTabSheet;
    pmForm: TPopupMenu;
    miFocusTree: TMenuItem;
    miTreeVisible: TMenuItem;
    miClosePage: TMenuItem;
    miNextPage: TMenuItem;
    miDoneComplex: TMenuItem;
    miPriorPage: TMenuItem;
    procedure TreeViewCreateNodeClass(Sender: TCustomTreeView;
                                        var NodeClass: TTreeNodeClass);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
                                        var AllowExpansion: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PgsMainChange(Sender: TObject);
    procedure TreeViewChanging(Sender: TObject; Node: TTreeNode;
                                       var AllowChange: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PgsMainCanCloseEx(Sender: TObject; ATabIndex: Integer;
                                                     var ACanClose: Boolean);
    procedure PgsMainGetTabHint(Sender: TObject; ATabIndex: Integer;
                                   var AHint: string; var ACanShow: Boolean);
    procedure miFocusTreeClick(Sender: TObject);
    procedure TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
                                                  var AllowCollapse: Boolean);
    procedure miTreeVisibleClick(Sender: TObject);
    procedure miClosePageClick(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure miNextPageClick(Sender: TObject);
    procedure miDoneComplexClick(Sender: TObject);
    procedure TreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure miPriorPageClick(Sender: TObject);
  protected // IExpl
    procedure DoDown(const FindID: TNodeID);
    procedure GoUp(AndClose: boolean);
    function  FindNode(const ID: TNodeID; out Node): boolean;
    function  ieGetTreeVisible: boolean;
    function  ieGetTreeWidth: integer;
    function  ieGetXProp(const PropName: string): string;
    function  ieMainViewer: IViewer;
    procedure ieSetComplex(Viewer: IViewer);
    function  ieSetTreeVisible(Value, doSetFocus: boolean): boolean;
    procedure ieSetTreeWidth(Value: integer);
    procedure ieStopModal(aResult: TVwrModalResult);
    function  ieTreeSingular: boolean;
  private
    fFindID: TNodeID;
    FComplex: boolean;
    FSavMousePos: TPoint;
    FSilents: integer;
    fTreeSingular: boolean;
  protected
    fAddProps: IAsCommaText;
    FDummyVC: TViewerClass;
    FDummyOnly: boolean;
    // спрятать дерево, если там ничего интересного
    procedure CheckTreeVisible;
    // показать/спрятать дерево и/или установить на него фокус
    procedure _ShowTree(Value, doSetFocus: boolean);
    // переключение в режим отображения структуры документа
    procedure BeginComplex(Viewer: TViewer);
    // выйти из режима отображения структуры документа
    procedure DoneComplex;
    // создать TreeView для отображения структуры документа
    function  MakeComplexTree(Viewer: TViewer): TxTreeView;
    //-- обработчики событий для ComplexTree
    procedure ComplexTreeChanging(Sender: TObject; Node: TTreeNode;
                                       var AllowChange: Boolean);
    procedure ComplexTreeChange(Sender: TObject; Node: TTreeNode);
    //-- последний TreeView
    function  TopTree: TxTreeView;
    //-- текущий TreeView
    function  CurrentTree: TxTreeView;
    //-- SilentOff и SilentOn - парные функции, запрещают/разрешают перерисовку
    //   окна. Допускаются вложенные вызовы. Самый внешний SilentOn выполнит
    //   принудительную перерисовку.
    procedure SilentOff;
    procedure SilentOn;
    // найти Viewer, для которого Sender - ComplexTree
    function  ViewerByComplexTree(tv: TxTreeView; out vwr: TViewer): boolean; overload;
    function  ViewerByComplexTree(tv: TCustomTreeView; out vwr: TViewer): boolean; overload;
    procedure CwmDown(var msg: TMessage); message CWM_DOWN;
    // при заполнении комплексного дерева в режиме добавления невозможно
    // сделать корневой узел текущим, пока дерево не отображается.
    // Извращаемся через очередь сообщений.
    procedure CwmSetCpxRoot(var Msg: TMessage); message CWM_SET_CPX_ROOT;
    procedure ComplexTreeCreateNodeClass(Sender: TCustomTreeView;
                                                 var NodeClass: TTreeNodeClass);
    procedure CloseViewerPage(Page: TxTabSheet);
    procedure UpdateActions; override;
    // Viewer на текущей закладке PgsMain
    function  CurrentViewer: TViewer;
    function  FindCurrentViewer(out Value: TViewer): boolean;
    // Viewer на первой закладке PgsMain
    function  MainViewer: TViewer;
    function  FindFirstViewer(out Value: TViewer): boolean;
    // Viewer на последней закладке PgsMain
    function  TopViewer: TViewer;
    function  FindTopViewer(out Value: TViewer): boolean;
    // Корневой узел Tree на первой закладке
    function  MainVRootNode: TVNode;
    // Запомнить позицию мыши в координатах Viewer.MainToolbar
    procedure SaveMouseToolBarPos(vwr: TViewer);
    // Переместить мышь в сохраненную позицию Viewer.MainToolbar
    procedure RestoreMouseToolBarPos(vwr: TViewer);
    // Реакция на смену текущего узла TreeView:
    // если Viewer не менялся, вернет nil, иначе вернет (пере)созданный Viewer
    function  Reinplace(Node: TVNode; Master: TViewer = nil): TViewer;
    function _ShowFor(const ID: TNodeID; const sAddProps: string): TViewer;
  public
    class function ShowFor(const ID: TNodeID; const aCaption: string;
                           const sAddProps: string = '';
                           CanDuplicate: boolean = false): TViewer;
// Viewer, чей контрол сейчас сфокусирован (Screen.ActiveControl)
    function ActiveViewer(vwr: TViewer): boolean;
    function AddProps: string;
  end;

procedure ReadNode(Node: TVNode; const ID: TNodeID);

var // Viewer, который используется, если для узла не найден класс его Viewer'а
  DummyViewerClass: TViewerClass = nil;

const
  sTreeWidth    = 'TreeWidth';
  sTreeVisible  = 'TreeVisible';

implementation
{$R *.dfm}
uses usClasses, udmCommon, ActnList, Math;

procedure _ReadNodeState(NodeState: TLastShow; const ID: TNodeID);
begin
  raise Exception.Create('Не назначена функция frExpl.ReadNodeState');
end;

procedure ReadNode(Node: TVNode; const ID: TNodeID);
var NodeState: TLastShow;
begin
  NodeState:= TLastShow.Create;
  try
    ReadNodeState(NodeState, ID);
    Node.AssignLS(NodeState);
  finally
    NodeState.Free;
  end;
end;

{ TExpl }

procedure TExpl.FormCreate(Sender: TObject);
begin
  fAddProps:= NewCommaText;
  tsPlace.TabVisible:= false;
  pgsMain.ActivePage:= tsPlace;
end;

procedure TExpl.FormDestroy(Sender: TObject);
var  s: string;
  Node: TVNode;
begin
  Node:= MainVRootNode;
  if Node = nil then exit;
  s:= Trim(Node.Text);
  if s = '' then exit;
  SaveBounds(s);
  Cfg.WriteInteger(s, sTreeWidth, ieGetTreeWidth);
end;

function FindExpl(const RootID: TNodeID; out ex: TExpl; sAddProps: string;
                                               DoPopup: boolean): boolean;
var  i: integer;
     f: TForm;
  Node: TVNode;
begin
  result:= false;
  ex:= nil;
  for i:= Screen.FormCount -1 downto 0 do begin
    f:= Screen.Forms[i];
    if not f.InheritsFrom(TExpl) then Continue;
    ex:= TExpl(f);
    if ex.TopTree = nil then Continue;
    Node:= ex.MainVRootNode;
    if Node.ID <> RootID then Continue;
    if ex.AddProps <> sAddProps then Continue;
    if DoPopup then
      ex.InsureVisible;
    Exit(true);
  end;
end;

class function TExpl.ShowFor(const ID: TNodeID; const aCaption,
                            sAddProps: string; CanDuplicate: boolean): TViewer;
var expl: TExpl;
begin
  if not CanDuplicate then
    if FindExpl(ID, expl, sAddProps, true) then exit(nil);
  expl:= Create(Application);
  try
    if aCaption <> '' then
      expl.Caption:= aCaption;
    result:= expl._ShowFor(ID, sAddProps);
  except
    expl.Free;
    raise;
  end;
end;

function TExpl._ShowFor(const ID: TNodeID; const sAddProps: string): TViewer;
  //---
  procedure CheckMDIChild;
  (* Устанавливать Placement окна до FormStyle:= fsMDIChild нет смысла, т.к.
     SetFormStyle устанавливает Position в poDefault; устанавливать после -
     все мигает, т.к. SetFormStyle устанавливает Visible:= true.
     К счастью, SetVisible не показывает форму, если fsCreating in FormState.
  *)
  begin
    if Application.MainForm.FormStyle <> fsMDIForm then exit;
    include(FFormState, fsCreating);
    try
      FormStyle:= fsMDIChild;
      HandleNeeded;
    finally
      exclude(FFormState, fsCreating);
    end;
  end;
  //---
var Root: TVNode;
  s: string;
  inf: TVwrInfo;
begin
  result:= nil;
  fAddProps.Text:= sAddProps;
  s:= fAddProps[DUMMY_VWR_CLASS];
  FDummyVC:= ViewerClassDummy;
  if TViewersDict.ByName(s, Inf) then
    FDummyVC:= inf.VwrClass;
  FDummyOnly:= fAddProps[DUMMY_VWR_ONLY] = '1';
  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;
    Root:= TreeView.Items.AddChild(nil, '') as TVNode;
    PushCursor;
    ReadNode(Root, ID);
//    Root.Options:= Root.Options or ofComplexRoot;
    s:= Root.Text;
    Root.Expand(false);
    Root.Selected:= true;
    CheckMDIChild;
    LoadBounds(s);
    ieSetTreeWidth(Cfg.ReadInteger(s, sTreeWidth, ieGetTreeWidth));
    if FindCurrentViewer(result) then
      result.Popup;
  finally
    TreeView.Items.EndUpdate;
    Show;
    InsureVisible;
  end;
end;

procedure TExpl.DoDown(const FindID: TNodeID);
begin
  fFindID:= FindID;
  PostMessage(Handle, CWM_DOWN, 0, 0);
end;

procedure TExpl.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
var vwr: TViewer;
begin
  vwr:= CurrentViewer;
  if vwr <> nil then
    Handled:= vwr.IsShortCut(Msg);
end;

procedure TExpl.FormShow(Sender: TObject);
var Root: TTreeNode;
begin
  if Caption <> '' then exit;
  Root:= TreeView.Items.GetFirstNode;
  if assigned(Root) then
    Caption:= Root.Text;
end;

procedure TExpl.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TExpl.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var vwr: TViewer;
begin
  if FindFirstViewer(vwr) then
    CanClose:= vwr.CloseQuery;
end;

procedure TExpl.CheckTreeVisible;
var Root: TTreeNode;
begin
  fTreeSingular:= false;
  if PgsTree.PageCount > 1 then exit;
  fTreeSingular:= true;
  Root:= TreeView.Items.GetFirstNode;
  if not Assigned(Root) then
    exit;
  fTreeSingular:= false;
  if Root.GetFirstChild <> nil then
    exit;
  fTreeSingular:= true;
  PgsTree.Visible:= false;
  Splitter.Visible:= false;
end;

procedure TExpl._ShowTree(Value, doSetFocus: boolean);
var ActiveIsTree: boolean;
    Tree: TxTreeView;
    b: boolean;
begin
  b:= Visible;
  if b then
    SilentOn;
  try
    ActiveIsTree:= Screen.ActiveControl is TxTreeView;
    Tree:= CurrentTree;
      Splitter.Visible:= Value;
    PgsTree.Visible:= Value;
    if Value then begin
      with Splitter do Left:= PgsTree.Left + PgsTree.Width + 1;
      if doSetFocus and Assigned(Tree) and Tree.HandleAllocated then
        Windows.SetFocus(Tree.Handle);
    end else if ActiveIsTree then
      ActiveControl:= FindNextControl(ActiveControl, true, true, false);
  finally
    if b then
      SilentOff;
  end;
end;

procedure TExpl.TreeViewChange(Sender: TObject; Node: TTreeNode);
var vwr: TViewer;
    b: boolean;
begin
  CheckTreeVisible;
  b:= Visible;
  if b then
    SilentOn;
  try
    if Assigned(Node) then begin
      vwr:= Reinplace(Node as TVNode);
      if assigned(vwr) then
        vwr.Popup(False);
    end else
      if FindTopViewer(vwr) then
        vwr.Close;
  finally
    if b then
      SilentOff;
  end;
end;

procedure TExpl.TreeViewChanging(Sender: TObject; Node: TTreeNode;
                                            var AllowChange: Boolean);
var vwr: TViewer;
begin
  if FindCurrentViewer(vwr) then
    AllowChange:= vwr.CheckDoneEdit;
end;

procedure TExpl.TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
                                              var AllowCollapse: Boolean);
begin
  AllowCollapse:= Node.Parent <> nil;
end;

function TExpl.ViewerByComplexTree(tv: TCustomTreeView; out vwr: TViewer): boolean;
begin
  if tv = nil then Exit(false);
  result:= ViewerByComplexTree(tv.Parent as TxTreeView, vwr);
end;

function TExpl.ViewerByComplexTree(tv: TxTreeView; out vwr: TViewer): boolean;
var i: integer;
begin
  result:= false;
  for i:= PgsMain.PageCount -1 downto 0 do begin
    if ChildOf(PgsMain.Pages[i], TViewer, vwr) then
      if vwr.ComplexTree = tv then exit(true);
  end;
end;

procedure TExpl.ComplexTreeChanging(Sender: TObject; Node: TTreeNode;
                                               var AllowChange: Boolean);
var vwr: TViewer;
begin
  AllowChange:= Assigned(Node) and
                ViewerByComplexTree(TTreeView(Sender), vwr) and
               ((TTreeView(Sender).Selected = nil) or vwr.CheckDoneEdit(false));
end;

procedure TExpl.ComplexTreeChange(Sender: TObject; Node: TTreeNode);
  //---
  function MakeComplexPage: TxTabSheet;
  begin
    result:= TxTabSheet.Create(PgsMain);
    result.PageControl:= PgsMain;
  end;
  //---
var vwr: TViewer;
     pg: TxTabSheet;
     dt: TViewer;
begin
  Assert(ViewerByComplexTree(TTreeView(Sender), vwr));
  SilentOn;
  try
    dt:= vwr.ComplexDetail;
    if Assigned(Node) and Assigned(Node.Parent) then begin
      pg:= nil;
      try
        if dt = nil then
          pg:= MakeComplexPage;
        dt:= Reinplace(Node as TVNode, vwr);
        if dt = nil then Abort;
        dt.Popup;
      except
        if Assigned(pg) and not(csDestroying in pg.ComponentState) then
          pg.Free;
        raise;
      end;
    end else
      if dt <> nil then
        dt.Close;
  finally
    SilentOff;
  end;
end;

procedure TExpl.GoUp(AndClose: boolean);
var i, n: integer;
begin
  if AndClose then begin
    CloseViewerPage(nil);
    exit;
  end;
  i:= PgsMain.ActivePageIndex;
  n:= PgsMain.PageCount;
  if n = 0 then exit;

  if i > 0 then
    PgsMain.ActivePageIndex:= i -1                   // set prior page
  else
    if assigned(CurrentTree.Selected) and
            (CurrentTree.Selected.Parent is TVNode)
    then                                             // focus parent node
      CurrentTree.Selected:= CurrentTree.Selected.Parent
    else
      CurrentViewer.DoneComplex;                     // DoneComplex
end;

procedure TExpl.miNextPageClick(Sender: TObject);
var i, n: integer;
begin
  i:= PgsMain.ActivePageIndex;
  n:= PgsMain.PageCount;
  if n > 0 then
    PgsMain.ActivePageIndex:= (i + 1) mod n;
end;

procedure TExpl.miPriorPageClick(Sender: TObject);
var i, n: integer;
begin
  n:= PgsMain.PageCount;
  if n < 2 then exit;
  i:= PgsMain.ActivePageIndex -1;
  if i < 0 then
    i:= n-1;
  PgsMain.ActivePageIndex:= i;
end;

function TExpl.ieGetTreeVisible: boolean;
begin
  result:= Splitter.Visible;
end;

function TExpl.ieGetXProp(const PropName: string): string;
begin
  result:= fAddProps[PropName];
end;

function TExpl.AddProps: string;
begin
  result:= fAddProps.Text;
end;

function TExpl.ieMainViewer: IViewer;
begin
  result:= MainViewer;
end;

procedure TExpl.ieSetComplex(Viewer: IViewer);
var vwr: TViewer;
begin
  vwr:= nil;
  if Assigned(Viewer) then
    vwr:= Viewer as TViewer;
  if Assigned(vwr) then
    FComplex:= vwr.IsComplex
  else FComplex:= false;
  if FComplex then
    BeginComplex(vwr)
  else
    DoneComplex;
end;

function TExpl.MakeComplexTree(Viewer: TViewer): TxTreeView;
var pg: TxTabSheet;
begin
  result:= TxTreeView.Create(Viewer);
  result.Tag:= 1; // признак "дерево структуры документа" (ComplexTree)
  result.Name:= 'tv_'+ Viewer.ClassName.Substring(1);
  result.Align:= alClient;
  result.Style.Color:= CL_COMPLEXTREE;
  result.Style.BorderStyle:= TreeView.Style.BorderStyle;
  result.HideSelection:= False;
  result.ReadOnly:= true;
  result.Images:= dmCommon.NilTree;
  result.OnCollapsing:= TreeViewCollapsing;
  result.OnChange:= ComplexTreeChange;
  result.OnChanging:= ComplexTreeChanging;
  result.OnCreateNodeClass:= ComplexTreeCreateNodeClass;
  result.OnExpanding:= TreeViewExpanding;
  result.ManualDock(PgsTree);
  pg:= ParentSheet(result);
  PgsTree.ActivePage:= pg;
  PostMessage(Handle, CWM_SET_CPX_ROOT, 0, 0);
end;

procedure TExpl.CwmSetCpxRoot(var Msg: TMessage);
var Root: TTReeNode;
begin
  Root:= CurrentTree.Items.GetFirstNode;
  if not Assigned(Root) then exit;
  Root.Focused:= true;
  Root.Selected:= true;
end;

procedure TExpl.miClosePageClick(Sender: TObject);
var vwr: TViewer;
begin
  if FindCurrentViewer(vwr) then
    vwr.CancelClose;
end;

procedure TExpl.miDoneComplexClick(Sender: TObject);
var vwr: TViewer;
begin
  if FindFirstViewer(vwr) then
    vwr.DoneComplex;
end;

procedure TExpl.miFocusTreeClick(Sender: TObject);
var tv: TxTreeView;
   vwr: TViewer;
begin
  tv:= CurrentTree;
  if (tv = nil) or (tv = TreeView) then begin  // если дерева нет - переключить
    if not FindCurrentViewer(vwr) then exit;     // текущий Viewer в IsComplex
//    vwr.IsComplex:= true;
    tv:= CurrentTree;
    if (tv = nil) {or (tv = TreeView)} then exit; // Выход, если не получилось
  end;
  if not ieGetTreeVisible then
    _ShowTree(true, false);
  if tv.Focused then
    ActiveControl:= FindNextControl(ActiveControl, true, true, false)
  else if tv.CanFocus then
    Windows.SetFocus(tv.Handle);
end;

procedure TExpl.miTreeVisibleClick(Sender: TObject);
var vwr: TViewer;
begin
  if FindTopViewer(vwr) then
    if vwr.CanComplex then
      vwr.IsComplex:= not vwr.IsComplex;
end;

procedure TExpl.SaveMouseToolBarPos(vwr: TViewer);
var P: TPoint;
begin
  FSavMousePos.X:= -1;
  if (vwr = nil) and not FindCurrentViewer(vwr) then exit;
  if not GetCursorPos(P) then exit;
  P:= vwr.ScreenToClient(P);
  if PtInRect(vwr.ClientRect, P) then
    FSavMousePos:= P;
end;

procedure TExpl.RestoreMouseToolBarPos(vwr: TViewer);
var P: Tpoint;
begin
  if FSavMousePos.X < 0 then exit;
  P:= FSavMousePos;
  FSavMousePos.X:= -1;
  if (vwr = nil) and not FindCurrentViewer(vwr) then exit;
  P:= vwr.ClientToScreen(P);
  SetCursorPos(P.X, P.Y);
end;

procedure TExpl.CloseViewerPage(Page: TxTabSheet);
var vwr: TViewer;
begin
  if Page = nil then Page:= PgsMain.ActivePage;
  if not ChildOf(Page, TViewer, vwr) then exit;
  SilentOn;
  try
    if vwr.Master is TViewer then
      TViewer(vwr.Master).SetRootComplex
    else
      vwr.DoneComplex;
  finally
    SilentOff;
  end;
end;

procedure TExpl.PgsMainCanCloseEx(Sender: TObject; ATabIndex: Integer;
                                             var ACanClose: Boolean);
begin
  ACanClose:= false;
  CloseViewerPage(PgsMain.Pages[ATabIndex]);
end;

procedure TExpl.PgsMainGetTabHint(Sender: TObject; ATabIndex: Integer;
                                 var AHint: string; var ACanShow: Boolean);
var vwr: TViewer;
   Node: TTreeNode;
begin
  ACanShow:= ChildOf(PgsMain.Pages[ATabIndex], TViewer, vwr);
  if not ACanShow then Exit;
  ACanShow:= vwr.ComplexTree.RootNode(Node);
  if ACanShow then
    AHint:= Node.Text;
end;

procedure TExpl.PgsMainChange(Sender: TObject);
var tv: TxTreeView;
   vwr: TViewer;
begin
  if not FindCurrentViewer(vwr) then exit;
  tv:= vwr.ComplexTree;
  if (tv = nil) and (vwr.Master is TViewer) then
    tv:= TViewer(vwr.Master).ComplexTree;
  if Assigned(tv) then
    PgsTree.ActivePage:= ParentSheet(tv);
end;

procedure TExpl.SilentOn;
begin
  if AtomicIncrement(FSilents) = 1 then
    _DisablePaint(Self);
end;

procedure TExpl.SilentOff;
begin
  if AtomicDecrement(FSilents) = 0 then
    _EnablePaint(Self);
end;

procedure TExpl.BeginComplex(Viewer: TViewer);
var tv: TxTreeView;
  Root: TTreeNode;
begin
  SaveMouseToolBarPos(nil);
  SilentOn;
  try
    tv:= MakeComplexTree(Viewer);
    Viewer.ivFillComplexTree(tv);
    _ShowTree(true, false);
    tsPlace.TabVisible:= true;
    Root:= tv.Items.GetFirstNode;
    if Assigned(Root) then begin
      Root.Focused:= true;
      Root.Selected:= true;
    end;
  finally
    SilentOff;
  end;
  RestoreMouseToolBarPos(nil);
end;

procedure TExpl.DoneComplex;
begin
  SaveMouseToolBarPos(nil);
  Application.ProcessMessages; // дать возможность удалиться всем желающим (вьюверам и закладкам)
  tsPlace.TabVisible:= PgsMain.PageCount > 1;
  pgsMain.ActivePageIndex:= pgsMain.PageCount -1;
  PgsTree.ActivePageIndex:= PgsTree.PageCount -1;
  CheckTreeVisible;
  RestoreMouseToolBarPos(nil);
end;

function TExpl.ieSetTreeVisible(Value, doSetFocus: boolean): boolean;
var ActiveIsTree: boolean;
begin
  ActiveIsTree:= Screen.ActiveControl is TxTreeView;
  result:= ieGetTreeVisible;
  if (result <> Value) or doSetFocus then
    _ShowTree(Value, doSetFocus or ActiveIsTree);
end;

function TExpl.ieGetTreeWidth: integer;
begin
  result:= PgsTree.Width;
end;

procedure TExpl.ieSetTreeWidth(Value: integer);
begin
  PgsTree.Width:= Value;
end;

procedure TExpl.ieStopModal(aResult: TVwrModalResult);
begin
 { TODO : ieStopModal }
end;

function TExpl.ieTreeSingular: boolean;
begin
  result:= fTreeSingular;
end;

function TExpl.TopViewer: TViewer;
begin
  if not FindTopViewer(result) then
    result:= nil;
end;

function TExpl.FindTopViewer(out Value: TViewer): boolean;
begin
  result:= ChildOf(PgsMain.LastPage, TViewer, Value);
end;

function TExpl.CurrentViewer: TViewer;
begin
  if not FindCurrentViewer(result) then
    result:= nil;
end;

function TExpl.FindCurrentViewer(out Value: TViewer): boolean;
begin
  result:= ChildOf(PgsMain.ActivePage, TViewer, Value);
end;

function TExpl.MainViewer: TViewer;
begin
  if not FindFirstViewer(result) then
    result:= nil;
end;

function TExpl.MainVRootNode: TVNode;
var xTRee: TxTreeView;
  Node: TTreeNode;
begin
  result:= nil;
  if not ChildOf(PgsTree.FirstPage, TxTreeView, xTRee) then exit;
  Node:= xTree.Items.GetFirstNode;
  if Node is TVNode then
    result:= TVNode(Node);
end;

function TExpl.FindFirstViewer(out Value: TViewer): boolean;
begin
  result:= ChildOf(PgsMain.FirstPage, TViewer, Value);
end;

function TExpl.TopTree: TxTreeView;
begin
  if not ChildOf(PgsTree.LastPage, TxTreeView, result) then
    result:= nil;
end;

function TExpl.CurrentTree: TxTreeView;
begin
  if not ChildOf(PgsTree.ActivePage, TxTreeView, result) then
    result:= nil;
end;

function TExpl.FindNode(const ID: TNodeID; out Node): boolean;
begin
  result:= TreeView.FindNode(ID, Node);
end;

procedure TExpl.CwmDown(var msg: TMessage);
var Node: TVNode;
    Prnt: TVNode;
begin
  if not FindNode(fFindID, Node) then exit;
  Prnt:= Node.ParentVNode;
  if Assigned(Prnt) and not Prnt.Expanded then
    Prnt.Expand(false);
  Node.Selected:= true;
end;

function TExpl.ActiveViewer(vwr: TViewer): boolean;
var ctl: TWinControl;
begin
  result:= false;
  if vwr = nil then exit;
  ctl:= Screen.ActiveControl;
  if ctl = nil then exit;
  while assigned(ctl) do begin
    if ctl = vwr then exit(true);
    ctl:= ctl.Parent;
  end;
end;

function TExpl.Reinplace(Node: TVNode; Master: TViewer = nil): TViewer;
var old: TViewer;
    vc: TViewerClass;
begin
  result:= nil;
  old:= TopViewer;
  vc:= Node.ViewerClass;
  if (vc = nil) or FDummyOnly then
    vc:= FDummyVC;
  if assigned(old) and (old.ClassType = vc) then begin
    old.RefreshData(Node, true);
    result:= old;
    exit;
  end;
  if vc <> nil then begin
    try
      result:= vc.CreateViewer(Self, Master);
      result.Align:= alClient;
      result.BorderStyle:= bsNone;
      result.Parent:= PgsMain.LastPage;
      result.RefreshData(Node);
      result.Visible:= true;
      result.BringToFront;
    except
      Tag:= 666;  // признак ошибки
      if result <> nil then
        result.Close;
      result:= nil;
      // reraise не рекомендуется: TreeView глотает Exception где-то "у ей внутре"
      Application.HandleException(nil);
    end;
  end;
  if assigned(old) then begin
    old.SendToBack;
    old.Hide;
    old.Parent:= nil; // prevent from Place.Destroy
    old.Close;
  end;
end;

procedure TExpl.TreeViewCreateNodeClass(Sender: TCustomTreeView;
                                                var NodeClass: TTreeNodeClass);
begin
  NodeClass:= BaseNodeClass;
end;

procedure TExpl.ComplexTreeCreateNodeClass(Sender: TCustomTreeView;
                                                var NodeClass: TTreeNodeClass);
begin
  NodeClass:= DetailNodeClass;
end;

procedure TExpl.TreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  (Node as TVNode).FreeInfo;
end;

procedure TExpl.TreeViewExpanding(Sender: TObject; Node: TTreeNode;
                                          var AllowExpansion: Boolean);
begin
  if Node.getFirstChild = nil then
    (Node as TVNode).FillSubTree;
  AllowExpansion:= Node.getFirstChild <> nil;
end;

procedure TExpl.UpdateActions;
begin
  inherited;
  if Tag = 666 then begin
    Close;
    Exit;
  end;
// убираем фокус с формы, "нажимая" TAB
  if Screen.ActiveControl <> Self then Exit;
  keybd_event(9, 0, 0, 0);
  keybd_event(9, 0, 0, KEYEVENTF_KEYUP);
end;

initialization
  ReadNodeState:= _ReadNodeState;

end.
