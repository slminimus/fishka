unit ufViewer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Actions, Vcl.ActnList, Vcl.ToolWin, Vcl.ComCtrls, dxBar, cxClasses,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator,
  dxDateRanges, Data.DB, cxDBData, dxBarBuiltInMenu, cxPC, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGridLevel,
  cxGrid, usIntfs, UITypes, Generics.Collections, usTools, uCornDefs,
  uEntities, Vcl.StdCtrls, Vcl.Menus, uBaseForms;

type
  TViewerClass = class of TViewer;

  CardViewAttribute = class(TCustomAttribute)
  private
    fCardID: TEntityID;
  public
    constructor Create(aCardID: TEntityID);
    property CardID: TEntityID read fCardID;
  end;

  TViewer = class(TBaseForm, IUsReceiver)
    MainActions: TActionList;
    acInsert: TAction;
    acDelete: TAction;
    acEdit: TAction;
    acRefresh: TAction;
    acPost: TAction;
    acCancel: TAction;
    BarMan: TdxBarManager;
    tbnInsert: TdxBarLargeButton;
    tbnRefresh: TdxBarLargeButton;
    tbnPost: TdxBarLargeButton;
    tbnDelete: TdxBarLargeButton;
    tbnCancel: TdxBarLargeButton;
    tbrMain: TdxBar;
    acInsCopy: TAction;
    ddmInsert: TdxBarPopupMenu;
    tbnInsCopy: TdxBarLargeButton;
    tbnCard: TdxBarLargeButton;
    acCard: TAction;
    ddmCard: TdxBarPopupMenu;
    tbnCardLeft: TdxBarButton;
    tbnCardRight: TdxBarButton;
    tbnCardTop: TdxBarButton;
    tbnCardBottom: TdxBarButton;
    tbnCardClient: TdxBarButton;
    tbnCardForm: TdxBarButton;
    dxBarSeparator2: TdxBarSeparator;
    procedure acInsertUpdate(Sender: TObject);
    procedure acDeleteUpdate(Sender: TObject);
    procedure acRefreshUpdate(Sender: TObject);
    procedure acPostUpdate(Sender: TObject);
    procedure acCancelUpdate(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acInsertExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acPostExecute(Sender: TObject);
    procedure acInsCopyUpdate(Sender: TObject);
    procedure acInsCopyExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure acCardExecute(Sender: TObject);
    procedure acCardUpdate(Sender: TObject);
    procedure tbnCardAlignClick(Sender: TObject);
  private
    // ���� Viewer ��� ������ �������� (��� ����������, ��� ������ ��������)
    FVwrModalResult: TVwrModalResult;
    FClosing: boolean;            // Viewer � �������� ��������
    fCardHost: TWinControl;
    fSavFocused: TWinControl;
    fChildNotify: integer;
    procedure WMCActiveControl(var Msg: TMessage); message WMC_ACTIVE_CONTROL;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure HandleAddProps;
    function  GetMaster: TViewer;
    procedure SetMaster(const Value: TViewer);
    procedure SetCardHost(const Value: TWinControl);
    procedure SaveDockExt;
    procedure CardHide;
    procedure CardShow(AutoCard: boolean);
    procedure _CardShow;
    procedure _CardHide;
    function GetCardAlign: TAlign;
    procedure SetCardAlign(const Value: TAlign);
  protected
    fDockExt: TPoint;
    fFloatExt: TRect;
    fCardViewClass: TViewerClass;
    fCardView: TViewer;
    fCardPanel: TWinControl;
    fAutoCard: boolean;
    // Lazy refresh: ���� RefreshData ����������, ����� Viewer �� �����, �������
    // Refresh �� �����������, � fNeedRefresh ���-�� � true. ��� �����������,
    // ��������������, fNeedRefresh ����������� � ����������� Refresh (���� ����).
    fNeedRefresh: boolean;
    fReadOnly: boolean;
    fMsgConfirmDelete: string;
    fMsgSaveNoCancel: string;
    fConfirms: TConfirms;
    procedure InitializeNewForm; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  protected
    // ��������� ���������� ��������� RefreshData
    fLastParams: string;
    fState: TViewerState;
    fRefreshIfHidden: boolean; // �������� Lazy refresh (��. ����������� � fNeedRefresh)
    fLastOper: TOper;          // ����� �������� ��������� � Commit (edit or insert)
    fOldState: TViewerState;
    type TPostError = (bpeOK, bpeNoDeleted, bpeBadState, bpeIsEmpty, bpeTooMany);
    procedure PostError(Code: TPostError);
    function  CardHostByAlign(aAlign: TAlign): TWinControl; dynamic;
    // �������� ��������� (���������� �� ������� ����������)
    function  SameParams(const P1, P2: string): boolean;
    procedure SetRefreshIfHidden(const Value: boolean);
    function  GetState: TViewerState; virtual;
    function  GetModified: Boolean; virtual;
    procedure SetModified(Value: Boolean); virtual;
    function  GetReadOnly: boolean; virtual;
    procedure SetReadOnly(const Value: boolean); virtual;
    // ���������� �� UpdateActions, ���� ���������� vState; ���������� �������� - � FOldState
    procedure StateChanged; virtual;
    procedure MasterDataChanged; virtual;
    function  GetCardVisible: boolean; virtual;
    procedure SetCardVisible(const Value: boolean); virtual;
    procedure FindCardClass; overload;
    procedure FindCardClass(const EntityID: TEntityID); overload;
    procedure CardDocking(Into: boolean); dynamic;
    procedure SetCardPlacement(aHost: TWinControl; aAlign: TAlign); overload;

    function _CheckDetailsDoneEdit(D: TArray<TViewer>; FreeIfOk: boolean): boolean;
    function  FormatMsgConfirmDelete: string; virtual;
    procedure UpdateActions; override;
    //-- ���������� ------
    procedure FixDataChanges; dynamic; // ������� ������ �� edit-��������� (������ TDataSet.UpdateRecord)
    procedure InternalCancel; dynamic;
    procedure InternalDelete; dynamic;
    procedure InternalEdit; dynamic;
    procedure InternalInsert(doCopy: boolean = false); dynamic;
    procedure InternalPost; dynamic;
    procedure InternalRefresh; dynamic;
    procedure CheckRequired; dynamic;

    type TCanOperation = (copCancel, copDelete, copEdit, copInsert, copPost, copRefresh);
    function  GetCan(aOperation: TCanOperation; IgnoreState: boolean = false): boolean; virtual;
  public
    class procedure RegisterViewer;
    class function MainEntityID: TEntityID;
    constructor CreateViewer(aMaster: TViewer = nil); overload; virtual;
    constructor InplaceInto(Place: TWinControl; aMaster: TViewer = nil); overload;
    constructor InplaceInto(Place: TWinControl; aAlign: TAlign; aMaster: TViewer = nil); overload;
    procedure AfterConstruction; override;
    function  IsDetail: boolean; inline; // true, ���� �������� Master
    function  AsUsData: IUsData; virtual; // ������ ������������ ������ ��� IUsData
    procedure CopyData(Src: IUsData); virtual;
    function  IsParentFor(ctl: TControl): boolean;
    function  CardInplaced: boolean;
  //-- ���������� � ������� ��������� ------
    function  AutoConfirm(State: TViewerState): boolean;
    function  GetCanRefresh: boolean;
    function  GetCanCancel: boolean;
    function  GetCanCard: boolean; virtual;
    function  GetCanDelete: boolean;
    function  GetCanEdit(IgnoreState: boolean = false): boolean;
    function  GetCanInsert(IgnoreState: boolean = false): boolean;
    function  GetCanInsCopy(IgnoreState: boolean = false): boolean;
    function  GetCanPost: boolean;
    function  IsEmpty: boolean; virtual;

    procedure SetClientRect(const NewRect: TRect);
    function  CloseQuery: Boolean; override;
    // ��������� � ������� ����������� �������� �����, ��������� � ����� ��������
    // �� ������ ������������� ���������. �������������� ������ ������ Detail �
    // ������� ���, ���� ����.
    function  CheckDoneEdit(FreeDetail: boolean = false): boolean; dynamic;
    function  CheckDetailsDoneEdit(FreeDetails: boolean): boolean; dynamic;
    procedure ChildNotifyEnable(DoNotify: boolean = true);
    function  ChildNotifyEnabled: boolean;
    procedure ChildNotifyDisable;
    procedure NotifyDetailsDataChanged;
    function  DoConfirmDelete: boolean; dynamic;
    // return Self
    function  RefreshData(const Params: string; force: boolean = false): TViewer; overload; virtual;
    function  RefreshData(force: boolean = false): TViewer; overload; virtual;
    function  PrimaryKey: Variant; virtual;   // �� ������� ������.
    function  MainText: string; virtual;      // ����� ������� ������; ��������, ��� ������������ Node.
    function  MasterPK: Variant; virtual;     // �� ������� ������ Master'� ��� null.
    function  Details<T: TViewer>: TArray<T>; // ��� Viewers, � ������� ���� - Master
    procedure Popup(Force: boolean = true);   // ������� ���� ������� ������������
    procedure PopupForm; // ������� ������� ����� ���� ��� ���� ParentForm
    procedure SetCardPlacement(aAlign: TAlign); overload; dynamic;
    // ��������������
     // �������� ������ � ������� ������ ������ ������ ������� Src (��������, ���� �����)
    procedure AssignRow(Src: IUsData); virtual;
    procedure DeleteRow; virtual;               // ������� ������� ������ (������ �� ������ ������)
    procedure InsertRow(Src: IUsData); virtual; // �������� ������ � ������� ������� (������ �� ������ ������)
    procedure Cancel;
    procedure Delete;
    procedure Edit;
    procedure Insert;
    procedure InsCopy;
    procedure Post; virtual;
  public
    property CardPanel: TWinControl read fCardPanel;
    property CardVisible: boolean read GetCardVisible write SetCardVisible;
    property Master: TViewer read GetMaster; // Viewer ��������� ���������
      // ����� ��� ������ RefreshData ��� Visible = false � RefreshIfHidden = false
    property NeedRefresh: boolean read fNeedRefresh;
    property Modified: Boolean read GetModified write SetModified;
    property vState: TViewerState read GetState;
  published
    property CardAlign: TAlign read GetCardAlign write SetCardAlign;
    property CardHost: TWinControl read fCardHost write SetCardHost;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default false;
    property RefreshIfHidden: boolean read fRefreshIfHidden write SetRefreshIfHidden default false;
    property MsgConfirmDelete: string read FMsgConfirmDelete write FMsgConfirmDelete;
    property MsgSaveNoCancel: string read FMsgSaveNoCancel write FMsgSaveNoCancel;
//    property MsgNewDocText: string read fMsgNewDocText write fMsgNewDocText;
  end;

  TViewerDict = class(TDictionary<TViewerClass, TEntityID>)
  private
    function Find(const EntityID: TEntityID; Card: boolean; RaiseError: boolean = true): TViewerClass;
  public
    function ListByEntityID(const EntityID: TEntityID): TViewerClass; overload;
    function ListByEntityID(const EntityID: TEntityID; out vc: TViewerClass): boolean; overload;
    function CardByEntityID(const EntityID: TEntityID): TViewerClass; overload;
    function CardByEntityID(const EntityID: TEntityID; out cc: TViewerClass): boolean; overload;
  end;

var
  BaseCardClass: TViewerClass = nil;
  ViewerDict: TViewerDict = nil;


implementation
{$R *.dfm}
uses dmDatas, Math, TypInfo;

function ParentSheet(ctl: TControl; out Sheet: TxTabSheet): boolean; overload;
var wc: TWinControl;
begin
  result:= false;
  if ctl = nil then exit;
  wc:= ctl.Parent;
  while assigned(wc) do begin
    if wc is TxTabSheet then begin
      Sheet:= TxTabSheet(wc);
      exit(true);
    end;
    wc:= wc.Parent;
  end;
end;

function ParentSheet(ctl: TControl): TxTabSheet; overload;
begin
  if not ParentSheet(ctl, result) then
    result:= nil;
end;

function ModalResultByState(vState: TViewerState): TVwrModalResult;
begin
  case vState of
    vstInsert:
      result:= vmrInserted;
    vstEdit:
      result:= vmrUpdated;
    //vsDeleting: result:= vmrInserted;
  else
    result:= vmrCanceled;
  end;
end;

{ CardViewAttribute }

constructor CardViewAttribute.Create(aCardID: TEntityID);
begin
  fCardID:= aCardID;
end;

{ TViewer }

constructor TViewer.CreateViewer(aMaster: TViewer);
var Own: TComponent;
begin
  if Assigned(aMaster) then
    SetMaster(aMaster);
  Own:= Master;
  if Own = nil then
    Own:= Application;
  inherited Create(Own);
end;

constructor TViewer.InplaceInto(Place: TWinControl; aMaster: TViewer);
begin
  InplaceInto(Place, alClient, aMaster);
end;

constructor TViewer.InplaceInto(Place: TWinControl; aAlign: TAlign; aMaster: TViewer);
begin
  CreateViewer(aMaster);
  Parent:= Place;
  Align:= aAlign;
end;

procedure TViewer.SetClientRect(const NewRect: TRect);
begin
  Width := Width  + NewRect.Width  - ClientWidth;
  Height:= Height + NewRect.Height - ClientHeight;
end;

procedure TViewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TViewer.FormShow(Sender: TObject);
begin
  if NeedRefresh then
    RefreshData(true);
end;

procedure TViewer.acCancelExecute(Sender: TObject);
begin
  Cancel;
end;

procedure TViewer.acCancelUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= GetCanCancel;
end;

procedure TViewer.acCardExecute(Sender: TObject);
begin
  CardVisible:= not CardVisible;
end;

procedure TViewer.acCardUpdate(Sender: TObject);
begin
  with TAction(Sender) do begin
    Enabled:= GetCanCard;
    Checked:= CardVisible;
  end;
end;

procedure TViewer.acDeleteExecute(Sender: TObject);
begin
  Delete;
end;

procedure TViewer.acDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= GetCanDelete;
end;

procedure TViewer.acInsCopyExecute(Sender: TObject);
begin
  InsCopy;
end;

procedure TViewer.acInsCopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= GetCanInsCopy;
end;

procedure TViewer.acInsertExecute(Sender: TObject);
begin
  Insert;
end;

procedure TViewer.acInsertUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= GetCanInsert;
end;

procedure TViewer.acPostExecute(Sender: TObject);
begin
  Post;
end;

procedure TViewer.acPostUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= GetCanPost;
end;

procedure TViewer.acRefreshExecute(Sender: TObject);
begin
  RefreshData(true);
end;

procedure TViewer.acRefreshUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= GetCanRefresh;
end;

procedure TViewer.AfterConstruction;
begin
  inherited;
  FindCardClass;
end;

procedure TViewer.FindCardClass;
var
  id: TEntityID;
begin
  if InheritsFrom(BaseCardClass) then exit;
  // id �������� ����� �� ��������,
  if not EnumAttrs<CardViewAttribute>(
    procedure(cv: CardViewAttribute; var Stop: boolean)
    begin
      Stop:= true;
      id:= cv.CardID;
    end
  ) then
    id:= MainEntityID;  // ���� ������� �� ������, ����� �����������
  FindCardClass(id);
end;

procedure TViewer.FindCardClass(const EntityID: TEntityID);
const
  ERR = '%s: Cannot find CardView class ID = %s';
begin
  if EntityID = '' then exit;
  fCardViewClass:= ViewerDict.CardByEntityID(EntityID);

  if fCardViewClass = nil then
    raise Exception.CreateFmt(ERR, [ClassName, EntityID]);
end;

procedure TViewer.AssignRow(Src: IUsData);
begin
end;

function TViewer.AsUsData: IUsData;
begin
  result:= nil;
end;

function TViewer.AutoConfirm(State: TViewerState): boolean;
begin
  result:= false;
  case State of
    vstEdit    : result:= not(cfEdit in FConfirms);
    vstInsert  : result:= not(cfIns  in FConfirms);
    vstDeleting: result:= not(cfDel  in FConfirms);
  end;
end;

procedure TViewer.Cancel;
begin
  if CardInplaced then begin
    fCardView.Cancel;
    if fAutoCard then
      CardVisible:= false;
    exit;
  end;
  if not GetCanCancel then exit;
  FLastOper:= opCancel;
  InternalCancel;
end;

function TViewer.CheckDetailsDoneEdit(FreeDetails: boolean): boolean;
begin
  result:= _CheckDetailsDoneEdit(Details<TViewer>, FreeDetails);
end;

function TViewer._CheckDetailsDoneEdit(D: TArray<TViewer>; FreeIfOk: boolean): boolean;
var f: TViewer;
begin
  for f in D do // ������� ���� ��������
    if not f.CheckDoneEdit(FreeIfOk) then exit(false);

  result:= true;
  if not FreeIfOk then exit;
  for f in D do  // � ������ ���� ��� �������� - ������
    f.Close;
end;

procedure TViewer.CheckRequired;
begin
end;

procedure TViewer.ChildNotifyDisable;
begin
  inc(fChildNotify);
end;

procedure TViewer.ChildNotifyEnable(DoNotify: boolean);
begin
  dec(fChildNotify);
  if DoNotify and (fChildNotify = 0) then
    NotifyDetailsDataChanged;
end;

function TViewer.ChildNotifyEnabled: boolean;
begin
  result:= fChildNotify = 0;
end;

function TViewer.CloseQuery: Boolean;
begin
  if csDesigning in ComponentState then
    Exit(inherited CloseQuery);

  FClosing:= true;
  try
    result:= inherited CloseQuery and CheckDoneEdit;
//    if result then
//      SaveState;
  finally
    FClosing:= false; // if Exception or not Result
  end;
end;

procedure TViewer.CopyData(Src: IUsData);
begin
end;

procedure TViewer.Delete;
begin
  if CardInplaced then begin
    fCardView.Delete;
    exit;
  end;
  if not(GetCanDelete and (AutoConfirm(vstDeleting) or DoConfirmDelete)) then exit;
  if not CheckDetailsDoneEdit(false) then exit;
  FLastOper:= opDelete;
  InternalDelete;
end;

procedure TViewer.DeleteRow;
begin
end;

function TViewer.Details<T>: TArray<T>;
var lst: TList<T>;
    i: integer;
begin
  lst:= TList<T>.Create;
  try
    for i:= 0 to ComponentCount -1 do
      if Components[i].InheritsFrom(T) then
        lst.Add(T(Components[i]));
    result:= lst.ToArray;
  finally
    lst.Free;
  end;
end;

function TViewer.DoConfirmDelete: boolean;
begin
  result:= MessageDlg(FormatMsgConfirmDelete, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TViewer.InternalRefresh;
begin
  fState:= vstActive;
end;

procedure TViewer.FixDataChanges;
begin
end;

function TViewer.FormatMsgConfirmDelete: string;
begin
  result:= fMsgConfirmDelete;
end;

function TViewer.GetCanCancel: boolean;
begin
  result:= GetCan(copCancel);
end;

function TViewer.GetCanCard: boolean;
begin
  result:= Assigned(fCardViewClass);
end;

function TViewer.GetCanDelete: boolean;
begin
  result:= GetCan(copDelete);
end;

function TViewer.GetCanEdit(IgnoreState: boolean): boolean;
begin
  result:= GetCan(copInsert, IgnoreState);
end;

function TViewer.GetCan(aOperation: TCanOperation; IgnoreState: boolean): boolean;
begin
  result:= false;
  if CardInplaced then
    exit(fCardView.GetCan(aOperation, IgnoreState));
  if CardVisible then
    exit;

  case aOperation of
    copCancel : result:= Modified or (vState in [vstEdit, vstInsert]);
    copDelete : result:= not ReadOnly and not IsEmpty and (vState = vstActive);
    copEdit   : result:= not ReadOnly and not IsEmpty and (IgnoreState or (vState = vstActive));
    copInsert : result:= not ReadOnly and (IgnoreState or (vState = vstActive));
    copPost   : result:= Modified or (vState in [vstEdit, vstInsert]);
    copRefresh: result:= not Modified and (vState in [vstInactive, vstActive]);
  end;
end;

function TViewer.GetCanInsCopy(IgnoreState: boolean): boolean;
begin
  result:= not IsEmpty and GetCan(copInsert, IgnoreState);
end;

function TViewer.GetCanInsert(IgnoreState: boolean): boolean;
begin
  result:= GetCan(copInsert, IgnoreState);
end;

function TViewer.GetCanPost: boolean;
begin
  result:= GetCan(copPost);
end;

function TViewer.GetCanRefresh: boolean;
begin
  result:= GetCan(copRefresh);
end;

function TViewer.IsParentFor(ctl: TControl): boolean;
begin
  result:= Assigned(ctl) and (GetParentForm(ctl) = Self);
end;

function TViewer.CardInplaced: boolean;
begin
  result:= CardVisible and IsParentFor(fCardView.CardPanel);
end;

function TViewer.GetMaster: TViewer;
begin
  result:= nil;
  if Owner is TViewer then
    result:= TViewer(Owner);
end;

function TViewer.GetModified: Boolean;
begin
  result:= false;
end;

function TViewer.GetReadOnly: boolean;
begin
  result:= FReadOnly;
end;

function TViewer.GetState: TViewerState;
begin
  result:= fState;
  if CardInplaced then
    result:= fCardView.vState;
end;

procedure TViewer.InitializeNewForm;
begin
  inherited;
//  fMsgNewDocText:= DEF_MSGNEWDOCTEXT;
  fMsgConfirmDelete:= DEF_MSGCONFIRMDELETE;
  fMsgSaveNoCancel:= DEF_MSGSAVENOCANCEL;
  FConfirms:= [cfIns, cfEdit, cfDel];
end;

procedure TViewer.Edit;
begin
  if not GetCanEdit then exit;
  CardShow(true);
  if CardVisible then
    fCardView.Edit
  else
    InternalEdit;
end;

procedure TViewer.InsCopy;
begin
  if not GetCanInsCopy then exit;
  CardShow(true);
  if CardVisible then
    fCardView.InsCopy
  else
    InternalInsert(true);
end;

procedure TViewer.Insert;
begin
  if not GetCanInsert then exit;
  CardShow(true);
  if CardVisible then
    fCardView.Insert
  else
    InternalInsert(false);
end;

procedure TViewer.InsertRow(Src: IUsData);
begin
end;

procedure TViewer.InternalCancel;
begin
  fState:= vstActive;
end;

procedure TViewer.InternalDelete;
begin
end;

procedure TViewer.InternalEdit;
begin
  fState:= vstEdit;
end;

procedure TViewer.InternalInsert(doCopy: boolean);
begin
  fState:= vstInsert;
end;

procedure TViewer.InternalPost;
begin
  fState:= vstActive;
end;

function TViewer.IsDetail: boolean;
begin
  result:= Assigned(Master);
end;

function TViewer.IsEmpty: boolean;
begin
  result:= true;
end;

procedure TViewer.Loaded;
begin
  inherited;
  fCardHost:= Self;
  fCardPanel:= Self;
  HandleAddProps;
end;

function TViewer.PrimaryKey: Variant;
var us: IUsData;
begin
  us:= AsUsData;
  if (us = nil) or us.EOF then exit(unassigned);
  result:= us.GetColData(0);
end;

function TViewer.MainText: string;
var us: IUsData;
begin
  us:= AsUsData;
  if (us = nil) or us.EOF or (us.ColCount < 2) then exit('');
  result:= us.GetColData(1);
end;

function TViewer.MasterPK: Variant;
var m: TViewer;
begin
  m:= Master;
  if m = nil then
    exit(null);
  result:= m.PrimaryKey;
end;

procedure TViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if   (AComponent <> fCardView)
    or (Operation <> opRemove)
    or (csDestroying in ComponentState)
  then
    exit;
  fCardView:= nil;
end;

procedure TViewer.NotifyDetailsDataChanged;
var f: TViewer;
begin
  if ChildNotifyEnabled then
    for f in Details<TViewer> do
      f.MasterDataChanged;
end;

procedure TViewer.HandleAddProps;
var
  info: PPropInfo;
  m: TMethod;
  a,p: TArray<string>;
  s: string;
  obj: TObject;
begin
  m.Data:= Self;
  a:= AttrProps;
  for s in a do begin
    m.Code:= nil;
    p:= s.Split(['=']);
    p[0]:= trim(p[0]);
    p[1]:= trim(p[1]);
    info:= GetPropInfo(Self, p[0]);
    if info = nil then
      raise Exception.CreateFmt('%s Property not found: "%s"', [ClassName, p[0]]);
    if SameText(p[1], 'Self') then
      SetObjectProp(Self, p[0], Self)
    else
    if SameText(p[1], 'nil') then
      case info.PropType^.Kind of
        tkClass : SetObjectProp(Self, p[0], nil);
        tkMethod: SetMethodProp(self, p[0], m);
        tkInterface,
        tkProcedure,
        tkPointer: SetOrdProp(Self, p[0], 0);
      end
    else begin
      case info.PropType^.Kind of
        tkClass: begin
            obj:= FindComponent(p[1]);
            if obj = nil then
              raise Exception.CreateFmt('%s Component not found: "%s"', [ClassName, p[1]]);
            SetObjectProp(Self, p[0], obj);
          end;
        tkMethod: begin
            m.Code:= MethodAddress(p[1]);
            SetMethodProp(self, p[0], m);
          end
      else
        SetPropValue(Self, p[0], p[1]);
      end;
    end;
  end;
end;

procedure TViewer.MasterDataChanged;
begin
  RefreshData(true);
end;

procedure TViewer.Popup(Force: boolean);
var
  f: TCustomForm;
  Sheet: TxTabSheet;
  wc: TWinControl;
begin
  if Force then
    PopupForm;
  CardPanel.Visible:= true;
  if GetParentForm(CardPanel, false) = Self then
    Show;
  CardPanel.BringToFront;
  f:= GetParentForm(CardPanel);
  f.BringToFront;
  if ParentSheet(CardPanel, Sheet) then
    Sheet.PageControl.ActivePage:= Sheet;
  wc:= TViewer(CardPanel).FindNextControl(nil, true, true, true);
  if wc <> nil then
    wc.SetFocus;
  PostMessage(CardPanel.Handle, WMC_ACTIVE_CONTROL, 0, 0);
end;

procedure TViewer.PopupForm;
var F: TCustomForm;
begin
  F:= GetParentForm(CardPanel);
  if not assigned(F) then F:= Self;
  if F.WindowState = wsMinimized then
    F.WindowState:= wsNormal;
  F.Show;
  F.BringToFront;
end;

procedure TViewer.Post;
begin
  if CardInplaced then begin
    fCardView.Post;
    exit;
  end;
  if not GetCanPost then exit;
  FLastOper:= opPost;
  if FVwrModalResult = vmrCanceled then // ���������� ������ ������ Post
    FVwrModalResult:= ModalResultByState(vState);
  FixDataChanges;
  if Modified then begin
    CheckRequired;
    InternalPost;
  end else
    InternalCancel;
  fState:= vstActive;
  if fAutoCard then
    CardVisible:= false;
end;

procedure TViewer.PostError(Code: TPostError);
const MSG = '������ ��������������. ���������� � ������������.'#13#10'%s [%s]';
begin
  raise Exception.CreateFmt(MSG, [
                DebugName(Self),
                Copy(GetEnumName(TypeInfo(TPostError), ord(Code)), 4, MaxInt)
  ]);
end;

function TViewer.RefreshData(force: boolean): TViewer;
begin
  result:= RefreshData(fLastParams, force);
end;

class procedure TViewer.RegisterViewer;
begin
  ViewerDict.Add(TViewerClass(Self), ObjMainEntityID);
end;

class function TViewer.MainEntityID: TEntityID;
begin
  if not ViewerDict.TryGetValue(TViewerClass(Self), result) then
    result:= ObjMainEntityID;
end;

function TViewer.RefreshData(const Params: string; force: boolean): TViewer;
begin
  result:= Self;
  if not SameParams(fLastParams, Params) or (vState = vstInactive) then
    force:= true;
  if not force then exit;

  if Showing or RefreshIfHidden then begin
    InternalRefresh;
    fNeedRefresh:= false;
    NotifyDetailsDataChanged;
  end else
    fNeedRefresh:= true;
  fLastParams:= Params;
end;

function TViewer.SameParams(const P1, P2: string): boolean;
begin
  result:= false;
end;

function TViewer.GetCardAlign: TAlign;
begin
  if tbnCardLeft.Down then
    exit(alLeft);
  if tbnCardRight.Down then
    exit(alRight);
  if tbnCardTop.Down then
    exit(alTop);
  if tbnCardBottom.Down then
    exit(alBottom);
  if tbnCardClient.Down then
    exit(alClient);
  if tbnCardForm.Down then
    exit(alNone);
end;

function TViewer.GetCardVisible: boolean;
begin
  result:= Assigned(fCardView);
end;

procedure TViewer.SetCardAlign(const Value: TAlign);
begin
  case Value of
    alLeft    : tbnCardLeft.Down:= true;
    alRight   : tbnCardRight.Down:= true;
    alTop     : tbnCardTop.Down:= true;
    alBottom  : tbnCardBottom.Down:= true;
    alClient  : tbnCardClient.Down:= true;
    alNone    : tbnCardForm.Down:= true;
  end;
end;

procedure TViewer.SetCardHost(const Value: TWinControl);
begin
  if fCardHost <> Value then
    SetCardPlacement(Value, CardAlign);
end;

procedure TViewer.SaveDockExt;
var cc: TWinControl;
begin
  if not Assigned(fCardView) then exit;
  cc:= fCardView.CardPanel;
  if cc.Align in [alLeft, alRight] then
    fDockExt.X:= cc.Width;
  if cc.Align in [alTop, alBottom] then
    fDockExt.Y:= cc.Height;
end;

procedure TViewer.SetCardPlacement(aAlign: TAlign);
begin
  SetCardPlacement(CardHostByAlign(aAlign), aAlign);
end;

procedure TViewer.SetCardPlacement(aHost: TWinControl; aAlign: TAlign);
begin
  fCardHost:= aHost;
  CardAlign:= aAlign;
  if not CardVisible then exit;
  SaveDockExt;
  _CardHide;
  _CardShow;
end;

procedure TViewer.SetCardVisible(const Value: boolean);
begin
  if not GetCanCard then exit;
  if Value = CardVisible then
    exit;
  if Value then
    CardShow(false)
  else
    CardHide;
end;

procedure TViewer.CardDocking(Into: boolean);
begin
end;

procedure TViewer.CardHide;
begin
  if not CardVisible then exit;
  if not fCardView.CheckDoneEdit then exit;
  _CardHide;
  FreeAndNil(fCardView);
  if Assigned(fSavFocused) and fSavFocused.CanFocus then
    fSavFocused.SetFocus;
end;

function TViewer.CardHostByAlign(aAlign: TAlign): TWinControl;
begin
  result:= Self;
  if aAlign = alNone then
    result:= nil;
end;

procedure TViewer._CardHide;
begin
  if fCardView.Visible then
    fFloatExt:= fCardView.BoundsRect
  else
    SaveDockExt;
  CardDocking(false);
end;

procedure TViewer.CardShow(AutoCard: boolean);
var
  cc: TWinControl;
begin
  fSavFocused:= nil;
  if CardVisible or not GetCanCard then exit;
  cc:= Screen.ActiveControl;
  if (cc <> nil) and (GetParentForm(cc) = Self) then
    fSavFocused:= cc;
  fCardView:= fCardViewClass.CreateViewer(Self);
  try
    fCardView.fAutoCard:= AutoCard;
    fCardView.RefreshData;
    _CardShow;
  except
    FreeAndNil(fCardView);
    raise;
  end;
  fAutoCard:= AutoCard;
end;

procedure TViewer._CardShow;
  procedure ReposToCenter;
  var R,D: TRect;
  begin
    R:= BoundsRect;
    D:= fCardView.BoundsRect;
    fCardView.Left:= (R.Left + R.Right + D.Left - D.Right) div 2;
    fCardView.Top:= (R.Top + R.Bottom + D.Top - D.Bottom) div 2;
  end;
  //---
const
  MSG = '%s: �� ��������� property CardPanel';
var
  cc: TWinControl;
begin
  cc:= fCardView.CardPanel;
  if cc = nil then
    raise Exception.CreateFmt(MSG, [fCardView.ClassName]);

  if fCardHost = nil then begin  // undock
    CardDocking(false);
    SaveDockExt;
    cc.Parent:= fCardView;
    cc.Align:= alClient;
    if fFloatExt.IsEmpty then
      ReposToCenter
    else
      fCardView.BoundsRect:= fFloatExt;
    fCardView.Popup;
  end else begin                 // dock
    fCardView.Hide;
    cc.Hide;
    cc.Align:= CardAlign;
    cc.Parent:= fCardHost;
    if fDockExt.X = 0 then fDockExt.X:= cc.Constraints.MinWidth;
    if fDockExt.Y = 0 then fDockExt.Y:= cc.Constraints.MinHeight;
    cc.Width:= min(fDockExt.X, 3 * cc.Parent.ClientWidth div 4);
    cc.Height:= min(fDockExt.Y, 3 * cc.Parent.ClientHeight div 4);
    fCardView.Popup;
    cc.Parent.Realign;
    CardDocking(true);
  end;
end;

procedure TViewer.SetMaster(const Value: TViewer);
begin
  if Owner = Value then exit;
  if Assigned(Value) then
    Value.InsertComponent(Self)
  else if Assigned(Owner) then
    Owner.RemoveComponent(Self);
end;

procedure TViewer.SetModified(Value: Boolean);
begin
end;

procedure TViewer.SetReadOnly(const Value: boolean);
begin
  FReadOnly:= Value;
end;

procedure TViewer.SetRefreshIfHidden(const Value: boolean);
begin
  if fRefreshIfHidden = Value then exit;
  fRefreshIfHidden:= Value;
  if Value and NeedRefresh then
    RefreshData(true);
end;

procedure TViewer.StateChanged;
  //--
  procedure CheckRights;
  begin
    case vState of
      vstInsert: if GetCanInsert(true) {or (vsCopy and GetCanInsCopy(true))} then exit;
      vstEdit:   if GetCanEdit(true) then exit;
      else exit;
    end;
    InternalCancel;
    Abort;
  end;
  //--
begin
  CheckRights;
  FOldState:= vState;
end;

procedure TViewer.tbnCardAlignClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: SetCardPlacement(alLeft);
    2: SetCardPlacement(alRight);
    3: SetCardPlacement(alTop);
    4: SetCardPlacement(alBottom);
    5: SetCardPlacement(alClient);
    6: SetCardPlacement(alNone);
  end;
end;

procedure TViewer.UpdateActions;
begin
  inherited;
  if csDesigning in ComponentState then exit;

  if FOldState <> vState then
    StateChanged;
  FLastOper:= opNothing;
end;

procedure TViewer.WMCActiveControl(var Msg: TMessage);
begin
  if Assigned(ActiveControl) and ActiveControl.CanFocusEx then
    ActiveControl.SetFocus;
end;

procedure TViewer.WMMove(var Message: TWMMove);
begin
  fFloatExt:= TRect.Empty;
  inherited;
end;

function TViewer.CheckDoneEdit(FreeDetail: boolean): boolean;
var i: integer;
begin
  if not CheckDetailsDoneEdit(FreeDetail) then exit(false);
  FixDataChanges;
  result:= not (vState in [vstInsert, vstEdit]) and not Modified;
  if result then exit;
  i:= IDNO;
  if Modified then begin
    i:= IDYES;
    if not AutoConfirm(vState) then begin
      Popup;
      i:= MessageDlg(MsgSaveNoCancel, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    end;
  end;
  case i of
    IDYES   :
    begin
      FLastOper:= opPost;
      InternalPost;
    end;
    IDNO    :
    begin
      FLastOper:= opCancel;
      InternalCancel;
    end;
    IDCANCEL: exit;
  end;
  result:= vState in [vstActive, vstInactive];
end;

{ TViewerDict }

function TViewerDict.CardByEntityID(const EntityID: TEntityID): TViewerClass;
begin
  result:= Find(EntityID, true);
end;

function TViewerDict.ListByEntityID(const EntityID: TEntityID): TViewerClass;
begin
  result:= Find(EntityID, false);
end;

function TViewerDict.CardByEntityID(const EntityID: TEntityID;
                                                out cc: TViewerClass): boolean;
begin
  cc:= Find(EntityID, true, false);
  result:= cc <> nil;
end;

function TViewerDict.ListByEntityID(const EntityID: TEntityID;
                                                out vc: TViewerClass): boolean;
begin
  vc:= Find(EntityID, false, false);
  result:= vc <> nil;
end;

function TViewerDict.Find(const EntityID: TEntityID; Card, RaiseError: boolean): TViewerClass;
const
  ERR = 'Viewer class �� ��������������� ��� EntityID = "%s"';
var
  pair: TPair<TViewerClass, TEntityID>;
begin
  for pair in Self do begin
    if (pair.Value <> EntityID) then Continue;
    if not Card xor pair.Key.InheritsFrom(BaseCardClass) then
      exit(pair.Key);
  end;
  if RaiseError then
    raise Exception.CreateFmt(ERR, [EntityID]);
  result:= nil;
end;

initialization
  ViewerDict:= TViewerDict.Create;

finalization
  FreeAndNil(ViewerDict);

end.

