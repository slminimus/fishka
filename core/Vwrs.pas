{-----------------------------------------------------------------------------
 Unit Name: Viewers
 Author:    sl.minimus@gmail.com
 Date:      03-окт-2016
 Purpose:
 History:
-----------------------------------------------------------------------------}
unit Vwrs;

interface
uses
  Windows, Types, Classes, SysUtils, Generics.Collections, DB, Messages,
  Dialogs, TypInfo, Controls, ComCtrls, Forms, System.Variants,
  UITypes, CornDefs, uBaseForms, usTools, usIntfs;

type
  TOnMasterNotification = procedure(Sender: TObject; Msg: TMasterNotifyEvent;
                                                     Param: Variant) of object;

  T_Viewer = class;
  TVwrClass = class of T_Viewer;

  T_Viewer = class(TView)
  private
    FOnMasterNotification: TOnMasterNotification;
    fOnRefresh: TOnRefreshEvent;
    fRefreshIfHidden: boolean;
    FMsgConfirmDelete: string;
    FMsgSaveNoCancel: string;
    FOnWMCDataChanged: TNotifyEvent;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMCActiveControl(var Msg: TMessage); message WMC_ACTIVE_CONTROL;
    procedure WMCDataChanged(var Message: TMessage); message WMC_DATACHANGED;
    procedure SetMaster(const Value: T_Viewer);
    procedure SetRefreshIfHidden(const Value: boolean);
    function ConfirmDeleteStored: Boolean;
    function NewDocTextStored: Boolean;
    function MsgSaveNoCancelStored: Boolean;
    function GetMaster: T_Viewer;
  protected
    FConfirms: TConfirms;
    FClosing: boolean;         // Viewer в процессе закрытия
    FReadOnly: boolean;
    fNeedRefresh: boolean;
    FSavedVersion: integer;
    FMainToolBar: TToolbar;
    fSavRO: boolean;
    FLastOper: TOper;
    FLastParams: Variant;
    FOldState: TViewerState;
    fMsgNewDocText: string;
    FVwrModalResult: TVwrModalResult;
    procedure DoRefresh(force: boolean); virtual;
    procedure StateChanged; virtual;
    procedure DoClose(var Action: TCloseAction); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateActions; override;
    procedure MasterNotification(Msg: TMasterNotifyEvent; Param: Variant); virtual;
    //-- управление ------
    procedure _Insert(AsCopy: boolean); virtual;
    procedure _Edit; virtual;
    procedure FixDataChanges; virtual; // забрать данные из edit-контролов (аналог TDataSet.UpdateRecord)
    procedure InternalCancel; virtual;
    procedure InternalDelete; virtual;
    procedure InternalEdit;   virtual;
    procedure InternalInsert(doCopy: boolean = false); virtual;
    procedure InternalPost;   virtual;
    procedure CheckRequired;  virtual;
  protected
    function _CheckDetailDoneEdit(D: TArray<T_Viewer>; FreeIfOk: boolean): boolean;
    function  FormatMsgConfirmDelete: string; virtual;
  //-- properties ---
    function  GetState: TViewerState; virtual;
    function  GetModified: Boolean; virtual;
    procedure SetModified(Value: Boolean); virtual;
    function  GetReadOnly: boolean; virtual;
    procedure SetReadOnly(const Value: boolean); virtual;
    procedure InternalSaveState; virtual;
    procedure PostDataChanged; virtual;
  public
  //-- информация о текущем состоянии ------
    function  AutoConfirm(State: TViewerState): boolean;
    function  GetCanRefresh: boolean; virtual;
    function  GetCanCancel: boolean; virtual;
    function  GetCanDelete: boolean; virtual;
    function  GetCanEdit(IgnoreState: boolean = false): boolean; virtual;
    function  GetCanInsert(IgnoreState: boolean = false): boolean; virtual;
    function  GetCanInsCopy(IgnoreState: boolean = false): boolean; virtual;
    function  GetCanPost: boolean; virtual;
    function  GetCanSaveState: boolean; virtual;
    function  IsEmpty: boolean; virtual;
    function  Frozen: boolean; virtual;
//    class procedure Register(ViewerClass: TViewerClass; Alias: string = '');
    constructor CreateViewer(aMaster: T_Viewer = nil); overload; virtual;
    destructor Destroy; override;
    procedure InitializeNewForm; override;
    function  CloseQuery: Boolean; override;
    // проверить и вернуть возможность закрытия формы, запросить у юзера действие
    // на случай несохраненных изменений. Переадресовать запрос своему Detail и
    // закрыть его, если надо.
    function  CheckDoneEdit(FreeDetail: boolean = false): boolean; virtual;
    function _CheckDoneEdit(FreeDetail: boolean = false): boolean;
    function  CheckDetailDoneEdit(FreeDetails: boolean): boolean; virtual;
    function  DoConfirmDelete: boolean; virtual;
    function  RefreshData(force: boolean = false): T_Viewer; overload; inline; // освежить данные по старым параметрам
    function  RefreshData(const Param: Variant; force: boolean = false): T_Viewer; overload;
    function  AsUsData: IUsData; virtual; abstract; // кэшированные данные
    procedure Popup(Force: boolean = true);  // сделать себя текущим отображаемым
    procedure PopupForm; // сделать верхним окном себя или свой Expl
    // отработка шотката: если редактируется - Cancel, иначе выйти из карточки или Close
    procedure CancelClose; virtual;
    // Редактирование
    procedure Cancel;
    procedure Delete;
    procedure Edit;
    procedure Insert;
    procedure InsCopy;
    procedure Post; virtual;
    // Персистентность
    procedure LoadState; virtual;
    procedure SaveState;
  public
    constructor InplaceInto(Place: TWinControl; aMaster: T_Viewer = nil);
    // не забыть у результата вызвать RefreshData и (Popup или FormStyle:= fsMDIChild)
//    class function ShowFor(const ID: string; CanDuplicate: boolean = false): T_Viewer;
//    class function ThisClass: TViewerClass;
    procedure NotifyDetails(Msg: TMasterNotifyEvent); overload; virtual;
    procedure NotifyDetails(Msg: TMasterNotifyEvent; Param: Variant); overload; virtual;
    function  IsDetail: boolean; inline; // true, если назначен Master
     // IsTop = true, если это основной вьювер (главный в TfrmExpl или свободный).
     // Проверка создания второго экз. выполняется только для вьюверов с IsTop = true
    function  IsTop: boolean; virtual;
    function  Details<T: TView>: TArray<T>; // все Viewers, у которых этот - Master
    function  PrimaryKey: Variant; virtual; // ПК текущей строки.
    function  MainText: string; virtual;    // текст текущей строки; например, для комплексного Node.
    function  MasterPK: Variant; virtual;   // ПК текущей строки Master'а или null.
  public
    property LastParams: Variant read FLastParams;  // параметры последнего RefreshData
    property Modified: Boolean read GetModified write SetModified;
    property Master: T_Viewer read GetMaster; // Viewer основного документа
      // Ранее был вызван RefreshData при Visible = false и RefreshIfHidden = false
    property NeedRefresh: boolean read fNeedRefresh;
      // текущее состояние
    property vState: TViewerState read GetState;
  published
    property MsgConfirmDelete: string read FMsgConfirmDelete write FMsgConfirmDelete stored ConfirmDeleteStored;
    property MsgSaveNoCancel: string read FMsgSaveNoCancel write FMsgSaveNoCancel stored MsgSaveNoCancelStored;
    property MsgNewDocText: string read fMsgNewDocText write fMsgNewDocText stored NewDocTextStored;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default false;
    property RefreshIfHidden: boolean read fRefreshIfHidden write SetRefreshIfHidden default false;
  published
    property OnMasterNotification: TOnMasterNotification read FOnMasterNotification write FOnMasterNotification;
    property OnRefresh: TOnRefreshEvent read fOnRefresh write fOnRefresh;
    property OnWMCDataChanged: TNotifyEvent read FOnWMCDataChanged write FOnWMCDataChanged;
  end;

{--- Типичный код:
  with DisablePaint(Place) do
    InplaceViewer(Place, TMyViewer, Self).RefreshData(nil).Show;
---}
function  InplaceViewer(Place: TWinControl; vc: TVwrClass; aMaster: T_Viewer = nil): T_Viewer; overload;
procedure CloseInplace(Place: TWinControl);

var
  DEF_MSGNEWDOCTEXT   : string = 'Новый документ';
  DEF_MSGSAVENOCANCEL : string = 'Есть изменения. Сохранить?';
  DEF_MSGCONFIRMDELETE: string = 'Удалить?';

implementation
uses StrUtils, ImgList, Graphics;

resourcestring
  SeFormNotAccess = 'Форма "%s": нет доступа';

procedure CloseInplace(Place: TWinControl);
begin
  if Place.ControlCount = 0 then exit;
  if Place.Controls[0] is T_Viewer then
    T_Viewer(Place.Controls[0]).Close;
end;

{ T_Viewer }

constructor T_Viewer.CreateViewer(aMaster: T_Viewer = nil);
var Own: TComponent;
begin
  if Assigned(aMaster) then
    SetMaster(aMaster);
  Own:= Master;
  if Own = nil then
    Own:= Application;
  inherited Create(Own);
end;

constructor T_Viewer.InplaceInto(Place: TWinControl; aMaster: T_Viewer);
begin
  CreateViewer(aMaster);
  BorderStyle:= bsNone;
  Parent:= Place;
  Align:= alClient;
end;

destructor T_Viewer.Destroy;
begin
{$IFNDEF PACKAGE}
  if not(csDesigning in ComponentState) then
  try
    DisablePaint(Self);
    SetMaster(nil);
    FLastParams:= null;
  except
    ApplicationHandleException(nil);
  end;
{$ENDIF}
  inherited;
end;

function T_Viewer.Details<T>: TArray<T>;
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

function T_Viewer.ConfirmDeleteStored: Boolean;
begin
  result:= MsgConfirmDelete <> DEF_MSGCONFIRMDELETE;
end;

procedure T_Viewer.InitializeNewForm;
begin
  inherited;
  MsgNewDocText:= DEF_MSGNEWDOCTEXT;
  MsgConfirmDelete:= DEF_MSGCONFIRMDELETE;
  MsgSaveNoCancel:= DEF_MSGSAVENOCANCEL;
  FConfirms:= [cfIns, cfEdit, cfDel];
end;

function InplaceViewer(Place: TWinControl; vc: TVwrClass; aMaster: T_Viewer = nil): T_Viewer;
var old: T_Viewer;
begin
  result:= nil;
  old:= nil;
  if Place.ControlCount > 0 then
    if Place.Controls[0] is T_Viewer then
      old:= T_Viewer(Place.Controls[0]);
    if not Assigned(old) or (old.ClassType <> vc) then begin
      if not Assigned(result) then
        result:= vc.CreateViewer(aMaster);
      result.BorderStyle:= bsNone;
      result.Parent:= Place;
      result.Align:= alClient;
    end
    else begin
      result:= old;
      old:= nil;
    end;
    if Assigned(old) then
      old.Close;
end;

procedure T_Viewer.Delete;
begin
  if not(GetCanDelete and (AutoConfirm(vsDeleting) or DoConfirmDelete)) then exit;
  if not CheckDetailDoneEdit(false) then exit;
  FLastOper:= opDelete;
  InternalDelete;
end;

procedure T_Viewer.InsCopy;
begin
  if GetCanInsCopy and CheckDetailDoneEdit(false) then
    _Insert(true);
end;

procedure T_Viewer.Insert;
begin
  if GetCanInsert and CheckDetailDoneEdit(false) then
    _Insert(false);
end;

procedure T_Viewer._Insert(AsCopy: boolean);
begin
  InternalInsert(AsCopy);
end;

procedure T_Viewer._Edit;
begin
  InternalEdit;
end;

procedure T_Viewer.Edit;
begin
  if GetCanEdit then
    _Edit;
end;

procedure T_Viewer.StateChanged;
  //--
  procedure CheckRights;
  begin
    case vState of
      vsInsert: if GetCanInsert(true) {or (vsCopy and GetCanInsCopy(true))} then exit;
      vsEdit:   if GetCanEdit(true) then exit;
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

procedure T_Viewer.UpdateActions;
begin
  inherited;
  if csDesigning in ComponentState then exit;

  if FOldState <> vState then
    StateChanged;
  FLastOper:= opNothing;
end;

function T_Viewer.GetCanCancel: boolean;
begin
  result:= Modified or (vState in [vsEdit, vsInsert]);
end;

function T_Viewer.GetCanDelete: boolean;
begin
  result:= not IsEmpty;
end;

function T_Viewer.GetCanEdit(IgnoreState: boolean): boolean;
begin
  result:= not IsEmpty;
end;

function T_Viewer.GetCanInsCopy(IgnoreState: boolean): boolean;
begin
  result:= false;
end;

function T_Viewer.GetCanInsert(IgnoreState: boolean): boolean;
begin
  result:= false;// and not IsComplex;
end;

function T_Viewer.GetCanPost: boolean;
begin
  result:= Modified or (vState in [vsEdit, vsInsert]);
end;

function T_Viewer.GetCanRefresh: boolean;
begin
  result:= false;
end;

function T_Viewer.GetCanSaveState: boolean;
begin
  result:= true;// ieAsSingleDoc;
end;

function T_Viewer.GetMaster: T_Viewer;
begin
  result:= nil;
  if Owner is T_Viewer then
    result:= T_Viewer(Owner);
end;

function T_Viewer.GetModified: Boolean;
begin
  result:= false;
end;

function T_Viewer.GetReadOnly: boolean;
begin
  result:= FReadOnly;
end;

function T_Viewer.IsEmpty: boolean;
begin
  result:= true;
end;

function T_Viewer.Frozen: boolean;
begin
  result:= false;
end;

function T_Viewer.IsTop: boolean;
begin
  result:= Parent <> nil;
end;

function T_Viewer.GetState: TViewerState;
begin
  result:= vsInactive;
end;

procedure T_Viewer.LoadState;
begin
//  FSavedVersion:= CfgReadInteger(CfgSection, 'StateVersion', -1);
end;

procedure T_Viewer.MasterNotification(Msg: TMasterNotifyEvent; Param: Variant);
begin
  if Assigned(FOnMasterNotification) then
    FOnMasterNotification(Self, Msg, Param);
end;

function T_Viewer.MasterPK: Variant;
var m: T_Viewer;
begin
  m:= Master;
  if m = nil then
    exit(null);
  result:= m.PrimaryKey;
end;

function T_Viewer.MsgSaveNoCancelStored: Boolean;
begin
  result:= MsgSaveNoCancel <> DEF_MSGSAVENOCANCEL;
end;

procedure T_Viewer.InternalSaveState;
begin
//  CfgWriteInteger(CfgSection, 'StateVersion', StateVersion);
end;

procedure T_Viewer.Popup(Force: boolean = true);
var Sheet: TxTabSheet;
begin
  if Force then
    PopupForm;
  Show;
  BringToFront;
  if ParentSheet(Self, Sheet) then
    Sheet.PageControl.ActivePage:= Sheet;
  PostMessage(Handle, WMC_ACTIVE_CONTROL, 0, 0);
end;

procedure T_Viewer.PopupForm;
var F: TCustomForm;
begin
  F:= GetParentForm(self);
  if not assigned(F) then F:= Self;
  if F.WindowState = wsMinimized then
    F.WindowState:= wsNormal;
  F.BringToFront;
end;

function ModalResultByState(vState: TViewerState): TVwrModalResult;
begin
 case vState of
  vsInsert:
   result := vmrInserted;
  vsEdit:
   result := vmrUpdated;
  // vsDeleting: result:= vmrInserted;
 else
  result := vmrCanceled;
 end;
end;

procedure T_Viewer.CheckRequired;
begin
end;

procedure T_Viewer.Post;
begin
  if not GetCanPost then exit;
  FLastOper:= opPost;
  if FVwrModalResult = vmrCanceled then // запоминаем только первый Post
    FVwrModalResult:= ModalResultByState(vState);
  FixDataChanges;
  if Modified then begin
    CheckRequired;
    InternalPost;
  end else
    InternalCancel;
end;

procedure T_Viewer.PostDataChanged;
begin
  if HandleAllocated then
    PostMessage(WindowHandle, WMC_DATACHANGED, 0, 0);
end;

function T_Viewer.PrimaryKey: Variant;
begin
  result:= unassigned;
end;

function T_Viewer.MainText: string;
begin
  result:= '';
end;

procedure T_Viewer.WMCActiveControl(var Msg: TMessage);
begin
  if Assigned(ActiveControl) and ActiveControl.CanFocusEx then
    ActiveControl.SetFocus;
end;

procedure T_Viewer.WMCDataChanged(var Message: TMessage);
var Msg: TMsg;
begin
  while PeekMessage(Msg, Handle, WMC_DATACHANGED, WMC_DATACHANGED, PM_REMOVE) do;
  try
    if Assigned(OnWMCDataChanged) then
      OnWMCDataChanged(Self);
  finally
    NotifyDetails(mneDataChanged);
  end;
end;

function T_Viewer.RefreshData(force: boolean = false): T_Viewer;
begin
  result:= RefreshData(Unassigned, force);
end;

function T_Viewer.RefreshData(const Param: Variant; force: boolean = false): T_Viewer;
begin
  result:= self;
  if not VarIsEmpty(Param) then
    FLastParams:= Param;
  if Showing or RefreshIfHidden then
    DoRefresh(force)
  else
    fNeedRefresh:= true;
end;

procedure T_Viewer.DoRefresh(force: boolean);
begin
  if Assigned(fOnRefresh) then
    fOnRefresh(Self, force);
end;

procedure T_Viewer.SaveState;
begin
  if not GetCanSaveState then
    InternalSaveState;
end;

procedure T_Viewer.SetModified(Value: Boolean);
begin
end;

procedure T_Viewer.CMShowingChanged(var Message: TMessage);
begin
  if Showing then begin
    DoRefresh(NeedRefresh);
    UpdateActions;
  end;
  inherited;
end;

procedure T_Viewer.CmTextChanged(var Msg: TMessage);
var Sheet: TxTabSheet;
begin
  inherited;
  if (csDestroying in ComponentState) or (Caption = '') then exit;
  if ParentSheet(Self, Sheet) then
    Sheet.Caption:= Caption;
end;

function T_Viewer.NewDocTextStored: Boolean;
begin
  result:= MsgNewDocText <> DEF_MSGNEWDOCTEXT;
end;

procedure T_Viewer.SetParent(AParent: TWinControl);
var Sav: TWinControl;
begin
  if csDesigning in ComponentState then begin
    inherited;
    exit;
  end;

  Sav:= ActiveControl;   // member
  inherited;   // тут ActiveControl будет сброшен в nil
  SetDesigning(true, false);
  try  // не в DesignTime установка ActiveControl сразу же его фокусирует
    ActiveControl:= Sav; // restore
  finally
    SetDesigning(false, false);
  end;
end;

procedure T_Viewer.SetReadOnly(const Value: boolean);
begin
  FReadOnly:= Value;
end;

procedure T_Viewer.SetRefreshIfHidden(const Value: boolean);
begin
  if fRefreshIfHidden = Value then exit;
  fRefreshIfHidden:= Value;
  if Value and NeedRefresh then
    RefreshData(true);
end;

procedure T_Viewer.SetMaster(const Value: T_Viewer);
begin
  if Owner = Value then exit;
  if Assigned(Value) then
    Value.InsertComponent(Self)
  else if Assigned(Owner) then
    Owner.RemoveComponent(Self);
end;

function T_Viewer.AutoConfirm(State: TViewerState): boolean;
begin
  result:= false;
  case State of
    vsEdit    : result:= not(cfEdit in FConfirms);
    vsInsert  : result:= not(cfIns  in FConfirms);
    vsDeleting: result:= not(cfDel  in FConfirms);
  end;
end;

procedure T_Viewer.FixDataChanges;
begin
end;

procedure T_Viewer.Cancel;
begin
  if not GetCanCancel then exit;
  FLastOper:= opCancel;
  InternalCancel;
end;

procedure T_Viewer.CancelClose;
begin
  if GetCanCancel then
    Cancel;
end;

function T_Viewer.CheckDoneEdit(FreeDetail: boolean): boolean;
begin
  FixDataChanges;
  result:= _CheckDoneEdit(FreeDetail);
end;

function T_Viewer._CheckDoneEdit(FreeDetail: boolean): boolean;
var i: integer;
begin
  if not CheckDetailDoneEdit(FreeDetail) then exit(false);
  result:= not (vState in [vsInsert, vsEdit]) and not Modified;
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
  result:= vState in [vsActive, vsInactive];
end;

function T_Viewer.CloseQuery: Boolean;
begin
  if csDesigning in ComponentState then
    Exit(inherited CloseQuery);

  FClosing:= true;
  try
    result:= inherited CloseQuery and CheckDoneEdit;
    if result then
      SaveState;
  finally
    FClosing:= false; // if Exception or not Result
  end;
end;

procedure T_Viewer.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  if csDesigning in ComponentState then exit;

  case Action of
    caMinimize,
    caHide : Action:= caFree;
    caNone : exit;
    caFree :;
  end;
  FClosing:= true;   // закрываем
  SaveState;
  FLastParams:= null;
end;

function T_Viewer.FormatMsgConfirmDelete: string;
begin
  result:= MsgConfirmDelete;
end;

function T_Viewer.DoConfirmDelete: boolean;
begin
  result:= MessageDlg(FormatMsgConfirmDelete, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure T_Viewer.InternalCancel;
begin
end;

procedure T_Viewer.InternalDelete;
begin
end;

procedure T_Viewer.InternalEdit;
begin
end;

procedure T_Viewer.InternalInsert(doCopy: boolean);
begin
end;

procedure T_Viewer.InternalPost;
begin
end;

function T_Viewer.IsDetail: boolean;
begin
  result:= Assigned(Master);
end;

function T_Viewer.CheckDetailDoneEdit(FreeDetails: boolean): boolean;
begin
  result:= _CheckDetailDoneEdit(Details<T_Viewer>, FreeDetails);
end;

function T_Viewer._CheckDetailDoneEdit(D: TArray<T_Viewer>; FreeIfOk: boolean): boolean;
var f: T_Viewer;
begin
  for f in D do // сначала всех опросить
    if not f.CheckDoneEdit(FreeIfOk) then exit(false);

  result:= true;
  if not FreeIfOk then exit;
  for f in D do  // и только если все согласны - мочить
    f.Close;
end;

procedure T_Viewer.NotifyDetails(Msg: TMasterNotifyEvent; Param: Variant);
var f: T_Viewer;
begin
  for f in Details<T_Viewer> do
    f.MasterNotification(Msg, Param);
end;

procedure T_Viewer.NotifyDetails(Msg: TMasterNotifyEvent);
begin
  NotifyDetails(Msg, null);
end;

function DefConfirmSave: integer;
const MSG = 'ВНИМАНИЕ!'#13#10'Данные изменены!'#13#10'Сохранить изменения?';
begin
  result:= MessageDlg(MSG, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;


end.
