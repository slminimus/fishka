unit uDSCtrls;

interface

uses
  System.SysUtils, System.Classes, DB, uControllers, uDbCtrls, UsIntfs;

type
  IDSController = interface(IDbController)
    function  CanDelete: boolean;
    function  CanRefresh: boolean;
    function  CanEdit(IgnoreState: boolean = false): boolean;
    function  CanInsert(IgnoreState: boolean = false): boolean;
    function  CanInsCopy(IgnoreState: boolean = false): boolean;
    function  CanPost: boolean;
    // Freeze/Defreeze считают вложенность
    procedure Freeze;             // Frozen:= true
    function  Defreeze: boolean;  // Frozen:= false
    function  Frozen: boolean;    // scroll, insert, delete is disabled
    function  GetReadOnly: boolean;
    procedure SetReadOnly(Value: boolean);
    function  GetModified: boolean;
    procedure SetModified(Value: boolean);
    function  MainDataSet: TDataSet;
    function  EditDataSet: TDataSet;
    procedure CheckRequired;
    // перенести значения edit controls в поля DataSet'а
    procedure FixDataChanges;
    function  IsEmpty: boolean;
    function  State: TDataSetState;
    procedure Cancel;
    procedure Delete;
    procedure Insert(doCopy: boolean);
    procedure Post;
    procedure Edit;
    property  ReadOnly: boolean read GetReadOnly write SetReadOnly;
    property  Modified: boolean read GetModified write SetModified;
  end;

  TDSController = class(TDbController, IController, IDSController)
  protected
    fMainDataSet: TDataSet;
    fEditDataSet: TDataSet;
    FFrozen: integer;
    procedure Loaded; override;
  protected //-- IDbController
    function  GetData(Proc: TProc<IUsData>): boolean; override;
  public //-- IDSController
    function  CanDelete: boolean;
    function  CanRefresh: boolean;
    function  CanEdit(IgnoreState: boolean = false): boolean;
    function  CanInsert(IgnoreState: boolean = false): boolean;
    function  CanInsCopy(IgnoreState: boolean = false): boolean;
    function  CanPost: boolean;
    // Freeze/Defreeze считают вложенность
    procedure Freeze;             // Frozen:= true
    function  Defreeze: boolean;  // Frozen:= false
    function  Frozen: boolean;    // scroll, insert, delete is disabled
    function  GetReadOnly: boolean;
    procedure SetReadOnly(Value: boolean);
    function  GetModified: boolean;
    procedure SetModified(Value: boolean);
    function  MainDataSet: TDataSet;
    function  EditDataSet: TDataSet;
    procedure CheckRequired;
    // перенести значения edit controls в поля DataSet'а
    procedure FixDataChanges;
    function  IsEmpty: boolean;
    function  State: TDataSetState;
    procedure Cancel;
    procedure Delete;
    procedure Insert(doCopy: boolean);
    procedure Post;
    procedure Edit;
  end;

implementation
{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}
uses TypInfo, usClasses, usDb;


{ TDSController }

procedure TDSController.Cancel;
begin
  if not Assigned(fEditDataSet) then exit;
  if fEditDataSet.State in dsEditModes then
    fEditDataSet.Cancel;
end;

function TDSController.CanDelete: boolean;
begin
  result:= not Frozen and not GetReadOnly and not IsEmpty
           and (fEditDataSet.State = dsBrowse)
;//           and IsEnabled(POP_DELETE);
end;

function TDSController.CanEdit(IgnoreState: boolean): boolean;
begin
  result:= not GetReadOnly
           and (IgnoreState or (EditDataSet.State = dsBrowse))
;//           and IsEnabled(POP_UPDATE);
end;

function TDSController.CanInsCopy(IgnoreState: boolean): boolean;
begin
  result:= CanInsert and not IsEmpty;
end;

function TDSController.CanInsert(IgnoreState: boolean): boolean;
begin
  result:= not Frozen and not GetReadOnly
           and (IgnoreState or (fEditDataSet.State = dsBrowse))
;//           and IsEnabled(POP_INSERT);
end;

function TDSController.CanPost: boolean;
begin
  result:=  Assigned(fEditDataSet) and (fEditDataSet.State in dsEditModes);
end;

function TDSController.CanRefresh: boolean;
begin
  result:= not Frozen
           and (fEditDataSet.State in [dsBrowse, dsInactive])
;//           and IsEnabled(POP_SELECT);
end;

procedure TDSController.CheckRequired;
begin
end;

function TDSController.Defreeze: boolean;
var i: integer;
begin
  i:= AtomicDecrement(FFrozen);
  if i < 0 then
    i:= AtomicIncrement(FFrozen);
  result:= i > 0;
end;

procedure TDSController.Delete;
begin
  if CanDelete then
    fEditDataSet.Delete;
end;

procedure TDSController.Edit;
begin
  if CanEdit then
    fEditDataSet.Edit;
end;

function TDSController.EditDataSet: TDataSet;
begin
  result:= fEditDataSet;
end;

procedure TDSController.FixDataChanges;
begin
  if not Assigned(fEditDataSet) then exit;
  if fEditDataSet.State in dsEditModes then
    fEditDataSet.UpdateRecord;
end;

procedure TDSController.Freeze;
begin
  AtomicIncrement(FFrozen);
end;

function TDSController.Frozen: boolean;
begin
  result:= FFrozen <> 0;
end;

function TDSController.GetData(Proc: TProc<IUsData>): boolean;
begin
  result:= fMainDataSet <> nil;
  if result then
    Proc(NewUsData(fMainDataSet));
end;

function TDSController.GetModified: boolean;
begin
  result:= Assigned(fEditDataSet) and fEditDataSet.Modified;
end;

function TDSController.GetReadOnly: boolean;
var P: PPropInfo;
begin
  result:= false;
  P:= GetPropInfo(fEditDataSet, 'ReadOnly');
  if Assigned(P) then
    result:= GetPropValue(fEditDataSet, P);
end;

procedure TDSController.Insert(doCopy: boolean);
begin
  if CanInsert then
    fEditDataSet.Insert;
end;

function TDSController.IsEmpty: boolean;
begin
  result:= not Assigned(fEditDataSet) or fEditDataSet.IsEmpty;
end;

procedure TDSController.Loaded;
var ds: TComponent;
begin
  inherited;
  fMainDataSet:= FindComponent('dsMain') as TDataSet;
  ds:= FindComponent('dsEdit');
  if ds <> nil then
    fEditDataSet:= ds as TDataSet
  else
    fEditDataSet:= fMainDataSet;
end;

function TDSController.MainDataSet: TDataSet;
begin
  result:= fMainDataSet;
end;

procedure TDSController.Post;
begin
  if CanPost then
    fEditDataSet.Insert;
end;

procedure TDSController.SetModified(Value: boolean);
begin
end;

procedure TDSController.SetReadOnly(Value: boolean);
var P: PPropInfo;
begin
  P:= GetPropInfo(EditDataSet, 'ReadOnly');
  if Assigned(P) then
    SetPropValue(EditDataSet, P, Value);
end;

function TDSController.State: TDataSetState;
begin
  result:= dsInactive;
  if Assigned(fEditDataSet) then
    result:= fEditDataSet.State
  else
  if Assigned(fMainDataSet) then
    result:= fMainDataSet.State;
end;


end.
