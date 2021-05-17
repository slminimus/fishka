unit uvCards;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, ufViewer, cxClasses,
  dxBar, System.Actions, Vcl.ActnList, DB, MemDS, usIntfs, uCornDefs, usTools,
  cxSplitter, uEntities, Vcl.ExtCtrls;

type
  TPanel = class(Vcl.ExtCtrls.TPanel)
  private
    Splitter: TcxSplitter;
  protected
    procedure SetParent(aParent: TWinControl); override;
    procedure Loaded; override;
  public
  end;

  TvwrCard = class(TViewer)
    srcRecord: TDataSource;
    dsRecord: THMemTable;
    DockPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
  private
    fDbRefresh: boolean;
    type THack = class(TDataSet);
    procedure GetEditRecord;
  protected
    procedure DoShow; override;
    procedure FixDataChanges; override;
    function  GetModified: Boolean; override;
    function  GetState: TViewerState; override;
    procedure InternalCancel; override;
    procedure InternalDelete; override;
    procedure InternalEdit; override;
    procedure InternalInsert(doCopy: boolean = false); override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
  public
    function  AsUsData: IUsData; override;
    function  IsEmpty: boolean; override;
  end;

implementation
{$R *.dfm}
uses TypInfo, usDb, usClasses;
{ TPanel }

procedure TPanel.Loaded;
begin
  inherited;
  if Tag <> -1 then exit;
  Splitter:= TcxSplitter.Create(Self);
  Splitter.Visible:= false;
  Splitter.Parent:= Self;
  Splitter.Control:= nil;
  Splitter.HotZoneClassName:= 'TcxMediaPlayer9Style';
  Splitter.ResizeUpdate:= true;
end;

procedure TPanel.SetParent(aParent: TWinControl);
begin
  inherited SetParent(aParent);
  if Tag <> -1 then exit;
  if [csReading, csDestroying] * ComponentState <> [] then exit;

  Splitter.Visible:= false;
  if (aParent = Owner) or not(Align in [alTop, alBottom, alLeft, alRight]) then begin
    Splitter.Control:= nil;
    Splitter.Parent:= Self;
    exit;
  end;
  Splitter.Parent:= aParent;
  Splitter.Control:= Self;
  Splitter.Align:= Align;
  Splitter.Visible:= true;
end;

{ TvwrCard }

procedure TvwrCard.FormCreate(Sender: TObject);
begin
  inherited;
  RefreshIfHidden:= true;
  fNeedRefresh:= true;
  fCardPanel:= DockPanel;
end;

procedure TvwrCard.acRefreshExecute(Sender: TObject);
begin
  fDbRefresh:= true;
  try
    inherited;
  finally
    fDbRefresh:= false;
  end;
end;

function TvwrCard.AsUsData: IUsData;
begin
  result:= NewUsData(dsRecord);
end;

procedure TvwrCard.DoShow;
begin
  inherited;
  CardPanel.Parent:= Self;
  CardPanel.Align:= alClient;
end;

procedure TvwrCard.FixDataChanges;
begin
  if dsRecord.State in [dsEdit, dsInsert] then
    dsRecord.UpdateRecord;
end;

procedure TvwrCard.GetEditRecord;
begin
  if not dsRecord.IsEmpty then exit;
  dsRecord.Reopen;
  GetOpMethod(OP_SELECT).SetParams(AsUsData)
                        .Invoke(dsRecord);
end;

function TvwrCard.GetModified: Boolean;
begin
  result:= dsRecord.Modified;
end;

function TvwrCard.GetState: TViewerState;
begin
  case dsRecord.State of
    dsBrowse  : result:= vstActive;
    dsEdit    : result:= vstEdit;
    dsInsert  : result:= vstInsert;
    else        result:= vstInactive;
  end;
end;

procedure TvwrCard.InternalCancel;
var b: boolean;
begin
  b:= dsRecord.State = dsInsert;
  dsRecord.Cancel;
  if fAutoCard or Master.IsEmpty then
    Close
  else if b then
    InternalRefresh;
end;

procedure TvwrCard.InternalDelete;
begin
  inherited;
  Master.DeleteRow;
  if Master.IsEmpty then
    Close;
end;

procedure TvwrCard.InternalEdit;
begin
  GetEditRecord;
  dsRecord.Edit;
end;

procedure TvwrCard.InternalInsert(doCopy: boolean);
begin
  if DoCopy and not IsEmpty then
    GetEditRecord
  else
    dsRecord.ClearData;
  dsRecord.Edit;
  dsRecord.Fields[0].Clear;
  dsRecord.Post; // clear Modified
  dsRecord.Edit;
  THack(dsRecord).SetState(dsInsert);
end;

procedure TvwrCard.InternalPost;
const
  MSG = '%s.Post: Error Viewer State';
var
  oper: string;
    pk: string;
   pkn: string;
    vs: TViewerState;
   row: IUsData;
begin
  pk:= '';
  pkn:= dsRecord.Fields[0].FieldName;
  case vState of
    vstEdit:   oper:= OP_UPDATE;
    vstInsert: oper:= OP_INSERT;
    else
      raise Exception.CreateFmt(MSG, [ClassName]);
  end;
  PushCursor;
  GetOpMethod(oper).SetParams(dsRecord).Invoke(
    procedure(us: IUsData)
    begin
      if not us.EOF then
        pk:= coalesce(us.Values[0], '');
    end
  );
  if pk = '' then
    PostError(bpeIsEmpty);

  vs:= vState;
  dsRecord.Post;
  GetOpMethod(OP_GETROW).SetParams(dsRecord).SetParam(pkn, pk).Invoke(
    procedure(us: IUsData)
    begin
      dsRecord.CopyData(us);
      case dsRecord.RecordCount of
        0:   PostError(bpeIsEmpty);
        1: ;
        else PostError(bpeTooMany);
      end;
    end
  );
  dsRecord.First;
  ModalResult:= mrOk;
  if Master = nil then exit;

  row:= AsUsData.Start;
  case vs of
    vstInsert: Master.InsertRow(row);
    vstEdit  : Master.AssignRow(row);
  end;

  if fAutoCard or Master.IsEmpty then
    Close;
end;

procedure TvwrCard.InternalRefresh;
var
   m: TViewer;
  us: IUsData;
begin
  dsRecord.Active:= false;
  m:= Master;
  if not Assigned(m) then exit;

  dsRecord.Active:= true;
  if not fDbRefresh then begin
    us:= m.AsUsData;
    fDbRefresh:= us.ColCount <> Cardinal(dsRecord.FieldCount);
  end;
  if fDbRefresh then
    GetOpMethod(OP_GETROW).SetParams(m.AsUsData)
                          .Invoke(dsRecord)
  else
    dsRecord.CopyData(us, 1);
  if dsRecord.IsEmpty then
    InternalInsert;
end;

function TvwrCard.IsEmpty: boolean;
begin
  result:= dsRecord.IsEmpty;
end;

end.
