unit uvCards;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, ufViewer, cxClasses,
  dxBar, System.Actions, Vcl.ActnList, DB, usIntfs, uCornDefs, slTools,
  cxSplitter, uEntities, Vcl.ExtCtrls, dxmdaset;

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
    DockPanel: TPanel;
    dsRecord: TdxMemData;
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
    procedure AssignRow(Src: IUsData); override;
    procedure InsertRow(Src: IUsData); override;
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

procedure TvwrCard.AssignRow(Src: IUsData);
begin
  UsCopyData(dsRecord, Src);
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
  dsRecord.Active:= false;
  dsRecord.Active:= true;
  GetOpMethod(OP_SELECT, true).SetParams(AsUsData)
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

procedure TvwrCard.InsertRow(Src: IUsData);
begin
  UsCopyData(dsRecord, Src);
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
  else begin
    dsRecord.Active:= false;
    dsRecord.Active:= true;
  end;
  dsRecord.Edit;
  dsRecord.Fields[0].Clear;
  dsRecord.Post; // clear Modified flag
  dsRecord.Edit;
  THack(dsRecord).SetState(dsInsert);
end;

procedure TvwrCard.InternalPost;
var
  vs: TViewerState;
begin
  vs:= vState;
  inherited;
  dsRecord.First;
  ModalResult:= mrOk;
  if Master = nil then exit;

  case vs of
    vstInsert: Master.InsertRow(AsUsData.Start);
    vstEdit  : Master.AssignRow(AsUsData.Start);
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
    GetOpMethod(OP_SELECT, true).SetParams(m.AsUsData)
                                .Invoke(dsRecord)
  else
    UsCopyData(dsRecord, us, 1);
  if dsRecord.IsEmpty then
    InternalInsert;
end;

function TvwrCard.IsEmpty: boolean;
begin
  result:= dsRecord.IsEmpty;
end;

end.
