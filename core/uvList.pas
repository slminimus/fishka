unit uvList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, ufViewer, cxClasses,
  System.Actions, Vcl.ActnList, dxBarBuiltInMenu, cxGraphics, cxControls, dxBar,
  cxLookAndFeels, cxLookAndFeelPainters, cxPC, Vcl.StdCtrls, cxStyles, cxGrid,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, cxGridTableView,
  cxEdit, dxDateRanges, cxGridCustomView, cxGridCustomTableView, cxGridLevel,
  CommCtrl, usIntfs, usCx;

type
  TvwrList = class(TViewer)
    pgsMain: TcxPageControl;
    tsList: TcxTabSheet;
    lvlMain: TcxGridLevel;
    grdMain: TcxGrid;
    tsCard: TcxTabSheet;
    procedure ViewCanFocusRecord(Sender: TcxCustomGridTableView;
                            ARecord: TcxCustomGridRecord; var AAllow: Boolean);
    procedure ViewFocusedRecordChanged(Sender: TcxCustomGridTableView;
                        APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord;
                                        ANewItemRecordFocusingChanged: Boolean);
    procedure ViewCellDblClick(Sender: TcxCustomGridTableView;
             ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
                                    AShift: TShiftState; var AHandled: Boolean);
    procedure FormCreate(Sender: TObject);
  private
  protected
    fdc: TcxGridDataController;
    procedure Loaded; override;
    procedure CardDocking(Into: boolean); override;
    function  CardHostByAlign(aAlign: TAlign): TWinControl; override;
  public
    function  MainView: TcxGridTableView; inline;
    function  AsUsData(CurrentRowOnly: boolean): IUsData; override;
    function  IsEmpty: boolean; override;
  end;

implementation
{$R *.dfm}
uses usDb, usClasses;

{ TvwrList }

procedure TvwrList.FormCreate(Sender: TObject);
begin
  inherited;
  pgsMain.ActivePage:= tsList;
end;

function TvwrList.IsEmpty: boolean;
begin
  result:= fdc.FilteredRecordCount = 0;
end;

procedure TvwrList.Loaded;
begin
  inherited;
  fdc:= MainView.DataController;

  pgsMain.Properties.HideTabs:= true;
  pgsMain.ActivePage:= tsList;
end;

procedure TvwrList.CardDocking(Into: boolean);
begin
  inherited;
  if not Into then
    pgsMain.ActivePage:= tsList;
end;

function TvwrList.CardHostByAlign(aAlign: TAlign): TWinControl;
begin
  case aAlign of
    alLeft   : result:= tsList;
    alRight  : result:= tsList;
    alTop    : result:= tsList;
    alBottom : result:= tsList;
    alClient : result:= tsCard;
//    alNone   : result:= nil;
    else       result:= nil;
  end;
end;

function TvwrList.MainView: TcxGridTableView;
begin
  result:= TcxGridTableView(lvlMain.GridView);
end;

function TvwrList.AsUsData(CurrentRowOnly: boolean): IUsData;
begin
  result:= NewUsData(MainView).Start(-1);
  if CurrentRowOnly then
    result:= NewUsDataCache(result, 1);
end;

procedure TvwrList.ViewCanFocusRecord(Sender: TcxCustomGridTableView;
                            ARecord: TcxCustomGridRecord; var AAllow: Boolean);
begin
  AAllow:= CheckDetailsDoneEdit(false);
end;

procedure TvwrList.ViewFocusedRecordChanged(Sender: TcxCustomGridTableView;
                        APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord;
                                        ANewItemRecordFocusingChanged: Boolean);
begin
  NotifyDetailsDataChanged;
end;

procedure TvwrList.ViewCellDblClick(Sender: TcxCustomGridTableView;
            ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
                                   AShift: TShiftState; var AHandled: Boolean);
begin
  if GetCanCard then
    CardVisible:= true;
end;

end.
