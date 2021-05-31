unit uvDSViewer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, ufViewer, cxClasses,
  dxBar, System.Actions, Vcl.ActnList, Data.DB, dxmdaset, uCornDefs, uEntities,
  usIntfs;

type
  TvwrDSViewer = class(TViewer)
    MemData: TdxMemData;
    DataSource: TDataSource;
    procedure MemDataBeforePost(DataSet: TDataSet);
  private
  protected
    procedure InternalRefresh; override;
    function  GetState: TViewerState; override;
  public
    procedure AssignRow(Src: IUsData); override;
    function  AsUsData(CurrentRowOnly: boolean): IUsData; override;
    function  IsEmpty: boolean; override;
  end;

implementation
{$R *.dfm}
uses usDb, usClasses, DbConsts;

{ TvwrDSViewer }

procedure TvwrDSViewer.AssignRow(Src: IUsData);
begin
  UsAssignRow(MemData, Src.Start(-1), true, true);
end;

function TvwrDSViewer.AsUsData(CurrentRowOnly: boolean): IUsData;
begin
  result:= NewUsData(MemData).Start(-1);
  if CurrentRowOnly then
    result:= NewUsDataCache(result, 1, -1);
end;

function TvwrDSViewer.GetState: TViewerState;
begin
  case MemData.State of
    dsBrowse  : result:= vstActive;
    dsEdit    : result:= vstEdit;
    dsInsert  : result:= vstInsert;
    else        result:= vstInactive;
  end;
end;

procedure TvwrDSViewer.InternalRefresh;
begin
  GetOpMethod(OP_SELECT, false).Invoke(MemData);
end;

function TvwrDSViewer.IsEmpty: boolean;
begin
  result:= MemData.IsEmpty;
end;

procedure TvwrDSViewer.MemDataBeforePost(DataSet: TDataSet);
var Oper: TPrivOper;
begin
  case DataSet.State of
    dsEdit  : Oper:= OP_UPDATE;
    dsInsert: Oper:= OP_INSERT;
    else
     DatabaseError(SNotEditing, DataSet);
  end;
  GetOpMethod(Oper, true).SetParams(DataSet).Invoke(
    procedure(us: IUsData)
    begin
      UsAssignRow(DataSet, us);
    end
  );
end;

end.
