unit uvListDb;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, uvList, cxGraphics,
  dxBarBuiltInMenu, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  dxBar, cxClasses, System.Actions, Vcl.ActnList, cxGridLevel, cxGrid, cxPC,
  cxGridCustomView, usIntfs, usTools, uCornDefs, uEntities, cxCustomData,
  cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  ufViewer, cxGridCustomTableView, cxGridTableView, usCx;

type
  TvwrListDB = class(TvwrList)
  private
  protected
    procedure InternalRefresh; override;
  public
    procedure AssignRow(us: IUsData); override;
    procedure DeleteRow; override;
    procedure InsertRow(us: IUsData); override;
    procedure CopyData(Src: IUsData); override;
  end;


implementation
{$R *.dfm}
uses Math;

{ TvwrListDB }

procedure TvwrListDB.AssignRow(us: IUsData);
begin
  MainView.AssignRow(us);
end;

procedure TvwrListDB.CopyData(Src: IUsData);
begin
  MainView.CopyData(Src);
end;

procedure TvwrListDB.DeleteRow;
begin
  fdc.DeleteFocused;
end;

procedure TvwrListDB.InsertRow(us: IUsData);
begin
  ChildNotifyDisable;
  try
    fdc.InsertRecord(max(0, fdc.FocusedRecordIndex));
    AssignRow(us);
  finally
    ChildNotifyEnable;
  end;
end;

procedure TvwrListDB.InternalRefresh;
begin
  GetOpMethod(OP_SELECT, false).Invoke(Self);
  fState:= vstActive;
end;

end.
