unit uvListTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, uvListDb, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, dxBar, cxClasses,
  dxBarBuiltInMenu, System.Actions, Vcl.ActnList, cxGridLevel, cxGrid, cxFilter,
  cxPC, cxCustomData, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  cxGridCustomTableView, cxGridTableView, cxGridCustomView, dmDatas, ufViewer,
  uEntities, uEntApp, Vcl.StdCtrls, uvListTblView, cxDropDownEdit,
  cxBarEditItem, Vcl.ExtCtrls;

type
  [Entity(CID_TEST)]
  TvwrListTest = class(TvwrListTblView)
    gvMain_CARDNUM: TcxGridColumn;
    gvMain_RELEASED: TcxGridColumn;
    gvMain_FIO: TcxGridColumn;
    gvMain_PHONES: TcxGridColumn;
    gvMain_EMAIL: TcxGridColumn;
    gvMain_DISCONT: TcxGridColumn;
  private
  public
  end;


implementation
{$R *.dfm}

initialization
  TvwrListTest.RegisterViewer;

end.
