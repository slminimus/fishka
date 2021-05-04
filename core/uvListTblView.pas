unit uvListTblView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, uvListDb, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, dxBar, cxClasses,
  dxBarBuiltInMenu, System.Actions, Vcl.ActnList, cxGridLevel, cxGrid, cxFilter,
  cxPC, cxCustomData, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  cxGridCustomTableView, cxGridTableView, cxGridCustomView, dmDatas,
  Vcl.StdCtrls, usCx;

type
  TvvrListTblView = class(TvwrListDB)
    gvMain: TcxGridTableView;
  private
  public
  end;


implementation

{$R *.dfm}

end.
