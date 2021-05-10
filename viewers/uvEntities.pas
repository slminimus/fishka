unit uvEntities;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, uvListTblView, cxPC,
  dxBarBuiltInMenu, cxGraphics, cxControls, cxLookAndFeels, cxStyles, cxEdit,
  cxLookAndFeelPainters, cxCustomData, cxFilter, cxData, cxDataStorage, cxGrid,
  cxNavigator, dxDateRanges, cxClasses, Actions, ActnList, cxGridLevel, dxBar,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, ufViewer, uEntities,
  uEntClasses;

type
  TvwrEntities = class(TvwrListTblView)
  private
  public
  end;

implementation
{$R *.dfm}


initialization
  TvwrEntities.RegisterViewer;

end.
