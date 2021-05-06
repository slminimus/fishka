unit dmDatas;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls,
  cxImageList, cxGraphics, cxStyles, cxGridTableView, cxClasses, cxVGrid;

type
  TDmDb = class(TDataModule)
    imlTools: TcxImageList;
    StyleRepo: TcxStyleRepository;
    stlHeader: TcxStyle;
    stlInactive: TcxStyle;
    stlCategory: TcxStyle;
    stlMandatory: TcxStyle;
    stlInfo: TcxStyle;
    stlNoUser: TcxStyle;
    stlStrikeOut: TcxStyle;
    stlGroup: TcxStyle;
    stlGridSelection: TcxStyle;
    stlGrdEven: TcxStyle;
    stlGridHeader: TcxStyle;
    stlGroupByBox: TcxStyle;
    stsGridView: TcxGridTableViewStyleSheet;
    stsVerticalGrid: TcxVerticalGridStyleSheet;
    stlGray: TcxStyle;
    stlGridInactive: TcxStyle;
  private
  public
  end;

var
  DmDb: TDmDb;

implementation
{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

end.
