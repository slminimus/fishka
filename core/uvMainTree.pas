unit uvMainTree;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  dxBar, cxClasses, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs,
  ufViewer, System.Actions, Vcl.ActnList, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxCustomData, cxStyles, cxTL, cxMaskEdit, Vcl.ImgList,
  cxTLdxBarBuiltInMenu, DB, dxmdaset, System.ImageList, cxImageList, cxDBTL,
  cxNavigator, cxInplaceContainer, cxTLData, uEntities, uEntSystem, UiTypes,
  Vcl.Menus;

type
  [Entity(CID_MTREE)]
  TvwrMainTree = class(TViewer)
    Tree: TcxDBTreeList;
    TreeID: TcxDBTreeListColumn;
    TreeNAME: TcxDBTreeListColumn;
    TreePARENT: TcxDBTreeListColumn;
    TreeENTITY: TcxDBTreeListColumn;
    TreeImages: TcxImageList;
    DataSource: TDataSource;
    MemData: TdxMemData;
    MemDataID: TStringField;
    MemDataNAME: TStringField;
    MemDataPARENT: TStringField;
    MemDataENTITY: TStringField;
    pmTree: TPopupMenu;
    miEditing: TMenuItem;
    procedure TreeGetNodeImageIndex(Sender: TcxCustomTreeList;
      ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType;
      var AIndex: TImageIndex);
    procedure TreeNavigatorButtonsButtonClick(Sender: TObject;
      AButtonIndex: Integer; var ADone: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure DataSourceStateChange(Sender: TObject);
    procedure miEditingClick(Sender: TObject);
  private
    procedure CheckRights;
  protected
    procedure InternalRefresh; override;
  public
  end;

implementation
{$R *.dfm}

{ TvwrMainTree }

procedure TvwrMainTree.FormCreate(Sender: TObject);
begin
  CheckRights;
  Height:= Screen.PrimaryMonitor.WorkareaRect.Height;
end;

procedure TvwrMainTree.CheckRights;
begin
  Tree.OptionsData.Deleting:= true;
  Tree.OptionsData.Appending:= true;
  Tree.OptionsData.Inserting:= true;
end;

procedure TvwrMainTree.InternalRefresh;
begin
  GetOpMethod(OP_SELECT, false).Invoke(MemData);
  Tree.FullExpand;
end;

procedure TvwrMainTree.miEditingClick(Sender: TObject);
begin
  if MemData.State = dsBrowse then
    Tree.Navigator.Buttons.Edit.Click;
end;

procedure TvwrMainTree.DataSourceStateChange(Sender: TObject);
begin
  Tree.OptionsSelection.CellSelect:= MemData.State <> dsBrowse;
end;

procedure TvwrMainTree.TreeGetNodeImageIndex(Sender: TcxCustomTreeList;
                ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType;
                                                    var AIndex: TImageIndex);
begin
  AIndex:= 0;
  if ANode.getFirstChild <> nil then exit;
  AIndex:= 1 + ord(VarIsNull(TreeENTITY.Values[ANode]));
end;

procedure TvwrMainTree.TreeNavigatorButtonsButtonClick(Sender: TObject;
                                    AButtonIndex: Integer; var ADone: Boolean);
begin
  case AButtonIndex of
    NBDI_REFRESH: RefreshData(true);
  end;
end;

end.
