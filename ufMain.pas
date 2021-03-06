unit ufMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Forms,
  dxBar, cxClasses, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs,
  ufViewer, System.Actions, Vcl.ActnList, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxCustomData, cxStyles, cxTL, cxMaskEdit, Vcl.ImgList,
  cxTLdxBarBuiltInMenu, DB, dxmdaset, System.ImageList, cxImageList, cxDBTL,
  cxNavigator, cxInplaceContainer, cxTLData, uEntities, uEntSystem, UiTypes,
  Vcl.Menus, uCornDefs, usIntfs, Vcl.ExtCtrls, uvDSViewer;

type
  [Entity(CID_MTREE)]
  TMainForm = class(TvwrDSViewer)
    Tree: TcxDBTreeList;
    TreeID: TcxDBTreeListColumn;
    TreeNAME: TcxDBTreeListColumn;
    TreePARENT: TcxDBTreeListColumn;
    TreeENTITY: TcxDBTreeListColumn;
    TreeImages: TcxImageList;
    pmTree: TPopupMenu;
    miEditing: TMenuItem;
    Panel1: TPanel;
    MemDataID: TStringField;
    MemDataNAME: TStringField;
    MemDataPARENT: TStringField;
    MemDataENTITY: TStringField;
    MemDataTAG: TIntegerField;
    procedure TreeGetNodeImageIndex(Sender: TcxCustomTreeList;
      ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType;
      var AIndex: TImageIndex);
    procedure FormCreate(Sender: TObject);
    procedure DataSourceStateChange(Sender: TObject);
    procedure miEditingClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CheckRights;
  protected
    procedure InternalCancel;override;
    procedure InternalDelete;override;
    procedure InternalEdit; override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    procedure InternalInsert(doCopy: boolean = false); override;
  public
    function  EditByCard: boolean; override;
  end;

var
  MainForm: TMainForm;

implementation
{$R *.dfm}
uses usClasses, UsDB;

{ TvwrMainTree }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fCardPanel:= Panel1;
  CardAlign:= alNone;
  CheckRights;
  RefreshData(true);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  inherited;
  Height:= Monitor.WorkareaRect.Height;
end;

procedure TMainForm.CheckRights;
begin
  Tree.OptionsData.Deleting:= true;
  Tree.OptionsData.Appending:= true;
  Tree.OptionsData.Inserting:= true;
end;

procedure TMainForm.InternalCancel;
begin
  Tree.Navigator.Buttons.Cancel.Click;
end;

procedure TMainForm.InternalDelete;
begin
  Tree.Navigator.Buttons.Delete.Click;
end;

procedure TMainForm.InternalEdit;
begin
  Tree.Navigator.Buttons.Edit.Click;
end;

procedure TMainForm.InternalInsert(doCopy: boolean);
begin
  Tree.Navigator.Buttons.Append.Click;
end;

procedure TMainForm.InternalPost;
begin
  Tree.Navigator.Buttons.Post.Click;
end;

procedure TMainForm.InternalRefresh;
begin
  inherited;
  Tree.FullExpand;
end;

procedure TMainForm.miEditingClick(Sender: TObject);
begin
  Tree.Navigator.Buttons.Edit.Click;
end;

procedure TMainForm.DataSourceStateChange(Sender: TObject);
begin
  Tree.OptionsSelection.CellSelect:= MemData.State <> dsBrowse;
end;

function TMainForm.EditByCard: boolean;
begin
  result:= false;
end;

procedure TMainForm.TreeGetNodeImageIndex(Sender: TcxCustomTreeList;
                ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType;
                                                    var AIndex: TImageIndex);
begin
  AIndex:= 0;
  if ANode.getFirstChild <> nil then exit;
  AIndex:= 1 + ord(VarIsNull(TreeENTITY.Values[ANode]));
end;

end.
