unit ufVPages;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxPC,
  dxBarBuiltInMenu, uBaseForms, Vcl.Menus, cxSplitter, Vcl.ExtCtrls,
  Viewers;

type
  TMainPages = class(TForm)
    Pgs: TcxPageControl;
    pmTabs: TPopupMenu;
    miClosePage: TMenuItem;
    miCloseOthers: TMenuItem;
    N4: TMenuItem;
    miSaveDesktop: TMenuItem;
    procedure PgsCanCloseEx(Sender: TObject; ATabIndex: Integer;
      var ACanClose: Boolean);
    procedure PgsDblClick(Sender: TObject);
    procedure PgsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PgsPageChanging(Sender: TObject; NewPage: TcxTabSheet;
      var AllowChange: Boolean);
    procedure PgsTabEndDrag(AControl: TcxCustomTabControl; ANewIndex: Integer);
    procedure PgsTabStartDrag(AControl: TcxCustomTabControl; AIndex: Integer);
    procedure miClosePageClick(Sender: TObject);
    procedure miCloseOthersClick(Sender: TObject);
    procedure miSaveDesktopClick(Sender: TObject);
  private
    fDragTab: integer;
    function UndockViewer(TabIndex: integer = -1; Maximize: boolean = false): TViewer;
    procedure CloseOtherPages(TabIndex: integer);
    procedure SaveDesktop;
    procedure LoadDesktop;
    procedure PagesToDesktop(lst: TStringList);
  public
    function GetViewer(index: integer = -1): TViewer; overload;
    function GetViewer(out Vwr: TViewer; index: integer = -1): boolean; overload;
  end;

implementation
{$R *.dfm}
uses Math;

function TMainPages.GetViewer(out Vwr: TViewer; index: integer): boolean;
var ts: TcxTabSheet;
begin
  result:= false;
  if index < 0 then
    index:= Pgs.ActivePageIndex;
  if (index < 0) or (index >= Pgs.PageCount) then exit;
  ts:= Pgs.Pages[index];
  result:= (ts.ControlCount > 0) and (ts.Controls[0] is TViewer);
  if result then
    Vwr:= TViewer(ts.Controls[0]);
end;

function TMainPages.GetViewer(index: integer): TViewer;
begin
  if not GetViewer(result, index) then
    result:= nil;
end;

procedure TMainPages.PgsCanCloseEx(Sender: TObject; ATabIndex: Integer;
                                          var ACanClose: Boolean);
var vwr: TViewer;
begin
  if GetViewer(vwr, ATabIndex) then
    ACanClose:= vwr.CloseQuery;
  if ACanClose and (Pgs.TabCount > 1) then
    Pgs.ActivePageIndex:= Max(0, ATabIndex -1);
end;

procedure TMainPages.PgsDblClick(Sender: TObject);
begin
  UndockViewer(-1, true);
end;

procedure TMainPages.PgsMouseUp(Sender: TObject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
var ndx: integer;
  P: TPoint;
begin
  if Button <> mbRight then exit;
  ndx:= Pgs.IndexOfTabAt(X, Y);
  if ndx < 0 then exit;
  pmTabs.Tag:= ndx;
  P:= Pgs.ClientToScreen(Point(X,Y));
  pmTabs.Popup(P.X, P.Y);
end;

procedure TMainPages.PgsPageChanging(Sender: TObject; NewPage: TcxTabSheet;
                                            var AllowChange: Boolean);
var Vwr: TViewer;
begin
  if GetViewer(Vwr, NewPage.PageIndex) then
    Vwr.RefreshData;
end;

procedure TMainPages.PgsTabEndDrag(AControl: TcxCustomTabControl;
                                            ANewIndex: Integer);
begin
  if ANewIndex = fDragTab then
    UndockViewer(fDragTab);
end;

procedure TMainPages.PgsTabStartDrag(AControl: TcxCustomTabControl;
                                              AIndex: Integer);
begin
  fDragTab:= AIndex;
end;

function TMainPages.UndockViewer(TabIndex: integer; Maximize: boolean): TViewer;
begin
  result:= nil;
  if TabIndex < 0 then
    TabIndex:= Pgs.ActivePageIndex;
  if not GetViewer(result, TabIndex) then exit;
  if Pgs.TabCount > 1 then
    Pgs.ActivePageIndex:= Max(0, TabIndex -1);
  result.Undock(Maximize);
end;

procedure TMainPages.PagesToDesktop(lst: TStringList);
var i: integer;
  vwr: TViewer;
begin
  for i:= 0 to Pgs.PageCount -1 do
    if GetViewer(vwr, i) then
      lst.Add(vwr.ClassName +'='+ vwr.SerializeParams);
end;

procedure TMainPages.SaveDesktop;
var
  lst: TStringList;
  vwr: TViewer;
begin
  lst:= TStringList.Create;
  try
    PagesToDesktop(lst);
    DmDb.JvAppStore.IniFile.EraseSection(sDesktop);
    DmDb.JvAppStore.WriteStringList(sDesktop, lst, 'Page');
    if GetViewer(vwr) then
      DmDb.JvAppStore.IniFile.WriteString(sDesktop, sActivePage, vwr.ClassName);
    DmDb.JvAppStore.Flush;
  finally
    lst.Free;
  end;
end;

procedure TMainPages.LoadDesktop;
var i: integer;
  lst: TStringList;
   vc: TViewerClass;
    a: TArray<TCustomViewer>;
   ap: string;
   pg: integer;
  dvc: TDictViewers;
begin
  DmDb.JvAppStore.Reload;
  dvc:= nil;
  lst:= TStringList.Create;
  try
    DmDb.JvAppStore.ReadStringList(sDesktop, lst, false, 'Page');
    if lst.Count = 0 then exit;
    dvc:= TDictViewers.Create;
    if GetViewers(dvc) = 0 then exit;
    ap:= DmDb.JvAppStore.IniFile.ReadString(sDesktop, sActivePage, '');
    SetLength(a, lst.Count);
    pg:= -1;
    for i:= 0 to High(a) do begin
      a[i]:= nil;
      if not dvc.TryGetValue(lst.Names[i], vc) then Continue;
      a[i]:= vc.Respawn;
      a[i].DeserializeParams(lst.ValueFromIndex[i]);
      if a[i].ClassNameIs(ap) then
        pg:= i;
    end;
    a[max(0, pg)].Popup;
    for i:= 0 to High(a) do
      if Assigned(a[i]) then
        a[i].Show;
  finally
    lst.Free;
    dvc.Free;
  end;
end;

procedure TMainPages.miCloseOthersClick(Sender: TObject);
begin
  CloseOtherPages(pmTabs.Tag);
end;

procedure TMainPages.miClosePageClick(Sender: TObject);
begin
  Pgs.CloseTab(pmTabs.Tag);
end;

procedure TMainPages.miSaveDesktopClick(Sender: TObject);
begin
  SaveDesktop;
end;

procedure TMainPages.CloseOtherPages(TabIndex: integer);
var  i: integer;
  Viewer: TCustomViewer;
begin
  if not GetViewer(Viewer, TabIndex) then exit;
  Viewer.Popup;
  for i:= Pgs.PageCount -1 downto 0 do
    if i <> TabIndex then
      Pgs.CloseTab(i);
end;

end.
