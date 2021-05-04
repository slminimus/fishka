unit ufMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  uBaseForms, Vcl.StdCtrls, uEntClasses, System.Actions, Vcl.ActnList,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxEdit, cxVGrid, cxInplaceContainer, Vcl.Menus, cxButtons, dmDatas;

type
  TMainForm = class(TBaseForm)
    Button1: TButton;
    ActionList1: TActionList;
    acConnect: TAction;
    acTest1: TAction;
    cxVerticalGrid1: TcxVerticalGrid;
    edURL: TcxEditorRow;
    edLogin: TcxEditorRow;
    edPass: TcxEditorRow;
    btnConnect: TcxButton;
    procedure Button1Click(Sender: TObject);
    procedure acConnectUpdate(Sender: TObject);
    procedure acConnectExecute(Sender: TObject);
    procedure acTest1Update(Sender: TObject);
    procedure acTest1Execute(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation
{$R *.dfm}
uses uEntities, usTools, ufViewer;

procedure TMainForm.acConnectExecute(Sender: TObject);
begin
  PushCursor;
  if DataService.Connected then
    DataService.Disconnect
  else
    DataService.Connect(edURL.Properties.Value, edLogin.Properties.Value, edPass.Properties.Value);
end;

procedure TMainForm.acConnectUpdate(Sender: TObject);
begin
  TAction(Sender).Checked:= DataService.Connected;
end;

procedure TMainForm.acTest1Execute(Sender: TObject);
begin
  ViewerDict.ListByEntityID(CID_TEST).Create(Application).RefreshData.Show;
end;

procedure TMainForm.acTest1Update(Sender: TObject);
begin
  TAction(Sender).Enabled:= DataService.Connected;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ViewerDict.ListByEntityID(CID_TEST).Create(Application).RefreshData.Show;
end;

end.
