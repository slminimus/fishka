unit ufMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  uBaseForms, Vcl.StdCtrls, uEntClasses, System.Actions, Vcl.ActnList,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles,
  cxEdit, cxVGrid, cxInplaceContainer, Vcl.Menus, cxButtons, dmDatas,
  cxDropDownEdit;

type
  TMainForm = class(TBaseForm)
    Button1: TButton;
    ActionList1: TActionList;
    acConnect: TAction;
    acTest1: TAction;
    procedure acTest1Execute(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation
{$R *.dfm}
uses ufViewer;

procedure TMainForm.acTest1Execute(Sender: TObject);
begin
  TViewer.ClassByEntityID(CID_TEST).ShowViewer;
end;

end.
