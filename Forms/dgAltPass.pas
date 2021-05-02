unit dgAltPass;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, Vcl.StdCtrls, cxTextEdit,
  cxMaskEdit, cxButtonEdit, System.Actions, Vcl.ActnList, Vcl.ExtCtrls;

type
  TdlgAltPass = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edPass: TcxButtonEdit;
    edPass2: TcxButtonEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    ActionList: TActionList;
    acOK: TAction;
    img: TImage;
    procedure edPassPropertiesButtonClick(Sender: TObject;
                                                  AButtonIndex: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acOKExecute(Sender: TObject);
    procedure acOKUpdate(Sender: TObject);
  private
    fPass: PString;
    function _Exec(Pass: PString): boolean;
    function GetPassVisible: boolean;
    procedure SetPassVisible(const Value: boolean);
    property PassVisible: boolean read GetPassVisible write SetPassVisible;
  public
    class function Exec(out Pass: string): boolean;
  end;

implementation
{$R *.dfm}

class function TdlgAltPass.Exec(out Pass: string): boolean;
begin
  result:= Create(Application)._Exec(@Pass);
end;

procedure TdlgAltPass.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
  if ModalResult = mrOk then
    fPass^:= trim(edPass.Text);
end;

function TdlgAltPass._Exec(Pass: PString): boolean;
begin
  fPass:= Pass;
  result:= ShowModal = mrOk;
end;

procedure TdlgAltPass.acOKExecute(Sender: TObject);
begin
  if not PassVisible and (Trim(edPass.Text) <> Trim(edPass2.Text)) then
    Application.MessageBox('Тексты не совпадают', '', MB_OK + MB_ICONSTOP)
  else
    ModalResult:= mrOk;
end;

procedure TdlgAltPass.acOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:= (trim(edPass.Text) <> '') and
                 (PassVisible or (Trim(edPass2.Text) = Trim(edPass.Text)));
end;

procedure TdlgAltPass.edPassPropertiesButtonClick(Sender: TObject;
                                                        AButtonIndex: Integer);
begin
  PassVisible:= not PassVisible;
end;

function TdlgAltPass.GetPassVisible: boolean;
begin
  result:= edPass.Properties.EchoMode = eemNormal;
end;

procedure TdlgAltPass.SetPassVisible(const Value: boolean);
begin
  if Value then
    edPass.Properties.EchoMode:= eemNormal
  else
    edPass.Properties.EchoMode:= eemPassword;
  edPass2.Properties.EchoMode:= edPass.Properties.EchoMode;
  edPass2.Enabled:= not Value;
end;

end.
