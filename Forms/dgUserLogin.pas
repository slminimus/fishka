unit dgUserLogin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,cxControls, cxContainer, cxEdit, cxTextEdit, cxLookAndFeelPainters,
  cxButtons, cxCheckBox, ExtCtrls, Menus, dxGDIPlusClasses, cxGraphics,
  cxMaskEdit, cxDropDownEdit, cxLookAndFeels, slTools, Generics.Collections,
  System.Actions, Vcl.ActnList;

type
  TfUserLogin = class(TForm)
    pnKupon: TPanel;
    lbKupon: TLabel;
    Label3: TLabel;
    pnlMain: TPanel;
    lblLogin: TLabel;
    lblPassword: TLabel;
    shp1: TShape;
    edLogin: TcxTextEdit;
    edPassword: TcxTextEdit;
    imgKupon: TImage;
    btnOk: TcxButton;
    btnCancel: TcxButton;
    cbSaveAuthData: TcxCheckBox;
    lbChangePassw: TLabel;
    Bevel1: TBevel;
    lblDB: TLabel;
    edDB: TcxComboBox;
    txtDbPath: TStaticText;
    Actions: TActionList;
    acOK: TAction;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edDBPropertiesChange(Sender: TObject);
    procedure acOKUpdate(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
    procedure lblDBDblClick(Sender: TObject);
  private type
    TRec = record
      Url : string;
      User: string;
      Psw: string;
    end;
    TDict = TDictStr<TRec>;
  private
    fDict: TDict;
    procedure DbChanged;
    procedure SaveAuthData;
    procedure ReadIniFiles;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    class function Execute: boolean;
  end;

implementation
uses Inifiles, uEntities, slfTools, dgDbAliases;
{$R *.DFM}

class function TfUserLogin.Execute: boolean;
begin
  result:= Create(Application).ShowModal = mrOk;
end;

procedure TfUserLogin.FormCreate(Sender: TObject);
begin
  fDict:= TDict.Create;
end;

procedure TfUserLogin.FormDestroy(Sender: TObject);
begin
  fDict.Free;
end;

procedure TfUserLogin.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    if (Application.MainForm = nil) or not Application.MainForm.Visible then
      ExStyle:= ExStyle or WS_EX_APPWINDOW; // place icon to windows taskbar
end;

procedure TfUserLogin.FormShow(Sender: TObject);
begin
  ReadIniFiles;
end;

procedure TfUserLogin.ReadIniFiles;
var i: integer;
  ini: TIniFile;
  pth: string;
    s: string;
    a: TArray<string>;
  lst: TStringList;
  rec: TRec;
begin
  txtDbPath.Caption:= '';
  KeyboardLayout.SetEng;

  edDb.Properties.Items.Clear;
  rec.User:= '';
  rec.Psw:= '';
  lst:= TStringList.Create;
  try
    pth:= ExtractFilePath(ParamStr(0));
    ini:= TIniFile.Create(pth + 'Connect.cfg');
    try
      ini.ReadSectionValues('DbAliases', lst);
      for i:= 0 to lst.Count -1 do begin
        s:= trim(lst.Names[i]);
        edDb.Properties.Items.Add(s);
        rec.Url:= trim(lst.ValueFromIndex[i]);
        fDict.AddOrSetValue(s, rec);
      end;
    finally
      ini.Free;
    end;

    ini:= TIniFile.Create(pth + OSUserName +'.ini');
    try
      ini.ReadSectionValues('DbAliases', lst);
      for i:= 0 to lst.Count -1 do begin
        s:= trim(lst.Names[i]);
        if fDict.TryGetValue(s, rec) then begin
          rec.User:= trim(lst.ValueFromIndex[i]);
          if rec.User <> '' then begin
            a:= rec.User.Split(['@']);
            if Length(a) > 1 then begin
              rec.User:= a[0];
              rec.Psw:= a[1];
            end;
          end;
          fDict[s]:= rec;
        end;
      end;
      i:= 0;
      s:= trim(ini.ReadString('Common', 'LastConnect', ''));
      if s <> '' then
        i:= edDb.Properties.Items.IndexOf(s);
    finally
      ini.Free;
    end;
  finally
    lst.Free;
  end;
  edDB.ItemIndex:= i;
  DbChanged;
end;

procedure TfUserLogin.lblDBDblClick(Sender: TObject);
begin
  if TdlgDbAliases.Execute('Connect.cfg') then
    ReadIniFiles;
end;

procedure TfUserLogin.SaveAuthData;
var s: string;
  ini: TIniFile;
  pth: string;
begin
    if edDB.Text = '' then exit;
    pth:= ExtractFilePath(ParamStr(0));
    ini:= TIniFile.Create(pth + OSUserName +'.ini');
    try
      ini.WriteString('Common', 'LastConnect', edDB.Text);
      s:= edLogin.Text;
      if GetKeyState(VK_CONTROL) < 0 then
        s:= s +'@'+ edPassword.Text;
      ini.WriteString('DbAliases', edDB.Text, s);
    finally
      ini.Free;
    end;
end;

procedure TfUserLogin.edDBPropertiesChange(Sender: TObject);
begin
  DbChanged;
end;

procedure TfUserLogin.DbChanged;
var s: string;
  rec: TRec;
begin
  s:= edDB.Text;
  if fDict.TryGetValue(s, rec) then begin
    txtDbPath.Caption:= rec.Url;
    edLogin.Text:= rec.User;
    edPassword.Text:= rec.Psw;
  end else begin
    txtDbPath.Caption:= '';
    edLogin.Text:= OSUserName;
    edPassword.Text:= '';
  end;
  if edLogin.Text = '' then
    edLogin.Text:= OSUserName;
end;

procedure TfUserLogin.acOKExecute(Sender: TObject);
begin
  if cbSaveAuthData.Checked then
    SaveAuthData;
  PushCursor;
  if DataService.Connected then
    DataService.Disconnect;

  DataService.Connect(txtDbPath.Caption, edLogin.Text, edPassword.Text);
  if DataService.Connected then
    ModalResult:= mrOk;
end;

procedure TfUserLogin.acOKUpdate(Sender: TObject);
begin
  btnOk.Enabled:= txtDbPath.Caption <> '';
end;

end.

