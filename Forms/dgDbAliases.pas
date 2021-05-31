unit dgDbAliases;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData,
  cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges, Data.DB,
  cxDBData, cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGrid;

type
  TdlgDbAliases = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    Level: TcxGridLevel;
    Grid: TcxGrid;
    View: TcxGridTableView;
    View_Alias: TcxGridColumn;
    View_DbPath: TcxGridColumn;
    Panel1: TPanel;
    procedure btnOKClick(Sender: TObject);
  private
    fIniFile: string;
    function Exec(const IniFile: string): boolean;
  public
    class function Execute(const IniFile: string): boolean;
  end;

implementation
{$R *.dfm}
uses Inifiles;

{ TdlgDbAliases }

class function TdlgDbAliases.Execute(const IniFile: string): boolean;
begin
  result:= Create(Application).Exec(ExtractFilePath(ParamStr(0)) + IniFile);
end;

procedure TdlgDbAliases.btnOKClick(Sender: TObject);
var i: integer;
  ini: TIniFile;
begin
  ini:= TIniFile.Create(fIniFile);
  try
    ini.EraseSection('DbAliases');
    for i:= 0 to view.DataController.RecordCount -1 do begin
      with view.DataController do
        ini.WriteString('DbAliases', Values[i, 0], Values[i, 1]);
    end;
  finally
    ini.Free;
  end;
  ModalResult:= mrOk;
end;

function TdlgDbAliases.Exec(const IniFile: string): boolean;
var i: integer;
  ini: TIniFile;
  lst: TStringList;
begin
  lst:= nil;
  fIniFile:= IniFile;
  view.BeginUpdate;
  ini:= TIniFile.Create(IniFile);
  try
    lst:= TStringList.Create;
    ini.ReadSectionValues('DbAliases', lst);
    view.DataController.SetRecordCount(lst.Count);
    for i:= 0 to lst.Count -1 do begin
      view.DataController.Values[i, 0]:= trim(lst.Names[i]);
      view.DataController.Values[i, 1]:= trim(lst.ValueFromIndex[i]);
    end;
  finally
    view.EndUpdate;
    ini.Free;
    lst.Free;
  end;
  result:= ShowModal = mrOk;
end;

end.
 
