unit udbcDummy;

interface

uses
  System.SysUtils, System.Classes, uDbCtrls;

type
  TdbcDummy = class(TDbController)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dbcDummy: TdbcDummy;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
