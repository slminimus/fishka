{-----------------------------------------------------------------------------
 Unit Name: uDBProvider
 Author:    sl
 Date:      12-���-2019
 Purpose:
    ��� ����������, ������������ ����������� � �� ������ ����. �������� �������
����������: � uses ���� ������� ���������� ���� (�����������) ������, �
������, ����������� ���������� uifProvider, ���������� �������� � ������.
-----------------------------------------------------------------------------}

unit uDBProvider;

interface
uses Classes, SysUtils, uifProvider;

type
  TDBProvider = class(TInterfacedObject, IDBProvider)
  private
    fConnection: IConnection;
    function  Connection: IConnection;
    function  Connect(const URL, aUser, aPassword: string; const aRole: string = ''): IConnection;
    procedure Disconnect;
    function  IsConnected: boolean;
  public
    destructor Destroy; override;
  end;

var
// ���� ������ �������� ������ ������, ����������� ���������� Connect
  NewConnectionProc: TNewConnectionProc = nil;

function DBProvider: IDBProvider;
// ����� ��������������� IMSDBQuery; ���� TRS = nil, ������������ �������� ���������� �� ���������.
function QPrepare(const SQL: string; TRS: ITransaction = nil): IDBQuery;
// ����� ��������������� IDBQuery � ����� ������� �����������. ������� ��� ����� Open.
// ���� ��� Destroy ���������� �������, ����������� Rollback.
function QPrepareWR(const SQL: string): IDBQuery;
// ����� ��������������� IDBQuery � ����� ������� �����������.
// �������������� ���������� ���� "�������".
// ���� ��� Destroy ���������� �������, ����������� Rollback.
function QPrepareBatch(const SQL: string): IDBQuery;

implementation

var
  fDBProvider: IDBProvider = nil;

function DBProvider: IDBProvider;
begin
  if not assigned(fDBProvider) then
    fDBProvider:= TDBProvider.Create;
  Result:= fDBProvider;
end;

function QPrepare(const SQL: string; TRS: ITransaction): IDBQuery;
begin
  Result:= DBProvider.Connection.QPrepare(SQL, TRS);
end;

function QPrepareWR(const SQL: string): IDBQuery;
begin
  Result:= DBProvider.Connection.QPrepareWR(SQL);
end;

function QPrepareBatch(const SQL: string): IDBQuery;
begin
  Result:= DBProvider.Connection.QPrepareBatch(SQL);
end;

{ TDBProvider }

function TDBProvider.Connect(const URL, aUser, aPassword, aRole: string): IConnection;
begin
  result:= NewConnectionProc(URL, aUser, aPassword, aRole);
  if not Assigned(fConnection) then
    fConnection:= result;
end;

function TDBProvider.Connection: IConnection;
begin
  result:= fConnection;
end;

destructor TDBProvider.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TDBProvider.Disconnect;
begin
  fConnection:= nil;
end;

function TDBProvider.IsConnected: boolean;
begin
  result:= Assigned(fConnection) and fConnection.IsConnected;
end;

initialization

finalization
  fDBProvider:= nil;

end.
