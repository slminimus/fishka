unit FIBTypes;

interface

Uses
  Classes
  , DB
  , DBClient
  , pFIBDatabase
  , pFIBDataSet
  , pFIBQuery
  , FIBQuery
  , SIBFIBEA
  ;

type
//  TMSDatabase = TpFIBDatabase;
//  TMSTransaction = TpFIBTransaction;
//  TMSInternalDBEvent = TSIBfibEventAlerter;

  TSQLVAR = class(TFIBXSQLVAR)
  protected
    function TryGetAsUUID(out UUIDStr :AnsiString) :Boolean;

    function   GetAsString :string;
    function   GetAsAnsiString :AnsiString;
    function   GetAsWideString :WideString;
    function   GetAsVariant :Variant;
    procedure  SetAsString(const aValue: string);
    procedure  SetAsAnsiString(const aValue: AnsiString);
    procedure  SetAsWideString(const aValue: WideString);
    procedure  SetAsVariant(aValue: Variant);
  public
    function IsGuid: boolean;
    //property AsGuid:TGUID read GetAsGUID write SetAsGUID ;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property Value: Variant read GetAsVariant write SetAsVariant;
    //property OldValue: Variant read FOldValue;
  end;

  TSQLDA = class(TFIBXSQLDA)
  protected
    function   GetXSQLVARByName(const Idx: string): TSQLVAR;
    function   GetXSQLVAR(Idx: Integer): TSQLVAR;
  public
    function   FindParam(const aParamName: string): TSQLVAR;
    function   ParamByName(const aParamName: string): TSQLVAR;
    property ByName[const Idx: string]: TSQLVAR read GetXSQLVARByName;
    property Vars[Idx: Integer]: TSQLVAR read GetXSQLVAR; default;
  end;
(*
  TMSDataSet = class(TpFIBDataSet)
  protected
    procedure PSReset; override;
{$IF CompilerVersion >= 21} //2010+
    function GetFieldClass(FieldDef: TFieldDef): TFieldClass; override;
{$ELSE}
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    procedure InternalInitFieldDefs; override;
    procedure CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef); override;
{$IFEND}
{$IF CompilerVersion >= 24} //XE3+
  public
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); override;
{$IFEND}
  public
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;

  protected
    function GetParams: TSQLDA;
  public
    function ParamByName(const ParamName: string): TSQLVAR;
    property Params: TSQLDA read GetParams;
    function FindParam(const ParamName: string): TSQLVAR;
  end;
*)
  TSQL = class(TpFIBQuery)
  protected
    function  GetSQLParams: TSQLDA;
    function  GetFields(const Idx: Integer): TSQLVAR;
  public
    property Params: TSQLDA read GetSQLParams;
    property Fields[const Idx: Integer]: TSQLVAR read GetFields;

    function FieldByName(const FieldName: string): TSQLVAR;
    function FindField(const FieldName: string): TSQLVAR;
    function FN(const FieldName: string): TSQLVAR;
    function  FieldByOrigin(const TableName,FieldName:string):TSQLVAR; //overload;
    function  ParamByName(const ParamName:string): TSQLVAR;
    function  FindParam  (const aParamName: string): TSQLVAR;

    property FldByName[const FieldName: string]: TSQLVAR read FieldByName;  default;
  end;

implementation

uses
  SysUtils
  ,Variants
  ,DBConsts
  // FIB
  ,IBase
  ,pFIBProps
  ,FIBDataSet
  ,pFIBFieldsDescr
  ;

const
  OCTETS_CHARSET_ID = 1;

procedure uuid_to_char (Buf :Pointer);
type
  tBufSrc = array [0..15] of Byte;
  tBufDst = array [0..36] of AnsiChar;
  pBufSrc = ^tBufSrc;
  pBufDst = ^tBufDst;
const
  HexTable :array[0..15] of AnsiChar = '0123456789ABCDEF';
begin
  pBufDst(Buf)[36] := #0;
  pBufDst(Buf)[35] := HexTable[pBufSrc(Buf)[15] and $F];
  pBufDst(Buf)[34] := HexTable[pBufSrc(Buf)[15] shr  4];
  pBufDst(Buf)[33] := HexTable[pBufSrc(Buf)[14] and $F];
  pBufDst(Buf)[32] := HexTable[pBufSrc(Buf)[14] shr  4];
  pBufDst(Buf)[31] := HexTable[pBufSrc(Buf)[13] and $F];
  pBufDst(Buf)[30] := HexTable[pBufSrc(Buf)[13] shr  4];
  pBufDst(Buf)[29] := HexTable[pBufSrc(Buf)[12] and $F];
  pBufDst(Buf)[28] := HexTable[pBufSrc(Buf)[12] shr  4];
  pBufDst(Buf)[27] := HexTable[pBufSrc(Buf)[11] and $F];
  pBufDst(Buf)[26] := HexTable[pBufSrc(Buf)[11] shr  4];
  pBufDst(Buf)[25] := HexTable[pBufSrc(Buf)[10] and $F];
  pBufDst(Buf)[24] := HexTable[pBufSrc(Buf)[10] shr  4];
  pBufDst(Buf)[23] := '-';
  pBufDst(Buf)[22] := HexTable[pBufSrc(Buf)[ 9] and $F];
  pBufDst(Buf)[21] := HexTable[pBufSrc(Buf)[ 9] shr  4];
  pBufDst(Buf)[20] := HexTable[pBufSrc(Buf)[ 8] and $F];
  pBufDst(Buf)[19] := HexTable[pBufSrc(Buf)[ 8] shr  4];
  pBufDst(Buf)[18] := '-';
  pBufDst(Buf)[17] := HexTable[pBufSrc(Buf)[ 7] and $F];
  pBufDst(Buf)[16] := HexTable[pBufSrc(Buf)[ 7] shr  4];
  pBufDst(Buf)[15] := HexTable[pBufSrc(Buf)[ 6] and $F];
  pBufDst(Buf)[14] := HexTable[pBufSrc(Buf)[ 6] shr  4];
  pBufDst(Buf)[13] := '-';
  pBufDst(Buf)[12] := HexTable[pBufSrc(Buf)[ 5] and $F];
  pBufDst(Buf)[11] := HexTable[pBufSrc(Buf)[ 5] shr  4];
  pBufDst(Buf)[10] := HexTable[pBufSrc(Buf)[ 4] and $F];
  pBufDst(Buf)[ 9] := HexTable[pBufSrc(Buf)[ 4] shr  4];
  pBufDst(Buf)[ 8] := '-';
  pBufDst(Buf)[ 7] := HexTable[pBufSrc(Buf)[ 3] and $F];
  pBufDst(Buf)[ 6] := HexTable[pBufSrc(Buf)[ 3] shr  4];
  pBufDst(Buf)[ 5] := HexTable[pBufSrc(Buf)[ 2] and $F];
  pBufDst(Buf)[ 4] := HexTable[pBufSrc(Buf)[ 2] shr  4];
  pBufDst(Buf)[ 3] := HexTable[pBufSrc(Buf)[ 1] and $F];
  pBufDst(Buf)[ 2] := HexTable[pBufSrc(Buf)[ 1] shr  4];
  pBufDst(Buf)[ 1] := HexTable[pBufSrc(Buf)[ 0] and $F];
  pBufDst(Buf)[ 0] := HexTable[pBufSrc(Buf)[ 0] shr  4];
end;

procedure char_to_uuid (pSrc :pAnsiChar; Len :Integer; pDst :pByte); overload;

  procedure Error;
  begin
    // ��������� pSrc ����� ��������� �� ����� ��� ������������ #0, ���� ����� ������ ����� Copy
    raise EConvertError.CreateFmt('''%s'' is not a valid IB/FB UUID value',[Copy(pSrc,1,Len)]);
  end;

var
  p :pAnsiChar;
  e :pAnsiChar;
begin
  if  Len <> 36  then
    Error;
  p := pSrc-1;
  e := pSrc+35;
  repeat
    Inc(p);
    case  p^  of
      '0'..'9': pDst^ := (Ord(p^) - Ord('0')    ) shl 4;
      'A'..'F': pDst^ := (Ord(p^) - Ord('A') +10) shl 4;
      'a'..'f': pDst^ := (Ord(p^) - Ord('a') +10) shl 4;
      '-'     : if (p-pSrc) in [8,13,18,23] then continue else Error;
      else Error;
    end;

    Inc(p);
    case  p^  of
      '0'..'9': pDst^ := (Ord(p^) - Ord('0')    ) or pDst^;
      'A'..'F': pDst^ := (Ord(p^) - Ord('A') +10) or pDst^;
      'a'..'f': pDst^ := (Ord(p^) - Ord('a') +10) or pDst^;
      else Error;
    end;

    Inc(pDst);
  until p >= e ;
end;

{ TUIDField }

type
//  TUIDField = class (TFIBWideStringField)
  TUIDField = class (TFIBStringField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetSize: Integer; override;
    procedure SetSize(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

class procedure TUIDField.CheckTypeSize(Value: Integer);
begin
  if not (Value in [16,36]) then
    DatabaseError(SInvalidFieldSize);
end;

constructor TUIDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Size:= 36;
end;

function TUIDField.GetSize: Integer;
begin
  Result:= 36;
end;

procedure TUIDField.SetSize(Value: Integer);
begin
  //
end;

{ TMSDataSet }
(*

procedure TMSDataSet.PSReset;
� TpFIBDataSet(FIB+) � TIBDataSet(IBX) ���� ����� ���������� �����������!

���, � ���� ������ ��������� Close � Open (���� ������� �������).

� ��������� ���� ����� "���������" ����������� (TDataSetProvider) �� ���������
"���������" ������ � TClientDataset, FDataSet ��������� ���� ������ ������ ���!
������, Dataset ���� �������� ������ ������ ������ ����������, � ������ "���������"
������ �������� ����������� �� ����, � DLL FireBird-������� (����������� ��������).

���� ������ �� ������, �� ������������ �������� ����� �� ������� (����� ������
�������� �� ������ � �����). � ��� ���� ������ "�������", �� ��������� ���������
������������: ��� �������� DataSet'� �� �������� ����������� "��������" ������
� ClientDataset ����� ����������, ����� ����� ������������� �� ��� ��� ���� ��
���������� ������� "���������" (��� ��������� DLL FireBird-������� ��� ������
API-������� isc_dsql_free_statement).

��������, ���� �������� ��������� ���������:

  procedure Proc (sSql :String; cds :tClientDataset);
  begin
    SVCProvider.DBConnection.DBSQL.PrepareStatement(sSql).PopulateToCDS(cds)
  end;

�� ������ sSql ����������� ������! ������ ��� ��� ���������� cds, � ������ ���
��� ��������� 'end' ��������� (��� �������������� ����������� ���������� �������������
������� PrepareStatement). ���� ������ "������", �� �� end'� ����� "����������"
�� �����. �������� � ���� ������ ��� ���� ����� 7-�� ������!

���� �� ������ sSQL ����� "�������� �������" �� ��� ��� ����!

��������, ��������� ��������:

  try  SVCProvider.DBConnection.DBSQL.ExecSQL('create generator TEST_GENERATOR');  except end;
  SVCProvider.DBConnection.DBSQL.ExecSQL('set generator TEST_GENERATOR to 0');

  cds := TClientDataSet.Create(Nil);
  try
    SVCProvider.DBConnection.DBSQL.PrepareStatement('select gen_id(TEST_GENERATOR,1) from rdb$database').PopulateToCDS(cds);
    SVCProvider.Dialog.ShowMessage(cds.Fields[0].AsString);

    SVCProvider.DBConnection.DBSQL.PrepareStatement('select gen_id(TEST_GENERATOR,1) from rdb$database').PopulateToCDS(cds);
    SVCProvider.Dialog.ShowMessage(cds.Fields[0].AsString);
  finally
    cds.Free;
  end;

  try  SVCProvider.DBConnection.DBSQL.ExecSQL('drop generator TEST_GENERATOR');  except end;

������� ���� � '1' � ����� � '3' ������ ��������� '1' � '2' !

�������, ���� ����� ������������ ������ ��� TDataSetProvider, �������, ������ � ����
������ ������ ������ �� ����, ����� ��� ��������� First, ���� �� ����� ��� �� �����
"����������" tClientDataset`� �������� TpFIBDataSet/TIBDataSet ��� ��������� � ������.
�� ���� ��� ����� �������� �� ���������, ��������� ����� ������� "����������" tClientDataset`�
�� ��� ���� �� � ������.
��������, ���� ������ ����������� TpFIBDataSet/TIBDataSet ����������������, ��
������������ ������ ����� ����� �����-�� �����, �� ��� ����� ����� �������������
��� ������� �� ����� ������.

����������� �������� ����������� <sysoev@i-on.ru>.
���� � ���� ���� ���������, ������.

begin
end;


{$IF CompilerVersion >= 21} // >= 2010

function TMSDataSet.GetFieldClass(FieldDef: TFieldDef): TFieldClass;
begin
  if (FieldDef.DataType = ftString) and (FieldDef.Size = 16) then with FQSelect.Current[FieldDef.FieldNo-1] do
  if (ServerSqlType = SQL_TEXT) and (Byte(ServerSQLSubType) = OCTETS_CHARSET_ID) and (ServerSize = 16) then
    Exit(TUIDField);
  Result := inherited;
end;

{$ELSE} // < 2010

procedure TMSDataSet.InternalInitFieldDefs;
var i :Integer;
begin
  inherited;
  for  i := 0 to FieldDefs.Count-1  do begin
    with  QSelect.Current[i].Data^  do begin
      if  ((sqltype and not 1)=SQL_TEXT)  and  (sqlsubtype = OCTETS_CHARSET_ID)  and  (sqllen = 16)  then begin
        with  FieldDefs[i]  do begin
          DataType := ftTypedBinary;
          Size     := 36; // ������ ���������� ������������� UUID
        end;
      end;
    end;
  end;
end;

function TMSDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := inherited GetFieldClass(FieldType);
  if  FieldType = ftTypedBinary  then
    Result := TUIDField;
end;

procedure TMSDataSet.CheckFieldCompatibility(Field: TField;
  FieldDef: TFieldDef);
begin
  if    (FieldDef.DataType = ftTypedBinary) and (FieldDef.Size = 36)
    and (Field   .DataType = ftString     ) and (Field   .Size = 36)
  then // ��� UUID
    // ���������� ���������
  else // ��� �� UUID
    inherited  CheckFieldCompatibility(Field,FieldDef); // ������������ ����� ������������ ������� �������
end;

{$IFEND}

function TMSDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  i, DataLen :Cardinal;
  LFieldsDescr: PFIBFieldDescr;
begin
  // ��������� ���� ��� ����� ����� ��������������� ����������� ������� ��� ����� CHAR(n) CHARACTER SET OCTETS
  // ����� ���� �� ����� ��������� poTrimCharFields ��� TUIDField
  Result:= inherited GetFieldData(Field,Buffer);
  if  not Result then
    Exit; // ������ ������

  if  not (Field is TFIBStringField)  then
    Exit; // �� ��������� ���� �� �������

  LFieldsDescr := vFieldDescrList[Field.FieldNo-1];

  if  LFieldsDescr.fdDataType <> SQL_TEXT  then
    Exit; // �� CHAR(n) ���� �� �������

  // � FIB ���� ����� ����������� ������� � ������ �������:
  //
  //   ��� �������� �������� ���� ���� CHAR(n) (SQL_TEXT) � ���, ��� n <= 20 � CHARACTER SET �������� �� OCTETS
  //   ������ ����������� ����� � ������ ������.
  //   � ���� ������, ��� n > 20 ��� ���� CHARACTER SET OCTETS, ������ �������� �������� ��������, � � ������
  //   ������ ����������� ������ �� ������ � ��������� �� ���. � ����� ����� ��������������� ������� fdIsSeparateString.
  //   �� �� ���� ������, �������� � ��������� �������� FIB'�� ������� ��������, ��� �������� ������ � ���,
  //   ���� ���� ����� ������� fdIsSeparateString � ���� ������ ��������� ����������� �������, �� �������� ��
  //   ������� poTrimCharFields � TpFIBDataSet.FOptions (��. ����� FIBCacheManage.TRecordsCache.SetStringFromPChar
  //   � FIBDataSet.TFIBCustomDataSet.FetchRecordToCache).
  //   �.�., ���� � TpFIBDataSet.FOptions �� ������� ����� poTrimCharFields (������ ��� ������ �� ���������),
  //   �� �������� ������ ����� ��������� ���� �������� ������� � ������� ���.
  //
  //   ��������, ������ 'select cast("A" as char(10)), cast("A" as char(30)) from rdb$database' ������ ������ � �����
  //   ������, ������ ����� �������� ������� ����� ����� 10 � 1 ! ������ �������? :)
  //
  //   ����� ����, ��� ���� ���� �� ����������� ��� ���� � CHARACTER SET OCTETS ������ �������� ������ ������� �����
  //   � ����� ��������� ����� � ����� �������. ���������� ��� ��� � ������������: ���������� �� UUID ���������������
  //   �� x'20'.
  //   �.�., ��� ����� � CHARACTER SET OCTETS ����� �������� �� ���������!
  //   ��, � ������, ������, �������� CHAR(n) CHARACTER SET OCTETS "��������" �� ��������� � ������� � ����� 0 :).
  //
  // �� ��������, ���� "�������" TFIBCustomDataSet.FetchRecordToCache, ��� ��������� ������������� ��������.
  // ��, ��������� ��������� ������ �������, ����� ��������� ����������� �������� �����, ������� ���������� �����
  // ������� ��� ���� ����� CHAR(n) ����:
  //     - � ���� CHARACTER SET OCTETS - ��� �������������� ����������� �������� ������
  // ��� - � ���� ������� fdIsSeparateString � � �������� ����������� poTrimCharFields � FOptions.
  //
  // �����, ����������� ������� ����� ����������� ��� ���� ����� CHAR(n) CHARACTER SET OCTETS � ��� ���� CHAR(n)
  // ���� � �������� �� �������� ����� poTrimCharFields.
  //
  // ��� ������ ���� � ��������

  if  (LFieldsDescr.fdSubType = OCTETS_CHARSET_ID) // �������� ������ - ������ ���������� ������������ ���������
   or (LFieldsDescr.fdIsSeparateString and not (poTrimCharFields in FOptions)) // �������� �� ���������� � �������� - ���������� ������������ ���������
    then begin
    // ��� ���� ��� �� ���������� ������ �������� � ���� ������ (����� ��������) ���� ���� �� ������������ ���� FValueLength � TFIBStringField,
    // �� ��� ������ �� private. ��� ��� �������� ����� � �����, � ��������������� ������� ���� ��� ��� ����� ������ ������ � ����.
    case State of
      dsNewValue: DataLen := PInteger(GetNewBuffer + LFieldsDescr.fdDataOfs)^;
      dsOldValue: DataLen := PInteger(GetOldBuffer + LFieldsDescr.fdDataOfs)^;
      else        DataLen := PInteger(GetActiveBuf + LFieldsDescr.fdDataOfs)^;
    end;
    // ������ ��������� ������� � ������ ���������� � ����, � ������� pAnsiChar-������
    for i := LFieldsDescr.fdDataSize - 1 downto DataLen do
      (PAnsiChar(Buffer)+i)^ := #$20;
  end;

  // �� � ������, ��� ����� TUIDField �������� �������������� UUID (CHAR(16) CHARACTER SET OCTETS) � ����� (CHAR(36) CHARACTER SET NONE)
  if  (Field is TUIDField)  then
    uuid_to_char(Buffer);
end;

procedure TMSDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  AOptions: TpFIBDsOptions;
  Temp :array of byte;
begin
  if not (Field is TUIDField) then
    inherited
  else begin
    AOptions := FOptions;
    try
      Exclude(FOptions, poTrimCharFields);

      SetLength(Temp,16);
      char_to_uuid(PAnsiChar(Buffer),36,pByte(Temp));

      inherited SetFieldData(Field, Temp);
    finally
      FOptions := AOptions;
    end;
  end;
end;

{$IF CompilerVersion >= 24} //XE3+
function TMSDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
begin
  Result := GetFieldData(Field,Pointer(Buffer));
end;

procedure TMSDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  SetFieldData(Field,Pointer(Buffer));
end;
{$IFEND}

function TMSDataSet.GetParams: TSQLDA;
begin
  Result := TSQLDA( inherited GetParams() );
end;

function TMSDataSet.ParamByName(const ParamName: string): TSQLVAR;
begin
  Result := TSQLVAR( inherited ParamByName(ParamName) );
end;

function TMSDataSet.FindParam(const ParamName: string): TSQLVAR;
begin
  Result := TSQLVAR( inherited FindParam(ParamName) );
end;
*)
{ TSQL }

function TSQL.FieldByOrigin(const TableName, FieldName: string): TSQLVAR;
begin
  Result := TSQLVAR( inherited FieldByOrigin(TableName, FieldName) );
end;

function TSQL.FindField(const FieldName: string): TSQLVAR;
begin
  Result := TSQLVAR( inherited FindField(FieldName) );
end;

function TSQL.FindParam(const aParamName: string): TSQLVAR;
begin
  Result := TSQLVAR( inherited FindParam(aParamName) );
end;

function TSQL.FN(const FieldName: string): TSQLVAR;
begin
  Result := TSQLVAR( inherited FN(FieldName) );
end;

function TSQL.ParamByName(const ParamName: string): TSQLVAR;
begin
  Result := TSQLVAR( inherited ParamByName(ParamName) );
end;

function TSQL.FieldByName(const FieldName: string): TSQLVAR;
begin
  Result := TSQLVAR( inherited FieldByName(FieldName) );
end;

function TSQL.GetSQLParams: TSQLDA;
begin
  Result := TSQLDA(inherited GetSQLParams());
end;

function TSQL.GetFields(const Idx: Integer): TSQLVAR;
begin
  Result := TSQLVAR( inherited GetFields(Idx) );
end;

{ TSQLDA }

function   TSQLDA.GetXSQLVAR(Idx: Integer): TSQLVAR;
begin
  Result := TSQLVAR(inherited GetXSQLVAR(Idx));
end;

function   TSQLDA.GetXSQLVARByName(const Idx: string): TSQLVAR;
begin
  Result := TSQLVAR(inherited GetXSQLVARByName(Idx));
end;

function   TSQLDA.FindParam(const aParamName: string): TSQLVAR;
begin
  Result := TSQLVAR(inherited FindParam(aParamName));
end;

function   TSQLDA.ParamByName(const aParamName: string): TSQLVAR;
begin
  Result := TSQLVAR(inherited ParamByName(aParamName));
end;

{ TSQLVAR }

function TSQLVAR.TryGetAsUUID(out UUIDStr :AnsiString) :Boolean;
var orgOptions: TpFIBQueryOptions;
begin
  Result := not IsMacro and not IsNull and IsGuid;
  if  Result  then begin
    orgOptions := FQuery.Options;
    try
      FQuery.Options := orgOptions - [qoTrimCharFields];
      UUIDStr := inherited GetAsAnsiString();
    finally
      FQuery.Options := orgOptions;
    end;
    SetLength(UUIDStr,36);
    uuid_to_char(@(UUIDStr[1]));
  end;
end;

function TSQLVAR.GetAsString: string;
begin
  //Result := GetAsWideString(); // ��� � ��������� FIB
  {$IFDEF UNICODE}
  Result := GetAsWideString();
  {$ELSE}
  Result := GetAsAnsiString();
  {$ENDIF}
end;

function TSQLVAR.GetAsAnsiString :AnsiString;
begin
  if  not TryGetAsUUID(Result)  then
    Result := inherited GetAsAnsiString();
end;

function TSQLVAR.GetAsWideString: WideString;
var Temp :AnsiString;
begin
  if  not TryGetAsUUID(Temp)  then
    Result := inherited GetAsWideString()
  else
    Result := WideString(Temp);
end;

function TSQLVAR.IsGuid: boolean;
begin
  result:= (FXSQLVar^.sqllen = 16) and (FXSQLVar^.SQLType and (not 1) = SQL_TEXT)
       and (FXSQLVar^.SQLSubType = OCTETS_CHARSET_ID); // SQL-��� ������������ UUID
end;

function TSQLVAR.GetAsVariant: Variant;
var Temp :AnsiString;
begin
  if  not TryGetAsUUID(Temp)  then
    Result := inherited GetAsVariant()
  else
    Result := Temp;
end;

procedure TSQLVAR.SetAsString(const aValue: string);
begin
 {$IFDEF UNICODE}
  SetAsWideString(aValue)
 {$ELSE}
  SetAsAnsiString(aValue)
 {$ENDIF}
end;

procedure TSQLVAR.SetAsAnsiString(const aValue: AnsiString);
var Temp :AnsiString;
begin
  if not IsMacro and IsGuid and (Length(aValue) <> 16) // �� UUID � �������� ����
    then begin
    SetLength(Temp,16);
    char_to_uuid(pAnsiChar(aValue),Length(aValue),pByte(Temp));
    inherited SetAsAnsiString(Temp);
    end
  else
    inherited SetAsAnsiString(aValue);
end;

procedure TSQLVAR.SetAsWideString(const aValue: WideString);
var Temp :AnsiString;
begin
  if not IsMacro and IsGuid and (Length(aValue) <> 16) // �� UUID � �������� ����
    then begin
    Temp := AnsiString( aValue );
    UniqueString(Temp);
    char_to_uuid(pAnsiChar(Temp),Length(Temp),pByte(Temp));
    SetLength(Temp,16);
    inherited SetAsAnsiString(Temp);
    end
  else
    inherited SetAsWideString(aValue);
end;

procedure TSQLVAR.SetAsVariant(aValue: Variant);
var Temp :AnsiString;
begin
  if not IsMacro and IsGuid then begin
    case  VarType(aValue)  of
      varOleStr ,varString {$IF Declared(varUString)},varUString{$IFEND} :
        begin
          Temp := AnsiString( VarToStr(aValue) );
          if (Length(Temp) <> 16)  then begin  // �� UUID � �������� ����
            UniqueString(Temp);
            char_to_uuid(pAnsiChar(Temp),Length(Temp),pByte(Temp));
            SetLength(Temp,16);
            inherited SetAsAnsiString(Temp);
            Exit;
          end;
        end;
      varVariant :
        begin
          AsVariant := Variant(PVarData(TVarData(Value).VPointer)^);
          Exit;
        end;
    end;
  end;
  inherited SetAsVariant(aValue);
end;

end.
