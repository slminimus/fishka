unit uBaseForms;

interface
uses Classes, Types, SysUtils, Messages, Controls, Forms, usTools,
     Windows, uControllers, uDbCtrls;
{$REGION 'Info'}
//Форма, работающая с TController. См. описание в uControllers.pas
{$ENDREGION 'Info'}

type
  TBaseForm = class(TForm)
  private
  protected
    fCtrls: TCtrls;
    procedure ReadState(Reader: TReader); override;
    function  IsTopMost: boolean;
    procedure CreateWnd; override;
    procedure InitializeNewForm; override;
  public
    // IsWindowVisible & not ClientRect.IsEmpty
    function IsVisible: boolean; virtual;
  public
    function Controller<T: TController>: T;
  end;


implementation
{$R *.dfm}

{ TBaseForm }

procedure TBaseForm.CreateWnd;
// Глюк VCL с незапамятных времен: в методе Release выполняется
// PostMessage(Handle, CM_RELEASE, 0, 0);
// не проверяя ни HandleAllocated, ни csDestroying. В результате,
// если в конструкторе был Exception, начинаются чудеса.
begin
  if not (csDestroying in ComponentState) then
    inherited;
end;

procedure TBaseForm.InitializeNewForm;
begin
  inherited;
  Position := poDesigned;
end;

function TBaseForm.IsTopMost: boolean;
begin
  result:= Parent = nil;
end;

function TBaseForm.IsVisible: boolean;
begin
  result:= HandleAllocated and IsWindowVisible(Handle) and not ClientRect.IsEmpty;
end;

procedure TBaseForm.ReadState(Reader: TReader);
var R: TFormReader;
begin
  R:= TFormReader.Create(Self, Reader, fCtrls);
  try
    inherited ReadState(Reader);
  finally
    R.Free;
  end;
end;

function TBaseForm.Controller<T>: T;
begin
  Assert(fCtrls.FindCtrl(T, result));
end;

end.
