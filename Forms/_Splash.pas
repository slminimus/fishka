unit _Splash;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, JclFileUtils;

const
  SplashBmpID = 'SPLASHBMP';

type
  T_SplashForm = class(TForm)
    Image1: TImage;
    Status: TStaticText;
    Label1: TLabel;
    lblTrial: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ShowStatusText(const Text: string);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
  end;

  TSplashFormClass = class of T_SplashForm;

procedure ShowSplash(const Text: string = '');
procedure HideSplash;
procedure SetSplashText(const Text: string);
function  SplashBottom: integer;

var
  SplashFormClass: TSplashFormClass = T_SplashForm;
  VerInfo: TJclFileVersionInfo;

implementation
uses ResDialogs, StrUtils, DmDatas;
{$R *.DFM}
{ $R SPLASH.RES}

var
  SplashForm: T_SplashForm;

function FormatAppName: string;
begin
  result:= VerInfo.InternalName + #13#10 + VerInfo.FileVersion;
end;

procedure T_SplashForm.FormCreate(Sender: TObject);
var bmp: TBitMap;
    h  : cardinal;
begin
  h:= FindResourceHInstance(hInstance);
  bmp:= TBitMap.Create;
  try
    try
      bmp.LoadFromResourceName(h, PChar(SplashBmpID));
      Image1.Picture.assign(bmp);
    except
//      Beep;
    end;
  finally
    bmp.Free;
  end;
  Label1.Caption:= FormatAppName;
{$IFDEF AIS_TRIAL}
  lblTrial.Visible:= true;
{$ENDIF}
end;

procedure T_SplashForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do begin
    Style:= Style and not WS_CAPTION;
    if (Application.MainForm = nil) or not Application.MainForm.Visible then
      ExStyle:= ExStyle or WS_EX_APPWINDOW; // place icon to windows taskbar
  end;
end;

procedure T_SplashForm.FormShow(Sender: TObject);
var R: TRect;
begin
  if Screen.ActiveCustomForm <> nil then
    R:= Screen.ActiveCustomForm.Monitor.WorkareaRect
  else
    SystemParametersInfo(SPI_GETWORKAREA,0,@R,0);
  Left:= (R.Right + R.Left - Width) div 2;
  Top:= (R.Bottom + R.Top) div 2  - Height;
end;

procedure T_SplashForm.ShowStatusText(const Text: string);
begin
 if Text = '' then begin
    Status.Hide;
    ClientHeight:= Image1.Height;
    AutoSize:= true;
    exit;
  end;
  AutoSize:= false;
  Status.Caption:= Text;
  if not Status.Visible then begin
    Status.Font.Name:= 'Courier New';
    Status.Font.Size:= 9;
    Status.Show;
    ClientHeight:= ClientHeight + Status.Height;
  end;
  Update;
  Application.ProcessMessages;
end;
//=================================

procedure ShowSplash(const Text: string = '');
begin
  if not assigned(SplashForm) then
    SplashForm:= SplashFormClass.Create(Application);
  SplashForm.Show;
  if Text = '' then
    Application.ProcessMessages
  else
    SplashForm.ShowStatusText(Text);
end;

procedure HideSplash;
begin
  FreeAndNil(SplashForm);
end;

procedure SetSplashText(const Text: string);
begin
  if assigned(SplashForm) then
    SplashForm.ShowStatusText(Text);
end;

function SplashBottom: integer;
begin
  result:= -1;
  if assigned(SplashForm) then
    result:= SplashForm.BoundsRect.Bottom;
end;

initialization
  VerInfo:= TJclFileVersionInfo.Create(ParamStr(0));

finalization
  VerInfo.Free;

end.
