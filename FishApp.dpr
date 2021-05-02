program FishApp;

uses
  Vcl.Forms,
  ufMain in 'ufMain.pas' {MainForm},
  dmDatas in 'dmDatas.pas' {DmDb: TDataModule},
  uControllers in 'core\uControllers.pas' {Controller: TDataModule},
  _Splash in 'Forms\_Splash.pas' {_SplashForm},
  FIBTypes in 'Provider\FBDirect\FIBTypes.pas',
  uDBProvider in 'Provider\uDBProvider.pas',
  uifProvider in 'Provider\uifProvider.pas',
  uFBProvider in 'Provider\FBDirect\uFBProvider.pas',
  uDbCtrls in 'core\uDbCtrls.pas' {DbController: TDataModule},
  uBaseForms in 'core\uBaseForms.pas' {BaseForm},
  uDSCtrls in 'core\uDSCtrls.pas' {DSController: TDataModule},
  CornDefs in 'core\CornDefs.pas',
  uEntities in 'core\uEntities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDmDb, DmDb);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
