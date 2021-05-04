program FishApp;

uses
  Vcl.Forms,
  ufMain in 'ufMain.pas' {MainForm},
  dmDatas in 'dmDatas.pas' {DmDb: TDataModule},
  _Splash in 'Forms\_Splash.pas' {_SplashForm},
  FIBTypes in 'Provider\FBDirect\FIBTypes.pas',
  uDBProvider in 'Provider\uDBProvider.pas',
  uifProvider in 'Provider\uifProvider.pas',
  uFBProvider in 'Provider\FBDirect\uFBProvider.pas',
  uBaseForms in 'core\uBaseForms.pas' {BaseForm},
  uEntities in 'core\uEntities.pas',
  uCornDefs in 'core\uCornDefs.pas',
  uDataServiceFB in 'core\uDataServiceFB.pas',
  uEntClasses in 'core\uEntClasses.pas',
  ufViewer in 'core\ufViewer.pas' {Viewer},
  uvCards in 'core\uvCards.pas' {vwrCard},
  uvList in 'core\uvList.pas' {vwrList},
  uvListDb in 'core\uvListDb.pas' {vwrListDB},
  uvListTblView in 'core\uvListTblView.pas' {vvrListTblView},
  uControllers in 'core\uControllers.pas' {Controller: TDataModule},
  uvListTest in 'viewers\uvListTest.pas' {vwrListTest},
  uvTestCard in 'viewers\uvTestCard.pas' {vwrTestCard};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDmDb, DmDb);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
