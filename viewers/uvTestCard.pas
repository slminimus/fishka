unit uvTestCard;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, uvCards,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,Data.DB,
  MemDS, dxBar, cxClasses, System.Actions, Vcl.ActnList, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxEdit, cxMaskEdit, cxVGrid,
  cxDBVGrid, cxInplaceContainer, uEntClasses, uEntities, Vcl.ExtCtrls;

type
  [Card(CID_TEST)]
  TvwrTestCard = class(TvwrCard)
    dsRecordID: TStringField;
    dsRecordCARDNUM: TStringField;
    dsRecordRELEASED: TDateTimeField;
    dsRecordFIO: TStringField;
    dsRecordPHONES: TStringField;
    dsRecordEMAIL: TStringField;
    dsRecordDISCONT: TIntegerField;
    vgEdit: TcxDBVerticalGrid;
    vgEditID: TcxDBEditorRow;
    vgEditCARDNUM: TcxDBEditorRow;
    vgEditRELEASED: TcxDBEditorRow;
    vgEditFIO: TcxDBEditorRow;
    vgEditPHONES: TcxDBEditorRow;
    vgEditEMAIL: TcxDBEditorRow;
    vgEditDISCONT: TcxDBEditorRow;
  private
  public
  end;

implementation
{$R *.dfm}
uses DmDatas;


initialization
  TvwrTestCard.RegisterViewer;

end.
