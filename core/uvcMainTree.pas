unit uvcMainTree;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uvCards, Data.DB, dxmdaset, dxBar,
  cxClasses, System.Actions, Vcl.ActnList, Vcl.ExtCtrls, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxEdit, cxMaskEdit, cxVGrid,
  cxDBVGrid, cxInplaceContainer, uEntSystem, uEntities;

type
  [Card(CID_MTREE)]
  TvwcMainTree = class(TvwrCard)
    vgEdit: TcxDBVerticalGrid;
    vgEditID: TcxDBEditorRow;
    vgEditNAME: TcxDBEditorRow;
    vgEditENTITY: TcxDBEditorRow;
    vgEditTAG: TcxDBEditorRow;
    dsRecordID: TStringField;
    dsRecordNAME: TStringField;
    dsRecordENTITY: TStringField;
    dsRecordPARENT: TStringField;
    dsRecordTAG: TIntegerField;
  private
  public
  end;

implementation
{$R *.dfm}
uses DmDatas;

initialization
  TvwcMainTree.RegisterViewer;

end.
