unit uCornDefs;

interface

uses Classes, Types, UITypes, SysUtils, Messages, Controls, Forms, cxPC;

const
  WM_AIS     = WM_USER + 200;
  WMC_ACTIVE_CONTROL = WM_AIS + 1;


type
  TxTabSheet    = TcxTabSheet;
  TxPageControl = TcxPageControl;

  TViewerState  = (vstInactive, vstActive, vstInsert, vstEdit, vstDeleting);

  TConfirm      = (cfIns = 0, cfEdit = 1, cfDel = 2);
  TConfirms     = set of TConfirm;

  TOper         = (opNothing, opPost, opCancel, opDelete);

  TVwrModalResult  = (vmrCanceled, vmrInserted, vmrUpdated);
  TVwrModalMode    = (vmmNone, vmmInsert, vmmEdit);

  TWinControlHelper = class helper for TWinControl
    function CanFocusEx: boolean; // слямзено с TcxControl.CanFocusEx
  end;

var
  DEF_MSGNEWDOCTEXT   : string = 'Новый документ';
  DEF_MSGSAVENOCANCEL : string = 'Есть изменения. Сохранить?';
  DEF_MSGCONFIRMDELETE: string = 'Удалить?';


implementation

{ TWinControlHelper }

function TWinControlHelper.CanFocusEx: boolean;
var AParentForm: TCustomForm;
begin
  AParentForm:= GetParentForm(Self);
  Result:= CanFocus and ((AParentForm = nil) or
    AParentForm.CanFocus and AParentForm.Enabled and AParentForm.Visible);
end;

end.
