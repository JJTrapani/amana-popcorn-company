Unit TypeManagementUnit;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  Menus;

Type

  { TfrmTypeManagement }
  TfrmTypeManagement = Class (TForm)
    gbDetails           : TGroupBox;
    btnCancel           : TButton;
    btnSave             : TButton;
    edtType             : TEdit;
    lblType             : TLabel;
    lstbxItems          : TListBox;
    pmuEdit             : TMenuItem;
    pmuRemove           : TMenuItem;
    pmuAdd              : TMenuItem;
    pmu                 : TPopupMenu;



    Procedure FormCreate             (                Sender: TObject           );
    Procedure lstbxItemsClick        (                Sender: TObject           );
    Procedure pmuPopup               (                Sender: TObject           );
    Procedure pmuAddClick            (                Sender: TObject           );
    Procedure pmuEditClick           (                Sender: TObject           );
    Procedure pmuRemoveClick         (                Sender: TObject           );
    Procedure btnCancelClick         (                Sender: TObject           );
    Procedure btnSaveClick           (                Sender: TObject           );
    Procedure edtKeyPress            (                Sender: TObject;
                                                     Var Key: Char              );

  End;

Var
  frmTypeManagement: TfrmTypeManagement;

Implementation

{$R *.lfm}

Uses
  DataModule,
  Global;




{ ----------------------------------------------------------------------------- }
{ ---------------------- FORM-INDUCED PUBLIC METHODS -------------------------- }
{ ----------------------------------------------------------------------------- }

Procedure TfrmTypeManagement.FormCreate                                  (         Sender: TObject               );
Begin

  { Hide and disable the Cancel/Save buttons }
  btnCancel.Enabled := False;
  btnCancel.Visible := False;
  btnSave.Enabled   := False;
  btnSave.Visible   := False;

  { Set the max lengths of the editboxes }
  edtType.MaxLength   := 30;

  With dmApp Do Begin

    { Load all the items from the popcorn Type table }
    qryAmana.SQL.Text := 'Select ID, Type ' +
                         'From popcorntype;';

    qryAmana.Open;

    While (Not qryAmana.Eof) Do Begin

      Application.ProcessMessages;

      lstbxItems.Items.AddObject (qryAmana.FieldByName ('Type').AsString, TObject (qryAmana.FieldByName ('ID').AsInteger));

      qryAmana.Next;

    End; { While Not Eof }

    qryAmana.Close;

  End; { With dmApp }

End; { FormCreate Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmTypeManagement.lstbxItemsClick                   (         Sender: TObject               );
Begin

  With dmApp Do Begin

    { If the user selected an valid item }
    If (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex]) <> -1) Then Begin

      qryAmana.SQL.Text := 'Select ID, Type ' +
                           'From popcorntype ' +
                           'Where (ID = ' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ');';

      qryAmana.Open;

      If (Not qryAmana.Eof) Then Begin

        edtType.Text   := qryAmana.FieldByName ('Type').AsString;

      End; { If Not Eof }

      qryAmana.Close;

    End; { If the user selected a valid item }

  End; { With dmApp }

End; { lstbxItemsClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmTypeManagement.pmuPopup                          (         Sender: TObject               );
Begin

  { Disable the inapplicable menu items }
  pmuEdit.Enabled   := (lstbxItems.ItemIndex > -1);
  pmuRemove.Enabled := (lstbxItems.ItemIndex > -1);

End; { pmuPopup Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmTypeManagement.pmuAddClick                       (         Sender: TObject               );
Begin

  { Clear off the form }
  btnCancel.Click;

  { Disable the listbox, so the user HAS to make a choice to edit the data }
  lstbxItems.Enabled := False;

  { Enable the Cancel/Save buttons so we can modify the data }
  btnCancel.Enabled := True;
  btnCancel.Visible := True;
  btnSave.Enabled   := True;
  btnSave.Visible   := True;

End; { pmuAddClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmTypeManagement.pmuEditClick                      (         Sender: TObject               );
Begin

  { Disable the listbox, so the user HAS to make a choice to edit the data }
  lstbxItems.Enabled := False;

  { Enable the Cancel/Save buttons so we can modify the data }
  btnCancel.Enabled := True;
  btnCancel.Visible := True;
  btnSave.Enabled   := True;
  btnSave.Visible   := True;

End; { pmuEditClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmTypeManagement.pmuRemoveClick                    (         Sender: TObject               );
Begin

  With dmApp Do Begin

    { If the user selected an valid item }
    If (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex]) <> -1) Then Begin

      If (MessageDlg ('Question', 'Do you wish to remove this item (it cannot be undone)?', mtConfirmation, [mbYes, mbNo],0) = mrYes) Then Begin

        qryAmana.SQL.Text := 'Delete ' +
                             'From popcorntype ' +
                             'Where (ID = ' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ');';

        qryAmana.ExecSQL;

        { Remove the item from the list }
        lstbxItems.Items.Delete (lstbxItems.ItemIndex);

      End; { If the user wished to proceed }

    End; { If the user selected a valid item }

  End; { With dmApp }

End; { pmuRemoveClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmTypeManagement.btnCancelClick                    (         Sender: TObject               );
Begin

  { Deselect all options and data and reset the form }
  lstbxItems.ItemIndex := -1;
  edtType.Text         := '';

  { Hide and disable the Cancel/Save buttons }
  btnCancel.Enabled := False;
  btnCancel.Visible := False;
  btnSave.Enabled   := False;
  btnSave.Visible   := False;

  { Enable the listbox }
  lstbxItems.Enabled := True;

End; { btnCancelClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmTypeManagement.btnSaveClick                      (         Sender: TObject               );
Var
  NewItemID : Integer;

Begin

  With dmApp Do Begin

    { If the list does not have an item highlighted, insert a new record }
    If (lstbxItems.ItemIndex = -1) Then Begin

      qryAmana.SQL.Text := 'Insert Into popcorntype (Type) Values (''' + CleanQueryVarStr (edtType.Text) + ''');';

      { Add a record to the audit trail }
      AddAuditTrailRecord (qryAmana.SQL.Text);

      { Insert, then open the SQL query }
      qryAmana.ExecSQL;
      qryAmana.SQL.Text := 'Select Last_Insert_ID() As ID;';
      qryAmana.Open;
      NewItemID := qryAmana.FieldByName ('ID').AsInteger;
      qryAmana.Close;

      { Add a record to the audit trail }
      AddAuditTrailRecord ('Insert Into popcorntypeaudit (Type, ID_Ptr, ChangedOn) Values (''' + CleanQueryVarStr (edtType.Text) + ''',' + IntToStr (NewItemID) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

      { Add the item to the list }
      lstbxItems.Items.AddObject (CleanQueryVarStr (edtType.Text), TObject (NewItemID));

    End

    { Otherwise, update the selected record }
    Else Begin

      qryAmana.SQL.Text := 'Update popcorntype Set ' +
                                   'Type = '''   + CleanQueryVarStr (edtType.Text)   + ''' ' +
                           'Where (ID = ' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ');';

      { Add a record to the audit trail }
      AddAuditTrailRecord ('Insert Into popcorntypeaudit (Type, ID_Ptr, ChangedOn) Values (''' + CleanQueryVarStr (edtType.Text) + ''',' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

      { Execute the SQL query }
      qryAmana.ExecSQL;

      { Update the listbox's item's label }
      lstbxItems.Items.Strings [lstbxItems.ItemIndex] := CleanQueryVarStr (edtType.Text);

    End;

  End; { With dmApp }


  { Hide and disable the Cancel/Save buttons }
  btnCancel.Enabled := False;
  btnCancel.Visible := False;
  btnSave.Enabled   := False;
  btnSave.Visible   := False;

  { Enable the listbox }
  lstbxItems.Enabled := True;

End; { btnSaveClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmTypeManagement.edtKeyPress                       (         Sender: TObject;
                                                                        Var Key: Char                  );
Var
  ShowErrorMsgs : Boolean;

Begin

  { If the save button is enabled, then limit the input of the keystrokes }
  ShowErrorMsgs := (btnSave.Enabled);

  { If not a backspace, 0-9, or decimal point }
  If (Not (Key In [#8, '0'..'9', '-', DecimalSeparator])) Then Begin

    If (ShowErrorMsgs)
      Then ShowMessage ('Invalid keypress, only use numerical values.');

    { Discard the key }
    Key := #0;

  End

  Else If ((Key = DecimalSeparator) Or
           (Key = '-')) And
          (Pos (Key, (Sender As TEdit).Text) > 0) Then Begin

    If (ShowErrorMsgs)
      Then ShowMessage ('Invalid Key: twice ' + Key);
    Key := #0;

  End

  Else If (Key = '-') And
          ((Sender As TEdit).SelStart <> 0) Then Begin

    If (ShowErrorMsgs)
      Then ShowMessage ('Only allowed at beginning of number: ' + Key);
    Key := #0;

  End;


End; { edtKeyPress Procedure }
{ ---------------------------------------------------------------------------- }



End.

