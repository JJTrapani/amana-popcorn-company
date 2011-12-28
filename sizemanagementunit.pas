Unit SizeManagementUnit;

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

  { TfrmSizeManagement }
  TfrmSizeManagement = Class (TForm)
    btnCancel           : TButton;
    btnSave             : TButton;
    edtCups             : TEdit;
    edtSize             : TEdit;
    edtHeight           : TEdit;
    edtWidth            : TEdit;
    edtDepth            : TEdit;
    gbDetails           : TGroupBox;
    lblDepth            : TLabel;
    lblCups             : TLabel;
    lblWidth            : TLabel;
    lblSize             : TLabel;
    lblHeight           : TLabel;
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
  frmSizeManagement: TfrmSizeManagement;

Implementation

{$R *.lfm}

Uses
  DataModule,
  Global;




{ ----------------------------------------------------------------------------- }
{ ---------------------- FORM-INDUCED PUBLIC METHODS -------------------------- }
{ ----------------------------------------------------------------------------- }

Procedure TfrmSizeManagement.FormCreate                                  (         Sender: TObject               );
Begin

  { Hide and disable the Cancel/Save buttons }
  btnCancel.Enabled := False;
  btnCancel.Visible := False;
  btnSave.Enabled   := False;
  btnSave.Visible   := False;

  { Set the max lengths of the editboxes }
  edtSize.MaxLength   := 15;
  edtHeight.MaxLength := 11;
  edtWidth.MaxLength  := 11;
  edtDepth.MaxLength  := 11;
  edtCups.MaxLength   := 11;

  With dmApp Do Begin

    { Load all the items from the popcorn size table }
    qryAmana.SQL.Text := 'Select ID, Size ' +
                         'From popcornsizes;';

    qryAmana.Open;

    While (Not qryAmana.Eof) Do Begin

      Application.ProcessMessages;

      lstbxItems.Items.AddObject (qryAmana.FieldByName ('Size').AsString, TObject (qryAmana.FieldByName ('ID').AsInteger));

      qryAmana.Next;

    End; { While Not Eof }

    qryAmana.Close;

  End; { With dmApp }

End; { FormCreate Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSizeManagement.lstbxItemsClick                   (         Sender: TObject               );
Begin

  With dmApp Do Begin

    { If the user selected an valid item }
    If (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex]) <> -1) Then Begin

      qryAmana.SQL.Text := 'Select ID, Size, Height, Width, Depth, Cups ' +
                           'From popcornsizes ' +
                           'Where (ID = ' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ');';

      qryAmana.Open;

      If (Not qryAmana.Eof) Then Begin

        edtSize.Text   := qryAmana.FieldByName ('Size').AsString;
        edtHeight.Text := qryAmana.FieldByName ('Height').AsString;
        edtWidth.Text  := qryAmana.FieldByName ('Width').AsString;
        edtDepth.Text  := qryAmana.FieldByName ('Depth').AsString;
        edtCups.Text   := qryAmana.FieldByName ('Cups').AsString;


      End; { If Not Eof }

      qryAmana.Close;

    End; { If the user selected a valid item }

  End; { With dmApp }

End; { lstbxItemsClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSizeManagement.pmuPopup                          (         Sender: TObject               );
Begin

  { Disable the inapplicable menu items }
  pmuEdit.Enabled   := (lstbxItems.ItemIndex > -1);
  pmuRemove.Enabled := (lstbxItems.ItemIndex > -1);

End; { pmuPopup Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSizeManagement.pmuAddClick                       (         Sender: TObject               );
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

Procedure TfrmSizeManagement.pmuEditClick                      (         Sender: TObject               );
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

Procedure TfrmSizeManagement.pmuRemoveClick                    (         Sender: TObject               );
Begin

  With dmApp Do Begin

    { If the user selected an valid item }
    If (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex]) <> -1) Then Begin

      If (MessageDlg ('Question', 'Do you wish to remove this item (it cannot be undone)?', mtConfirmation, [mbYes, mbNo],0) = mrYes) Then Begin

        qryAmana.SQL.Text := 'Delete ' +
                             'From popcornsizes ' +
                             'Where (ID = ' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ');';

        qryAmana.ExecSQL;

        { Remove the item from the list }
        lstbxItems.Items.Delete (lstbxItems.ItemIndex);

      End; { If the user wished to proceed }

    End; { If the user selected a valid item }

  End; { With dmApp }

End; { pmuRemoveClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSizeManagement.btnCancelClick                    (         Sender: TObject               );
Begin

  { Deselect all options and data and reset the form }
  lstbxItems.ItemIndex := -1;
  edtSize.Text         := '';
  edtHeight.Text       := '';
  edtWidth.Text        := '';
  edtDepth.Text        := '';
  edtCups.Text         := '';

  { Hide and disable the Cancel/Save buttons }
  btnCancel.Enabled := False;
  btnCancel.Visible := False;
  btnSave.Enabled   := False;
  btnSave.Visible   := False;

  { Enable the listbox }
  lstbxItems.Enabled := True;

End; { btnCancelClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSizeManagement.btnSaveClick                      (         Sender: TObject               );
Var
  NewItemID : Integer;

Begin

  With dmApp Do Begin

    { If the list does not have an item highlighted, insert a new record }
    If (lstbxItems.ItemIndex = -1) Then Begin

      qryAmana.SQL.Text := 'Insert Into popcornsizes (Size, Height, Width, Depth, Cups) Values (''' +
                                       CleanQueryVarStr (edtSize.Text)   + ''',''' +
                                       CleanQueryVarStr (edtHeight.Text) + ''',''' +
                                       CleanQueryVarStr (edtWidth.Text)  + ''',''' +
                                       CleanQueryVarStr (edtDepth.Text)  + ''',''' +
                                       CleanQueryVarStr (edtCups.Text)   + ''');';

      { Add a record to the audit trail }
      AddAuditTrailRecord (qryAmana.SQL.Text);

      { Insert, then open the SQL query }
      qryAmana.ExecSQL;
      qryAmana.SQL.Text := 'Select Last_Insert_ID() As ID;';
      qryAmana.Open;
      NewItemID := qryAmana.FieldByName ('ID').AsInteger;
      qryAmana.Close;

      { Add a record to the audit trail }
      AddAuditTrailRecord ('Insert Into popcornsizesaudit (Size, Height, Width, Depth, Cups, ID_Ptr, ChangedOn) Values (''' + CleanQueryVarStr (edtSize.Text) + ''',''' + CleanQueryVarStr (edtHeight.Text) + ''',''' + CleanQueryVarStr (edtWidth.Text) + ''',''' + CleanQueryVarStr (edtDepth.Text) + ''',''' + CleanQueryVarStr (edtCups.Text) + ''',' + IntToStr (NewItemID) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

      { Add the item to the list }
      lstbxItems.Items.AddObject (CleanQueryVarStr (edtSize.Text), TObject (NewItemID));

    End

    { Otherwise, update the selected record }
    Else Begin

      qryAmana.SQL.Text := 'Update popcornsizes Set ' +
                                   'Size = '''   + CleanQueryVarStr (edtSize.Text)   + ''',' +
                                   'Height = ''' + CleanQueryVarStr (edtHeight.Text) + ''',' +
                                   'Width = '''  + CleanQueryVarStr (edtWidth.Text)  + ''',' +
                                   'Depth = '''  + CleanQueryVarStr (edtDepth.Text)  + ''',' +
                                   'Cups = '''   + CleanQueryVarStr (edtCups.Text)   + ''' ' +
                            'Where (ID = ' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ');';

      { Add a record to the audit trail }
      AddAuditTrailRecord ('Insert Into popcornsizesaudit (Size, Height, Width, Depth, Cups, ID_Ptr, ChangedOn) Values (''' + CleanQueryVarStr (edtSize.Text) + ''',''' + CleanQueryVarStr (edtHeight.Text) + ''',''' + CleanQueryVarStr (edtWidth.Text) + ''',''' + CleanQueryVarStr (edtDepth.Text) + ''',''' + CleanQueryVarStr (edtCups.Text) + ''',' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

      { Execute the SQL query }
      qryAmana.ExecSQL;

      { Update the listbox's item's label }
      lstbxItems.Items.Strings [lstbxItems.ItemIndex] := CleanQueryVarStr (edtSize.Text);

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

Procedure TfrmSizeManagement.edtKeyPress                       (         Sender: TObject;
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

