Unit PriceManagementUnit;

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
  Menus,
  ExtCtrls;

Type

  { TfrmPriceManagement }
  TfrmPriceManagement = Class (TForm)
    btnTypeManagement   : TButton;
    btnSizeManagement   : TButton;
    cbxType             : TComboBox;
    cbxSize             : TComboBox;
    gbDetails           : TGroupBox;
    btnCancel           : TButton;
    btnSave             : TButton;
    edtPrice            : TEdit;
    lblType             : TLabel;
    lblPrice            : TLabel;
    lstbxItems          : TListBox;
    pnlMid              : TPanel;
    pnlMidBot           : TPanel;
    pnlBot              : TPanel;
    pmuEdit             : TMenuItem;
    pmuRemove           : TMenuItem;
    pmuAdd              : TMenuItem;
    pmu                 : TPopupMenu;
    sptr                : TSplitter;




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
    Procedure btnTypeManagementClick (                Sender: TObject           );
    Procedure btnSizeManagementClick (                Sender: TObject           );


    Private
      Procedure RefreshTypeList;
      Procedure RefreshSizeList;

  End;

Var
  frmPriceManagement: TfrmPriceManagement;

Implementation

{$R *.lfm}

Uses
  DataModule,
  Global,
  TypeManagementUnit,
  SizeManagementUnit;




{ ----------------------------------------------------------------------------- }
{ ---------------------- FORM-INDUCED PUBLIC METHODS -------------------------- }
{ ----------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.FormCreate                       (         Sender: TObject               );
Begin

  { Hide and disable the Cancel/Save buttons }
  btnCancel.Enabled         := False;
  btnCancel.Visible         := False;
  btnSave.Enabled           := False;
  btnSave.Visible           := False;
  btnTypeManagement.Enabled := False;
  btnTypeManagement.Visible := False;
  btnSizeManagement.Enabled := False;
  btnSizeManagement.Visible := False;

  { Set the max lengths of the editboxes }
  edtPrice.MaxLength       := 6;

  With dmApp Do Begin

    { Load all the items from the popcorn type prices table }
    qryAmana.SQL.Text := 'Select ID, Price ' +
                         'From popcorntypeprices ' +
                         'Order By Price;';

    qryAmana.Open;

    While (Not qryAmana.Eof) Do Begin

      Application.ProcessMessages;

      lstbxItems.Items.AddObject (FormatFloat ('0.00', qryAmana.FieldByName ('Price').AsFloat), TObject (qryAmana.FieldByName ('ID').AsInteger));

      qryAmana.Next;

    End; { While Not Eof }

    qryAmana.Close;

    { Load the type and size lists }
    RefreshTypeList;
    RefreshSizeList;
	
  End; { With dmApp }

End; { FormCreate Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.lstbxItemsClick                  (         Sender: TObject               );
Var
  cbxIndex : Integer;

Begin

  With dmApp Do Begin

    { If the user selected an valid item }
    If (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex]) <> -1) Then Begin

      qryAmana.SQL.Text := 'Select ID, Price, Type_Ptr, Size_Ptr ' +
                           'From popcorntypeprices ' +
                           'Where (ID = ' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ');';


      qryAmana.Open;

      If (Not qryAmana.Eof) Then Begin

        edtPrice.Text      := FormatFloat ('0.00', qryAmana.FieldByName ('Price').AsFloat);

        { If the Type Pointer points to a valid type still, then find it and select that item from the list }
        cbxIndex := cbxType.Items.IndexOfObject (TObject (qryAmana.FieldByName ('Type_Ptr').AsInteger));

        If (cbxIndex > -1)
          Then cbxType.ItemIndex := cbxIndex;

        { If the Size Pointer points to a valid size still, then find it and select that item from the list }
        cbxIndex := cbxSize.Items.IndexOfObject (TObject (qryAmana.FieldByName ('Size_Ptr').AsInteger));

        If (cbxIndex > -1)
          Then cbxSize.ItemIndex := cbxIndex;
    
      End; { If Not Eof }

      qryAmana.Close;

    End; { If the user selected a valid item }

  End; { With dmApp }

End; { lstbxItemsClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.pmuPopup                         (         Sender: TObject               );
Begin

  { Disable the inapplicable menu items }
  pmuEdit.Enabled   := (lstbxItems.ItemIndex > -1);
  pmuRemove.Enabled := (lstbxItems.ItemIndex > -1);

End; { pmuPopup Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.pmuAddClick                      (         Sender: TObject               );
Begin

  { Clear off the form }
  btnCancel.Click;

  { Disable the listbox, so the user HAS to make a choice to edit the data }
  lstbxItems.Enabled := False;

  { Enable the Cancel/Save buttons so we can modify the data }
  btnCancel.Enabled         := True;
  btnCancel.Visible         := True;
  btnSave.Enabled           := True;
  btnSave.Visible           := True;
  btnTypeManagement.Enabled := True;
  btnTypeManagement.Visible := True;
  btnSizeManagement.Enabled := True;
  btnSizeManagement.Visible := True;
  
End; { pmuAddClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.pmuEditClick                     (         Sender: TObject               );
Begin

  { Disable the listbox, so the user HAS to make a choice to edit the data }
  lstbxItems.Enabled := False;

  { Enable the Cancel/Save buttons so we can modify the data }
  btnCancel.Enabled         := True;
  btnCancel.Visible         := True;
  btnSave.Enabled           := True;
  btnSave.Visible           := True;
  btnTypeManagement.Enabled := True;
  btnTypeManagement.Visible := True;
  btnSizeManagement.Enabled := True;
  btnSizeManagement.Visible := True;
  
End; { pmuEditClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.pmuRemoveClick                   (         Sender: TObject               );
Begin

  With dmApp Do Begin

    { If the user selected an valid item }
    If (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex]) <> -1) Then Begin

      If (MessageDlg ('Question', 'Do you wish to remove this item (it cannot be undone)?', mtConfirmation, [mbYes, mbNo],0) = mrYes) Then Begin

        qryAmana.SQL.Text := 'Delete ' +
                             'From popcorntypeprices ' +
                             'Where (ID = ' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ');';

        qryAmana.ExecSQL;

        { Remove the item from the list }
        lstbxItems.Items.Delete (lstbxItems.ItemIndex);

      End; { If the user wished to proceed }

    End; { If the user selected a valid item }

  End; { With dmApp }

End; { pmuRemoveClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.btnCancelClick                   (         Sender: TObject               );
Begin

  { Deselect all options and data and reset the form }
  lstbxItems.ItemIndex := -1;
  edtPrice.Text       := '';

  { Hide and disable the Cancel/Save buttons }
  btnCancel.Enabled         := False;
  btnCancel.Visible         := False;
  btnSave.Enabled           := False;
  btnSave.Visible           := False;
  btnTypeManagement.Enabled := False;
  btnTypeManagement.Visible := False;
  btnSizeManagement.Enabled := False;
  btnSizeManagement.Visible := False;
  
  { Enable the listbox }
  lstbxItems.Enabled := True;

End; { btnCancelClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.btnSaveClick                     (         Sender: TObject               );
Var
  NewItemID : Integer;

Begin

  With dmApp Do Begin

    { If the list does not have an item highlighted, insert a new record }
    If (lstbxItems.ItemIndex = -1) Then Begin

      qryAmana.SQL.Text := 'Insert Into popcorntypeprices (Price, Type_Ptr, Size_Ptr) Values (''' + CleanQueryVarStr (edtPrice.Text) + ''',' + IntToStr (Integer (cbxType.Items.Objects [cbxType.ItemIndex])) + ',' + IntToStr (Integer (cbxSize.Items.Objects [cbxSize.ItemIndex])) + ');';

      { Insert, then open the SQL query }
      qryAmana.ExecSQL;
      qryAmana.SQL.Text := 'Select Last_Insert_ID() As ID;';
      qryAmana.Open;
      NewItemID := qryAmana.FieldByName ('ID').AsInteger;
      qryAmana.Close;

      { Add a record to the audit trail }
      AddAuditTrailRecord ('Insert Into popcorntypepricesaudit (Price, Type_Ptr, Size_Ptr, ID_Ptr, ChangedOn) Values (''' + CleanQueryVarStr (edtPrice.Text) + ''',' + IntToStr (Integer (cbxType.Items.Objects [cbxType.ItemIndex])) + ',' + IntToStr (Integer (cbxSize.Items.Objects [cbxSize.ItemIndex])) + ',' + IntToStr (NewItemID) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

      { Add the item to the list }
      lstbxItems.Items.AddObject (CleanQueryVarStr (edtPrice.Text), TObject (NewItemID));

    End

    { Otherwise, update the selected record }
    Else Begin

      qryAmana.SQL.Text := 'Update popcornflavors Set ' +
                                   'Price = '''       + CleanQueryVarStr (edtPrice.Text)                              + ''',' +
                                   'Type_Ptr = '      + IntToStr (Integer (cbxType.Items.Objects [cbxType.ItemIndex])) + ',' +
							       'Size_Ptr = '      + IntToStr (Integer (cbxSize.Items.Objects [cbxSize.ItemIndex])) + ' ' +

                           'Where (ID = ' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ');';

      { Add a record to the audit trail }
      AddAuditTrailRecord ('Insert Into popcornflavorsaudit (Price, Type_Ptr, Size_Ptr, ID_Ptr, ChangedOn) Values (''' + CleanQueryVarStr (edtPrice.Text) + ''',' + IntToStr (Integer (cbxType.Items.Objects [cbxType.ItemIndex])) + ',' + IntToStr (Integer (cbxSize.Items.Objects [cbxSize.ItemIndex])) + ',' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

      { Execute the SQL query }
      qryAmana.ExecSQL;

      { Update the listbox's item's label }
      lstbxItems.Items.Strings [lstbxItems.ItemIndex] := CleanQueryVarStr (edtPrice.Text);

    End;

  End; { With dmApp }


  { Hide and disable the Cancel/Save buttons }
  btnCancel.Enabled         := False;
  btnCancel.Visible         := False;
  btnSave.Enabled           := False;
  btnSave.Visible           := False;
  btnTypeManagement.Enabled := False;
  btnTypeManagement.Visible := False;
  btnSizeManagement.Enabled := False;
  btnSizeManagement.Visible := False;
  
  { Enable the listbox }
  lstbxItems.Enabled := True;

End; { btnSaveClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.edtKeyPress                      (         Sender: TObject;
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

Procedure TfrmPriceManagement.btnTypeManagementClick           (         Sender: TObject               );
Var
  ItemIDSelected : Integer;

Begin

  { Allow the user to add more types }
  Application.CreateForm (TfrmTypeManagement, frmTypeManagement);
  frmTypeManagement.ShowModal;
  frmTypeManagement.Free;


  { Remember which ID was selected }
  ItemIDSelected := Integer (cbxType.Items.Objects [cbxType.ItemIndex]);

  { Refresh the type list }
  RefreshTypeList;

  { If the type still exists, re-choose it for the user }
  If (cbxType.Items.IndexOfObject (TObject (ItemIDSelected)) > -1)
    Then cbxType.ItemIndex := cbxType.Items.IndexOfObject (TObject (ItemIDSelected));

End; { btnTypeManagementClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.btnSizeManagementClick           (         Sender: TObject               );
Var
  ItemIDSelected : Integer;

Begin

  { Allow the user to add more types }
  Application.CreateForm (TfrmSizeManagement, frmSizeManagement);
  frmSizeManagement.ShowModal;
  frmSizeManagement.Free;


  { Remember which ID was selected }
  ItemIDSelected := Integer (cbxSize.Items.Objects [cbxSize.ItemIndex]);

  { Refresh the type list }
  RefreshSizeList;

  { If the type still exists, re-choose it for the user }
  If (cbxSize.Items.IndexOfObject (TObject (ItemIDSelected)) > -1)
    Then cbxSize.ItemIndex := cbxSize.Items.IndexOfObject (TObject (ItemIDSelected));

End; { btnSizeManagementClick Procedure }
{ ---------------------------------------------------------------------------- }












{ ---------------------------------------------------------------------------- }
{ ---------------------------- PRIVATE METHODS ------------------------------- }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.RefreshTypeList;
Begin

  cbxType.Clear;

  { Insert the first option }
  cbxType.Items.AddObject ('Select a Type...', TObject (0));

  { Auto select the first option }
  cbxType.ItemIndex := 0;


  { Reload all of the Popcorn Types from the type table }
  With dmApp Do Begin

    qryAmana.SQL.Text := 'Select ID, Type From popcorntype Order By Type;';

    qryAmana.Open;

    While (Not qryAmana.Eof) Do Begin

      Application.ProcessMessages;

      { Add a new object }
      cbxType.Items.AddObject (qryAmana.FieldByName ('Type').AsString, TObject (qryAmana.FieldByName ('ID').AsInteger));

      qryAmana.Next;

    End; { While }

    qryAmana.Close;

  End; { With dmApp }

End; { RefreshTypeList Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.RefreshSizeList;
Begin

  cbxSize.Clear;

  { Insert the first option }
  cbxSize.Items.AddObject ('Select a Size...', TObject (0));

  { Auto select the first option }
  cbxSize.ItemIndex := 0;


  { Reload all of the popcorn sizes from the size table }
  With dmApp Do Begin

    qryAmana.SQL.Text := 'Select ID, Size From popcornsizes Order By Size;';

    qryAmana.Open;

    While (Not qryAmana.Eof) Do Begin

      Application.ProcessMessages;

      { Add a new object }
      cbxSize.Items.AddObject (qryAmana.FieldByName ('Size').AsString, TObject (qryAmana.FieldByName ('ID').AsInteger));

      qryAmana.Next;

    End; { While }

    qryAmana.Close;

  End; { With dmApp }

End; { RefreshSizeList Procedure }
{ ---------------------------------------------------------------------------- }

End.

