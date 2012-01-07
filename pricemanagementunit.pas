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
  ExtCtrls,
  Grids;

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
    lblSize             : TLabel;
    lblType             : TLabel;
    lblPrice            : TLabel;
    pnlMid              : TPanel;
    pnlMidBot           : TPanel;
    pnlBot              : TPanel;
    pmuEdit             : TMenuItem;
    pmuRemove           : TMenuItem;
    pmuAdd              : TMenuItem;
    pmu                 : TPopupMenu;
    sptr                : TSplitter;
    grdPrice            : TStringGrid;




    Procedure FormCreate             (                Sender: TObject           );
    procedure grdPriceResize         (                Sender: TObject           );
    Procedure grdPriceClick          (                Sender: TObject           );
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
      IsNewRecord : Boolean;

      Procedure RefreshTypeList;
      Procedure RefreshSizeList;
      Procedure AutoSizeCol          (                  Grid: TStringGrid;
                                                      Column: Integer           );

      Procedure RefreshGridData;

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

  { Load the type and size lists }
  RefreshTypeList;
  RefreshSizeList;
	
  { Load the grid data }
  RefreshGridData;

End; { FormCreate Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.grdPriceResize                   (         Sender: TObject               );
Var
  ColIndex : Integer;

Begin

  For ColIndex := 0 To ((Sender As TStringGrid).ColCount - 1) Do Begin
    AutoSizeCol ((Sender As TStringGrid), ColIndex);
  End; { For }

End; { grdPriceResize Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.grdPriceClick                    (         Sender: TObject               );
Var
  RowIndex : Integer;
  cbxIndex : Integer;

Begin

  With dmApp Do Begin

    { Get the FIRST selected row }
    RowIndex := grdPrice.Selection.Top;

    { If the user selected an valid item }
    If (RowIndex > 0) Then Begin

      edtPrice.Text := FormatFloat ('0.00', StrToFloat (StringReplace (grdPrice.Cells [0, RowIndex], '$', '', [rfReplaceAll])));


      { If the Size string is still a valid size in the list, then find it and select that item from the list }
      cbxIndex := cbxSize.Items.IndexOf (grdPrice.Cells [1, RowIndex]);
      If (cbxIndex > -1)
        Then cbxSize.ItemIndex := cbxIndex;


      { If the Type string is still a valid type in the list, then find it and select that item from the list }
      cbxIndex := cbxType.Items.IndexOf (grdPrice.Cells [2, RowIndex]);
      If (cbxIndex > -1)
          Then cbxType.ItemIndex := cbxIndex;
    
    End; { If the user selected a valid item }

  End; { With dmApp }

End; { grdPriceClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.pmuPopup                         (         Sender: TObject               );
Begin

  { Disable the inapplicable menu items }
  pmuEdit.Enabled   := True;
  pmuRemove.Enabled := True;

End; { pmuPopup Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.pmuAddClick                      (         Sender: TObject               );
Begin

  { Clear off the form }
  btnCancel.Click;

  { We would perform an insert on the database if the user decides to save it }
  IsNewRecord := True;

  { Disable the grid, so the user HAS to make a choice to edit the data }
  grdPrice.Enabled := False;

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

  { We would perform an update on the database if the user decides to save it }
  IsNewRecord := False;

  { Disable the grid, so the user HAS to make a choice to edit the data }
  grdPrice.Enabled := False;

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
Var
  RowIndex : Integer;
  Size_Ptr : Integer;
  Type_Ptr : Integer;

Begin

  With dmApp Do Begin

    { Grab the selected top row's index number }
    RowIndex := grdPrice.Selection.Top;

    { If the user wanted to delete a valid item }
    If (MessageDlg ('Question', 'Do you wish to remove this item (it cannot be undone)?', mtConfirmation, [mbYes, mbNo],0) = mrYes) Then Begin

      Size_Ptr := 0;
      Size_Ptr := cbxSize.Items.IndexOf (grdPrice.Cells [1, RowIndex]);

      Type_Ptr := 0;
      Type_Ptr := cbxType.Items.IndexOf (grdPrice.Cells [2, RowIndex]);

      { Only delete an item IF it exists }
      If (Size_Ptr <= 0) Or
         (Type_Ptr <= 0)
        Then qryAmana.SQL.Text := ''
        Else qryAmana.SQL.Text := 'Delete ' +
                                  'From popcorntypeprices ' +
                                  'Where (Price = ' + grdPrice.Cells [0, RowIndex] + ') And ' +
                                        '(Size_Ptr = ' + IntToStr (Size_Ptr) + ') And ' +
                                        '(Type_Ptr = ' + IntToStr (Type_Ptr) + ')';

      qryAmana.ExecSQL;

      { Remove the item from the grid }
      RefreshGridData;

    End; { If the user wished to proceed }

  End; { With dmApp }

End; { pmuRemoveClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.btnCancelClick                   (         Sender: TObject               );
Begin

  { Deselect all options and data and reset the form }
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
  
  { Enable the grid }
  grdPrice.Enabled          := True;

End; { btnCancelClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.btnSaveClick                     (         Sender: TObject               );
Var
  NewItemID : Integer;
  RowIndex  : Integer;
  Size_Ptr  : Integer;
  Type_Ptr  : Integer;
  Orig_ID   : Integer;
  Price     : Extended;

Begin

  With dmApp Do Begin

    { Grab the selected top row's index number }
    RowIndex := grdPrice.Selection.Top;

    { Remove the dollar sign before saving it to the database }
    Price := StrToFloat (StringReplace (edtPrice.Text, '$', '', [rfReplaceAll]));

    { Get the Pointers needed to store the links of size and type }
    Size_Ptr := -1;
    Size_Ptr := Integer (cbxSize.Items.Objects [cbxSize.ItemIndex]);

    Type_Ptr := -1;
    Type_Ptr := Integer (cbxType.Items.Objects [cbxType.ItemIndex]);


    { If the list does not have an item highlighted, insert a new record }
    If (IsNewRecord) Then Begin

      qryAmana.SQL.Text := 'Insert Into popcorntypeprices (Price, Type_Ptr, Size_Ptr) Values (' + FormatFloat ('0.00', Price) + ',' + IntToStr (Type_Ptr) + ',' + IntToStr (Size_Ptr) + ');';

      { Insert, then open the SQL query }
      qryAmana.ExecSQL;
      qryAmana.SQL.Text := 'Select Last_Insert_ID() As ID;';
      qryAmana.Open;
      NewItemID := qryAmana.FieldByName ('ID').AsInteger;
      qryAmana.Close;

      { Add a record to the audit trail }
      AddAuditTrailRecord ('Insert Into popcorntypepricesaudit (Price, Type_Ptr, Size_Ptr, ID_Ptr, ChangedOn) Values (' + FormatFloat ('0.00', Price) + ',' + IntToStr (Type_Ptr) + ',' + IntToStr (Size_Ptr) + ',' + IntToStr (NewItemID) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

      { Add the item to the list-REFRESH the grid }
      RefreshGridData;

    End

    { Otherwise, update the selected record }
    Else Begin

      { Get the updated ID for the Audit Trail }
      Orig_ID := 0;
      qryAmana.SQL.Text := 'Select ID ' +
                           'From popcorntypeprices ' +
                           'Where (Price = ' + RightStr (grdPrice.Cells [0, RowIndex], Length (grdPrice.Cells [0, RowIndex]) - 1) + ') And ' +
                                 '(Size_Ptr = ' + IntToStr (Integer (cbxSize.Items.Objects [cbxSize.Items.IndexOf (grdPrice.Cells [1, RowIndex])])) + ') And ' +
                                 '(Type_Ptr = ' + IntToStr (Integer (cbxType.Items.Objects [cbxType.Items.IndexOf (grdPrice.Cells [2, RowIndex])])) + ')';

      qryAmana.Open;
      Orig_ID := qryAmana.FieldByName ('ID').AsInteger;
      qryAmana.Close;


      { Update the record }
      qryAmana.SQL.Text := 'Update popcorntypeprices Set ' +
                                   'Price = '         + FormatFloat ('0.00', Price) + ',' +
                                   'Type_Ptr = '      + IntToStr (Type_Ptr) + ',' +
				   'Size_Ptr = '      + IntToStr (Size_Ptr) + ' ' +

                           'Where (ID = ' + IntToStr (Orig_ID) + ');';

      { Add a record to the audit trail }
      AddAuditTrailRecord ('Insert Into popcorntypepricesaudit (Price, Type_Ptr, Size_Ptr, ID_Ptr, ChangedOn) Values (' + FormatFloat ('0.00', Price) + ',' + IntToStr (Type_Ptr) + ',' + IntToStr (Size_Ptr) + ',' + IntToStr (Orig_ID) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

      { Execute the SQL query }
      qryAmana.ExecSQL;

      { Updatge the item on the list-REFRESH the grid }
      RefreshGridData;

      { Resize the columns to autoexpand }
      grdPriceResize (grdPrice);

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
  
  { Enable the grid }
  grdPrice.Enabled := True;

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

    qryAmana.SQL.Text := 'Select ID, Size From popcornsizes Order By ID;';

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

Procedure TfrmPriceManagement.AutoSizeCol                      (         Grid: TStringGrid;
                                                                       Column: Integer                 );
Var
  RowIndex  : Integer;
  ItemWidth : Integer;
  WidthMax  : Integer;

Begin

  WidthMax := 0;

  For RowIndex := 0 To (Grid.RowCount - 1) Do Begin

    Application.ProcessMessages;

    ItemWidth := Grid.Canvas.TextWidth (Grid.Cells [Column, RowIndex]);

    If (ItemWidth > WidthMax)
      Then WidthMax := ItemWidth;
  End;

  { Set the column width to the longest string }
  Grid.ColWidths [Column] := WidthMax + 5;

End; { AutoSizeCol Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmPriceManagement.RefreshGridData;
Var
  RowIndex    : Integer;
  TempIndex   : Integer;

Begin

  { Clear off the grid and reset the row count }
  grdPrice.Clear;

  With dmApp Do Begin

    { Alert the grid how many rows we'll have }
    qryAmana.SQL.Text := 'Select COUNT(popcorntypeprices.Price) As Number ' +
                         'From popcorntypeprices ';
    qryAmana.Open;

    { Set the number of rows on the grid, + the header row }
    grdPrice.RowCount := qryAmana.FieldByName ('Number').AsInteger + 1;

    qryAmana.Close;




    { Initialize a default RowIndex }
    RowIndex := 0;

    { Add the header row }
    grdPrice.Cells [0, RowIndex] := 'Price';
    grdPrice.Cells [1, RowIndex] := 'Size';
    grdPrice.Cells [2, RowIndex] := 'Type';

    { Find all the prices for each of the types/sizes in the database }
    qryAmana.SQL.Text := 'Select ID, Price, Type_Ptr, Size_Ptr ' +
                         'From popcorntypeprices ';

    qryAmana.Open;

    While (Not qryAmana.Eof) Do Begin

      Application.ProcessMessages;

      { Set the next row }
      Inc (RowIndex);

      grdPrice.Cells [0, RowIndex] := '$' + CleanQueryVarStr (qryAmana.FieldByName ('Price').AsString);


      { If the Size Pointer points to a valid size still, then find it and select that item from the list }
      TempIndex := cbxSize.Items.IndexOfObject (TObject (qryAmana.FieldByName ('Size_Ptr').AsInteger));

      If (TempIndex > -1)
        Then grdPrice.Cells [1, RowIndex] := cbxSize.Items.Strings [TempIndex];


      { If the Type Pointer points to a valid type still, then find it and select that item from the list }
      TempIndex := cbxType.Items.IndexOfObject (TObject (qryAmana.FieldByName ('Type_Ptr').AsInteger));

      If (TempIndex > -1)
        Then grdPrice.Cells [2, RowIndex] := cbxType.Items.Strings [TempIndex];

      { Get the next record }
      qryAmana.Next;

    End; { While }

    qryAmana.Close;

  End; { With dmApp }

End; { RefreshGridData Procedure }
{ ---------------------------------------------------------------------------- }

End.

