Unit SaleItemUnit;

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

  { TfrmSaleItem }
  TfrmSaleItem = Class (TForm)
    btnFlavorManagement : TButton;
    btnSizeManagement   : TButton;
    btnPriceManagement  : TButton;
    cbxFlavor           : TComboBox;
    cbxSize             : TComboBox;
    cbxPrice            : TComboBox;
    gbDetails           : TGroupBox;
    btnCancel           : TButton;
    btnSave             : TButton;
    lblFlavor           : TLabel;
    lblSize             : TLabel;
    lblPrice            : TLabel;
    pnlTop              : TPanel;
    pnlMid              : TPanel;
    pnlMidBot           : TPanel;
    pnlBot              : TPanel;


    Procedure FormCreate               (              Sender: TObject           );
    Procedure FormShow                 (              Sender: TObject           );
    Procedure cbxFlavorSelect          (              Sender: TObject           );
    Procedure cbxSizeSelect            (              Sender: TObject           );
    Procedure btnCancelClick           (              Sender: TObject           );
    Procedure btnSaveClick             (              Sender: TObject           );
    Procedure btnFlavorManagementClick (              Sender: TObject           );
    Procedure btnSizeManagementClick   (              Sender: TObject           );
    Procedure btnPriceManagementClick  (              Sender: TObject           );


    Private
      Flavor_ID : Integer;
      Size_ID   : Integer;
      Price_ID  : Integer;

      Procedure RefreshFlavorList;
      Procedure RefreshSizeList;
      Procedure RefreshPriceList;

   Public
     Property PassInFlavor : Integer Write Flavor_ID;
     Property PassInSize   : Integer Write Size_ID;
     Property PassInPrice  : Integer Write Price_ID;


  End;

Var
  frmSaleItem: TfrmSaleItem;

Implementation

{$R *.lfm}

Uses
  DataModule,
  Global,
  FlavorManagementUnit,
  SizeManagementUnit,
  PriceManagementUnit;


{ ----------------------------------------------------------------------------- }
{ ---------------------- FORM-INDUCED PUBLIC METHODS -------------------------- }
{ ----------------------------------------------------------------------------- }

Procedure TfrmSaleItem.FormCreate                              (         Sender: TObject               );
Begin

  { Initialize these internal values as zero }
  Flavor_ID := 0;
  Size_ID   := 0;
  Price_ID  := 0;

  { Display and enable the Cancel/Save buttons }
  btnCancel.Enabled           := True;
  btnCancel.Visible           := True;
  btnSave.Enabled             := True;
  btnSave.Visible             := True;
  btnFlavorManagement.Enabled := True;
  btnFlavorManagement.Visible := True;
  btnPriceManagement.Enabled  := True;
  btnPriceManagement.Visible  := True;
  btnSizeManagement.Enabled   := True;
  btnSizeManagement.Visible   := True;

End; { FormCreate Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSaleItem.FormShow                                (         Sender: TObject               );
Begin

  { Refresh the flavor list... }
  RefreshFlavorList;

  { If we are editting a record, attempt to load its data when showing the form }
  If (Flavor_ID > 0) And
     (cbxFlavor.Items.IndexOfObject (TObject (Flavor_ID)) > -1) Then Begin

    { Select the flavor item and load the size list }
    cbxFlavor.ItemIndex := cbxFlavor.Items.IndexOfObject (TObject (Flavor_ID));
    RefreshSizeList;

    If (Size_ID > 0) And
       (cbxSize.Items.IndexOfObject (TObject (Size_ID)) > -1) Then Begin

      { Select the size item and load the price list }
      cbxSize.ItemIndex := cbxSize.Items.IndexOfObject (TObject (Size_ID));
      RefreshPriceList;

      If (Price_ID > 0) And
         (cbxPrice.Items.IndexOfObject (TObject (Price_ID)) > -1)
        Then cbxPrice.ItemIndex := cbxPrice.Items.IndexOfObject (TObject (Price_ID));

    End
    Else
      cbxPrice.Enabled := False;

  End
  Else Begin
    cbxSize.Enabled  := False;
    cbxPrice.Enabled := False;
  End; { Else }

End; { FormShow Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSaleItem.cbxFlavorSelect                         (         Sender: TObject               );
Begin

  If (cbxFlavor.ItemIndex > 0) Then Begin

    cbxSize.Enabled := True;
    RefreshSizeList;

  End
  Else Begin

    cbxSize.Clear;
    cbxSize.Enabled := False;

    cbxPrice.Clear;
    cbxPrice.Enabled := False;

  End;

End; { cbxFlavorSelect Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSaleItem.cbxSizeSelect                           (         Sender: TObject               );
Begin

  If (cbxSize.ItemIndex > 0) Then Begin

    cbxPrice.Enabled := True;
    RefreshPriceList;

  End
  Else Begin

    cbxPrice.Clear;
    cbxPrice.Enabled := False;

  End;

End; { cbxSizeSelect Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSaleItem.btnCancelClick                          (         Sender: TObject               );
Begin



End; { btnCancelClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSaleItem.btnSaveClick                     (         Sender: TObject               );
Var
  NewItemID : Integer;

Begin

  With dmApp Do Begin

(*
    { If the user selected one from each column, then insert a new record }
    If (cbxFlavor.ItemIndex > 0) And
       (cbxSize.ItemIndex > 0) And
       (cbxPrice.ItemIndex > 0) Then Begin

      qryAmana.SQL.Text := 'Insert Into popcorntypeprices (Price, Type_Ptr, Size_Ptr) Values (''' + CleanQueryVarStr (edtPrice.Text) + ''',' + IntToStr (Integer (cbxSize.Items.Objects [cbxSize.ItemIndex])) + ',' + IntToStr (Integer (cbxPrice.Items.Objects [cbxPrice.ItemIndex])) + ');';

      { Add a record to the audit trail }
      AddAuditTrailRecord (qryAmana.SQL.Text);

      { Insert, then open the SQL query }
      qryAmana.ExecSQL;
      qryAmana.SQL.Text := 'Select Last_Insert_ID() As ID;';
      qryAmana.Open;
      NewItemID := qryAmana.FieldByName ('ID').AsInteger;
      qryAmana.Close;

      { Add a record to the audit trail }
      AddAuditTrailRecord ('Insert Into popcorntypepricesaudit (Price, Type_Ptr, Size_Ptr, ID_Ptr, ChangedOn) Values (''' + CleanQueryVarStr (edtPrice.Text) + ''',' + IntToStr (Integer (cbxSize.Items.Objects [cbxSize.ItemIndex])) + ',' + IntToStr (Integer (cbxPrice.Items.Objects [cbxPrice.ItemIndex])) + ',' + IntToStr (NewItemID) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

      { Add the item to the list }
      lstbxItems.Items.AddObject (CleanQueryVarStr (edtPrice.Text), TObject (NewItemID));

    End

    { Otherwise, update the selected record }
    Else Begin

      qryAmana.SQL.Text := 'Update popcornflavors Set ' +
                                   'Price = '''       + CleanQueryVarStr (edtPrice.Text)                              + ''',' +
                                   'Type_Ptr = '      + IntToStr (Integer (cbxSize.Items.Objects [cbxSize.ItemIndex])) + ',' +
							       'Size_Ptr = '      + IntToStr (Integer (cbxPrice.Items.Objects [cbxPrice.ItemIndex])) + ' ' +

                           'Where (ID = ' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ');';

      { Add a record to the audit trail }
      AddAuditTrailRecord ('Insert Into popcornflavorsaudit (Price, Type_Ptr, Size_Ptr, ID_Ptr, ChangedOn) Values (''' + CleanQueryVarStr (edtPrice.Text) + ''',' + IntToStr (Integer (cbxSize.Items.Objects [cbxSize.ItemIndex])) + ',' + IntToStr (Integer (cbxPrice.Items.Objects [cbxPrice.ItemIndex])) + ',' + IntToStr (Integer (lstbxItems.Items.Objects [lstbxItems.ItemIndex])) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

      { Execute the SQL query }
      qryAmana.ExecSQL;

      { Update the listbox's item's label }
      lstbxItems.Items.Strings [lstbxItems.ItemIndex] := CleanQueryVarStr (edtPrice.Text);

    End;
*)

  End; { With dmApp }

End; { btnSaveClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSaleItem.btnFlavorManagementClick                (         Sender: TObject               );
Var
  ItemIDSelected : Integer;

Begin

  { Allow the user to add more flavors }
  Application.CreateForm (TfrmFlavorManagement, frmFlavorManagement);
  frmFlavorManagement.ShowModal;
  frmFlavorManagement.Free;


  { Remember which ID was selected }
  ItemIDSelected := Integer (cbxFlavor.Items.Objects [cbxFlavor.ItemIndex]);

  { Refresh the flavor list }
  RefreshFlavorList;

  { If the flavor still exists, re-choose it for the user }
  If (cbxFlavor.Items.IndexOfObject (TObject (ItemIDSelected)) > -1)
    Then cbxFlavor.ItemIndex := cbxFlavor.Items.IndexOfObject (TObject (ItemIDSelected));

End; { btnFlavorManagementClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSaleItem.btnSizeManagementClick           (         Sender: TObject               );
Var
  ItemIDSelected : Integer;

Begin

  { Allow the user to add more types }
  Application.CreateForm (TfrmSizeManagement, frmSizeManagement);
  frmSizeManagement.ShowModal;
  frmSizeManagement.Free;


  { Remember which ID was selected }
  ItemIDSelected := Integer (cbxPrice.Items.Objects [cbxPrice.ItemIndex]);

  { Refresh the type list }
  RefreshSizeList;

  { If the type still exists, re-choose it for the user }
  If (cbxPrice.Items.IndexOfObject (TObject (ItemIDSelected)) > -1)
    Then cbxPrice.ItemIndex := cbxPrice.Items.IndexOfObject (TObject (ItemIDSelected));

End; { btnSizeManagementClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSaleItem.btnPriceManagementClick                 (         Sender: TObject               );
Var
  ItemIDSelected : Integer;

Begin

  { Allow the user to add more prices }
  Application.CreateForm (TfrmPriceManagement, frmPriceManagement);
  frmPriceManagement.ShowModal;
  frmPriceManagement.Free;


  { Remember which ID was selected }
  ItemIDSelected := Integer (cbxPrice.Items.Objects [cbxPrice.ItemIndex]);

  { Refresh the price list }
  RefreshPriceList;

  { If the price still exists, re-choose it for the user }
  If (cbxPrice.Items.IndexOfObject (TObject (ItemIDSelected)) > -1)
    Then cbxPrice.ItemIndex := cbxPrice.Items.IndexOfObject (TObject (ItemIDSelected));

End; { btnPriceManagementClick Procedure }
{ ---------------------------------------------------------------------------- }











{ ---------------------------------------------------------------------------- }
{ ---------------------------- PRIVATE METHODS ------------------------------- }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSaleItem.RefreshFlavorList;
Begin

  { Clear off the olf flavors }
  cbxFlavor.Clear;

  { Insert the first option }
  cbxFlavor.Items.AddObject ('Select a Flavor...', TObject (0));

  { Auto select the first option }
  cbxFlavor.ItemIndex := 0;

  { Reload all of the applicable Popcorn Flavor from the table }
  With dmApp Do Begin

    qryAmana.SQL.Text := 'Select popcornflavors.ID, popcornflavors.Flavor, popcorntype.Type From popcornflavors Inner Join popcorntype On (popcorntype.ID = popcornflavors.Type_Ptr) Order By popcorntype.Type, popcornflavors.Flavor;';

    qryAmana.Open;

    While (Not qryAmana.Eof) Do Begin

      Application.ProcessMessages;

      { Add a new object }
      cbxFlavor.Items.AddObject (qryAmana.FieldByName ('Type').AsString + '-' + qryAmana.FieldByName ('Flavor').AsString, TObject (qryAmana.FieldByName ('ID').AsInteger));

      qryAmana.Next;

    End; { While }

    qryAmana.Close;

  End; { With dmApp }

End; { RefreshFlavorList Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmSaleItem.RefreshSizeList;
Begin

  { Clear off the old sizes }
  cbxSize.Clear;

  { Insert the first option }
  cbxSize.Items.AddObject ('Select a Size...', TObject (0));

  { Auto select the first option }
  cbxSize.ItemIndex := 0;


  { Reload all of the popcorn sizes from the size table }
  With dmApp Do Begin

    qryAmana.SQL.Text := 'Select popcornsizes.ID, popcornsizes.Size From popcornsizes Order By popcornsizes.ID;';

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

Procedure TfrmSaleItem.RefreshPriceList;
Var
  TypePtr : Integer;

Begin

  { Clear off the old prices }
  cbxPrice.Clear;

  { Initialize the type pointer }
  TypePtr := 0;

  { Insert the first option }
  cbxPrice.Items.AddObject ('Select a Price...', TObject (0));

  { Auto select the first option }
  cbxPrice.ItemIndex := 0;


  { Grab the type from the selected flavor }
  If (cbxFlavor.ItemIndex > 0) Then Begin

    With dmApp Do Begin

        qryAmana.SQL.Text := 'Select Type_Ptr From popcornflavors Where (ID = ' + IntToStr (Integer (cbxFlavor.Items.Objects [cbxFlavor.ItemIndex])) + ');';

        qryAmana.Open;

        If (Not qryAmana.Eof)
          Then TypePtr := qryAmana.FieldByName ('Type_Ptr').AsInteger;

        qryAmana.Close;

    End; { With dm }

  End;

  { If we found a valid type pointer }
  If (TypePtr <> 0) Then Begin

    { Reload all of the applicable Popcorn Prices from the table }
    With dmApp Do Begin

      qryAmana.SQL.Text := 'Select popcorntypeprices.ID, popcorntypeprices.Price From popcorntypeprices Where (popcorntypeprices.Size_Ptr = ' + IntToStr (Integer (cbxSize.Items.Objects [cbxSize.ItemIndex]))+ ') And (popcorntypeprices.Type_Ptr = ' + IntToStr (TypePtr) + ') Order By popcorntypeprices.Price;';

      qryAmana.Open;

      While (Not qryAmana.Eof) Do Begin

        Application.ProcessMessages;

        { Add a new object }
        cbxPrice.Items.AddObject ('$' + qryAmana.FieldByName ('Price').AsString, TObject (qryAmana.FieldByName ('ID').AsInteger));

        qryAmana.Next;

      End; { While }

      qryAmana.Close;

    End; { With dmApp }

  End; { If we had a valid type pointer }

End; { RefreshPriceList Procedure }
{ ---------------------------------------------------------------------------- }


End.

