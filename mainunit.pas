Unit MainUnit;

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
  Menus,
  Grids,
  ComCtrls;


Type

  { TfrmMain }

  TfrmMain = Class (TForm)
    mnuOptionsActive                    : TMenuItem;
    mnuOptions                          : TMenuItem;
    mnuEditCopy                         : TMenuItem;
    mnuEdit                             : TMenuItem;
    pmuEdit                             : TMenuItem;
    pmuDeactivateFlavor                 : TMenuItem;
    pmuAdd                              : TMenuItem;
    mnuManagementPrices                 : TMenuItem;
    mnuManagementTypes                  : TMenuItem;
    mnuManagementFlavors                : TMenuItem;
    mnuManagementSize                   : TMenuItem;
    mnuFileExit                         : TMenuItem;
    mnuManagement                       : TMenuItem;
    mnuFile                             : TMenuItem;
    mnu                                 : TMainMenu;
    grdMain                             : TStringGrid;
    pmu                                 : TPopupMenu;
    sBar                                : TStatusBar;



    Procedure FormCreate                (             Sender: TObject           );
    Procedure FormShow                  (             Sender: TObject           );
    Procedure grdMainHeaderClick        (             Sender: TObject;
                                                    IsColumn: Boolean;
                                                       Index: Integer           );
    Procedure FocusCellLikeLeftClick    (             Sender: TObject;
                                                      Button: TMouseButton;
                                                       Shift: TShiftState;
                                                           X: Integer;
                                                           Y: Integer           );
    Procedure grdMainResize             (             Sender: TObject           );
    Procedure mnuOptionsActiveClick     (             Sender: TObject           );
    Procedure mnuEditCopyClick          (             Sender: TObject           );
    Procedure mnuFileExitClick          (             Sender: TObject           );
    Procedure mnuManagementFlavorsClick (             Sender: TObject           );
    Procedure mnuManagementPricesClick  (             Sender: TObject           );
    Procedure mnuManagementSizeClick    (             Sender: TObject           );
    Procedure mnuManagementTypesClick   (             Sender: TObject           );
    Procedure pmuAddClick               (             Sender: TObject           );
    Procedure pmuEditClick              (             Sender: TObject           );
    Procedure pmuDeactivateFlavorClick  (             Sender: TObject           );
    Procedure FormClose                 (             Sender: TObject;
                                             Var CloseAction: TCloseAction      );
    procedure pmuPopup(Sender: TObject);

    Private
      GridShown : Boolean;

      Procedure ResetColumnHeader       (       NumberOfRows: Integer = 1       );
      Procedure RefreshGridData;

  End;

Var
  frmMain: TfrmMain;

Implementation

{$R *.lfm}

{ TfrmMain }


Uses
  ClipBrd,
  DataModule,
  Global,
  SizeManagementUnit,
  TypeManagementUnit,
  FlavorManagementUnit,
  PriceManagementUnit,
  SaleItemUnit;


Const
  GridColCount = 12;

  GridColFlavor   = 0;
  GridColDesc     = 1;
  GridColType     = 2;
  GridColPrice    = 3;
  GridColSize     = 4;
  GridColHeight   = 5;
  GridColDepth    = 6;
  GridColWidth    = 7;
  GridColCups     = 8;
  GridColFlavorID = 9;
  GridColSizeID   = 10;
  GridColPriceID  = 11;


{ ---------------------------------------------------------------------------- }
{ ---------------------- FORM-INDUCED PUBLIC METHODS ------------------------- }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.FormCreate                                  (         Sender: TObject               );
Begin

  { Do not run the grdMain Resize procedure until AFTER the FormShow procedure }
  GridShown := False;

  { Set the global variables }
  APPLICATION_PATH := ExtractFilePath (Application.EXEName);

  { Initially, only show the active flavors }
  ONLY_SHOW_ACTIVE := True;

  { Load our global settings }
  LoadGlobalSettings (frmMain);

  { After loading in the settings, check/uncheck the active switch }
  mnuOptionsActive.Checked := ONLY_SHOW_ACTIVE;

  { Create the datamodule to use throughout the application }
  Application.CreateForm (TdmApp, dmApp);

  { Set up some min. constraints for our main window }
  frmMain.Constraints.MinWidth  := 500;
  frmMain.Constraints.MinHeight := 300;

End; { FormCreate Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.FormShow                                    (         Sender: TObject               );
Var
  Index  : Integer;
  Column : TGridColumn;

Begin

  { Do not set the first column as a fixed one...allow us to put in dynamic data into it... }
  grdMain.FixedCols := 0;

  { Add all the grid columns }
  For Index := 0 To GridColCount - 1 Do Begin

    { Add a custom column a grid }
    Column := grdMain.Columns.Add;

    { Make this the X'th column }
    Column.Index := Index;

  End; { For }

  { Load the grid data }
  RefreshGridData;

  { Enable resizing of the grid }
  GridShown := True;

End; { FormShow Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.grdMainHeaderClick                          (         Sender: TObject;
                                                                       IsColumn: Boolean;
                                                                          Index: Integer               );
Begin

  If (IsColumn) Then Begin

    { Reverse the sorting order }
    If (grdMain.SortOrder = soDescending)
      Then grdMain.SortOrder := soAscending
      Else grdMain.SortOrder := soDescending;

    { Sort the selected column }
    grdMain.SortColRow (IsColumn, Index);

  End; { If the user clicked on a column }

End; { grdMainHeaderClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.FocusCellLikeLeftClick                      (         Sender: TObject;
                                                                         Button: TMouseButton;
                                                                          Shift: TShiftState;
                                                                              X: Integer;
                                                                              Y: Integer               );
Var
  SelectedCol : Integer;
  SelectedRow : Integer;

Begin

  { If the user right clicked on the grid }
  If (Button = mbRight) Then Begin

    { Initialize the col/rows }
    SelectedCol := 0;
    SelectedRow := 0;

    { Get the col/row of the right click action }
    grdMain.MouseToCell (X, Y, SelectedCol, SelectedRow);

    { Clear the previously selected rect }
    grdMain.Selection := TGridRect (Rect (-1, -1, -1, -1));

    { Force the selection of this cell }
    grdMain.Col := SelectedCol;
    grdMain.Row := SelectedRow;

    grdMain.SetFocus;

  End;

End; { FocusCellLikeLeftClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.grdMainResize                               (         Sender: TObject               );
Var
  TotalColumnWidths : Integer;
  Index             : Integer;

Begin

  If (GridShown) Then Begin

    { Init total column widths }
    TotalColumnWidths := 0;

    { Get the total for all the column width (-4 means to EXCLUDE THE ID HOLDER COLUMNS) }
    For Index := 0 To grdMain.ColCount - 4
      Do TotalColumnWidths := TotalColumnWidths + grdMain.Columns [Index].Width;



    For Index := 0 To grdMain.ColCount - 4 Do Begin

      { Use it as a percentage, and round down }
      grdMain.Columns [Index].Width := Trunc ((grdMain.Columns [Index].Width / TotalColumnWidths) * frmMain.Width);

      { Do not allow the column to get smaller than 30 pixels }
      If (grdMain.Columns [Index].Width < 30)
        Then grdMain.Columns [Index].Width := 30;

    End; { For }
  End; { If grid is shown }
End; { grdMainResize Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.mnuOptionsActiveClick                       (         Sender: TObject               );
Begin

  ONLY_SHOW_ACTIVE := mnuOptionsActive.Checked;

  { Reload the grid data }
  RefreshGridData;

End; { mnuOptionsActiveClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.mnuEditCopyClick                            (         Sender: TObject               );
Var
  SelectedRect : TGridRect;
  RowIndex     : Integer;
  ColumnIndex  : Integer;
  SelectedText : String;

Begin

  { Grab the selected part of the grid }
  SelectedRect := grdMain.Selection;

  { Initialize the text }
  SelectedText := '';

  For RowIndex := SelectedRect.Top To SelectedRect.Bottom Do Begin

    For ColumnIndex := SelectedRect.Left To SelectedRect.Right Do Begin

      SelectedText := SelectedText + grdMain.Cells [ColumnIndex, RowIndex];

      If (ColumnIndex < SelectedRect.Right)
        Then SelectedText := SelectedText + #9;

    End; { For each Column }

    If (RowIndex < SelectedRect.Bottom)
      Then SelectedText := SelectedText + #13#10;

  End; { For each Row }

  If (SelectedText <> '')
    Then ClipBoard.AsText := SelectedText;

End; { mnuEditCopyClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.mnuFileExitClick                            (         Sender: TObject               );
Begin

  { Close the application }
  Close;

End; { mnuFileExitClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementFlavorsClick                   (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmFlavorManagement, frmFlavorManagement);
  frmFlavorManagement.ShowModal;
  frmFlavorManagement.Free;

  { Reload the grid data }
  RefreshGridData;

End; { mnuManagementFlavorsClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementPricesClick                    (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmPriceManagement, frmPriceManagement);
  frmPriceManagement.ShowModal;
  frmPriceManagement.Free;

  { Reload the grid data }
  RefreshGridData;

End; { mnuManagementPricesClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementSizeClick                      (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmSizeManagement, frmSizeManagement);
  frmSizeManagement.ShowModal;
  frmSizeManagement.Free;

  { Reload the grid data }
  RefreshGridData;

End; { mnuManagementSizeClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementTypesClick                     (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmTypeManagement, frmTypeManagement);
  frmTypeManagement.ShowModal;
  frmTypeManagement.Free;

  { Reload the grid data }
  RefreshGridData;

End; { mnuManagementTypesClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.pmuAddClick                                 (         Sender: TObject               );
Begin

  { Create, display, and free the form }
  Application.CreateForm (TfrmSaleItem, frmSaleItem);

  If (frmSaleItem.ShowModal = mrOK)
    Then RefreshGridData;

  frmSaleItem.Free;

  { Reload the grid data }
  RefreshGridData;

End; { pmuAddClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.pmuEditClick                                (         Sender: TObject               );
Var
  RowIndex     : Integer;

Begin

  { If the user selected an cell on the grid }
  If (grdMain.Selection.Top > 0) Then Begin

    { Create, display, and free the form }
    Application.CreateForm (TfrmSaleItem, frmSaleItem);

    { Get the FIRST selected row }
    RowIndex := grdMain.Selection.Top;

    frmSaleItem.PassInFlavor := StrToInt (grdMain.Cells [GridColFlavorID, RowIndex]);
    frmSaleItem.PassInSize   := StrToInt (grdMain.Cells [GridColSizeID, RowIndex]);
    frmSaleItem.PassInPrice  := StrToInt (grdMain.Cells [GridColPriceID, RowIndex]);

    { Refresh the grid's data }
    If (frmSaleItem.ShowModal = mrOK)
      Then RefreshGridData;

    { Free the form }
    frmSaleItem.Free;

  End; { If the user selected a part of the form }

End; { pmuEditClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.pmuDeactivateFlavorClick                    (         Sender: TObject               );
Var
  RowIndex : Integer;
  FlavorID : Integer;
  SizeID   : Integer;
  PriceID  : Integer;

Begin

  { Initialize the IDs }
  FlavorID := 0;
  SizeID   := 0;
  PriceID  := 0;

  { If the user selected an cell on the grid }
  If (grdMain.Selection.Top > 0) Then Begin

    { Get the FIRST selected row }
    RowIndex := grdMain.Selection.Top;

    FlavorID := StrToInt (grdMain.Cells [GridColFlavorID, RowIndex]);
    SizeID   := StrToInt (grdMain.Cells [GridColSizeID, RowIndex]);
    PriceID  := StrToInt (grdMain.Cells [GridColPriceID, RowIndex]);

  End; { Get the selected row }


  { Ensure that the user wants to continue }
  If ((FlavorID > 0) And
      (SizeID > 0) Or
      (PriceID > 0)) And

     (MessageDlg ('Question', 'Do you wish to deactivate this flavor?', mtConfirmation, [mbYes, mbNo],0) = mrYes) Then Begin

    { Use datamodule }
    With dmApp Do Begin

      Try

        { Deactivate this item }
        qryAmana.SQL.Text := 'Update popcornflavors Set ' +
                                    'popcornflavors.Active = 0, ' +
                                    'popcornflavors.Type_Ptr = 0 ' +
                             'Where (popcornflavors.ID = ' + IntToStr (FlavorID) + ');';

        qryAmana.ExecSQL;

        { Add a record to the audit trail }
        AddAuditTrailRecord ('Insert Into popcornflavorsaudit (Flavor, Description, Type_Ptr, Active, ID_Ptr, ChangedOn) Values ('''','''',0,0,' + IntToStr (FlavorID) + ',''' + FormatDateTime ('YYYY-MM-DD hh:nn:ss', Now) + ''');');

        { Reload the grid data }
        RefreshGridData;

      Except
        On E : Exception
          Do ShowMessage (E.Message);
      End; { Try }

    End; { With dmApp }

  End; { If the user agrees to coninue }

End; { pmuDeactivateFlavorClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.FormClose                                   (         Sender: TObject;
                                                                Var CloseAction: TCloseAction          );
Begin

  { Free the data module }
  dmApp.Free;

  { Save our global settings }
  SaveGlobalSettings (frmMain);

End; { FormClose Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.pmuPopup                                    (         Sender: TObject               );
Var
  RowIndex : Integer;
  SizeID   : Integer;
  PriceID  : Integer;

Begin

  { Initialize the IDs }
  SizeID   := 0;
  PriceID  := 0;

  { If the user selected an cell on the grid }
  If (grdMain.Selection.Top > 0) Then Begin

    { Get the FIRST selected row }
    RowIndex := grdMain.Selection.Top;

    SizeID   := StrToInt (grdMain.Cells [GridColSizeID, RowIndex]);
    PriceID  := StrToInt (grdMain.Cells [GridColPriceID, RowIndex]);

  End; { Get the selected row }

  { Only allow the user to disable items that have already been disabled }
  pmuDeactivateFlavor.Enabled := ((SizeID <> 0) Or
                                  (PriceID <> 0));

End; { pmuPopup Procedure }
{ ---------------------------------------------------------------------------- }







{ ---------------------------------------------------------------------------- }
{ ---------------------------- PRIVATE METHODS ------------------------------- }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.ResetColumnHeader                           (         NumberOfRows: Integer = 1       );
Begin

  { Add a new row }
  grdMain.RowCount := NumberOfRows;

  { Set up the default column widths }
  grdMain.Columns [GridColFlavor   ].Width := 100;
  grdMain.Columns [GridColDesc     ].Width := 300;
  grdMain.Columns [GridColType     ].Width := 100;
  grdMain.Columns [GridColPrice    ].Width := 80;
  grdMain.Columns [GridColSize     ].Width := 40;
  grdMain.Columns [GridColHeight   ].Width := 40;
  grdMain.Columns [GridColDepth    ].Width := 40;
  grdMain.Columns [GridColWidth    ].Width := 40;
  grdMain.Columns [GridColCups     ].Width := 50;
  grdMain.Columns [GridColFlavorID ].Width := 0;
  grdMain.Columns [GridColSizeID   ].Width := 0;
  grdMain.Columns [GridColPriceID  ].Width := 0;


  { Set up the default column text }
  grdMain.Columns [GridColFlavor   ].Title.Caption := 'Flavor';
  grdMain.Columns [GridColDesc     ].Title.Caption := 'Description';
  grdMain.Columns [GridColType     ].Title.Caption := 'Type';
  grdMain.Columns [GridColPrice    ].Title.Caption := 'Price';
  grdMain.Columns [GridColSize     ].Title.Caption := 'Size';
  grdMain.Columns [GridColHeight   ].Title.Caption := 'Height';
  grdMain.Columns [GridColDepth    ].Title.Caption := 'Depth';
  grdMain.Columns [GridColWidth    ].Title.Caption := 'Width';
  grdMain.Columns [GridColCups     ].Title.Caption := 'Cups';
  grdMain.Columns [GridColFlavorID ].Title.Caption := 'FlavorID';
  grdMain.Columns [GridColSizeID   ].Title.Caption := 'SizeID';
  grdMain.Columns [GridColPriceID  ].Title.Caption := 'PriceID';

End; { ResetColumnHeader }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.RefreshGridData;
Var
  RowIndex    : Integer;
  WhereSQL    : String;

Begin

  { Clear off the grid and reset the row count }
  grdMain.Clear;

  { Show all or show only active flavors }
  If (ONLY_SHOW_ACTIVE)
    Then WhereSQL := ' Where (popcornflavors.Active = 1) '
    Else WhereSQL := '';


  With dmApp Do Begin
    qryAmana.SQL.Text := 'Select COUNT(popcornflavors.Flavor) As Number ' +

                         'From popcornflavors ' +

                         'Left Outer Join popcorntype On (popcorntype.ID = popcornflavors.Type_Ptr) ' +
                         'Left Outer Join popcorntypeprices On (popcorntypeprices.Type_Ptr = popcornflavors.Type_Ptr) ' +
                         'Left Outer Join popcornsizes On (popcornsizes.ID = popcorntypeprices.Size_Ptr) ' +
                         WhereSQL;
    qryAmana.Open;

    { Set the number of rows on the grid, + the header row }
    grdMain.RowCount := qryAmana.FieldByName ('Number').AsInteger + 1;

    qryAmana.Close;


    qryAmana.SQL.Text := 'Select popcornflavors.ID As Flavor_ID, ' +
                                'popcornflavors.Flavor, ' +
                                'popcornflavors.Description, ' +
                                'popcorntype.Type, ' +

                                'popcorntypeprices.ID As Price_ID, ' +
                                'popcorntypeprices.Price, ' +

                                'popcornsizes.ID As Size_ID, ' +
                                'popcornsizes.Size, ' +
                                'popcornsizes.Height, ' +
                                'popcornsizes.Depth, ' +
                                'popcornsizes.Width, ' +
                                'popcornsizes.Cups ' +

                         'From popcornflavors ' +

                         'Left Outer Join popcorntype On (popcorntype.ID = popcornflavors.Type_Ptr) ' +
                         'Left Outer Join popcorntypeprices On (popcorntypeprices.Type_Ptr = popcornflavors.Type_Ptr) ' +
                         'Left Outer Join popcornsizes On (popcornsizes.ID = popcorntypeprices.Size_Ptr) ' +

                         WhereSQL +

                         'Order By popcorntype.Type, popcornflavors.Flavor, popcornsizes.ID ';

    qryAmana.Open;

    { Initialize a default RowIndex }
    RowIndex := 0;

    { Add the header rows }
    If (grdMain.RowCount > 1)
      Then ResetColumnHeader (grdMain.RowCount)
      Else ResetColumnHeader (1);


    While (Not qryAmana.Eof) Do Begin

      sBar.Panels [0].Text := 'Processing ' + qryAmana.FieldByName ('Flavor').AsString;
      Application.ProcessMessages;

      { Set the next row }
      Inc (RowIndex);


      grdMain.Cells [GridColFlavor, RowIndex]   := qryAmana.FieldByName ('Flavor').AsString;
      grdMain.Cells [GridColDesc, RowIndex]     := qryAmana.FieldByName ('Description').AsString;
      grdMain.Cells [GridColType, RowIndex]     := qryAmana.FieldByName ('Type').AsString;
      grdMain.Cells [GridColPrice, RowIndex]    := '$' + FormatFloat ('0.00', qryAmana.FieldByName ('Price').AsFloat);
      grdMain.Cells [GridColSize, RowIndex]     := qryAmana.FieldByName ('Size').AsString;
      grdMain.Cells [GridColHeight, RowIndex]   := IntToStr (qryAmana.FieldByName ('Height').AsInteger);
      grdMain.Cells [GridColDepth, RowIndex]    := IntToStr (qryAmana.FieldByName ('Depth').AsInteger);
      grdMain.Cells [GridColWidth, RowIndex]    := IntToStr (qryAmana.FieldByName ('Width').AsInteger);
      grdMain.Cells [GridColCups, RowIndex]     := IntToStr (qryAmana.FieldByName ('Cups').AsInteger);
      grdMain.Cells [GridColFlavorID, RowIndex] := IntToStr (qryAmana.FieldByName ('Flavor_ID').AsInteger);
      grdMain.Cells [GridColSizeID, RowIndex]   := IntToStr (qryAmana.FieldByName ('Size_ID').AsInteger);
      grdMain.Cells [GridColPriceID, RowIndex]  := IntToStr (qryAmana.FieldByName ('Price_ID').AsInteger);

      qryAmana.Next;

    End; { While }

    qryAmana.Close;

    sBar.Panels [0].Text := 'Idle...';

  End; { With dmApp }

End; { RefreshGridData Procedure }
{ ---------------------------------------------------------------------------- }

End.

