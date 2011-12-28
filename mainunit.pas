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
  Grids;


Type

  { TfrmMain }

  TfrmMain = Class (TForm)
    mnuEditCopy                         : TMenuItem;
    mnuEdit                             : TMenuItem;
    pmuEdit                             : TMenuItem;
    pmuRemove                           : TMenuItem;
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



    Procedure FormCreate                (             Sender: TObject           );
    Procedure FormShow                  (             Sender: TObject           );
    Procedure grdMainHeaderClick        (             Sender: TObject;
                                                    IsColumn: Boolean;
                                                       Index: Integer           );
    Procedure grdMainResize             (             Sender: TObject           );
    Procedure mnuEditCopyClick          (             Sender: TObject           );
    Procedure mnuFileExitClick          (             Sender: TObject           );
    Procedure mnuManagementFlavorsClick (             Sender: TObject           );
    Procedure mnuManagementPricesClick  (             Sender: TObject           );
    Procedure mnuManagementSizeClick    (             Sender: TObject           );
    Procedure mnuManagementTypesClick   (             Sender: TObject           );
    Procedure pmuAddClick               (             Sender: TObject           );
    Procedure pmuEditClick              (             Sender: TObject           );
    Procedure pmuRemoveClick            (             Sender: TObject           );
    Procedure FormClose                 (             Sender: TObject;
                                             Var CloseAction: TCloseAction      );

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
  PriceManagementUnit;


Const
  GridColCount = 9;

  GridColFlavor = 0;
  GridColDesc   = 1;
  GridColType   = 2;
  GridColPrice  = 3;
  GridColSize   = 4;
  GridColHeight = 5;
  GridColDepth  = 6;
  GridColWidth  = 7;
  GridColCups   = 8;


{ ---------------------------------------------------------------------------- }
{ ---------------------- FORM-INDUCED PUBLIC METHODS ------------------------- }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.FormCreate                                  (         Sender: TObject               );
Begin

  { Do not run the grdMain Resize procedure until AFTER the FormShow procedure }
  GridShown := False;

  { Set the global variables }
  APPLICATION_PATH := ExtractFilePath (Application.EXEName);

  { Load our global settings }
  LoadGlobalSettings (frmMain);

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

End; { grdMainHeaderClick }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.grdMainResize                               (         Sender: TObject               );
Var
  TotalColumnWidths : Integer;
  Index             : Integer;

Begin

  If (GridShown) Then Begin

    { Init total column widths }
    TotalColumnWidths := 0;

    For Index := 0 To grdMain.ColCount - 1 Do Begin
      TotalColumnWidths := TotalColumnWidths + grdMain.Columns [Index].Width;
    End; { For }

    For Index := 0 To grdMain.ColCount - 1 Do Begin

      { Use it as a percentage, and round down }
      grdMain.Columns [Index].Width := Trunc ((grdMain.Columns [Index].Width / TotalColumnWidths) * frmMain.Width);

      { Do not allow the column to get smaller than 30 pixels }
      If (grdMain.Columns [Index].Width < 30)
        Then grdMain.Columns [Index].Width := 30;

    End; { For }
  End; { If grid is shown }
End; { grdMainResize Procedure }
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

 //     If (False)
 //       Then grdMain.Cells [ColumnIndex, RowIndex] := '';

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

End; { mnuManagementFlavorsClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementPricesClick                    (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmPriceManagement, frmPriceManagement);
  frmPriceManagement.ShowModal;
  frmPriceManagement.Free;

End; { mnuManagementPricesClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementSizeClick                      (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmSizeManagement, frmSizeManagement);
  frmSizeManagement.ShowModal;
  frmSizeManagement.Free;

End; { mnuManagementSizeClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementTypesClick                     (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmTypeManagement, frmTypeManagement);
  frmTypeManagement.ShowModal;
  frmTypeManagement.Free;

End; { mnuManagementTypesClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.pmuAddClick                                 (         Sender: TObject               );
Begin

  ResetColumnHeader;

End; { pmuAddClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.pmuEditClick                                (         Sender: TObject               );
Begin

End; { pmuEditClick Procedure }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.pmuRemoveClick                              (         Sender: TObject               );
Begin

End; { pmuRemoveClick Procedure }
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







{ ---------------------------------------------------------------------------- }
{ ---------------------------- PRIVATE METHODS ------------------------------- }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.ResetColumnHeader                           (         NumberOfRows: Integer = 1       );
Begin

  { Add a new row }
  grdMain.RowCount := NumberOfRows ;

  { Set up the default column widths }
  grdMain.Columns [GridColFlavor ].Width := 100;
  grdMain.Columns [GridColDesc   ].Width := 300;
  grdMain.Columns [GridColType   ].Width := 100;
  grdMain.Columns [GridColPrice  ].Width := 80;
  grdMain.Columns [GridColSize   ].Width := 40;
  grdMain.Columns [GridColHeight ].Width := 40;
  grdMain.Columns [GridColDepth  ].Width := 40;
  grdMain.Columns [GridColWidth  ].Width := 40;
  grdMain.Columns [GridColCups   ].Width := 50;

  { Set up the default column text }
  grdMain.Columns [GridColFlavor ].Title.Caption := 'Flavor';
  grdMain.Columns [GridColDesc   ].Title.Caption := 'Description';
  grdMain.Columns [GridColType   ].Title.Caption := 'Type';
  grdMain.Columns [GridColPrice  ].Title.Caption := 'Price';
  grdMain.Columns [GridColSize   ].Title.Caption := 'Size';
  grdMain.Columns [GridColHeight ].Title.Caption := 'Height';
  grdMain.Columns [GridColDepth  ].Title.Caption := 'Depth';
  grdMain.Columns [GridColWidth  ].Title.Caption := 'Width';
  grdMain.Columns [GridColCups   ].Title.Caption := 'Cups';

End; { ResetColumnHeader }
{ ---------------------------------------------------------------------------- }

Procedure TfrmMain.RefreshGridData;
Var
  RowIndex    : Integer;

Begin

  { Clear off the grid and reset the row count }
  grdMain.Clear;

  With dmApp Do Begin
    qryAmana.SQL.Text := 'Select COUNT(popcornflavors.Flavor) As Number ' +

                         'From popcornflavors ' +

                         'Left Outer Join popcorntype On (popcorntype.ID = popcornflavors.Type_Ptr) ' +
                         'Left Outer Join popcorntypeprices On (popcorntypeprices.Type_Ptr = popcornflavors.Type_Ptr) ' +
                         'Left Outer Join popcornsizes On (popcornsizes.ID = popcorntypeprices.Size_Ptr) ';
    qryAmana.Open;

    { Set the number of rows on the grid, + the header row }
    grdMain.RowCount := qryAmana.FieldByName ('Number').AsInteger + 1;

    qryAmana.Close;


    qryAmana.SQL.Text := 'Select popcornflavors.Flavor, ' +
                                'popcornflavors.Description, ' +
                                'popcorntype.Type, ' +
                                'popcorntypeprices.Price, ' +
                                'popcornsizes.Size, ' +
                                'popcornsizes.Height, ' +
                                'popcornsizes.Depth, ' +
                                'popcornsizes.Width, ' +
                                'popcornsizes.Cups ' +

                         'From popcornflavors ' +

                         'Left Outer Join popcorntype On (popcorntype.ID = popcornflavors.Type_Ptr) ' +
                         'Left Outer Join popcorntypeprices On (popcorntypeprices.Type_Ptr = popcornflavors.Type_Ptr) ' +
                         'Left Outer Join popcornsizes On (popcornsizes.ID = popcorntypeprices.Size_Ptr) ' +
                         'Order By popcorntype.Type, popcornflavors.Flavor, popcornsizes.ID ';

    qryAmana.Open;

    { Initialize a default RowIndex }
    RowIndex := 0;

    { Add the header rows }
    If (grdMain.RowCount > 1)
      Then ResetColumnHeader (grdMain.RowCount)
      Else ResetColumnHeader (1);


    While (Not qryAmana.Eof) Do Begin

      Application.ProcessMessages;

      { Set the next row }
      Inc (RowIndex);

      grdMain.Cells [GridColFlavor, RowIndex] := qryAmana.FieldByName ('Flavor').AsString;
      grdMain.Cells [GridColDesc, RowIndex]   := qryAmana.FieldByName ('Description').AsString;
      grdMain.Cells [GridColType, RowIndex]   := qryAmana.FieldByName ('Type').AsString;
      grdMain.Cells [GridColPrice, RowIndex]  := '$' + FormatFloat ('0.00', qryAmana.FieldByName ('Price').AsFloat);
      grdMain.Cells [GridColSize, RowIndex]   := qryAmana.FieldByName ('Size').AsString;
      grdMain.Cells [GridColHeight, RowIndex] := IntToStr (qryAmana.FieldByName ('Height').AsInteger);
      grdMain.Cells [GridColDepth, RowIndex]  := IntToStr (qryAmana.FieldByName ('Depth').AsInteger);
      grdMain.Cells [GridColWidth, RowIndex]  := IntToStr (qryAmana.FieldByName ('Width').AsInteger);
      grdMain.Cells [GridColCups, RowIndex]   := IntToStr (qryAmana.FieldByName ('Cups').AsInteger);

      qryAmana.Next;

    End; { While }

    qryAmana.Close;

  End; { With dmApp }

End; { RefreshGridData Procedure }
{ ---------------------------------------------------------------------------- }

End.

