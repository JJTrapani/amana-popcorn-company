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

  End;

Var
  frmMain: TfrmMain;

Implementation

{$R *.lfm}

{ TfrmMain }


Uses
  DataModule,
  SizeManagementUnit,
  TypeManagementUnit,
  FlavorManagementUnit,
  PriceManagementUnit;



{ ----------------------------------------------------------------------------- }
{ ---------------------- FORM-INDUCED PUBLIC METHODS -------------------------- }
{ ----------------------------------------------------------------------------- }

Procedure TfrmMain.FormCreate                                  (         Sender: TObject               );
Begin

  { Create the datamodule to use throughout the application }
  Application.CreateForm (TdmApp, dmApp);

End; { FormCreate Procedure }
{ ----------------------------------------------------------------------------- }

Procedure TfrmMain.mnuFileExitClick                            (         Sender: TObject               );
Begin

  { Close the application }
  Close;

End; { mnuFileExitClick Procedure }
{ ----------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementFlavorsClick                   (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmFlavorManagement, frmFlavorManagement);
  frmFlavorManagement.ShowModal;
  frmFlavorManagement.Free;

End; { mnuManagementFlavorsClick Procedure }
{ ----------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementPricesClick                    (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmPriceManagement, frmPriceManagement);
  frmPriceManagement.ShowModal;
  frmPriceManagement.Free;

End; { mnuManagementPricesClick Procedure }
{ ----------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementSizeClick                      (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmSizeManagement, frmSizeManagement);
  frmSizeManagement.ShowModal;
  frmSizeManagement.Free;

End; { mnuManagementSizeClick Procedure }
{ ----------------------------------------------------------------------------- }

Procedure TfrmMain.mnuManagementTypesClick                     (         Sender: TObject               );
Begin

  Application.CreateForm (TfrmTypeManagement, frmTypeManagement);
  frmTypeManagement.ShowModal;
  frmTypeManagement.Free;

End; { mnuManagementTypesClick Procedure }
{ ----------------------------------------------------------------------------- }
Procedure TfrmMain.pmuAddClick                                 (         Sender: TObject               );
Begin

  With dmApp Do Begin

  //  qryAmana.Database := dbAmana;
{
    qryAmana.SQL.Text := 'Select Type From PopcornType;';

    qryAmana.Open;
    While (Not qryAmana.Eof) Do Begin
      showmessage (qryAmana.Fields [0].AsString);
      qryAmana.Next;
    End;

    qryAmana.Close;
 }

  // Check if we have an active connection. If so, let's close it.
  If (dbAmana.Connected)
    Then dbAmana.CloseTransactions;

  // Set the connection parameters.
  ShowMessage ('Opening a connection to server: localhost');

  dbAmana.Open;

  // First lets get a list of available databases.
  If (dbAmana.Connected) Then Begin
    ShowMessage ('Retrieving list of available databases.');

    qryAmana.SQL.Text := 'Show Databases;';
    qryAmana.Open;

    While (Not qryAmana.Eof) Do Begin

      ShowMessage (qryAmana.Fields [0].AsString);
      qryAmana.Next;

    End;

    qryAmana.Close;

    ShowMessage ('List of databases received!');

  End;


  End;

End; { pmuAddClick Procedure }
{ ----------------------------------------------------------------------------- }

Procedure TfrmMain.pmuEditClick                                (         Sender: TObject               );
Begin

End; { pmuEditClick Procedure }
{ ----------------------------------------------------------------------------- }

Procedure TfrmMain.pmuRemoveClick                              (         Sender: TObject               );
Begin

End; { pmuRemoveClick Procedure }
{ ----------------------------------------------------------------------------- }

Procedure TfrmMain.FormClose                                   (         Sender: TObject;
                                                                Var CloseAction: TCloseAction          );
Begin

  { Free the data module }
  dmApp.Free;

End; { FormClose Procedure }
{ ----------------------------------------------------------------------------- }


End.

