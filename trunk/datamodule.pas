Unit DataModule;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  SysUtils,
  mysql50conn,
  sqldb,
  odbcconn,
  FileUtil,
  Dialogs;

Type

  { TdmApp }
  TdmApp = Class (TDataModule)
    dbAmana      : TMySQL50Connection;
    qryAmana     : TSQLQuery;
    qryQuick     : TSQLQuery;
    qryAudit: TSQLQuery;
    sqlTran      : TSQLTransaction;


    Procedure DataModuleCreate                                 (         Sender: TObject               );

    Public
     Function CleanQueryVarStr                                 (          Input: String                ): String;
     Procedure AddAuditTrailRecord                             (         SQLTxt: String                );

  End;

Var
  dmApp: TdmApp;

Implementation

{$R *.lfm}

{ ---------------------------------------------------------------------------- }

Procedure TdmApp.DataModuleCreate                              (         Sender: TObject               );
Begin

  { Set up the database connection properties when using MySQL... }
  dbAmana.UserName     := 'root';
  dbAmana.Password     := 'sabers';
  dbAmana.HostName     := '127.0.0.1';
  dbAmana.Port         := 3306;
  dbAmana.DatabaseName := 'amana';

  dbAmana.Transaction  := sqlTran;
  qryAmana.Database    := dbAmana;
  qryQuick.Database    := dbAmana;

End; { DataModuleCreate Procedure }
{ ---------------------------------------------------------------------------- }

Function TdmApp.CleanQueryVarStr                               (          Input: String                ): String;
Begin

  { Duplicate the comma's to eliminate SQL Injection }
  Result := StringReplace (Input, '''', '''''', [rfReplaceAll]);

End; { CleanQueryVarStr Function }
{ ---------------------------------------------------------------------------- }

Procedure TdmApp.AddAuditTrailRecord                           (         SQLTxt: String                );
Begin

  { Copy over the SQL to the audit trail }
  qryAudit.SQL.Text := SQLTxt;

  { Execute the SQL, Insert a new record }
  qryAudit.ExecSQL;

End; { AddAuditTrailRecord Procedure }
{ ---------------------------------------------------------------------------- }



End.

