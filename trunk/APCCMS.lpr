program APCCMS;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, 
  Forms, 
  MainUnit, 
  DataModule;

{$R *.res}

begin

  Application.Title := 'Amana Popcorn Company Content Management System';
  Application.Initialize;
  Application.CreateForm (TfrmMain, frmMain);
  Application.Run;

end.

