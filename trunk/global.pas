Unit Global;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  SysUtils,
  FileUtil,
  Windows,
  Controls,
  Dialogs,
  Forms;



Function GetPCName: String;
Procedure LoadGlobalSettings             (          MainForm: TForm           );
Procedure SaveGlobalSettings             (          MainForm: TForm           );

{ Global Variables }
Var
  APPLICATION_PATH                        : String;
  SAVE_DIALOG_LOC                         : String;
  SEARCH_TERM                             : String;

  ONLY_SHOW_ACTIVE                        : Boolean;

  SAVE_DIALOG_FILTER_INDEX                : Integer;
  FORM_HIDE_LEFT                          : Integer;
  FORM_HIDE_TOP                           : Integer;
  FORM_HIDE_HEIGHT                        : Integer;
  FORM_HIDE_WIDTH                         : Integer;

  GRDMAIN_COLUMNS_WIDTHS                  : TStringList;
  HIDDEN_GRDMAIN_COLUMNS                  : TStringList;


Implementation









{ ---------------------------------------------------------------------------- }

Function GetPCName: String;
Var
  ComputerNameBuffer : Array [0..255] Of Char;
  SizeBuffer         : DWord;

Begin

  {$IFDEF MSWINDOWS}
    SizeBuffer := 256;
    Windows.GetComputerName (ComputerNameBuffer, SizeBuffer);
    Result := String (ComputerNameBuffer);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
    Result := GetHostName;
  {$ENDIF UNIX}

End; { GetPCName Function }
{ ---------------------------------------------------------------------------- }

Procedure LoadGlobalSettings                                   (         MainForm: TForm               );
  { -------------------------------------------------------------------------- }

  { Function returns the first part of a string up to a delimeter }
  Function GetSettingParam (    Line      : String;
                                Delimeter : String ) : String;
  Begin

    { Init result }
    Result := '';

    { Find delimeter }
    If Pos (Delimeter, Line) > 0
      Then Result := Copy (Line, 1, Pos (Delimeter, Line) - 1);


  End; { End GetSettingParam function }
  { -------------------------------------------------------------------------- }

  { Function returns the second part of a string after a delimeter             }
  Function GetSettingValue                                     (         Line      : String;
                                                                         Delimeter : String            ) : String;
  Begin

    { Init result }
    Result := '';

    { Find delimeter }
    If (Pos (Delimeter, Line) > 0) Then Begin

      { Delete up to the delimeter }
      Delete (Line, 1, Pos (Delimeter, Line));

      { Return what is left }
      Result := Line;

    End;

  End; { End GetSettingValue function }
  { -------------------------------------------------------------------------- }

Var
  Settings          : TStringList;
  Index             : Integer;
  GrdMainIndex      : Integer;
  FileName          : String;
  TheLine           : String;
  Param             : String;


Begin

  { Get the settings file name }
  FileName := APPLICATION_PATH + '\Settings\' + GetPCName + '.dat';

  { If the file does not exist, exit }
  If (Not FileExists (FileName))
    Then Exit;

  { Create list to load settings to }
  Settings := TStringList.Create;
  Settings.Clear;

  Try
    { Load settings }
    Settings.LoadFromFile (FileName);
  Except
    { Show error and leave }
    ShowMessage ('Settings could not be loaded!');
    Settings.Free;
    Exit;
  End;

  { Start at column 0 }
  GrdMainIndex := 0;

  { Loop through all lines in settings file }
  For Index := 0 To Settings.Count - 1 Do Begin

    { Get the line }
    TheLine := Settings [Index];

    { Put the whole thing in an exception catcher }
    Try

      { Get a param to the left of the = if there is one }
      Param   := GetSettingParam (TheLine, '=');

      { Read Form options }
      If (Param = 'FormPositionLeft')
        Then MainForm.Left   := StrToInt (GetSettingValue (TheLine, '='))

      Else If (Param = 'FormPositionTop')
        Then MainForm.Top    := StrToInt (GetSettingValue (TheLine, '='))

      Else If (Param = 'FormSizeWidth')
        Then MainForm.Width  := StrToInt (GetSettingValue (TheLine, '='))

      Else If (Param = 'FormSizeHeight')
        Then MainForm.Height := StrToInt (GetSettingValue (TheLine, '='))

      Else If (Param = 'FormMaximized') Then Begin
             If (StrToInt (GetSettingValue (TheLine, '=')) = 0)
               Then MainForm.WindowState := wsNormal
               Else MainForm.WindowState := wsMaximized;
      End


      { Read global/edit options  }
      Else If (Param = 'ONLY_SHOW_ACTIVE')
        Then ONLY_SHOW_ACTIVE := StrToBool (GetSettingValue (TheLine, '='))

      Else If (Param = 'SAVE_DIALOG_LOC') Then Begin
        If (DirectoryExists (GetSettingValue (TheLine, '=')))
          Then SAVE_DIALOG_LOC := GetSettingValue (TheLine, '=');
      End

      Else If (Param = 'SVE_DLG_FLTR_INDX')
        Then SAVE_DIALOG_FILTER_INDEX := StrToInt (GetSettingValue (TheLine, '='))

      Else If (Param = 'GRDMAIN_COLUMNS_WIDTHS') Then Begin
        GRDMAIN_COLUMNS_WIDTHS.Strings [GrdMainIndex] := GetSettingValue (TheLine, '=');
        Inc (GrdMainIndex);
      End

      Else If (Param = 'HIDDEN_GRDMAIN_COLUMNS')
        Then HIDDEN_GRDMAIN_COLUMNS.Add (GetSettingValue (TheLine, '='))

      Else If (Param = 'FORM_HIDE_LEFT')
        Then FORM_HIDE_LEFT:= StrToInt (GetSettingValue (TheLine, '='))

      Else If (Param = 'FORM_HIDE_TOP')
        Then FORM_HIDE_TOP := StrToInt (GetSettingValue (TheLine, '='))

      Else If (Param = 'FORM_HIDE_WIDTH')
        Then FORM_HIDE_WIDTH := StrToInt (GetSettingValue (TheLine, '='))

      Else If (Param = 'FORM_HIDE_HEIGHT')
        Then FORM_HIDE_HEIGHT := StrToInt (GetSettingValue (TheLine, '='))

      Else If (Param = 'SEARCH_TERM')
        Then SEARCH_TERM := GetSettingValue (TheLine, '=');

    Except
      { Do nothing on exceptions.  Just continue }

    End; { Try }

  End; { Loop through settings file }

  { Free the settings file }
  Settings.Free;

End; { LoadGlobalSettings Procedure }
{ ---------------------------------------------------------------------------- }

Procedure SaveGlobalSettings                                   (         MainForm: TForm               );
Var
  Settings  : TStringList;
  FileName  : String;
  Maximized : Boolean;
  Index     : Integer;


Begin


  { Get the file name }
  FileName := APPLICATION_PATH + '\Settings\' + GetPCName + '.dat';

  { Make sure the directory exists }
  ForceDirectories (ExtractFilePath (FileName));

  { Create string list to populate with settings }
  Settings := TStringList.Create;
  Settings.Clear;

  { Remember if we have the window maximized }
  Maximized := (MainForm.WindowState = wsMaximized);

  { Make sure we drop back to normal mode so we can get valid position/size info }
  If (MainForm.WindowState <> wsNormal)
    Then MainForm.WindowState := wsNormal;

  { Save last time accessed.  This is just misc info }
  Settings.Add ('DateLastAccessed=' + FormatDateTime ('mm/dd/yyyy', Date));

  { Write Form options }
  Settings.Add ('FormPositionLeft=' + IntToStr (MainForm.Left));
  Settings.Add ('FormPositionTop='  + IntToStr (MainForm.Top));
  Settings.Add ('FormSizeWidth='    + IntToStr (MainForm.Width));
  Settings.Add ('FormSizeHeight='   + IntToStr (MainForm.Height));
  Settings.Add ('FormMaximized='    + IntToStr (Integer (Maximized)));

  { Write other terms and variables }
  Settings.Add ('ONLY_SHOW_ACTIVE=' + IntToStr (Integer (ONLY_SHOW_ACTIVE)));
  Settings.Add ('SAVE_DIALOG_LOC='  + SAVE_DIALOG_LOC);
  Settings.Add ('SVE_DLG_FLTR_INDX='+ IntToStr (SAVE_DIALOG_FILTER_INDEX));

  { Set up the column widths }
  For Index := 0 To GRDMAIN_COLUMNS_WIDTHS.Count - 1
    Do Settings.Add ('GRDMAIN_COLUMNS_WIDTHS=' + GRDMAIN_COLUMNS_WIDTHS.Strings [Index]);

  { Track all of the hidden columns }
  For Index := 0 To HIDDEN_GRDMAIN_COLUMNS.Count - 1
    Do Settings.Add ('HIDDEN_GRDMAIN_COLUMNS=' + HIDDEN_GRDMAIN_COLUMNS.Strings [Index]);

  Settings.Add ('FORM_HIDE_LEFT='  + IntToStr (FORM_HIDE_LEFT));
  Settings.Add ('FORM_HIDE_TOP='   + IntToStr (FORM_HIDE_TOP));
  Settings.Add ('FORM_HIDE_HEIGHT='+ IntToStr (FORM_HIDE_HEIGHT));
  Settings.Add ('FORM_HIDE_WIDTH=' + IntToStr (FORM_HIDE_WIDTH));
  Settings.Add ('SEARCH_TERM='     + SEARCH_TERM);




  Try
    { Save settings file }
    Settings.SaveToFile (FileName);
  Except
    ShowMessage ('Could not save program settings.');
  End;

  { Free the settings file }
  Settings.Free;

End; { SaveGlobalSettings Procedure }
{ ---------------------------------------------------------------------------- }

End.

