program nuclidecalcc;

{$APPTYPE CONSOLE}

uses
  SysUtils, Math, DateUtils, JvJCLUtils, LibUnit;

var AppPath, CurLibPath, S: string;
    OldAct, NewAct, tHalf: Double;  // old n new activity, half-life time
    OldDate, NewDate: TDateTime; // old and new date
    Nuclide: String; // current nulide name
    CurLib: TLibrary; // nuclide library
    ConvertResult: Boolean; // converting parameters result
    isNewNuclide: Boolean = False; // Is using temporary nuclide with entered tHalf

begin
  { TODO -oUser -cConsole Main : Insert code here }
  if (DecimalSeparator <> '.') then DecimalSeparator := '.';
  AppPath := ExtractFilePath(ParamStr(0));
  CurLibPath := AppPath + 'NuclideCalc.lib';

  if (ParamCount < 3) then
  begin
    Writeln('Program for activity recalc version 1.0.0:');
    Writeln('Format: nuclidecalcc <nuclidename> <oldactivity> <olddate> [<newdate>]');
    Writeln('Format: nuclidecalcc <tHalf-life> <oldactivity> <olddate> [<newdate>]');
    //Readln(S);
    Exit;
  end;

  // load library from file
  CurLib := TLibrary.Create;
  if (not CurLib.OpenShortTabLib(CurLibPath)) then //    LoadLibraryFromFile(CurLibPath)
  begin
    Writeln('Library ' + CurLibPath + ' is not loaded correct. Default library will be used');
    CurLib.DefaultLibFill();
  end;

  // reading cmd parameters
  Nuclide := ParamStr(1);
  ConvertResult := TryStrToFloat(ParamStr(2), OldAct);
  ConvertResult := ConvertResult and TryStrToDate(ParamStr(3), OldDate);
  if (ParamCount > 3) then
    ConvertResult := ConvertResult and TryStrToDate(ParamStr(4), NewDate)
  else NewDate := Now;
  if (Length(Nuclide) > 0) and (Nuclide[1] >= '0') and (Nuclide[1] <= '9') then // Half-time is entered
  begin
    isNewNuclide := True;
    ConvertResult := ConvertResult and TryStrToFloat(Nuclide, tHalf);
    Nuclide := 'Temp-199';
  end
  else
    isNewNuclide := False;

  if (not ConvertResult) then
  begin
    Writeln('Input data isn''t correct');
    Exit;
  end;

  // calculating new activity
  if (isNewNuclide) then
    CurLib.AddNuclide(Nuclide, tHalf);

  NewAct := CurLib.RecalcActivityToDate(Nuclide, OldAct, OldDate, NewDate);

  // output activity
  Writeln(FloatToStr(NewAct));
  //Readln(S);

  CurLib.Free;
end.
