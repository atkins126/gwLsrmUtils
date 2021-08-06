unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Math, ComCtrls, IniFiles, Grids,
  JvExStdCtrls, JvEdit, JvValidateEdit, 
  SourcePosUnit, UCoordinates, Matrix;

const
  DET_NUM = 3;
  INI_NAME = 'SourceSearch.ini';

type
  TfrmMain = class(TForm)
    btnTestCalc: TButton;
    pbSource: TPaintBox;
    mmoOutput: TMemo;
    stat1: TStatusBar;
    btnCalc: TButton;
    strngrdDet: TStringGrid;
    procedure btnTestCalcClick(Sender: TObject);
    procedure pbSourceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnCalcClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pbSourcePaint(Sender: TObject);
  private
    procedure ReadParams();   // read coord and cr from stringgrid
    procedure OutputShape(CurShape: TCircleLine);
    procedure OutputCircle(CurCircle: TCircle);
    procedure OutputLine(CurLine: TLine);
    procedure DrawDetsGrid();
    procedure DrawLine(CurLine: TLine);
    function ConvertCS2pixels(CurPoint: TFPoint): TPoint;
    function ConvertCircleCS2pixels(center: TFPoint; Radius: Double): TRect;
    function ConvertPixels2CS(X,Y: Integer): TFPoint;
    procedure SetGraphScale();
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  appPath: string;
  DetCount: Integer; // число детекторов
  Dets: TCoordVector; // положение детекторов
  Counts, dCounts: TDoubleVector; // count rate and dCR
  Sh_Num: Integer;  // число ГМТ
  Shapes: array of TCircleLine; // ГМТ
  Source1: TFPoint;
  MaxX, MinX, MaxY, MinY: Double;
  KX, KY: Double;   // коэффциенты перехода из СК к пикселям
  IsCalced: Boolean = False;
  IsResCalced: Boolean = False;
  CurColors: array[0..5] of TColor;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var IniFile: TIniFile;
    i: Integer;
begin
  if (DecimalSeparator <> '.') then
    DecimalSeparator := '.';
  appPath := ExtractFilePath(Application.ExeName);

  // set stringgrid column names
  for i := 1 to 10 do
    strngrdDet.Cells[0, i] := 'D' + IntToStr(i);
  strngrdDet.Cells[1, 0] := 'x';
  strngrdDet.Cells[2, 0] := 'y';
  strngrdDet.Cells[3, 0] := 'n';
  strngrdDet.Cells[4, 0] := 'dn';

  // reading last coordinates and CR
  IniFile := TIniFile.Create(appPath + INI_NAME);
  try
    for i := 1 to strngrdDet.RowCount - 1 do
    begin
      strngrdDet.Cells[1,i] := FloatToStr(IniFile.ReadFloat('DET_POSITION', 'Det' + IntToStr(i) + 'x', 0));
      strngrdDet.Cells[2,i] := FloatToStr(IniFile.ReadFloat('DET_POSITION', 'Det' + IntToStr(i) + 'y', 0));
      strngrdDet.Cells[3,i] := FloatToStr(IniFile.ReadFloat('DET_COUNT_RATE', 'n' + IntToStr(i), 0));
      strngrdDet.Cells[4,i] := FloatToStr(IniFile.ReadFloat('DET_COUNT_RATE', 'dn' + IntToStr(i), 0));
    end;
  finally
    IniFile.Free;
  end;
  Source1.x := 0;
  Source1.y := 0;
  Dets := TCoordVector.Create();
  Counts := TDoubleVector.Create();
  dCounts := TDoubleVector.Create();
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var IniFile: TIniFile;
    i: Integer;
begin
  IniFile := TIniFile.Create(appPath + INI_NAME);
  try
    // writing coordinates and CR
    for i := 1 to strngrdDet.RowCount - 1 do
    begin
      IniFile.WriteFloat('DET_POSITION', 'Det' + IntToStr(i) + 'x', StrToFloat(strngrdDet.Cells[1, i]));
      IniFile.WriteFloat('DET_POSITION', 'Det' + IntToStr(i) + 'y', StrToFloat(strngrdDet.Cells[2, i]));
      IniFile.WriteFloat('DET_COUNT_RATE', 'n' + IntToStr(i), StrToFloat(strngrdDet.Cells[3, i]));
      IniFile.WriteFloat('DET_COUNT_RATE', 'dn' + IntToStr(i), StrToFloat(strngrdDet.Cells[4, i]));
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TfrmMain.btnTestCalcClick(Sender: TObject);
var i, j, k: Integer;
begin
  ReadParams();

  // получение ГМТ
  Sh_Num := Trunc(DetCount * (DetCount - 1) / 2);
  k := 0;
  SetLength(Shapes, Sh_Num);
  for i := 0 to DetCount - 2 do
    for j := i + 1 to DetCount - 1 do
    begin
      Shapes[k] := GMTfrom2measure(Dets[i], Dets[j], Counts[i], Counts[j]);
      Inc(k);
    end;

  // вывод их в mmo  
  for i := 0 to Sh_Num - 1 do
    OutputShape(Shapes[i]);

  // отрисовка ГМТ
  DrawDetsGrid();
  IsCalced := True;
end;

procedure TfrmMain.btnCalcClick(Sender: TObject);
var SourceCoord, SourceError: TCoordVector;
    ErrorCode: Integer;
    TempRect: TRect;
    i, SourceNum: Integer;
begin
  ReadParams();

  ErrorCode := SearchSource(Dets, 0.01, Counts, SourceNum, SourceCoord, SourceError);

  // show calc results
  if ErrorCode = 0 then
  begin
    for i := 0 to SourceNum - 1 do
    begin
      mmoOutput.Lines.Add(Format('x=%3.2f, y=%3.2f', [SourceCoord[i].x, SourceCoord[i].y]));
      if IsCalced then
      begin
        pbSource.Canvas.Pen.Color := clGreen;
        pbSource.Canvas.Brush.Color := clGreen;
        TempRect := ConvertCircleCS2pixels(SourceCoord[i], Max(SourceError[i].x, SourceError[i].y) + 5);
        pbSource.Canvas.Ellipse(TempRect);
      end;
    end;
    IsResCalced := True;
  end
  else
    ShowMessage('Error # ' + IntToStr(ErrorCode));
end;

function TfrmMain.ConvertCS2pixels(CurPoint: TFPoint): TPoint;
begin
  Result.X := Round(KX * (CurPoint.x - MinX));
  Result.Y := Round(KY * (MaxY - CurPoint.y));
end;

function TfrmMain.ConvertCircleCS2pixels(center: TFPoint; Radius: Double): TRect;
begin
  Result.Left := Round(KX * (center.X - Radius - MinX));
  Result.Right := Round(KX * (center.X + Radius - MinX));
  Result.Top := Round(KY * (MaxY - (center.Y + Radius)));
  Result.Bottom := Round(KY * (MaxY - (center.Y - Radius)));
end;

procedure TfrmMain.DrawDetsGrid;
var TempRect: TRect;
    CurPoint: TFPoint;
    CurPixel: TPoint;
    i: Integer;
begin
  CurColors[0] := clRed;
  CurColors[1] := clYellow;
  CurColors[2] := clGreen;
  CurColors[3] := clOlive;
  CurColors[4] := clBlue;
  CurColors[5] := clNavy;

  SetGraphScale;
  pbSource.Canvas.Brush.Style := bsSolid;
  pbSource.Canvas.Brush.Color := clBtnFace;
  pbSource.Canvas.FillRect(pbSource.ClientRect);

  // детекторы
  pbSource.Canvas.Pen.Color := clBlack;
  pbSource.Canvas.Brush.Color := clBlack;
  for i := 0 to DetCount - 1 do
  begin
    TempRect := ConvertCircleCS2pixels(Dets[i], 5);
    pbSource.Canvas.FillRect(TempRect);
  end;
  
  // оси - пока не нужно

  // источник
  pbSource.Canvas.Pen.Color := clRed;
  pbSource.Canvas.Brush.Color := clRed;
  TempRect := ConvertCircleCS2pixels(Source1, 5);
  pbSource.Canvas.Ellipse(TempRect);

  // окружности/прямые
  for i := 0 to Sh_Num - 1 do
  begin
    pbSource.Canvas.Pen.Color := CurColors[i mod 6];
    pbSource.Canvas.Brush.Style := bsClear;
    if Shapes[i].IsCircle then
    begin
      TempRect := ConvertCircleCS2pixels(Shapes[i].Circle.Center, Shapes[i].Circle.R);
      pbSource.Canvas.Ellipse(TempRect);
    end
    else
      DrawLine(Shapes[i].Line);
  end;
end;

procedure TfrmMain.OutputCircle(CurCircle: TCircle);
var S: string;
begin
  S := Format('Circle: center (%3.1f,%3.1f), radius,cm= %3.1f',
    [CurCircle.Center.x, CurCircle.Center.y, CurCircle.R]);
  mmoOutput.Lines.Add(S);
end;

procedure TfrmMain.OutputLine(CurLine: TLine);
var S: string;
begin
  S:=Format('Line (ax+by=c): a=%f, b=%f, c=%f',[CurLine.a,CurLine.b,CurLine.c]);
  mmoOutput.Lines.Add(S);
end;

procedure TfrmMain.ReadParams;
var i: Integer;
begin
  for i := 1 to strngrdDet.RowCount - 1 do
    if (strngrdDet.Cells[1, i] = '') or (strngrdDet.Cells[2, i] = '') or
      (strngrdDet.Cells[3, i] = '') or (strngrdDet.Cells[1, i] = '0') or
      (strngrdDet.Cells[2, i] = '0') or (strngrdDet.Cells[3, i] = '0') then
    begin
      DetCount := i - 1;
      Break;
    end;

  if (DetCount >= 3) then
  begin
    Dets.capacity := DetCount;
    Counts.capacity := DetCount;
    Dets.clear();
    Counts.clear();
    for i := 0 to DetCount - 1 do
    begin
      Dets.push_back(StrToFloat(strngrdDet.Cells[1, i + 1]),
                     StrToFloat(strngrdDet.Cells[2, i + 1]));
      Counts.push_back(StrToFloat(strngrdDet.Cells[3, i + 1]));
    end;
  end;
end;

procedure TfrmMain.pbSourceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var CurPnt: TFPoint;
begin
  if IsCalced then
  begin
    CurPnt := ConvertPixels2CS(X, Y);
    stat1.Panels[0].Text := FormatFloat('0.0', CurPnt.x);
    stat1.Panels[1].Text := FormatFloat('0.0', CurPnt.y);
  end;
end;

function TfrmMain.ConvertPixels2CS(X, Y: Integer): TFPoint;
begin
  Result.x := X / KX + MinX;
  Result.y := MaxY - Y / KY;
end;

procedure TfrmMain.SetGraphScale;
begin
  {MaxX:=Max(Det1.x, Max(Det2.x,Max(Det3.x,Source1.x)));
  MaxY:=Max(Det1.y, Max(Det2.y,Max(Det3.y,Source1.y)));
  MinX:=Min(Det1.x, Min(Det2.x,Min(Det3.x,Source1.x)));
  MinY:=Min(Det1.y, Min(Det2.y,Min(Det3.y,Source1.y)));}
  MaxX := 400;
  MinX := -400;
  MaxY := 400;
  MinY := -400;
  if MaxX > MinX then
    KX := pbSource.Width / (MaxX - MinX)
  else
    KX := 1;
  if MaxY > MinY then
    KY := pbSource.Height / (MaxY - MinY)
  else
    KY := 1;
end;

procedure TfrmMain.OutputShape(CurShape: TCircleLine);
begin
  if CurShape.IsCircle then
    OutputCircle(CurShape.Circle)
  else
    OutputLine(CurShape.Line);
end;

procedure TfrmMain.pbSourcePaint(Sender: TObject);
begin
  if IsCalced then  DrawDetsGrid();
end;


procedure TfrmMain.DrawLine(CurLine: TLine);
var CurPixel: TPoint;
    CurPoint: TFPoint;
begin
  if (CurLine.B <> 0) then
  begin
    CurPoint.x := MinX;
    CurPoint.y := -CurLine.a / CurLine.b * CurPoint.x - CurLine.c / CurLine.b;
  end
  else
  begin
    CurPoint.y := MinY;
    CurPoint.x := CurLine.c / CurLine.a;
  end;
  CurPixel := ConvertCS2pixels(CurPoint);
  pbSource.Canvas.MoveTo(CurPixel.X, CurPixel.Y);
  if (CurLine.B<>0) then
  begin
    CurPoint.x := MaxX;
    CurPoint.y := -CurLine.a / CurLine.b * CurPoint.x - CurLine.c / CurLine.b;
  end
  else
  begin
    CurPoint.y := MaxY;
    CurPoint.x := CurLine.c / CurLine.a;
  end;
  CurPixel := ConvertCS2pixels(CurPoint);
  pbSource.Canvas.LineTo(CurPixel.X, CurPixel.Y);
end;

end.
