unit UGammaConst;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids,
  JvExStdCtrls, JvEdit, JvValidateEdit,
  LibUnit;

type
  TfrmGammaConst = class(TForm)
    rg1: TRadioGroup;
    lbl1: TLabel;
    jvedtThresh: TJvValidateEdit;
    strngrdGamma: TStringGrid;
    lbl2: TLabel;
    btnCalc: TButton;
    grp1: TGroupBox;
    jvedtDist: TJvValidateEdit;
    jvedtDose: TJvValidateEdit;
    lblDose: TLabel;
    lbl3: TLabel;
    jvedtAct: TJvValidateEdit;
    lblDoseUnits: TLabel;
    procedure FormShow(Sender: TObject);
    procedure GrmCreate(Sender: TObject);
    procedure rg1Click(Sender: TObject);
    procedure jvedtThreshExit(Sender: TObject);
    procedure btnCalcClick(Sender: TObject);
  private
    { Private declarations }
    procedure FillStrnGrid(); // заполнение таблицы энерги€/гамма-константа
  public
    { Public declarations }
  end;

var
  frmGammaConst: TfrmGammaConst;
  curNuclide: TNuclide;

implementation

{$R *.dfm}

uses NMGammaConstFunction;

procedure TfrmGammaConst.FillStrnGrid();
var i: Integer;
    GSumm, enThreshold: Double;
begin
  GSumm := 0;
  enThreshold := jvedtThresh.Value;
  for i:=0 to curNuclide.LinesCount-1 do
  begin
    strngrdGamma.Cells[0,i+1] := FloatToStr(curNuclide.Lines[i].Energy);
    curNuclide.Lines[i].GammaConst := GammaConstLine(curNuclide.Lines[i], TDoseUnits(rg1.ItemIndex));
    strngrdGamma.Cells[1,i+1] := FloatToStrF(curNuclide.Lines[i].GammaConst, ffGeneral, 4, 3);
    if (curNuclide.Lines[i].Energy >= enThreshold) then GSumm := GSumm + curNuclide.Lines[i].GammaConst;
  end;
  strngrdGamma.Cells[0,curNuclide.LinesCount+1] := 'Summ';
  strngrdGamma.Cells[1,curNuclide.LinesCount+1] := FloatToStrF(GSumm, ffGeneral, 4, 3);
end;

procedure TfrmGammaConst.FormShow(Sender: TObject);
begin
  strngrdGamma.RowCount:= curNuclide.LinesCount + 2;
  FillStrnGrid();
end;

procedure TfrmGammaConst.GrmCreate(Sender: TObject);
begin
  strngrdGamma.Cells[0,0] := 'Energy, keV';
  strngrdGamma.Cells[1,0] := 'GammaConst';
end;

procedure TfrmGammaConst.rg1Click(Sender: TObject);
begin
  FillStrnGrid();
end;

procedure TfrmGammaConst.jvedtThreshExit(Sender: TObject);
begin
  FillStrnGrid();
end;

procedure TfrmGammaConst.btnCalcClick(Sender: TObject);
var i: Integer;
    dist, threshold, act, dose: Double;
begin
  jvedtDose.Value := 0;
  threshold := jvedtThresh.Value;
  dist := jvedtDist.Value;    // рассто€ние в см
  if (dist <= 0) then Exit;
  if (rg1.ItemIndex > 0) then begin
    dist := dist/100;         // рассто€ние в м
    act := jvedtAct.Value     // активность в Ѕк
  end else
    act := jvedtAct.Value / 3.7e7; // активность в м и
  dose := 0;
  for i:=0 to curNuclide.LinesCount-1 do
  begin
    if (curNuclide.Lines[i].Energy >= threshold) then
      dose := dose + curNuclide.Lines[i].GammaConst * act / Sqr(dist);
  end;
  case rg1.ItemIndex of
  0: begin
      dose := dose * 1000;
      lblDoseUnits.Caption := 'м–/ч';
     end;
  1: begin
      dose := dose * 1e-12 * 3600;
      lblDoseUnits.Caption := 'мк√р/ч';
     end;
  2: begin
      dose := dose * 1e-12 * 3600;
      if (dose <= 1000) then
        lblDoseUnits.Caption := 'мк«в/ч'
      else if (dose <= 1e6) then
      begin
        dose := dose / 1000;
        lblDoseUnits.Caption := 'м«в/ч'
      end
      else
      begin
        dose := dose / 1e6;
        lblDoseUnits.Caption := '«в/ч'
      end;
     end;
  end;
  jvedtDose.Value := dose;
end;

end.
