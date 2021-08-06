unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin;

type
  TElCom = record
    mdFe, mdC, mdH, mdO, mdN, mdRest: Double;
    Rho: Double;
  end;
  TfrmMain = class(TForm)
    jvspnedtRho: TJvSpinEdit;
    lbl1: TLabel;
    mmoResult: TMemo;
    btnCalc: TButton;
    chkCorrectToNearest: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnCalcClick(Sender: TObject);
  private
    { Private declarations }
    procedure InitConstElCom(); // инициализация начальных элементых составов для чистой сыпучки
    procedure CalculateElcomFromRho(Rho: Double; var ElCom: TElCom);  // расчёт элементного состава для указанного значения плотности
    procedure ShowResults();  // вывод результатов
    procedure Approx2Nearest(Rho: Double; var ElCom: TElCom);  // аппроксимирует элементный состав к ближайщему табличному
  public
    { Public declarations }
  end;

const
  // Константы для аппроксимации линейной зависимости 1/Rho = A*mdFe + B*mdRest
  A = 0.141528;
  B = 1.770678;

var
  frmMain: TfrmMain;
  RestElCom: TElCom; // элементный состав сыпучки без железа
  CurElCom: TElCom; // элементный состав сыпучки с текущей плотностью
  CurRho: Double;   // текущая плотность
  ElComTable: array of TElcom;
  TableRhoArray: array of Double;

implementation

{$R *.dfm}

uses MathLib, gwAlgorithm, Matrix, Interpolation;

{ TfrmMain }

procedure TfrmMain.InitConstElCom;
begin
  // без железа
  RestElCom.mdFe:=0;
  RestElCom.mdC:=0.721;
  RestElCom.mdH:=0.076;
  RestElCom.mdO:=0.173;
  RestElCom.mdN:=0.03;
  // табличные с заданными плотностями
  SetLength(ElComTable,7);                                               
  ElComTable[0].Rho:=0.6;
  ElComTable[0].mdFe:=0.151;
  ElComTable[0].mdC:=0.612;
  ElComTable[0].mdH:=0.064;
  ElComTable[0].mdO:=0.151;
  ElComTable[0].mdN:=0.021;
  ElComTable[1].Rho:=1;
  ElComTable[1].mdFe:=0.459;
  ElComTable[1].mdC:=0.390;
  ElComTable[1].mdH:=0.041;
  ElComTable[1].mdO:=0.094;
  ElComTable[1].mdN:=0.016;
  ElComTable[2].Rho:=1.2;
  ElComTable[2].mdFe:=0.5717;
  ElComTable[2].mdC:=0.309;
  ElComTable[2].mdH:=0.0323;
  ElComTable[2].mdO:=0.075;
  ElComTable[2].mdN:=0.012;
  ElComTable[3].Rho:=1.3;
  ElComTable[3].mdFe:=0.6155;
  ElComTable[3].mdC:=0.277;
  ElComTable[3].mdH:=0.0295;
  ElComTable[3].mdO:=0.067;
  ElComTable[3].mdN:=0.011;
  ElComTable[4].Rho:=1.4;
  ElComTable[4].mdFe:=0.663;
  ElComTable[4].mdC:=0.242;
  ElComTable[4].mdH:=0.026;
  ElComTable[4].mdO:=0.059;
  ElComTable[4].mdN:=0.010;
  ElComTable[5].Rho:=1.5;
  ElComTable[5].mdFe:=0.685;
  ElComTable[5].mdC:=0.227;
  ElComTable[5].mdH:=0.024;
  ElComTable[5].mdO:=0.055;
  ElComTable[5].mdN:=0.010;
  ElComTable[6].Rho:=2.5;
  ElComTable[6].mdFe:=0.872;
  ElComTable[6].mdC:=0.090;
  ElComTable[6].mdH:=0.010;
  ElComTable[6].mdO:=0.022;
  ElComTable[6].mdN:=0.005;
  SetLength(TableRhoArray,7);
  TableRhoArray[0]:=0.6;
  TableRhoArray[1]:=1;
  TableRhoArray[2]:=1.2;
  TableRhoArray[3]:=1.3;
  TableRhoArray[4]:=1.4;
  TableRhoArray[5]:=1.5;
  TableRhoArray[6]:=2.5;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitConstElCom();
end;

procedure TfrmMain.CalculateElcomFromRho(Rho: Double; var ElCom: TElCom);
begin
  ElCom.mdFe:=(1/Rho - B)/(A - B);
  ElCom.mdRest:=1-ElCom.mdFe;
  ElCom.mdC:=RestElCom.mdC*ElCom.mdRest;
  ElCom.mdH:=RestElCom.mdH*ElCom.mdRest;
  ElCom.mdO:=RestElCom.mdO*ElCom.mdRest;
  ElCom.mdN:=RestElCom.mdN*ElCom.mdRest;
end;

procedure TfrmMain.btnCalcClick(Sender: TObject);
begin
  CurRho:=jvspnedtRho.Value;
  if ((CurRho<0.599) or (CurRho>2.501)) then
  begin
    ShowMessage('Wrong rho');
    Exit;
  end;
  if (chkCorrectToNearest.Checked) then Approx2Nearest(CurRho,CurElCom) else  CalculateElcomFromRho(CurRho,CurElCom);
  ShowResults();
end;

procedure TfrmMain.ShowResults;
var S: string;
begin
  mmoResult.Clear;
  s:='H'+#09+'C'+#09+'N'+#09+'O'+#09+'Fe';
  mmoResult.Lines.Add(s);
  s:=FloatToStrF(CurElCom.mdH*100,ffFixed,3,1)+#09+FloatToStrF(CurElCom.mdC*100,ffFixed,3,1)
     +#09+FloatToStrF(CurElCom.mdN*100,ffFixed,3,1)+#09+FloatToStrF(CurElCom.mdO*100,ffFixed,3,1)
     +#09+FloatToStrF(CurElCom.mdFe*100,ffFixed,3,1);
  mmoResult.Lines.Add(s);
end;

procedure TfrmMain.Approx2Nearest(Rho: Double; var ElCom: TElCom);
var j,i: Integer;
    TempElCom: TElCom;
    Xi,Yi: TVector;
begin
  i:=gwAlgorithm.low_index(TVector(TableRhoArray),Rho);
  SetLength(Xi,2);
  SetLength(Yi,2);
  for j:=0 to 1 do
  begin
    Xi[j]:=1/TableRhoArray[i+j];
    Yi[j]:= ElComTable[i+j].mdFe;
  end;  
  InterPol(1,Xi,Yi,1/Rho,ElCom.mdFe);
  ElCom.mdRest:=1-ElCom.mdFe;
  ElCom.mdC:=RestElCom.mdC*ElCom.mdRest;
  ElCom.mdH:=RestElCom.mdH*ElCom.mdRest;
  ElCom.mdO:=RestElCom.mdO*ElCom.mdRest;
  ElCom.mdN:=RestElCom.mdN*ElCom.mdRest;
end;

end.
