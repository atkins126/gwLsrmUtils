unit AlphaCalcThreadUnit;

interface

uses
  Classes, Windows, Messages,
  MC_Calc, AlphaDetector;

const
  CALC_END = WM_USER+2;

type
  TAlphaCalcThread = class(TThread)
    FIsParamsSet: Boolean; // установлены ли параметры
    FDetector: TDet;
    FDs,       // размеры источника
    FR,        // расстояние ист - дет
    FRS: Double; // радиальное смещение
    FN: Int64; // число испытаний
    FIsDebug: Boolean;
    FEff, FEffError: Double;
    FMainWnd: HWND;
    FThreadNum: Integer;
  private
    { Private declarations }
  protected
    procedure Execute; override;
    procedure UpdateResults;
  public
    procedure SetParams(const Detector: TDet; Ds, R, RS: Double; N: Int64; MainWnd: HWND; IsDebug: Boolean; ThreadNum: Integer);
    function ReturnResult: Double;
  end;

implementation

uses
  MainUnit;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TAlphaCalcThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TAlphaCalcThread }

procedure TAlphaCalcThread.Execute;
begin
  { Place thread code here }
  if FIsParamsSet then
  begin
    FEff := MCAlpha(FDetector, FDs, FR, FRS, FN, FMainWnd, FThreadNum, FEffError);

    Synchronize(UpdateResults);
  end;
end;

function TAlphaCalcThread.ReturnResult: Double;
begin
  Result := FEff;
end;

procedure TAlphaCalcThread.SetParams(const Detector: TDet; Ds, R, RS: Double; N: Int64; MainWnd: HWND; IsDebug: Boolean; ThreadNum: Integer);
begin
  FDetector := Detector;
  FDs := Ds;
  FR := R;
  FRS := RS;
  FN := N;
  FIsDebug := IsDebug;
  FIsParamsSet := True;
  FMainWnd := MainWnd;
  FThreadNum := ThreadNum;
end;

procedure TAlphaCalcThread.UpdateResults;
begin
  if (FThreadNum = 1) then
  begin
    frmMain.Eff1 := FEff;
    frmMain.Eff1err := FEffError;
  end
  else
  begin
    frmMain.Eff2 := FEff;
    frmMain.Eff2err := FEffError;
  end;
  SendMessage(FMainWnd, CALC_END, FThreadNum, 0);
end;

end.
