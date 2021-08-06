unit OrtecMCA;


interface

uses Classes, UMCBILib_TLB;

type
  TOrtecMCA = class     // класс для анализатора Ортек
    FConn: TUCONN2;     // connect
    FBuffSize: Word;
    FBuffer: array of Integer;
  private
    FIsHVOn, FIsHVOverload, FIsHVPos: Boolean;
    FHV, FHVTarget, FHVCurrent:  Integer;
    FVacuum, FVacuumTarg: Integer; // vacuum values for alpha
    FLT, FRT: Integer;  // live and real times
    IsAlpha: Boolean;
    procedure getSuppFunctions();
  public
    constructor Create(const ParamStr: string);
    destructor Destroy(); override;
    procedure Run();
    procedure Stop();
    procedure Clear();
    function getBuffSize(): Word;
    function getLT(): Integer;
    procedure getData();
    procedure getState();
    (*High voltage*)
    function getHVState(): Boolean;   // получение состояния для высокого
    function getActualHV(): Boolean;  // получения текущего значения высокого
    function getTargetHV(): Boolean;  // получения текущего значения высокого
    function getHVCurrent(): Boolean; // получения текущего значения высокого
    procedure getHVShutdownList(var List: TStringList); // получения списка способов сброса высокого
    procedure setHVState(EnableHV: Boolean);         // установка состояния выского
    function getHVSHutdownMethod(): string; // получение метода снятия высокоо
    (*vacuum*)
    procedure getVacuumModeList(var List: TStringList);
    function getVacuumMode(out modeNum: Integer): string; // возвращает режим вакуумирования либо номер, либо строку
    function getVacuum(): Boolean; // получение текущего вакуума
    function getVacuumTarget(): Boolean; // получение уставки вакуума
    procedure setVacuumMode(Vac_Mode: Integer);
    procedure setVacuumTarget(Pressure: Integer); // получение уставки вакуума
    (*properties*)
    property isHVOn: Boolean read FIsHVOn;
    property isHVOverload: Boolean read FIsHVOverload;
    property isHVPos: Boolean read FIsHVPos;
    property HV: Integer read FHV;
    property HVTarget: Integer read FHVTarget;
    property HVCurrent: Integer read FHVCurrent;
    property vacuum: Integer read FVacuum;
    property vacuumTarg: Integer read FVacuumTarg;
    property LT: Integer read FLT;
    property RT: Integer read FRT;
  end;

  
implementation

uses SysUtils, JvJCLUtils, MCBAPI32;

{ TOrtecMCA }

procedure TOrtecMCA.Clear;
begin
  FConn.Comm('CLEAR');
  FLT := 0;
  FRT := 0;
end;

constructor TOrtecMCA.Create(const ParamStr: string);
var
  S: string;
  P: Integer;
begin
  // get connection string
  P:= Pos('|', ParamStr);
  if (P > 0) then
    S:= Copy(ParamStr, P + 1, Length(ParamStr) - P + 1)
  else
    raise EInvalidOperation.Create('Invalid description');

  // create connection  
  FConn:= TUCONN2.Create(Nil);
  FConn.Address:= S;
  try
    FConn.Open();
    GetSuppFunctions();
    FBuffSize:= GetBuffSize();
    SetLength(FBuffer, FBuffSize);
  except
    raise EInvalidOperation.Create('Error connecting to MCA');
  end;
end;

destructor TOrtecMCA.Destroy;
begin
  FConn.Free();
  FBuffer := nil;
  inherited;
end;

function TOrtecMCA.getActualHV: Boolean;
var S, SH, SE: string; // S - общая строка, SH - 1-я часть строки, SL - 2-я часть строки, SE - 1-e 2 символа
begin
  S := FConn.Comm('SHOW_HV_ACTUAL');
  SE := Copy(S, 1, 2);
  if (SE <> '$D') then
  begin
    Result := False;
    Exit;
  end;
  SH := Copy(S, 3, 5);
  //SL:=Copy(S,8,5);
  FHV := StrToIntDef(SH, 0);
  Result := True;
end;

function TOrtecMCA.getBuffSize: Word;
var s: string;
begin
  // возвращает число каналов в виде $Cxxxxxccc<CR>, где xxxxx - число каналов, а c - контрольная сумма
  s := FConn.Comm('SHOW_GAIN_CONVERSION');
  s := Copy(s, 3, 5);
  FBuffSize := StrToIntDef(s, 0);
  Result := FBuffSize;
end;

procedure TOrtecMCA.getData;
begin
  FBuffer := FConn.GetData(0, FBuffSize);
end;

function TOrtecMCA.getHVState: Boolean;
var S, SL, SE: string; // S - общая строка, SH - 1-я часть строки, SL - 2-я часть строки, SE - 1-e 2 символа
    HVState: Integer;
begin
  Result := False;
  //$Dvvvvvsssssccc, где vvvvv – высокое в вольтах, sssss – состояние двоичное переведено в 10-чное
  S := FConn.Comm('SHOW_HV');
  SE := Copy(S, 1, 2);
  if (SE <> '$D') then
    Exit;

  //SH:=Copy(S,3,5);
  SL := Copy(S, 8, 5);
  HVState := StrToIntDef(SL, 0);
  FIsHVPos := ((HVState mod 2) = 0);
  FIsHVOn := (HVState > 3);
  FIsHVOverload := (HVState and $02) <> $02; //(HVState = 0) or (HVState = 1) or (HVState = 4) or (HVState = 5);
  Result := True;
end;

function TOrtecMCA.getHVCurrent: Boolean;
var S, SH, SE: string; // S - общая строка, SH - 1-я часть строки, SL - 2-я часть строки, SE - 1-e 2 символа
begin
  if (IsAlpha) then
  begin
    Result := False;
    S := FConn.Comm('SHOW_HV_CURRENT');
    SE := Copy(S, 1, 2);
    if (SE <> '$C') then
      Exit;

    SH := Copy(S, 3, 5);
    FHVCurrent := StrToIntDef(SH, 0);
    Result := True;
  end
  else
  begin
    FHVCurrent := 0;
    Result := False;
  end;
end;

function TOrtecMCA.getLT: Integer;
var S: string;
begin
  // $Gxxxxxxxxxxccc, x - LT в 20ms, x - 32bit Unsigned number (LongWord)
  S := FConn.Comm('SHOW_LIVE');
  S := Copy(S, 3, 10);
  FLT := StrToIntDef(S, 0);
  FLT := Round(FLT * 20);
  Result := FLT;
end;

procedure TOrtecMCA.getState;
begin

end;

function TOrtecMCA.getTargetHV: Boolean;
var S, SH, SE: string; // S - общая строка, SH - 1-я часть строки, SL - 2-я часть строки, SE - 1-e 2 символа
begin
  Result := False;
  S := FConn.Comm('SHOW_HV_TARGET');
  SE := Copy(S, 1, 2);
  if (SE <> '$D') then
    Exit;

  SH := Copy(S, 3, 5);
  //SL:=Copy(S,8,5);
  FHVTarget := StrToIntDef(SH, 0);
  Result := True;
end;

function TOrtecMCA.getVacuum: Boolean;
var S, SH, SE: string; // S - общая строка, SH - 1-я часть строки, SL - 2-я часть строки, SE - 1-e 2 символа
begin
  S := FConn.Comm('SHOW_VACUUM');
  SE := Copy(S, 1, 2);
  if SE <> '$C' then
  begin
    Result := False;
    Exit;
  end;
  SH := Copy(S, 3, 5);
  FVacuum := StrToInt(SH);
  Result := True;
end;

function TOrtecMCA.getVacuumMode(out modeNum: Integer): string;
var S, SH: string; // S - общая строка, SH - 1-я часть строки, SL - 2-я часть строки, SE - 1-e 2 символа
begin
  modeNum := -1;
  Result := '';
  // can be string or number after string VACU_MODE
  S := FConn.Comm('SHOW_VACUUM_MODE');
  SH := ExtractWord(2, S, [' ']);
  if SH[1] = '0' then // режим может храниться как в виде слова, так и ввиде номера
    modeNum := StrToIntDef(SH[Length(SH)-1], -1)
  else
    Result := SH;
end;

function TOrtecMCA.getVacuumTarget: Boolean;
var S, SH, SE: string; // S - общая строка, SH - 1-я часть строки, SL - 2-я часть строки, SE - 1-e 2 символа
begin
  S := FConn.Comm('SHOW_VACUUM_TARG');
  SE := Copy(S, 1, 2);
  if SE <> '$C' then
  begin
    Result := False;
    Exit;
  end;
  SH := Copy(S, 3, 5);
  FVacuumTarg := StrToInt(SH);
  Result := True;
end;

procedure TOrtecMCA.Run;
begin
  FConn.Comm('START');
end;

procedure TOrtecMCA.setVacuumTarget(Pressure: Integer);
const MAX_PRESSURE = 19990;
      MIN_PRESSURE = 10;
var S: string; // S - общая строка, SH - 1-я часть строки, SL - 2-я часть строки, SE - 1-e 2 символа
begin
  if (Pressure < MIN_PRESSURE) then
    Pressure := MIN_PRESSURE
  else if (Pressure > MAX_PRESSURE) then
    Pressure := MAX_PRESSURE;
  S := FConn.Comm(Format('SET_VACUUM_TARG %d', [Pressure]));
end;

procedure TOrtecMCA.Stop;
begin
  FConn.Comm('STOP');
end;

procedure TOrtecMCA.setHVState(EnableHV: Boolean);
begin
  if (EnableHV) then
    FConn.Comm('ENABLE_HV')
  else
    FConn.Comm('DISABLE_HV');
end;

procedure TOrtecMCA.setVacuumMode(Vac_Mode: Integer);
begin
  FConn.Comm('SET_VACUUM_MODE ' + IntToStr(Vac_Mode));
end;

procedure TOrtecMCA.getVacuumModeList(var List: TStringList);
var S, SL: string;
    i, N: Integer;
begin
  List.Clear;
  S := FConn.Comm('LIST_VACUUM_MODE');   // формат S -- "VACU_MODE Vent Pump Hold"
  if (S <> '') then
    Delete(S, Length(S), 1); // удаление конца строки $A
  N := WordCount(S, [' ']); // число режимов + 1
  if (N <= 1) then
    Exit;
    
  for i := 2 to N do
  begin
    SL := ExtractWord(i, S, [' ']);
    List.Add(SL);
  end;
end;

procedure TOrtecMCA.getSuppFunctions;
begin
  IsAlpha := FConn.IsFeature[MIOFEAT_ALPHAHV];
  IsAlpha := IsAlpha or FConn.IsFeature[MIOFEAT_VACUUM];
end;

procedure TOrtecMCA.getHVShutdownList(var List: TStringList);
var S, SL: string;
    i, N: Integer;
begin
  List.Clear;
  S := FConn.Comm('LIST_SHUT');
  N := WordCount(S, [' ']);
  if (N <= 1) then
    Exit;
    
  for i := 2 to N do
  begin
    SL := ExtractWord(i, S, [' ']);
    List.Add(SL);
  end;
end;

function TOrtecMCA.getHVSHutdownMethod: string;
var S, SE, SH: string;
    ParamLength: Integer;
begin
  // $FORT or $F1ORT – ORTEC, $FTTL or $F0TTL – TTL, $FSM1 or $F2SM1 - SMART-1
  S := FConn.Comm('SHOW_SHUTDOWN');
  // check header
  SE := Copy(S, 1, 2);
  if (SE <> '$F') then
    Exit;
  // cut header and end: 2 вначале и конец строки в конце
  ParamLength := Length(S) - 3;
  SH := Copy(S, 3, ParamLength);
  // return result string
  if (SH = 'ORT') or (SH = '1ORT') then
    Result := 'ORTEC'
  else if (SH = 'TTL') and (SH = '0TTL') then
    Result := 'TTL'
  else if (SH = 'SM1') and (SH = '2SM1') then
    Result := 'SMART';
end;

end.
