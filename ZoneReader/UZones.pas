unit UZones;

interface

uses Classes, RusClipboard;

type
  TVector = array of Double;

  TZone = record
    Counter,              // номер зоны
    BeginCh,              // начальный канал
    EndCh,                // последний канал
    PeaksCount: Integer;  // число пиков в зоне
    Area,                 // сумма отсчётов в зоне
    dArea,                // погрешность суммы отсчётов
    CR: Double;           // скорость счёта в зоне
    Nuclide: string;      // имя нуклида
  end;

  TZoneList = class
  private
    FZonesCount: Integer; // zones count
    FHead,             // SpectrName
    FConfName: string; // Config file name
    FLT, FRT: Double;   // Live n Real times
    FLoadResult: Boolean;
    function internalLoadZones(fileList: TStringList): Boolean;
    procedure setSize(aSize: Integer);
  public
    zones: array of TZone;
    constructor loadFromFile(const filename: string);
    constructor loadFromClipboard(aClipBoard: TRusClipBoard);
    constructor loadFromList(aStrList: TStringList);
    property zonesCount: Integer read FZonesCount write setSize;
    property head: string read FHead;
    property confName: string read FConfName;
    property LT: Double read FLT;
    property RT: Double read FRT;
    property loadResult: Boolean read FLoadResult;
  end;

function Zone2Str(Zone: TZone): string;

implementation

uses SysUtils, Windows, JvJCLUtils;

function Zone2Str(Zone: TZone): string;
begin
  Result := IntToStr(Zone.Counter) + '  ' + IntToStr(Zone.BeginCh) + '  ' +
    IntToStr(Zone.EndCh) + '  ' + IntToStr(Zone.PeaksCount) + '  ' + FloatToStr(Zone.Area) +
    '  ' + FloatToStr(Zone.dArea) + '  ' + FloatToStr(Zone.CR) + '  ' + Zone.Nuclide;
end;

{ TZoneList }

function TZoneList.internalLoadZones(fileList: TStringList): Boolean;
var S1, S2: string;
      i, j, ZoneBeg: Integer;
begin
  Result := False;
  {check that is .zon-file}
  if (fileList.Count < 6) then Exit; // there are no zones
  // check live time
  S1 := fileList.Strings[2];
  if WordCount(S1, ['=']) < 2 then Exit;
  // check real time
  S2 := fileList.Strings[3];
  if WordCount(S2, ['=']) < 2 then Exit;

  {reading zones}
  // reading header
  FHead := fileList.Strings[0];
  FConfName := fileList.Strings[1];
  S1 := ExtractWord(2, fileList.Strings[2], ['=']);
  S2 := ExtractWord(2, fileList.Strings[3], ['=']);
  try
    FLT := StrToFloat(S1);
    FRT := StrToFloat(S2);
  except
    Result := False;
    Exit;
  end;
  S1 := ExtractWord(2, fileList.Strings[4], [':']);
  S1 := Trim(S1);
  FZonesCount := StrToInt(S1);
  setSize(FZonesCount);
  if (FZonesCount < 0) then Exit;

  // reading zones
  ZoneBeg := -1;
  for i:=0 to fileList.Count-1 do
  begin
    S1 := fileList.Strings[i];
    if (S1[1]='#') and (S1[2]='#') then
    begin
      ZoneBeg := i;
      Break;
    end;
  end;

  j := 0;
  if (ZoneBeg + 1 < fileList.Count) then
    for i:=ZoneBeg+1 to fileList.Count-1 do
    begin
      zones[j].Counter := StrToInt(ExtractWord(1, fileList.Strings[i], [#09]));
      zones[j].BeginCh := StrToInt(ExtractWord(2, fileList.Strings[i], [#09]));
      zones[j].EndCh := StrToInt(ExtractWord(3, fileList.Strings[i], [#09]));
      zones[j].PeaksCount := StrToInt(ExtractWord(4, fileList.Strings[i], [#09]));
      zones[j].Area := StrToFloat(ExtractWord(5, fileList.Strings[i], [#09]));
      zones[j].dArea := StrToFloat(ExtractWord(6, fileList.Strings[i], [#09]));
      zones[j].CR := StrToFloat(ExtractWord(7, fileList.Strings[i], [#09]));
      Inc(j);
    end;
  Result := True;
end;

constructor TZoneList.loadFromClipboard(aClipBoard: TRusClipBoard);
var BuffList: TStringList;
begin
  if aClipBoard.HasFormat(CF_TEXT)then
  begin
    BuffList := TStringList.Create;
    try
      aClipBoard.Open;
      BuffList.Text := (aClipBoard.AsText);
      aClipBoard.Close;
      FLoadResult := internalLoadZones(BuffList);
      //BuffList.SaveToFile(AppPath + 'temp.txt');
    finally
      BuffList.Free;
    end;
  end;
end;

constructor TZoneList.loadFromFile(const filename: string);
var fileList: TStringList;
begin
  fileList := TStringList.Create;
  try
    fileList.LoadFromFile(filename);
    FLoadResult := internalLoadZones(fileList);
  finally
    fileList.Free();
  end;
end;

constructor TZoneList.loadFromList(aStrList: TStringList);
begin
  FLoadResult := internalLoadZones(aStrList);
end;

procedure TZoneList.setSize(aSize: Integer);
begin
  if (aSize >= 0) then
    FZonesCount := aSize
  else
    FZonesCount := 0;

  SetLength(Zones, FZonesCount);
end;

end.
