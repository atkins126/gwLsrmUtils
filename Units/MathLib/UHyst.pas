unit UHyst;

interface

type
  TIntHyst = class                // class of Integer hystograms
  private
    FSize: Integer;
    FCapacity: Integer;
    procedure setSize(aSize: Integer);
    procedure setCapacity(ACapacity: Integer);
    function getXValue(idx: Integer): Integer;
    procedure setXValue(idx: Integer; aValue: Integer);
    function getYValue(idx: Integer): Integer;
    procedure setYValue(idx: Integer; aValue: Integer);
    procedure zeroElements(firstIdx, lastIdx: Integer);
  public
    FXValue,                     // x-axes
    FYValue: array of Integer;   // y-axes
    destructor Destroy(); override;
    procedure saveCountsToFile(const filename: string); // saves only y -- pure hystogram
    procedure saveToFile(const filename: string);       // saves x and y
    procedure addXY(AX, AY: Integer);                   // add value
    procedure setPoint(idx: Integer; AX, AY: Integer);           // set value for point if exists
    procedure shrink_to_fit();                          // shrink hysto size: free unused memmory
    procedure clear();                                  // deletes all elements in hysto
    property length: Integer read FSize write setSize;           // set used size, but not size of Hysto
    property capacity: Integer read FCapacity write setCapacity; // set real hysto size
    property x[idx: Integer]: Integer read getXValue write setXValue;
    property y[idx: Integer]: Integer read getYValue write setYValue;
  end;

  TDoubleHyst = class              // class of float-point hystograms
  private
    FSize: Integer;
    FCapacity: Integer;
    procedure setSize(aSize: Integer); virtual;
    procedure setCapacity(ACapacity: Integer); virtual;
    function getXValue(idx: Integer): Double;
    procedure setXValue(idx: Integer; aValue: Double);
    function getYValue(idx: Integer): Double;
    procedure setYValue(idx: Integer; aValue: Double);
    procedure zeroElements(firstIdx, lastIdx: Integer); virtual;
  public
    FXValue,                    // x-axes
    FYValue: array of Double;   // y-axes
    destructor Destroy(); override;
    procedure saveCountsToFile(const filename: string);     // saves only y -- pure hystogram
    procedure saveToFile(const filename: string); virtual;  // saves x and y
    procedure addXY(AX, AY: Double); overload; virtual;     // add value
    procedure setPoint(idx: Integer; AX, AY: Double); overload; virtual;    // set value for point if exists
    procedure shrink_to_fit();                              // shrink hysto size: free unused memmory
    procedure clear();                                      // deletes all elements in hysto
    property length: Integer read FSize write setSize;            // set used size, but not size of Hysto
    property capacity: Integer read FCapacity write setCapacity;  // set real hysto size
    property x[idx: Integer]: Double read getXValue write setXValue;
    property y[idx: Integer]: Double read getYValue write setYValue;
  end;

  TDoubleHystErr = class (TDoubleHyst)
  private
    procedure setCapacity(ACapacity: Integer); override;
    function getDYValue(idx: Integer): Double;
    procedure setDYValue(idx: Integer; aValue: Double);
    procedure zeroElements(firstIdx, lastIdx: Integer); override;
  public
    FYError: array of Double;
    destructor Destroy(); override;
    procedure saveToFile(const filename: string); override;
    procedure addXY(AX, AY, ADY: Double); reintroduce; overload;
    procedure setPoint(idx: Integer; AX, AY, AdY: Double); reintroduce; overload;    // set value for point if exists
    property dy[idx: Integer]: Double read getDYValue write setDYValue;
  end;

implementation

uses SysUtils;

{ TIntHyst }

procedure TIntHyst.addXY(AX, AY: Integer);
begin
  setSize(FSize + 1);
  FXValue[FSize - 1] := AX;
  FYValue[FSize - 1] := AY;
end;

procedure TIntHyst.setSize(aSize: Integer);
var lastSize: Integer;
begin
  lastSize := FSize;
  if (aSize >= 0) then
    Fsize := aSize
  else
    Fsize := 0;
  // increase capacity if needed
  if (Fsize > FCapacity) then
    setCapacity(Fsize);
  // zero new elements
  if (lastSize < FSize) then
    zeroElements(lastSize, FSize-1);
end;

destructor TIntHyst.Destroy;
begin
  FXValue := nil;
  FYValue := nil;  
  inherited;
end;

function TIntHyst.getXValue(idx: Integer): Integer;
begin
  if (idx >= 0) and (idx < FSize) then
    Result := FXValue[idx]
  else
    Result := 0;
end;

function TIntHyst.getYValue(idx: Integer): Integer;
begin
  if (idx >= 0) and (idx < FSize) then
    Result := FYValue[idx]
  else
    Result := 0;
end;

procedure TIntHyst.saveCountsToFile(const filename: string);
var F: TextFile;
    i: Integer;
begin
  AssignFile(F, filename);
  Rewrite(F);
  for i:=0 to FSize-1 do
    Writeln(F, IntToStr(i) + #09 + IntToStr(FYValue[i]));
  CloseFile(F);
end;

procedure TIntHyst.saveToFile(const filename: string);
var F: TextFile;
    i: Integer;
begin
  AssignFile(F, filename);
  Rewrite(F);
  for i:=0 to FSize-1 do
    Writeln(F, IntToStr(FXValue[i]) + #09 + IntToStr(FYValue[i]));
  CloseFile(F);
end;

procedure TIntHyst.setCapacity(ACapacity: Integer);
begin
  if (aCapacity >= 0) then
    FCapacity := aCapacity
  else
    FCapacity := 0;
  SetLength(FXValue, FCapacity);
  SetLength(FYValue, FCapacity);
end;

procedure TIntHyst.setXValue(idx: Integer; aValue: Integer);
begin
  if (idx >= 0) and (idx < FSize) then
    FXValue[idx] := aValue;
end;

procedure TIntHyst.setYValue(idx: Integer; aValue: Integer);
begin
  if (idx >= 0) and (idx < FSize) then
    FYValue[idx] := aValue;
end;

procedure TIntHyst.setPoint(idx: Integer; AX, AY: Integer);
begin
  if (idx >= 0) and (idx < FSize) then begin
    FXValue[idx] := AX;
    FYValue[idx] := AY;
  end;
end;

procedure TIntHyst.shrink_to_fit;
begin
  if (FCapacity > Fsize) then
    setCapacity(Fsize);
end;

procedure TIntHyst.clear;
begin
  setSize(0);
end;

procedure TIntHyst.zeroElements(firstIdx, lastIdx: Integer);
var i: Integer;
begin
  for i:=firstIdx to lastIdx do
  begin
    FXValue[i] := 0;
    FYValue[i] := 0;
  end;
end;

{ TDoubleHyst }

procedure TDoubleHyst.addXY(AX, AY: Double);
begin
  setSize(FSize + 1);
  FXValue[FSize - 1] := AX;
  FYValue[FSize - 1] := AY;
end;

procedure TDoubleHyst.setSize(aSize: Integer);
var lastSize: Integer;
begin
  lastSize := FSize;
  if (aSize >= 0) then
    Fsize := aSize
  else
    Fsize := 0;
  // increase capacity if needed
  if (Fsize > FCapacity) then
    setCapacity(Fsize);
  // zero new elements
  if (lastSize < FSize) then
    zeroElements(lastSize, FSize-1);
end;

destructor TDoubleHyst.Destroy;
begin
  FXValue := nil;
  FYValue := nil;
  inherited;
end;

function TDoubleHyst.getXValue(idx: Integer): Double;
begin
  if (idx >= 0) and (idx < FSize) then
    Result := FXValue[idx]
  else
    Result := 0;
end;

function TDoubleHyst.getYValue(idx: Integer): Double;
begin
  if (idx >= 0) and (idx < FSize) then
    Result := FYValue[idx]
  else
    Result := 0;
end;

procedure TDoubleHyst.saveCountsToFile(const filename: string);
var F: TextFile;
    i: Integer;
begin
  AssignFile(F, filename);
  Rewrite(F);
  for i:=0 to FSize-1 do
    Writeln(F, IntToStr(i) + #09 + FloatToStr(FYValue[i]));
  CloseFile(F);
end;

procedure TDoubleHyst.saveToFile(const filename: string);
var F: TextFile;
    i: Integer;
begin
  AssignFile(F, filename);
  Rewrite(F);
  for i:=0 to FSize-1 do
    Writeln(F, FloatToStr(FXValue[i]) + #09 + FloatToStr(FYValue[i]));
  CloseFile(F);
end;

procedure TDoubleHyst.setXValue(idx: Integer; aValue: Double);
begin
  if (idx >= 0) and (idx < FSize) then
    FXValue[idx] := aValue;
end;

procedure TDoubleHyst.setYValue(idx: Integer; aValue: Double);
begin
  if (idx >= 0) and (idx < FSize) then
    FYValue[idx] := aValue;
end;

procedure TDoubleHyst.setCapacity(ACapacity: Integer);
begin
  if (aCapacity >= 0) then
    FCapacity := aCapacity
  else
    FCapacity := 0;
  SetLength(FXValue, FCapacity);
  SetLength(FYValue, FCapacity);
end;

procedure TDoubleHyst.setPoint(idx: Integer; AX, AY: Double);
begin
  if (idx >= 0) and (idx < FSize) then begin
    FXValue[idx] := AX;
    FYValue[idx] := AY;
  end;
end;

procedure TDoubleHyst.shrink_to_fit;
begin
  if (FCapacity > Fsize) then
    setCapacity(Fsize);
end;

procedure TDoubleHyst.clear;
begin
  setSize(0);
end;

procedure TDoubleHyst.zeroElements(firstIdx, lastIdx: Integer);
var i: Integer;
begin
  for i:=firstIdx to lastIdx do
  begin
    FXValue[i] := 0;
    FYValue[i] := 0;
  end;
end;

{ TDoubleHystErr }

procedure TDoubleHystErr.addXY(AX, AY, ADY: Double);
begin
  inherited addXY(AX, AY);
  FYError[FSize - 1] := ADY;
end;

destructor TDoubleHystErr.Destroy;
begin
  FYError := nil;
  inherited;
end;

function TDoubleHystErr.getDYValue(idx: Integer): Double;
begin
  if (idx >= 0) and (idx < FSize) then
    Result := FYError[idx]
  else
    Result := 0;
end;

procedure TDoubleHystErr.saveToFile(const filename: string);
var F: TextFile;
    i: Integer;
begin
  AssignFile(F, filename);
  Rewrite(F);
  for i:=0 to FSize-1 do
    Writeln(F, FloatToStr(FXValue[i]) + #09 + FloatToStr(FYValue[i]) + #09 + FloatToStr(FYError[i]));
  CloseFile(F);
end;

procedure TDoubleHystErr.setDYValue(idx: Integer; aValue: Double);
begin
  if (idx >= 0) and (idx < FSize) then
    FYError[idx] := aValue;
end;

procedure TDoubleHystErr.setPoint(idx: Integer; AX, AY, AdY: Double);
begin
  inherited addXY(AX, AY);
  if (idx >= 0) and (idx < FSize) then begin
    FYError[idx] := AdY;
  end;  
end;

procedure TDoubleHystErr.zeroElements(firstIdx, lastIdx: Integer);
var i: Integer;
begin
  //inherited;
  for i:=firstIdx to lastIdx do begin
    FXValue[i] := 0;
    FYValue[i] := 0;
    FYError[i] := 0;
  end;
end;

procedure TDoubleHystErr.setCapacity(ACapacity: Integer);
begin
  inherited;
  SetLength(FYError, FCapacity);
end;

end.
