unit ParamStrUtils;

interface

function readParam(paramNum: Integer; defValue: String = ''): String;
function getAllParamString(): String;

implementation

function readParam(paramNum: Integer; defValue: String = ''): String;
begin
  if (ParamCount >= paramNum) then
    Result := ParamStr(paramNum)
  else
    Result := defValue;
end;

function getAllParamString(): String;
var i: Integer;
begin
  Result := '';
  for i:=1 to ParamCount do begin
    Result := Result + ParamStr(i) + ' ';
  end;
end;

end.
