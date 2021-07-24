unit MetrologRound;

interface

uses SysUtils;

function metrolRound(uncertainty: Double): string; overload;
function metrolRound(value, uncertainty: Double): string; overload;
function metrolRound(uncertainty: Double; const FormatSettings: TFormatSettings): string; overload;
function metrolRound(value, uncertainty: Double; const FormatSettings: TFormatSettings): string; overload;

implementation

uses Math;

function findFormatString(value: Double; var dec_cnt: Integer): string;
begin
  value := Abs(value);
  if (value < 1e-6) then begin
    Result := '0.##########';
    dec_cnt := -8;
    Exit;
  end;
  Result := '0.';
  dec_cnt := 0;
  while (value < 2.9) do begin
    value := value * 10;
    Result := Result + '0';
    Dec(dec_cnt);
  end;
end;

function metrolRound(uncertainty: Double): string;
var fmt_str: string;
    dec_cnt: Integer;
begin
  if ((Abs(uncertainty) > 1e5) or (Abs(uncertainty) < 1e-5) and (Abs(uncertainty) > 0)) then begin
    Result := FloatToStrF(uncertainty, ffExponent, 4, 2);
    Exit;
  end;
  // count digits
  fmt_str := findFormatString(uncertainty, dec_cnt);
  Result := FormatFloat(fmt_str, RoundTo(uncertainty, dec_cnt));
end;

function metrolRound(value, uncertainty: Double): string; overload;
var fmt_str: string;
    dec_cnt: Integer;
begin
  if ((Abs(value) > 1e5) or (Abs(value) < 1e-5) and (Abs(value) > 0)) then begin
    Result := FloatToStrF(value, ffExponent, 4, 2);
    Exit;
  end;
  // count digits
  fmt_str := findFormatString(uncertainty, dec_cnt);
  Result := FormatFloat(fmt_str, RoundTo(value, dec_cnt));
end;

function metrolRound(uncertainty: Double; const formatSettings: TFormatSettings): string; overload;
var fmt_str: string;
    dec_cnt: Integer;
begin
  if ((Abs(uncertainty) > 1e5) or (Abs(uncertainty) < 1e-5) and (Abs(uncertainty) > 0)) then begin
    Result := FloatToStrF(uncertainty, ffExponent, 4, 2);
    Exit;
  end;
  // count digits
  fmt_str := findFormatString(uncertainty, dec_cnt);
  Result := FormatFloat(fmt_str, RoundTo(uncertainty, dec_cnt), formatSettings);
end;


function metrolRound(value, uncertainty: Double; const formatSettings: TFormatSettings): string; overload;
var fmt_str: string;
    dec_cnt: Integer;
begin
  if ((Abs(value) > 1e5) or (Abs(value) < 1e-5) and (Abs(value) > 0)) then begin
    Result := FloatToStrF(value, ffExponent, 4, 2);
    Exit;
  end;
  // count digits
  fmt_str := findFormatString(uncertainty, dec_cnt);
  Result := FormatFloat(fmt_str, RoundTo(value, dec_cnt), formatSettings);
end;



end.
