unit api_json;

interface

uses uLkJSON, Classes, SysUtils;

type

  ExceptionFieldJSON=class(Exception);


  TErrorResponse=class(TObject)
  private
    FFields:TStringList;
    FMessage:string;
    FCode:string;
    FTag:string;

    function GetField(idx:string):string;
  protected
    procedure Parse(js:TlkJSONobject);
  public
    class function FromJSON(js:TlkJSONobject):TErrorResponse;
    constructor Create(); virtual;
    destructor Destroy;override;
    property Message:string read FMessage;
    property Code:string read FCode;
    property Tag: string read FTag;
    property Field[idx:string]:string read GetField;
  end;

  ExceptionResponse=class(Exception)
  private
    FResponse: TErrorResponse;
  public
    constructor CreateFromResponse(response:TErrorResponse);
    destructor Destroy; override;
    property Response:TErrorResponse read FResponse;
  end;



function IsError(js:TlkJSONobject):Boolean;
function GetValueJSON(js:TlkJSONbase;const name:string; default:Variant):Variant;
function MustField(js:TlkJSONobject;const name:string):TlkJSONbase;
function IsNullJSON(js:TlkJSONobject;const name:string):Boolean;

function JsStrToDateTime(const s:string): TDateTime;
function JsStrToDate(const s:string): TDateTime;

function JsStrToDateTimeDef(const s:string;const def:TDateTime): TDateTime;


function JsStrToFloatDef(const S: string; const Default: Extended): Extended;

implementation

constructor ExceptionResponse.CreateFromResponse(response:TErrorResponse);
begin
  inherited Create(response.Message);
  Self.FResponse := response;
end;

destructor ExceptionResponse.Destroy;
begin
  if Assigned(Self.FResponse) then
    Self.FResponse.Free;
end;



class function TErrorResponse.FromJSON(js:TlkJSONobject):TErrorResponse;
begin
  if not IsError(js) then
    exit;
  Result := TErrorResponse.Create();
  Result.Parse(js);
end;

constructor TErrorResponse.Create();
begin
  inherited;
  Self.FFields := TStringList.Create;
end;

destructor TErrorResponse.Destroy;
begin
  Self.FFields.Free;
end;

function TErrorResponse.GetField(idx:string):string;
begin
  Result:=Self.FFields.Values[idx];
end;

procedure TErrorResponse.Parse(js:TlkJSONobject);
var
  jError:TlkJSONobject;
  jFields:TlkJSONobject;
  i:Integer;
  sName:string;
begin
  jError:=js.Field['error'] as TlkJSONobject;
  if not Assigned(jError) then
    raise ExceptionFieldJSON.CreateFmt('field "%s" not found',['error']);

  Self.FMessage := GetValueJSON(jError,'message','');
  Self.FCode := GetValueJSON(jError,'code','');
  Self.FTag := GetValueJSON(jError,'tag','');
  if not IsNullJSON(jError,'fields') then
  begin
    jFields:=jError.Field['fields'] as TlkJSONobject;
    for i:=0 to jFields.Count-1 do
    begin
      sName:=jFields.NameOf[i];
      FFields.Values[sName]:=GetValueJSON(jFields,sName,'');
    end;
  end;
  // TODO: parse fields
end;

function IsError(js:TlkJSONobject):Boolean;
begin
  Result := Assigned(js) and Assigned(js.Field['status'])
    and (js.Field['status'] as TlkJSONnumber).Value = 0;
end;

function GetValueJSON(js:TlkJSONbase;const name:string; default:Variant):Variant;
var
  jsField:TlkJSONbase;
begin
  jsField := js.Field[name];
  if not Assigned(jsField) or ( jsField.SelfType=jsNull)  then
  begin
    Result:=default;
    Exit;
  end;
  Result := jsField.Value;
end;

function MustField(js:TlkJSONobject;const name:string):TlkJSONbase;
begin
  if js.IndexOfName(name)=-1 then
  	raise ExceptionFieldJSON.CreateFmt('field "%s" not found',[name]);
  Result := js.Field[name];
end;

function IsNullJSON(js:TlkJSONobject;const name:string):Boolean;
begin
  Result:=True;
  if js.IndexOfName(name)=-1 then exit;
  if js.Field[name].SelfType = jsNull then Exit;
  Result := False; 
end;

function JsStrToDateTimeDef(const s:string;const def:TDateTime): TDateTime;
var
  oldDateFormat : String;
  oldLongDateFormat : String;
  oldDateSeparator:Char;
  oldTimeSeparator:Char;
begin

  oldDateFormat:=ShortDateFormat;
  oldLongDateFormat := LongDateFormat;
  ShortDateFormat := 'yyyy-mm-dd';
  LongDateFormat := 'yyyy-mm-dd hh:nn:ss.z';
  oldDateSeparator:= DateSeparator;
  oldTimeSeparator:= TimeSeparator;
  DateSeparator := '-';
  TimeSeparator := ':';
  try
    Result:=StrToDateTimeDef(StringReplace(s,'T',' ',[]),def);
  finally
    ShortDateFormat:=oldDateFormat;
    LongDateFormat:=oldLongDateFormat;
    DateSeparator:= oldDateSeparator;
    TimeSeparator:=oldTimeSeparator;
  end;
end;


function JsStrToDateTime(const s:string): TDateTime;
var
  oldDateFormat : String;
  oldLongDateFormat : String;
  oldDateSeparator:Char;
  oldTimeSeparator:Char;
begin

  oldDateFormat:=ShortDateFormat;
  oldLongDateFormat := LongDateFormat;
  ShortDateFormat := 'yyyy-mm-dd';
  LongDateFormat := 'yyyy-mm-dd hh:nn:ss.z';
  oldDateSeparator:= DateSeparator;
  oldTimeSeparator:= TimeSeparator;
  DateSeparator := '-';
  TimeSeparator := ':';
  try
    Result:=StrToDateTime(StringReplace(s,'T',' ',[]));
  finally
    ShortDateFormat:=oldDateFormat;
    LongDateFormat:=oldLongDateFormat;
    DateSeparator:= oldDateSeparator;
    TimeSeparator:=oldTimeSeparator;
  end;
end;

function JsStrToDate(const s:string): TDateTime;
var
  oldDateFormat : String;
  oldLongDateFormat : String;
  oldDateSeparator:Char;
  oldTimeSeparator:Char;
begin
  oldDateFormat:=ShortDateFormat;
  oldLongDateFormat := LongDateFormat;
  ShortDateFormat := 'yyyy-mm-dd';
  LongDateFormat := 'yyyy-mm-dd hh:nn:ss.z';
  oldDateSeparator:= DateSeparator;
  oldTimeSeparator:= TimeSeparator;
  DateSeparator := '-';
  TimeSeparator := ':';
  try
    Result:=StrToDate(StringReplace(s,'T',' ',[]));
  finally
    ShortDateFormat:=oldDateFormat;
    LongDateFormat:=oldLongDateFormat;
    DateSeparator:= oldDateSeparator;
    TimeSeparator:=oldTimeSeparator;
  end;
end;

function JsStrToFloatDef(const S: string; const Default: Extended): Extended;
begin
  Result := StrToFloatDef(StringReplace(
  	StringReplace(s,'.',DecimalSeparator,[]),',',DecimalSeparator,[]),
  	Default);
end;

end.


