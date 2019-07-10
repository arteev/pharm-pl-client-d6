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
function GetValueJSON(js:TlkJSONbase; name:string; default:Variant):Variant;
function MustField(js:TlkJSONobject;name:string):TlkJSONbase;

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
begin
  jError:=js.Field['error'] as TlkJSONobject;
  if not Assigned(jError) then
    raise ExceptionFieldJSON.CreateFmt('field "%s" not found',['error']);

  Self.FMessage := GetValueJSON(jError,'message','');
  Self.FCode := GetValueJSON(jError,'code','');
  Self.FTag := GetValueJSON(jError,'tag','');
  // TODO: parse fields
end;

function IsError(js:TlkJSONobject):Boolean;
begin
  Result := Assigned(js) and Assigned(js.Field['status'])
    and (js.Field['status'] as TlkJSONnumber).Value = 0;
end;

function GetValueJSON(js:TlkJSONbase; name:string; default:Variant):Variant;
var
  jsField:TlkJSONbase;
begin
  jsField := js.Field[name];
  if not Assigned(jsField) then
  begin
    Result:=default;
    Exit;
  end;
  Result := jsField.Value;
end;

function MustField(js:TlkJSONobject;name:string):TlkJSONbase;
begin
  if js.IndexOfName(name)=-1 then
  	raise ExceptionFieldJSON.CreateFmt('field "%s" not found',[name]);
  Result := js.Field[name];
end;

end.
