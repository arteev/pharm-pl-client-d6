unit http;

interface

uses ulkJSON, IdHTTP, Classes, SysUtils;


type
  TTypeRequest = (GET_REQUEST = $0, POST_REQUEST = $1);




  IHTTPClient = interface
    function Get(const url:string; Params:TStrings; Headers:TStrings):TlkJsonObject;
    function Post(const url:string; Params:TStream; Headers:TStrings):TlkJsonObject;
  end;

   HTTPClient = class(TInterfacedObject, IHTTPClient)
  private
    FidHTTPClient: TIdHTTP;
  public
    constructor Create(idHTTPClient:TIdHTTP);
    //function DoRequest(var req:Request):TlkJsonObject;
    //function Get(req:Request):TlkJsonObject;
    function Get(const url:string; Params:TStrings; Headers:TStrings):TlkJsonObject;
    function Post(const url:string; Params:TStream; Headers:TStrings):TlkJsonObject;
  end;


function CreateDefaultClient(AOwner:TComponent):TIdHTTP;

implementation


function CreateDefaultClient(AOwner:TComponent):TIdHTTP;
begin
  Result := TIdHTTP.Create(AOwner);
  Result.ProtocolVersion := pv1_1;
end;

constructor HTTPClient.Create(idHTTPClient:TIdHTTP);
begin
  Self.FidHTTPClient := idHTTPClient;
  Self.FidHTTPClient.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; rv:44.0) Gecko/20100101 Firefox/44.0';
  inherited Create;
end;

function HTTPClient.Get(const url:string; Params:TStrings; Headers:TStrings):TlkJsonObject;
var
  Stream:TStringStream;
  i:Integer;
begin
  Stream:=TStringStream.Create('');
  try
    Self.FidHTTPClient.Request.ExtraHeaders.Clear;
    Self.FidHTTPClient.Request.ExtraHeaders.Values['Content-Type']:= 'application/x-www-form-urlencoded';
    if Headers<>nil then
    begin
      for i:=0 to Headers.Count-1 do
      begin
        Self.FidHTTPClient.Request.ExtraHeaders.Values[Headers.Names[i]] :=
          Headers.Values[Headers.Names[i]];
      end;
    end;
    Self.FidHTTPClient.Get(url, Stream);

    Result := TlkJSONstreamed.ParseText(Stream.DataString) as TlkJSONobject;
  finally
    Stream.Free;
  end;
end;


function HTTPClient.Post(const url:string; Params:TStream; Headers:TStrings):TlkJsonObject;
var
  Stream:TMemoryStream;
  i:Integer;
begin
  Stream:=TMemoryStream.Create();
  try
    Self.FidHTTPClient.Request.ExtraHeaders.Clear;
    Self.FidHTTPClient.Request.ExtraHeaders.Values['Content-Type']:= 'application/x-www-form-urlencoded';
    if Headers<>nil then
    begin
      for i:=0 to Headers.Count-1 do
      begin
        Self.FidHTTPClient.Request.ExtraHeaders.Values[Headers.Names[i]] :=
          Headers.Values[Headers.Names[i]];
      end;
    end;
    Self.FidHTTPClient.Post(url,Params,Stream);
    Result := TlkJSONstreamed.ParseText(Pchar(Stream.Memory)) as TlkJSONobject;;
  finally
    Stream.Free;
  end;
end;





end.
