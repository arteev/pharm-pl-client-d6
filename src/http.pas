unit http;

interface

uses ulkJSON, IdHTTP, Classes, SysUtils;


type
  TTypeRequest = (GET_REQUEST = $0, POST_REQUEST = $1);


  Request=class;

  IHTTPClient = interface
    function Get(req:Request):TlkJsonObject;
    function Post(const url:string; params:TStream; Headers:TStrings):TlkJsonObject;
  end;

  Request=class
  private
    typ:TTypeRequest;
    url:string;
    FParams:TStream;
  public
    class function Get(const url:string):Request;
    class function Post(const url:string; params:TStream):Request;
    property Params:TStream read FParams;
  end;

  HTTPClient = class(TInterfacedObject, IHTTPClient)
  private
    FidHTTPClient: TIdHTTP;
  public
    constructor Create(idHTTPClient:TIdHTTP);
    function DoRequest(var req:Request):TlkJsonObject;
    function Get(req:Request):TlkJsonObject;
    function Post(const url:string; params:TStream; Headers:TStrings):TlkJsonObject;
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

function HTTPClient.Get(req:Request):TlkJsonObject;
begin
  req.typ := GET_REQUEST;
  Result := Self.DoRequest(req);
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

function HTTPClient.DoRequest(var req:Request):TlkJsonObject;
var

  m:TIdHTTPMethod;
  Stream:TMemoryStream;

  s:TlkJSONobject;

begin

  case req.typ of

  GET_REQUEST:
    begin
      m:=hmGet;
    end;
  POST_REQUEST:
    begin
      m:=hmPost;
    end;

  end;

  Stream:=TMemoryStream.Create();
  Self.FidHTTPClient.Connect;


  Self.FidHTTPClient.DoRequest(m,req.url,req.Params, Stream);
  
  Self.FidHTTPClient.Disconnect;

  s :=TlkJSONstreamed.ParseText(Pchar(Stream.Memory)) as TlkJSONobject;

  Result := s;
  Stream.Free;
end;



class function Request.Get(const  url:string):Request;
begin
  Result:=Request.Create;
  Result.url := url;
  Result.typ := GET_REQUEST;
end;

class function Request.Post(const url:string;params:TStream):Request;
begin
  Result:=Request.Create;
  Result.url := url;
  Result.typ := POST_REQUEST;
  Result.FParams := params;
end;


end.
