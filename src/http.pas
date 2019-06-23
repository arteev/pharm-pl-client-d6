unit http;

interface

uses ulkJSON, IdHTTP;


type
  TTypeRequest = (GET_REQUEST = $0, POST_REQUEST = $1);

type
  IHTTPClient = interface
  end;

  Request=class
  private
    typ:TTypeRequest;
    url:string;
  public
    class function Get(url:string):Request;
  end;

  HTTPClient = class
  public
    constructor Create;
    procedure DoRequest(var req:Request);
  end;



implementation

constructor HTTPClient.Create;
var
  h:TIdHTTP;
begin

  h:=TIdHTTP.Create(nil);
  //h.
  inherited;
end;

procedure HTTPClient.DoRequest(var req:Request);
begin

end;



class function Request.Get(url:string):Request;
begin
  Result:=Request.Create;
  Result.url := url;
  Result.typ := GET_REQUEST;
end;

end.
