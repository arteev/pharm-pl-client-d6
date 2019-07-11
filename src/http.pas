unit http;

interface

uses
  ulkJSON, IdHTTP, Classes, SysUtils, NMURL;

type
  IHTTPClient = interface
    function Get(const url: string; Params: TStrings; Headers: TStrings): TlkJsonObject;
    function Post(const url: string; Params: TStream; Headers: TStrings): TlkJsonObject; overload;
    function Post(const url: string; Params: TStrings; Headers: TStrings): TlkJsonObject; overload;
  end;

  HTTPClient = class(TInterfacedObject, IHTTPClient)
  private
    FidHTTPClient: TIdHTTP;
    FURLEncoder: TNMURL;
  public
    constructor Create(idHTTPClient: TIdHTTP);
    destructor Destroy; override;
    function Get(const url: string; Params: TStrings; Headers: TStrings): TlkJsonObject;
    function Post(const url: string; Params: TStream; Headers: TStrings): TlkJsonObject; overload;
    function Post(const url: string; Params: TStrings; Headers: TStrings): TlkJsonObject; overload;
  end;

function CreateDefaultClient(AOwner: TComponent): TIdHTTP;

implementation

function CreateDefaultClient(AOwner: TComponent): TIdHTTP;
begin
  Result := TIdHTTP.Create(AOwner);
  Result.ProtocolVersion := pv1_1;
end;

constructor HTTPClient.Create(idHTTPClient: TIdHTTP);
begin
  Self.FidHTTPClient := idHTTPClient;
  Self.FidHTTPClient.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; rv:44.0) Gecko/20100101 Firefox/44.0';
  FURLEncoder := TNMURL.Create(nil);
  inherited Create;
end;

destructor HTTPClient.Destroy;
begin
  FURLEncoder.Free;
  inherited;
end;

function HTTPClient.Get(const url: string; Params: TStrings; Headers: TStrings): TlkJsonObject;
var
  Stream: TMemoryStream;
  i: Integer;
  tryURL: string;
begin

  Stream := TMemoryStream.Create;
  tryURL := url;
  try
    Self.FidHTTPClient.Request.ExtraHeaders.Clear;
    if Headers <> nil then
    begin
      for i := 0 to Headers.Count - 1 do
      begin
        Self.FidHTTPClient.Request.ExtraHeaders.Values[Headers.Names[i]] := Headers.Values[Headers.Names[i]];
      end;
    end;
    if Params <> Nil then
    begin
      tryURL := '';
      for i := 0 to Params.Count - 1 do
      begin
        FURLEncoder.InputString := Params.Values[Params.Names[i]];
        tryURL := tryURL + Params.Names[i] + '=' + FURLEncoder.Encode + '&';
      end;
      tryURL := url + '?' + tryURL;
    end;
    Self.FidHTTPClient.Get(tryURL, Stream);
    Stream.Position := 0;
    Result := TlkJSONstreamed.LoadFromStream(Stream) as TlkJSONobject;
  finally
    Stream.Free;
  end;
end;

function HTTPClient.Post(const url: string; Params: TStream; Headers: TStrings): TlkJsonObject;
var
  Stream: TMemoryStream;
  i: Integer;
begin
  Stream := TMemoryStream.Create();
  try
    Self.FidHTTPClient.Request.ExtraHeaders.Clear;
    Self.FidHTTPClient.Request.ExtraHeaders.Values['Content-Type'] := 'application/x-www-form-urlencoded';
    if Headers <> nil then
    begin
      for i := 0 to Headers.Count - 1 do
      begin
        Self.FidHTTPClient.Request.ExtraHeaders.Values[Headers.Names[i]] := Headers.Values[Headers.Names[i]];
      end;
    end;
    Self.FidHTTPClient.Post(url, Params, Stream);
    Stream.Position := 0;
    Result := TlkJSONstreamed.LoadFromStream(Stream) as TlkJSONobject;
  finally
    Stream.Free;
  end;
end;

function HTTPClient.Post(const url: string; Params, Headers: TStrings): TlkJsonObject;
var
  Stream: TMemoryStream;
  i: Integer;
  ParamsEncode: TStringStream;
  sParams: string;
begin
  Stream := TMemoryStream.Create();
  try
    Self.FidHTTPClient.Request.ExtraHeaders.Clear;
    Self.FidHTTPClient.Request.ExtraHeaders.Values['Content-Type'] := 'application/x-www-form-urlencoded';
    if Headers <> nil then
    begin
      for i := 0 to Headers.Count - 1 do
      begin
        Self.FidHTTPClient.Request.ExtraHeaders.Values[Headers.Names[i]] := Headers.Values[Headers.Names[i]];
      end;
    end;
    if Params <> Nil then
    begin

      for i := 0 to Params.Count - 1 do
      begin
        FURLEncoder.InputString := Params.Values[Params.Names[i]];
        sParams := sParams + Params.Names[i] + '=' + FURLEncoder.Encode;
        if i <> Params.Count - 1 then
          sParams := sParams + '&';
      end;
      ParamsEncode := TStringStream.Create(sParams);
    end;
    Self.FidHTTPClient.Post(url, ParamsEncode, Stream);
    Stream.Position := 0;
    Result := TlkJSONstreamed.LoadFromStream(Stream) as TlkJSONobject;
  finally
    if ParamsEncode <> nil then
      ParamsEncode.Free;
    Stream.Free;
  end;
end;

end.

