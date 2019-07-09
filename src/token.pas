unit token;

interface

uses Classes, SysUtils, IdBaseComponent, IdCoder, IdCoder3To4, uLkJSON, Variants,
  unix_utils;

type
  ExceptionToken=class(Exception)
  private
    FToken:string;
  public
    constructor CreateWithToken(const Token: string;const Msg: string='');
    constructor CreateWithTokenFmt(const Token: string;const Msg: string;
      const Args: array of const);
    property Token:string read FToken;
  end;

  InvalidToken=class(ExceptionToken);

  TTypeToken = (UNKNOWN=$0, ACCESS=$1, REFRESH=$2);
  TToken = class
  private
    FID:string;
    FUser:Integer;
    FToken:string;
    FTypeToken:TTypeToken;
    FExpirationTime:TDateTime;
    FIssuer:string;
    FVersion:Integer;
  protected
    function GetTypeTokenFromString(const typ:string): TTypeToken;
    procedure Parse(token:string);virtual;
  public
    constructor Create(const Token:string);
    function Expired():Boolean;
    procedure Validate();
    property AsString:string read FToken;
    property TypeToken:TTypeToken read FTypeToken;
    property ExpirationTime:TDateTime read FExpirationTime;
    property ID:string read FID;
    property User:Integer read FUser;
    property Issuer:string read FIssuer;
    property Version:Integer read FVersion;
  end;

implementation

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.DelimitedText   := Str;
end;

procedure Fetch(var Result:string; Ch:Char);
var
  idx:Integer;
begin
  idx := Pos(Ch,Result);
  Delete(Result,1,idx);
end;

constructor ExceptionToken.CreateWithToken(const Token: string;
  const Msg: string='');
begin
  if Msg ='' then
    inherited CreateFmt('token: %s', [Self.ClassName])
  else
    inherited Create(Msg);
  Self.FToken:=Token;
end;

constructor ExceptionToken.CreateWithTokenFmt(const Token, Msg: string;
  const Args: array of const);
begin
  inherited CreateFmt(Msg,Args);
  Self.FToken:=Token;
end;

constructor TToken.Create(const token:string);
begin
  Self.Parse(token);
  Self.FToken:=token;
end;

function TToken.Expired():Boolean;
begin
  Result:= NowUTC >= Self.FExpirationTime;
end;

function TToken.GetTypeTokenFromString(const typ: string): TTypeToken;
begin
  Result := UNKNOWN;
  if typ='refresh_token' then
    Result := REFRESH
  else if typ = 'access_token' then
    Result := ACCESS;
end;

procedure TToken.Parse(Token:string);
var
  TokenParts:TStrings;
  s64: string;
  js:TlkJSONobject;
begin
  TokenParts:=TStringList.Create;
  try
    Split('.',token,TokenParts);
    if TokenParts.Count<>3 then
      raise InvalidToken.CreateWithToken(Token);
    with TIdBase64Decoder.Create(nil) do
    try
      AddCRLF := False;
      CodeString(TokenParts.Strings[1]);
      s64 := CompletedInput();
      Fetch(s64,';');
    finally
      Free;
    end;
  finally
    TokenParts.Free;
  end;

  //parse from json
  js:=TlkJSON.ParseText(s64) as TlkJSONobject;
  try
    Self.FID := VarToStr(js.Field['jti'].Value);
    Self.FUser := js.Field['u'].Value;
    Self.FTypeToken:=Self.GetTypeTokenFromString(js.Field['sub'].Value);
    Self.FExpirationTime := UnixToDateTime(js.Field['exp'].Value);
    Self.FIssuer := js.Field['iss'].Value;
    Self.FVersion := js.Field['v'].Value;
  finally
    js.Free;
  end;

  Self.Validate();
end;

procedure TToken.Validate();
begin
  if Self.FTypeToken = UNKNOWN then
    raise InvalidToken.CreateWithTokenFmt(Self.FToken,'UNKNOWN type token',[]);
end;



end.
