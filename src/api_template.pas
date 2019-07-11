unit api_template;

interface

uses
  http, Classes;

type
  IAPIParams = interface
    procedure ApplyHeaders(strings: TStrings);
    procedure ApplyParams(strings: TStrings);
  end;

  IAPIRequiredParams = interface(IAPIParams)
    function GetExtra(): IAPIParams;
    property Extra: IAPIParams read GetExtra;
  end;

  TAPIRequiredParams = class(TInterfacedObject, IAPIRequiredParams)
    FToken: string;
    FProvider: string;
    FExtra: IAPIParams;
  public
    constructor Create(const AProvider, AToken: string; Extra: IAPIParams = nil);
    destructor Destroy; override;
    procedure ApplyHeaders(strings: TStrings);
    procedure ApplyParams(strings: TStrings);
    function GetExtra(): IAPIParams;
  end;

  TAPITemplate = class(TInterfacedObject)
  protected
    Client: IHTTPClient;
    URL: string;
  public
    constructor Create(client: IHTTPClient; URL: string);
    destructor Destroy; override;
  end;

implementation



{ TAPITemplate }

constructor TAPITemplate.Create(client: IHTTPClient; URL: string);
begin
  self.Client := client;
  self.URL := URL;
end;

destructor TAPITemplate.Destroy;
begin
  client := nil;
  inherited Destroy;
end;

{ TAPIRequiredParams }

procedure TAPIRequiredParams.ApplyHeaders(strings: TStrings);
begin
  strings.Values['token'] := FToken;
  strings.Values['provider'] := FProvider;
end;

procedure TAPIRequiredParams.ApplyParams(strings: TStrings);
begin
  //nothing
end;

constructor TAPIRequiredParams.Create(const AProvider, AToken: string; Extra: IAPIParams);
begin
  FProvider := AProvider;
  FToken := AToken;
  FExtra := Extra;
end;

destructor TAPIRequiredParams.Destroy;
begin
  FExtra := nil;
  inherited;
end;

function TAPIRequiredParams.GetExtra: IAPIParams;
begin
  Result := FExtra;
end;

end.

