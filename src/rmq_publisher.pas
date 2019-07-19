unit rmq_publisher;

interface
uses Classes, Sysutils, publisher, api_template, rmq_wrapper;

type
  TPublisherRMQ =class(TInterfacedObject,IPurchasePublisher)
  private
    FRabbit:TRabbitMQ;
    FDefaultChannel:TChannelMQ;
    FExchange:string;
    FKey:string;
    FURL:string;
    FQueue:string;
	FTimeout:integer;

  protected
    procedure InitRabbit();
    procedure CheckConnection();
  public
    constructor Create();overload;
    constructor Create(const AURL,AExchange,AQueue,AKey:string;const Timeout:integer=0);overload;
    destructor Destroy;override;
    procedure Publish(RequestParameters: IAPIParams);
    procedure Reconnect();
  end;

implementation

{ TPublisherRMQ }

procedure TPublisherRMQ.CheckConnection;
begin
  if (FRabbit=nil) or (not FRabbit.Connected) then
    raise EPublisherConnFailed.Create('publisher not connected');
end;

constructor TPublisherRMQ.Create(const AURL, AExchange, AQueue,
  AKey: string; const Timeout: integer);
begin
  FURL:=AURL;
  FExchange:=AExchange;
  FQueue:=AQueue;
  FKey:=AKey;
  FTimeout:=Timeout;
  InitRabbit();
end;

constructor TPublisherRMQ.Create;
begin
  //
end;

destructor TPublisherRMQ.Destroy;
begin
  if Assigned(FRabbit) then
  	FRabbit.Free;
  inherited;
end;

procedure TPublisherRMQ.InitRabbit();
begin
  FRabbit := TRabbitMQ.Create(FURL, FTimeout);
  try
    FRabbit.Connect();
    FDefaultChannel := FRabbit.CreateChannel();
    FDefaultChannel.ExchangeDeclare(FExchange, 'fanout', True, False, False, False);
    FDefaultChannel.QueueDeclare(FQueue, true, false, false, false);
    FDefaultChannel.QueueBind(FQueue, FKey, FExchange, false);
  except
    on E: EConnectionRMQ do
      raise EPublisherConnFailed.Create(e.Message);
  end;
end;

procedure TPublisherRMQ.Publish(RequestParameters: IAPIParams);
var
  stream : TMemoryStream;
begin
  CheckConnection();
  stream := TMemoryStream.Create();
  try
    RequestParameters.ApplyParams(stream);
    FDefaultChannel.Publish(FExchange,FKey,False,False,stream);
  finally
    stream.Free;
  end;
end;

procedure TPublisherRMQ.Reconnect;
begin
  if FRabbit<>nil then
    FRabbit.Free;
  InitRabbit();
end;

end.
