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
    FArgsQueue:TStrings;
    FArgsExchange:TStrings;
    FArgsBind:TStrings;
  protected
    procedure InitRabbit();
    procedure CheckConnection();
  public
    constructor Create();overload;
    constructor Create(const AURL,AExchange,AQueue,AKey:string;
	  ArgsExchange, ArgsQueue, ArgsBind:TStrings;
      const Timeout:integer=0);overload;
    destructor Destroy;override;
    procedure Publish(RequestParameters: IAPIParams;const AMessageID:string);
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
  AKey: string;
  ArgsExchange, ArgsQueue, ArgsBind:TStrings;
  const Timeout: integer);
begin
  FURL:=AURL;
  FExchange:=AExchange;
  FQueue:=AQueue;
  FKey:=AKey;
  FTimeout:=Timeout;

  if ArgsQueue <> nil then
  begin
    FArgsQueue := TStringList.Create;
    FArgsQueue.Text := ArgsQueue.Text;
  end;

  if ArgsExchange <> nil then
  begin
    FArgsExchange := TStringList.Create;
    FArgsExchange.Text := ArgsExchange.Text;
  end;

  if ArgsBind <> nil then
  begin
    FArgsBind := TStringList.Create;
    FArgsBind.Text := ArgsBind.Text;
  end;

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

  if Assigned(FArgsQueue) then
    FArgsQueue.Free;

  if Assigned(FArgsExchange) then
    FArgsExchange.Free;

  if Assigned(FArgsBind) then
    FArgsBind.Free;

  inherited;
end;

procedure TPublisherRMQ.InitRabbit();
begin
  FRabbit := TRabbitMQ.Create(FURL, FTimeout);
  try
    FRabbit.Connect();
    FDefaultChannel := FRabbit.CreateChannel();
    FDefaultChannel.ExchangeDeclare(FExchange, 'fanout', True, False, False, False,FArgsExchange);
    FDefaultChannel.QueueDeclare(FQueue, true, false, false, false,FArgsQueue);
    FDefaultChannel.QueueBind(FQueue, FKey, FExchange, false,FArgsBind);
  except
    on E: EConnectionRMQ do
      raise EPublisherConnFailed.Create(e.Message);
  end;
end;

procedure TPublisherRMQ.Publish(RequestParameters: IAPIParams;
  const AMessageID:string);
var
  stream : TMemoryStream;
begin
  CheckConnection();
  stream := TMemoryStream.Create();
  try
    RequestParameters.ApplyParams(stream);
    FDefaultChannel.Publish(FExchange,FKey,False,False,AMessageID,stream);
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
