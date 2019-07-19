unit rmq_wrapper;

interface
uses Classes,SysUtils,Windows,rmq_header;

var
  BoolToGoBool: array[Boolean] of GoUint8 = (0,1);
type
  EConnectionRMQ =class(Exception);
  EChannelNotFound = class(Exception);
  EChannelCreate =class(Exception);
  EChannelDecrare = class(Exception);


  TRabbitMQ = class;

  TChannelMQ=class(TObject)
  private
    FConnected: Boolean;
    FChannel:GoUintptr;
    FConnection:TRabbitMQ;
  protected
    procedure Disconnected();
    property Handle:GoUintptr read FChannel;
  public
    constructor Create(connection:TRabbitMQ);
    destructor Destroy;override;
    procedure Close();


    procedure QueueDeclare(const AName:string;
      const durable,autoDelete,exclusive,noWait:boolean);
    procedure ExchangeDeclare(const AName,AKind:string;
      const durable, autoDelete, internal, noWait:Boolean);
    procedure QueueBind(const AName,AKey,AExchange:string;const noWait:Boolean);
    procedure Publish(const AExchange,Akey:string;
      const mandatory,immediate:Boolean;
      const AStream:TStream);
    property Connected:Boolean read FConnected;
  end;

  TRabbitMQ = class(TObject)
  private
    FURL:string;
    FConnection:GoUintptr;
    FChannels: TList;
    FTimeOut:Integer;
    function GetConnected: Boolean;
    function GetChannelMQ(index: Integer): TChannelMQ;
    function GetCountChannels: Integer;
  protected
	procedure DeleteChannel(channel:TChannelMQ);

    property Handle:GoUintptr read FConnection;

  public
    constructor Create(const AURL:string;const ATimeOut:Integer);
    destructor Destroy;override;

    procedure Connect();
    procedure Disconnect();

    function CreateChannel():TChannelMQ;
    procedure CloseAllChannels();

    property Connected:Boolean read GetConnected;
    property URL:string read FURL;
    property CountChannels:Integer read GetCountChannels;
    property Channels[index:Integer]:TChannelMQ read GetChannelMQ; 
  end;

implementation

{ TRabbitMQ }

procedure TRabbitMQ.CloseAllChannels;
var
  i:integer;
  channel:TChannelMQ;
begin
  while FChannels.Count<>0 do
  begin
    channel:=TChannelMQ(FChannels.Last);
    channel.Free;
  end;
end;

procedure TRabbitMQ.Connect;
var
  gURL : GoString;
begin
  if Connected then Exit;
  gURL:=StrToGoString(FURL);
  try
  	FConnection:=ConnectRMQ(gURL,FTimeOut);
    if FConnection=0 then
    	EConnectionRMQ.Create('not connected');
  finally
    DisposeGoString(gURL);
  end;
end;

constructor TRabbitMQ.Create(const AURL: string;const ATimeOut:Integer);
begin
  Self.FURL:=AURL;
  FChannels:=TList.Create;
  FTimeOut := ATimeOut;
end;

function TRabbitMQ.CreateChannel: TChannelMQ;
var
  channel : TChannelMQ;
begin
  if FConnection=0 then
    EConnectionRMQ.Create('not connected');
  channel := TChannelMQ.Create(Self);
  FChannels.Add(channel);
  Result := channel;
end;

procedure TRabbitMQ.DeleteChannel(channel: TChannelMQ);
var idx:Integer;
begin
  idx:=FChannels.IndexOf(channel);
  if idx=-1 then exit;
  FChannels.Delete(idx);
end;

destructor TRabbitMQ.Destroy;
begin
  Self.Disconnect();
  CloseAllChannels;
  FChannels.Free;
  inherited;
end;

procedure TRabbitMQ.Disconnect;
var i:Integer;
begin
  if FConnection=0 then Exit;
  DisconnectRMQ(FConnection);
  ReturnGCObjectRMQ(FConnection);
  FConnection:=0;
  for i:=0 to CountChannels-1 do
    Channels[i].Disconnected();
end;

function TRabbitMQ.GetChannelMQ(index: Integer): TChannelMQ;
begin
  Result := TChannelMQ(FChannels.Items[index]);
end;

function TRabbitMQ.GetConnected: Boolean;
begin
  Result := ConnectedRMQ(FConnection)
end;


function TRabbitMQ.GetCountChannels: Integer;
begin
  Result := FChannels.Count;
end;

{ TChannelMQ }

procedure TChannelMQ.Close;
begin
  if FChannel=0 then exit;
  CloseChannelRMQ(FChannel);
  ReturnGCObjectRMQ(FChannel);
  FChannel := 0;
end;

constructor TChannelMQ.Create(connection:TRabbitMQ);
begin
  FConnection := connection;
  Self.FChannel:=NewChannelRMQ(connection.FConnection);
  if FChannel=0 then
  	raise EChannelCreate.Create('could not create channel');
  FConnected:=True;

end;

destructor TChannelMQ.Destroy;
begin
  Close();
  FConnection.DeleteChannel(self);
  inherited;
end;

procedure TChannelMQ.Disconnected;
begin
  FConnected:=false;
  Close();
end;

procedure TChannelMQ.ExchangeDeclare(const AName, AKind: string;
  const durable, autoDelete, internal, noWait: Boolean);
var
  gName,gKind:GoString;
begin
  gName := StrToGoString(AName);
  gKind := StrToGoString(AKind);
  try
    if ExchangeDeclareRMQ(FChannel,gName,gKind,
        BoolToGoBool[durable],
        BoolToGoBool[autoDelete],
        BoolToGoBool[internal],
        BoolToGoBool[noWait]) = 0 then
        raise EChannelDecrare.CreateFmt('could not ExchangeDeclare: %s',[AName]);
  finally
    DisposeGoString(gName);
    DisposeGoString(gKind);
  end;
end;

procedure TChannelMQ.Publish(const AExchange, Akey: string;
  const mandatory, immediate: Boolean; const AStream: TStream);
var
  gKey,gExchange:GoString;
  data:GoSlice;
begin
  gKey := StrToGoString(AKey);
  gExchange :=StrToGoString(AExchange);
  try
  	data.Len := AStream.Size;
    data.Cap := data.Len;
    data.Data :=AllocMem(AStream.Size);
    AStream.Position:=0;
    AStream.Read(data.Data^,AStream.Size);
    if PublishRMQ(FChannel,gExchange,gKey,
      BoolToGoBool[mandatory],
      BoolToGoBool[immediate],
      data) = 0 then
      raise EChannelDecrare.CreateFmt('could not Publish: %s (%s)',[AExchange,AKey]);
  finally
    if data.Data<>nil then
      FreeMem(data.Data);
    DisposeGoString(gKey);
    DisposeGoString(gExchange);
  end;

end;

procedure TChannelMQ.QueueBind(const AName, AKey, AExchange: string;
  const noWait: Boolean);
var
  gName,gKey,gExchange:GoString;
begin
  gName := StrToGoString(AName);
  gKey := StrToGoString(AKey);
  gExchange :=StrToGoString(AExchange);
  try
    if QueueBindRMQ(FChannel,gName,gKey,gExchange,BoolToGoBool[noWait]) = 0 then
      raise EChannelDecrare.CreateFmt('could not ExchangeDeclare: %s',[AName]);
  finally
    DisposeGoString(gName);
    DisposeGoString(gKey);
    DisposeGoString(gExchange);
  end;
end;

procedure TChannelMQ.QueueDeclare(const AName: string; const durable,
  autoDelete, exclusive, noWait: boolean);
var
  gName,gKind:GoString;
begin
  gName := StrToGoString(AName);
  try
    if QueueDeclareRMQ(FChannel,gName,
        BoolToGoBool[durable],
        BoolToGoBool[autoDelete],
        BoolToGoBool[exclusive],
        BoolToGoBool[noWait]) = 0 then
        raise EChannelDecrare.CreateFmt('could not QueueDeclare: %s',[AName]);
  finally
    DisposeGoString(gName);
  end;
end;

end.