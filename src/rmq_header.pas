unit rmq_header;

interface
uses Classes,SysUtils;

type
  GoInt = Integer;
  GoUint8 = Byte;
  
  PGoString = ^GoString;
  GoString = record
    S:Pointer;
    Size:GoInt;
  end;
  GoUintptr = Cardinal;


  GoSlice = record
  	Data: Pointer;
    Len: GoInt;
    Cap: GoInt;
  end;


  function InitLog(AFileName:GoString):Byte;cdecl;external 'rmq.dll' name 'InitLog';
  procedure PrintLog(S:GoString);cdecl;external 'rmq.dll' name 'PrintLog';
  procedure CloseLog();cdecl;external 'rmq.dll' name 'CloseLog';


  function ConnectRMQ(URL: GoString;TimeOut:GoInt):GoUintptr;cdecl;external 'rmq.dll' name 'Connect';
  procedure DisconnectRMQ(conn:GoUintptr);cdecl;external 'rmq.dll' name 'Disconnect';
  function ConnectedRMQ(conn:GoUintptr):LongBool;cdecl;external 'rmq.dll' name 'Connected';

  function NewChannelRMQ(conn:GoUintptr):GoUintptr;cdecl;external 'rmq.dll' name 'NewChannel';
  procedure CloseChannelRMQ(channel:GoUintptr);cdecl;external 'rmq.dll' name 'CloseChannel';

  function QueueDeclareRMQ(channel:GoUintptr;name:GoString;
     durable,autoDelete,exclusive,noWait:GoUint8;Args:GoUintptr):GoUint8;cdecl;
     external 'rmq.dll' name 'QueueDeclare';

  function ExchangeDeclareRMQ(channel:GoUintptr;name,kind:GoString;
      durable, autoDelete, internal, noWait:GoUint8;Args:GoUintptr):GoUint8;cdecl;
     external 'rmq.dll' name 'ExchangeDeclare';

  function QueueBindRMQ(channel:GoUintptr;name,key,exchange:GoString;
      noWait:GoUint8;Args:GoUintptr):GoUint8;cdecl;
     external 'rmq.dll' name 'QueueBind';

  function PublishRMQ(channel:GoUintptr;exchange, key :GoString;
     mandatory, immediate:GoUint8;MessageID:GoString;data:GoSlice):GoUint8;cdecl;
     external 'rmq.dll' name 'Publish';



  procedure ReturnGCObjectRMQ(ptr:GoUintptr);cdecl;
      external 'rmq.dll' name 'FreeObject';

  function MapArgs():GoUintptr;cdecl;external 'rmq.dll' name 'MapArgs';
  procedure MapArgsAdd(Map:GoUintptr;key,value:GoString);cdecl;
  	external 'rmq.dll' name 'MapArgsAdd';



function StrToGoString(const S:string):GoString;

procedure StrToGoString2(const S:string;var goS:GoString);
procedure DisposeGoString(S:GoString);

implementation

procedure DisposeGoString(S:GoString);
begin
  if (S.S<>nil) and (S.Size<>0) then
    FreeMem(S.S)
end;

procedure StrToGoString2(const S:string;var goS:GoString);
var
  utfs:UTF8String;
  Temp : GoString;
begin
  if S='' then
  begin
    goS.S := nil;
    goS.Size := 0;
    exit;
  end;
  utfs:=AnsiToUtf8(S);
  goS.S:=AllocMem(Length(utfs));
  goS.Size := Length(utfs);
  Move(utfs[1], goS.S^, Length(utfs));
end;


function StrToGoString(const S:string):GoString;
var
  utfs:UTF8String;
begin
  if S='' then
  begin
    Result.S := nil;
    Result.Size := 0;
    exit;
  end;
  utfs:=AnsiToUtf8(S);
  Result.S:=AllocMem(Length(utfs));
  Result.Size := Length(utfs);
  Move(utfs[1], Result.S^, Length(utfs));
end;end.
