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

 //typedef struct { void *data; GoInt len; GoInt cap; } GoSlice;
  GoSlice = record
  	Data: Pointer;
    Len: GoInt;
    Cap: GoInt;
  end;


	function InitLog(AFileName:GoString):Byte;cdecl;external 'rmq.dll' name 'InitLog';
    procedure PrintLog(S:GoString);cdecl;external 'rmq.dll' name 'PrintLog';
    procedure CloseLog();cdecl;external 'rmq.dll' name 'CloseLog';


    function Connect(URL: GoString;b:LongBool):GoUintptr;cdecl;external 'rmq.dll' name 'Connect';
    procedure Disconnect(conn:GoUintptr);cdecl;external 'rmq.dll' name 'Disconnect';
    function Connected(conn:GoUintptr):LongBool;cdecl;external 'rmq.dll' name 'Connected';

    function NewChannel(conn:GoUintptr):GoUintptr;cdecl;external 'rmq.dll' name 'NewChannel';
    procedure CloseChannel(channel:GoUintptr);cdecl;external 'rmq.dll' name 'CloseChannel';

    function QueueDeclare(channel:GoUintptr;name:GoString;
       durable,autoDelete,exclusive,noWait:GoUint8):GoUint8;cdecl;
       external 'rmq.dll' name 'QueueDeclare';

    function ExchangeDeclare(channel:GoUintptr;name,kind:GoString;
    	durable, autoDelete, internal, noWait:GoUint8):GoUint8;cdecl;
       external 'rmq.dll' name 'ExchangeDeclare';

    function QueueBind(channel:GoUintptr;name,key,exchange:GoString;
        noWait:GoUint8):GoUint8;cdecl;
       external 'rmq.dll' name 'QueueBind';

    function Publish(channel:GoUintptr;exchange, key :GoString;
       mandatory, immediate:GoUint8; data:GoSlice):GoUint8;cdecl;
       external 'rmq.dll' name 'Publish';


 
    procedure ReturnGCObject(ptr:GoUintptr);cdecl;
    	external 'rmq.dll' name 'FreeObject';
          (*

extern GoUint8 Publish(GoUintptr p0, GoString p1, GoString p2, GoUint8 p3, GoUint8 p4, GoSlice p5);


          *)


function StrToGoString(const S:string):GoString;

implementation

function StrToGoString(const S:string):GoString;
var
  utfs:UTF8String;
  Temp : GoString;
begin
  if S='' then
  begin
    Result.S := nil;
    Result.Size := 0;
    exit;
  end;
  utfs:=AnsiToUtf8(S);
  Temp.Size := Length(utfs);
  Temp.S := addr(utfs[1]);  Result := Temp;end;end.
