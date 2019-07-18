unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,rmq_header, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    edtURL: TEdit;
    btnConnect: TButton;
    lbl1: TLabel;
    btnDisconn: TButton;
    tmr1: TTimer;
    lblStatus: TLabel;
    btnNEwChannel: TButton;
    btnCloseChannel: TButton;
    edtPublish: TEdit;
    btnPublish: TButton;
    btn1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnClick(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure btnNEwChannelClick(Sender: TObject);
    procedure btnCloseChannelClick(Sender: TObject);
    procedure btnPublishClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    procedure CheckPTR(ptr:GoUintptr);
  public
    { Public declarations }
    conn:GoUintptr;
    channel :GoUintptr;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  name : GoString;
begin
  name := StrToGoString('/home/inf/log/cmn.log');
  InitLog(name);
  PrintLog(StrToGoString('Test: Это тест!'));
 // CloseLog();
end;

procedure TForm1.btnConnectClick(Sender: TObject);
var
  conStr:GoString;
begin
  conStr:=StrToGoString(edtURL.Text);
  conn:=Connect(conStr,true);
  CheckPTR(conn)
end;

procedure TForm1.btnDisconnClick(Sender: TObject);
begin
  Disconnect(conn);
  ReturnGCObject(conn);
end;

procedure TForm1.CheckPTR(ptr: GoUintptr);
begin
  if ptr=0 then
	raise Exception.Create('Wrong GoUintptr');
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  if Connected(conn) then
    lblStatus.Caption := 'Connected'
  else
    lblStatus.Caption := 'Not connected'


end;

procedure TForm1.btnNEwChannelClick(Sender: TObject);
var
  exchangeName:GoString;
  kind : GoString;
  qname: GoString;
  emString:GoString;
begin
  channel := NewChannel(conn);
  CheckPTR(channel);
   StrToGoString2('logs',exchangeName);
   StrToGoString2('fanout',kind);
   StrToGoString2('my',qname);
  StrToGoString2('',emString);

  try
  if ExchangeDeclare(channel,
  	exchangeName,
    kind,
    1,0,0,0)=0 then
    raise Exception.Create('Failed ExchangeDeclare');

  if QueueDeclare(channel,
  	qname,
    1,0,0,0)=0 then
    raise Exception.Create('Failed QueueDeclare');

  if QueueBind(channel,
    	qname,emString,exchangeName,0)=0 then
    raise Exception.Create('Failed QueueBind');
  finally
  	DisposeGoString(exchangeName);
 	DisposeGoString(kind);
	DisposeGoString(qname);
    DisposeGoString(emString);
  end;
  
end;

procedure TForm1.btnCloseChannelClick(Sender: TObject);
begin
  CloseChannel(channel);
  ReturnGCObject(channel);
end;

procedure TForm1.btnPublishClick(Sender: TObject);
var
  data:GoSlice;
  utfs:UTF8String;
  exchangeName :GoString;
  emString:GoString;
begin
  StrToGoString2('logs',exchangeName);
  StrToGoString2('',emString);
  try
    utfs:=AnsiToUtf8(edtPublish.Text);
    data.Data :=AllocMem(Length(utfs));
    Move(utfs[1],data.Data^,Length(utfs));
    data.Len := Length(utfs);
    data.Cap := data.Len;
    if Publish(channel, exchangeName,emString,0,0,data)=0 then
         raise Exception.Create('Failed Publish');
  finally
    if data.Data<>nil then
    	FreeMem(data.Data);
    DisposeGoString(exchangeName);
    DisposeGoString(emString);
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
var
  exchangeName:GoString;
  kind : GoString;
  qname: GoString;
  emString:GoString;
  Temp:WideString;
  s:UTF8String;
begin
  StrToGoString2('logs2',exchangeName);
  StrToGoString2('fanout',kind);
  StrToGoString2('my2',qname);
  StrToGoString2('',emString);


  SetLength(s,exchangeName.Size);
  FillChar(s[1],exchangeName.Size,#0);
  CopyMemory(@s[1],exchangeName.S,exchangeName.Size);
  ShowMessage(Utf8ToAnsi(s) );

  SetLength(s,qname.Size);
  FillChar(s[1],qname.Size,#0);
  CopyMemory(@s[1],qname.S,qname.Size);
  ShowMessage(Utf8ToAnsi(s) );

  DisposeGoString(exchangeName);
  DisposeGoString(kind);
  DisposeGoString(qname);
  DisposeGoString(emString);
end;

end.
