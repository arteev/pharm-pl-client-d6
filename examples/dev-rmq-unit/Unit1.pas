unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,rmq_header, StdCtrls, ExtCtrls, rmq_wrapper;

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
    grpDirect: TGroupBox;
    grpWrapper: TGroupBox;
    btnWconn: TButton;
    btnWDiscon: TButton;
    lblWStatus: TLabel;
    Button1: TButton;
    btnFree: TButton;
    btnWCreateChannel: TButton;
    btnWCloseChannel: TButton;
    btnWCloseAllChannels: TButton;
    btnWPublish: TButton;
    edtWPublish: TEdit;
    btnWFreeChannel: TButton;
    lblWCountChannel: TLabel;
    btnWstart: TButton;
    tmr2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnClick(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure btnNEwChannelClick(Sender: TObject);
    procedure btnCloseChannelClick(Sender: TObject);
    procedure btnPublishClick(Sender: TObject);
    procedure btnWconnClick(Sender: TObject);
    procedure btnWDisconClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnFreeClick(Sender: TObject);
    procedure btnWCreateChannelClick(Sender: TObject);
    procedure btnWCloseChannelClick(Sender: TObject);
    procedure btnWFreeChannelClick(Sender: TObject);
    procedure btnWCloseAllChannelsClick(Sender: TObject);
    procedure btnWPublishClick(Sender: TObject);
    procedure tmr2Timer(Sender: TObject);
    procedure btnWstartClick(Sender: TObject);
  private
    { Private declarations }
    idMessage:integer;
    FWrap : TRabbitMQ;
    procedure CheckPTR(ptr:GoUintptr);

    procedure PubilshAll;
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
  conn:=ConnectRMQ(conStr,true);
  CheckPTR(conn)
end;

procedure TForm1.btnDisconnClick(Sender: TObject);
begin
  DisconnectRMQ(conn);
  ReturnGCObjectRMQ(conn);
end;

procedure TForm1.CheckPTR(ptr: GoUintptr);
begin
  if ptr=0 then
	raise Exception.Create('Wrong GoUintptr');
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  if ConnectedRMQ(conn) then
    lblStatus.Caption := 'Connected'
  else
    lblStatus.Caption := 'Not connected';

  if FWrap<>nil then
  begin
    lblWCountChannel.Caption := IntToStr(FWrap.CountChannels);
    if FWrap.Connected then
    	lblWStatus.Caption := 'Connected'
    else
    	lblWStatus.Caption := 'Disconnected'

  end
  else
    lblWStatus.Caption := '<NIL>' 

end;

procedure TForm1.btnNEwChannelClick(Sender: TObject);
var
  exchangeName:GoString;
  kind : GoString;
  qname: GoString;
  emString:GoString;
begin
  channel := NewChannelRMQ(conn);
  CheckPTR(channel);
   StrToGoString2('logs',exchangeName);
   StrToGoString2('fanout',kind);
   StrToGoString2('my',qname);
  StrToGoString2('',emString);

  try
  if ExchangeDeclareRMQ(channel,
  	exchangeName,
    kind,
    1,0,0,0)=0 then
    raise Exception.Create('Failed ExchangeDeclare');

  if QueueDeclareRMQ(channel,
  	qname,
    1,0,0,0)=0 then
    raise Exception.Create('Failed QueueDeclare');

  if QueueBindRMQ(channel,
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
  CloseChannelRMQ(channel);
  ReturnGCObjectRMQ(channel);
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
    if PublishRMQ(channel, exchangeName,emString,0,0,data)=0 then
         raise Exception.Create('Failed Publish');
  finally
    if data.Data<>nil then
    	FreeMem(data.Data);
    DisposeGoString(exchangeName);
    DisposeGoString(emString);
  end;
end;

procedure TForm1.btnWconnClick(Sender: TObject);
begin
  if FWrap=nil then exit;
  FWrap.Connect();
end;

procedure TForm1.btnWDisconClick(Sender: TObject);
begin
  if FWrap=nil then exit;
  FWrap.Disconnect();
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FWrap<>nil then exit;
  FWrap := TRabbitMQ.Create(edtURL.Text);
end;

procedure TForm1.btnFreeClick(Sender: TObject);
begin
  if FWrap<>nil then
  begin
    FWrap.Free;
    FWrap := nil;
  end;
end;

procedure TForm1.btnWCreateChannelClick(Sender: TObject);
var
  ch : TChannelMQ;
begin
  ch:=FWrap.CreateChannel();
  ch.ExchangeDeclare('logs','fanout',True,False,False,False);
  ch.QueueDeclare('my',True,False,False,False);
  ch.QueueBind('my','','logs',False);
end;

procedure TForm1.btnWCloseChannelClick(Sender: TObject);
begin
  if FWrap=nil then exit;
  FWrap.Channels[0].Close;
end;

procedure TForm1.btnWFreeChannelClick(Sender: TObject);
begin
  if FWrap=nil then exit;
  FWrap.Channels[0].Free;
end;

procedure TForm1.btnWCloseAllChannelsClick(Sender: TObject);
begin
  if FWrap=nil then exit;
  FWrap.CloseAllChannels;
end;

procedure TForm1.btnWPublishClick(Sender: TObject);
begin
  PubilshAll;
end;

procedure TForm1.tmr2Timer(Sender: TObject);
begin

  PubilshAll;
end;

procedure TForm1.btnWstartClick(Sender: TObject);
begin
  if tmr2.Enabled then
  begin
    tmr2.Enabled := False;
    btnWstart.Caption:= 'Start';
    exit;
  end;
  tmr2.Enabled := True;
  btnWstart.Caption:= 'Stop'
end;

procedure TForm1.PubilshAll;
var
  i: integer;
  ss: TStringStream;
begin
  if FWrap = nil then
    exit;
  idMessage:=idMessage+1;
  for i := 0 to FWrap.CountChannels - 1 do
  if FWrap.Channels[i].Connected then
  begin
    ss := TStringStream.Create(edtWPublish.Text +' id:'+intToStr(idMessage)+ ' channel:'+ IntToStr(i));
    try
      FWrap.Channels[0].Publish('logs', '', false, False, ss);
    finally
      ss.Free;
    end;

  end;

end;

end.
