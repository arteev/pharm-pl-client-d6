unit publisher;

interface
uses Classes, SysUtils,api_template;

type
  EPublisherConnFailed = class(Exception);
  IPurchasePublisher=interface
  	procedure Publish(RequestParameters: IAPIParams;const AMessageID:string);
    procedure Reconnect;
  end;
implementation

end.
