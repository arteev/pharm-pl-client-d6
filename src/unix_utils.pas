unit unix_utils;

 interface
 uses Classes, Windows, SysUtils;

 function UnixToDateTime(USec: Longint): TDateTime;
 function DateTimeToUnix(ConvDate: TDateTime): Longint;
 function NowUTC: TDateTime;

 implementation

 const
   // Sets UnixStartDate to TDateTime of 01/01/1970
  UnixStartDate: TDateTime = 25569.0;

 function DateTimeToUnix(ConvDate: TDateTime): Longint;
 begin
   //example: DateTimeToUnix(now); 
  Result := Round((ConvDate - UnixStartDate) * 86400);
 end;

 function UnixToDateTime(USec: Longint): TDateTime;
 begin
   //Example: UnixToDateTime(1003187418); 
  Result := (Usec / 86400) + UnixStartDate;
 end;


function NowUTC: TDateTime;
var
  system_datetime: TSystemTime;
begin
  GetSystemTime(system_datetime);
  Result := SystemTimeToDateTime(system_datetime);
end;

 end.
