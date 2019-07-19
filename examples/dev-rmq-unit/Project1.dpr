program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  rmq_header in '..\..\src\rmq_header.pas',
  rmq_wrapper in '..\..\src\rmq_wrapper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
