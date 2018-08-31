program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, zcomponent, ikscomponents,
  suites, main;

{$R *.res}

begin
  Application.Title:='Compiler Test Configurations Manager';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm_Configurations, Form_Configurations);
  Application.Run;
end.

