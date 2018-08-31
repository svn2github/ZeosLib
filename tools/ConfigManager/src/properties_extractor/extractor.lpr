program extractor;

{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },
  ZConnection, ZDataset, zcomponent, Interfaces, ikshelper;

type

  { PropertiesExtractor }

  PropertiesExtractor = class(TCustomApplication)
    procedure CreateProperties;
    procedure DeleteFiles(extension: String);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure ExitWithText(msg: String);
  private

  end;

{ PropertiesExtractor }

procedure PropertiesExtractor.ExitWithText(msg: String);
begin
  writeln('[Compiler Test Script Generator]: '+msg);
  System.ExitCode:=1337;
//(*<DEBUG>*)ReadLn;(*</DEBUG>*)
  Terminate;
end;

procedure PropertiesExtractor.DeleteFiles(extension: String);
var
  mysearch: TSearchRec;
begin
  //writeln('Deleting files with extension: '+extension);
  FindFirst(GetCurrentDir +  '\*.' + extension, faAnyFile+faReadOnly, mysearch);
  DeleteFile(GetCurrentDir +  '\' + mysearch.Name);
  while FindNext(mysearch)=0 do
    DeleteFile(GetCurrentDir +  '\' + mysearch.Name);
  FindClose(mysearch);
end;

procedure PropertiesExtractor.CreateProperties;
var
  suite: String;
  ZCon: TZConnection;
  ZReadQuery: TZReadOnlyQuery;
  cID: Longint;
  testexe: String;

  configpath : String;
begin
  {$IFDEF WINDOWS}
    configpath:= ExtractFilePath(ParamStr(0)) + 'connection.ini';
  {$ELSE}
    configpath:= '/etc/compilertest/connection.ini';
  {$ENDIF}


  ZCon := TZConnection.Create(nil);
  //writeln('Loading Configurations Database');
  if not FileExists(configpath) then begin
    with TStringList.Create do begin
      try
        Add('[Database]');
        Add('Protocol=firebird');
        Add('Name=');
        Add('Host=localhost');
        Add('username=SYSDBA');
        Add('password=');
        Add('library=');
        Add('Port=3050');
        SaveToFile(configpath);
      finally
        Free;
      end;
    end;
    ExitWithText('connection.ini not found! Creating default file, please fill in the required information!');
  end
  else begin
    with TFBHelper.Create(nil) do
      try
        LoadConfig(configpath);
        ConfigConnection(ZCon);
      finally
        Free;
      end;
  end;

  if not FileExists(ZCon.Database) then begin
    ExitWithText('Database not found!');
  end;

  ZCon.Connect;
  ZReadQuery := TZReadOnlyQuery.Create(nil);
  ZReadQuery.Connection:=ZCon;
  ZReadQuery.SQL.Text:='SELECT * FROM COMPILERS';
  ZReadQuery.Open;
  if ZReadQuery.Locate('NAME', GetOptionValue('c', 'compiler'), []) then begin //Check if compiler found
    cID := ZReadQuery.FieldByName('COMPILER_ID').AsInteger;
    ZReadQuery.Close;
    ZReadQuery.SQL.Text:='SELECT SUITES.* FROM SUITES JOIN PAIRING ON SUITES.SUITE_ID = PAIRING.SUITE_ID WHERE COMPILER_ID =' + cID.ToString;
    ZReadQuery.Open;
    if ZReadQuery.EOF then ExitWithText('No active suites found for this compiler!');
    DeleteFiles('properties');
    FileClose(FileCreate( GetCurrentDir + '\test.basic.properties'));
    testexe:= GetOptionValue('e', 'exe');
    with TStringlist.Create do begin //Write batch file
      try
        Add('@echo off');
        Add('');
        Add('echo testing basic stuff...');
        Add(testexe + ' -c test.basic.properties --xml=test.basic.xml --suite=core,parsesql --SuiteName=basic');
        Add('IF %errorlevel% NEQ 0 GOTO :error');
        Add('');

        while not ZReadQuery.EOF do begin //parse through activated suites
          //writeln('Processing data unit ' + ZReadQuery.FieldByName('SUITENAME').AsString);
          suite:= ZReadQuery.FieldByName('SUITENAME').AsString;

          //batch part
          Add('echo testing '+suite+'...');
          Add(testexe + ' -c test.'+suite+'.properties --xml=test.'+suite+'.xml --suite=dbc,component,bugreport --SuiteName='+suite);
          Add('IF %errorlevel% NEQ 0 GOTO :error');

          //properties part
          with TStringList.Create do //write properties file
            try
              Add(';==================================');
              Add(';File created automatically.');
              Add('');
              Add('[common]');
              Add('common.connections='+suite);
              Add('common.scriptpath='+GetOptionValue('s', 'scriptpath'));
              Add('');
              Add(';============ Variables ===========');
              Add('');
              Add('['+suite+']');
              Add('');
              Add(suite+'.protocol='+ZReadQuery.FieldByName('PROTOCOL').AsString);
              Add(suite+'.host='+ZReadQuery.FieldByName('HOST').AsString);
              Add(suite+'.port='+ZReadQuery.FieldByName('PORT').AsString);
              Add(suite+'.database='+ZReadQuery.FieldByName('DATABASE').AsString);
              Add(suite+'.user='+ZReadQuery.FieldByName('USERNAME').AsString);
              Add(suite+'.password='+ZReadQuery.FieldByName('PASSWORD').AsString);
              Add(suite+'.rebuild='+ZReadQuery.FieldByName('DO_REBUILD').AsString);
              Add(suite+'.delimiter.type='+ZReadQuery.FieldByName('DELIMITER_TYPE').AsString);
              Add(suite+'.delimiter='+ZReadQuery.FieldByName('DELIMITER').AsString);
              Add(suite+'.create.scripts='+ZReadQuery.FieldByName('CREATE_SCRIPTS').AsString.Replace(LineEnding, ','));
              Add(suite+'.drop.scripts='+ZReadQuery.FieldByName('DROP_SCRIPTS').AsString.Replace(LineEnding, ','));
              Add(suite+'.properties='+ZReadQuery.FieldByName('PROPERTIES').AsString.Replace(LineEnding, ';'));
              SaveToFile(GetCurrentDir + '\test.'+suite+'.properties');
              ZReadQuery.Next;
            finally
              Free;
            end; //properties file written
        end;//while not EOF
        Add('goto :end');
        Add(':error');
        Add('echo "Some Error occured"');
        Add('exit 1');
        Add(':end');
        Add('echo "The End"');
        SaveToFile(GetCurrentDir + '\' + GetOptionValue('b','batch'));
      finally //batch written
        Free;
      end; //all files written
    end;
    end//if compiler found
  else begin
    ExitWithText('Compiler not found!');
  end;//if-else compiler found
  ZReadQuery.Close;
  ZCon.Disconnect;
  ZReadQuery.Free;
  ZCon.Free;
end;

procedure PropertiesExtractor.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hc:b:e:s:', 'help compiler: batch: exe: scriptpath:');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    ExitWithText('Wrong Syntax');
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  if HasOption('c', 'compiler') and HasOption('b', 'batch') and HasOption('e', 'exe') and HasOption('s', 'scriptpath') then begin
    if FileExists(GetOptionValue('e', 'exe')) then CreateProperties
    else ExitWithText('Test-Executable not found at '+GetOptionValue('e','exe'));
    end
  else begin
    WriteHelp;
    ExitWithText('Missing parameter!');
    Exit;
  end;

  // stop program loop
  Terminate;
end;

constructor PropertiesExtractor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor PropertiesExtractor.Destroy;
begin
  inherited Destroy;
end;

procedure PropertiesExtractor.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -c <compiler name> -b <batch file name> -e <path to test.exe> -s <sql-script-path>');
end;

var
  Application: PropertiesExtractor;

{$R *.res}

begin
  Application:=PropertiesExtractor.Create(nil);
  Application.Title:='Compiler Test Script Generator';
  Application.Run;
  Application.Free;
end.

