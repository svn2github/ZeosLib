unit main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, DbCtrls, CheckLst, DBGrids, ZConnection, ZDataset,
  ZSequence, db;

type

  { TForm_Configurations }

  TForm_Configurations = class(TForm)
    OpenSuites: TButton;
    B_Save: TButton;
    B_Reset: TButton;
    Checklistbox_SuitesToUse: TCheckListBox;
    ComboBox1: TComboBox;
    DataSource_EditConfigurations: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    ZConnectionMain: TZConnection;
    ZQuery_EditConfigurations: TZQuery;
    ZQuery_Insert: TZQuery;
    ZQuery_Pairing: TZQuery;
    ZQuery_Suites: TZQuery;
    ZSequenceGeneric: TZSequence;
    procedure B_ResetClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadConfigsAndSuites();
    procedure LoadIni;
    procedure OnSelectionChange();
    procedure OpenSuitesClick(Sender: TObject);
    procedure ZQuery_EditConfigurationsAfterOpen(Dataset: TDataset);
    procedure ZQuery_EditConfigurationsAfterScroll();
    procedure ZQuery_EditConfigurationsBeforeDelete(DataSet: TDataSet);
  private
  public

  end;

var
  Form_Configurations: TForm_Configurations;

implementation

{$R *.lfm}

{ TForm_Configurations }

uses
  suites, ikshelper;

procedure TForm_Configurations.OnSelectionChange();
var
  index : Integer;
begin
  Checklistbox_SuitesToUse.CheckAll(cbUnchecked);
  ZQuery_Pairing.SQL.Text := 'SELECT PAIRING.SUITE_ID, SUITES.SHORTNAME FROM PAIRING JOIN SUITES ON PAIRING.SUITE_ID = SUITES.SUITE_ID WHERE COMPILER_ID = :CID';
  ZQuery_Pairing.ParamByName('CID').Value := ZQuery_EditConfigurations.FieldByName('COMPILER_ID').Value;
  ZQuery_Pairing.Open;

  while not ZQuery_Pairing.EOF do begin
    index :=  Checklistbox_SuitesToUse.Items.IndexOf(ZQuery_Pairing.FieldByName('SHORTNAME').AsString);
    if index < 0 then exit;
    Checklistbox_SuitesToUse.State[index]:= cbChecked;
    ZQuery_Pairing.Next;
  end;
  ZQuery_Pairing.Close;
end;

procedure TForm_Configurations.OpenSuitesClick(Sender: TObject);
begin
  Form_Configurations.Hide;
  Form_Suites.ShowModal;
end;

procedure TForm_Configurations.ZQuery_EditConfigurationsAfterOpen(Dataset: TDataset);
begin
  Dataset.FieldByName('COMPILER_ID').visible := false;
end;

procedure TForm_Configurations.ZQuery_EditConfigurationsAfterScroll();
begin
  OnSelectionChange();
end;

procedure TForm_Configurations.ZQuery_EditConfigurationsBeforeDelete(
  DataSet: TDataSet);
begin
  ZQuery_Pairing.SQL.Text := 'DELETE FROM PAIRING WHERE COMPILER_ID = :CID';
  ZQuery_Pairing.ExecSQL;
end;

procedure TForm_Configurations.LoadIni;
var
  configpath: TFileName;
begin
  {$IFDEF WINDOWS}
    configpath:= ExtractFilePath(ParamStr(0)) + 'connection.ini';
  {$ELSE}
    configpath:= '/etc/compilertest/connection.ini';
  {$ENDIF}

  if not FileExists(configpath) then
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
    end
  else begin
    with TFBHelper.Create(nil) do
      try
        LoadConfig(configpath);
        ConfigConnection(ZConnectionMain);
      finally
        Free;
      end;
  end;
end;

procedure TForm_Configurations.FormCreate(Sender: TObject);
begin
  LoadIni;
  if not FileExists(ZConnectionMain.Database) then begin
    ShowMessage('Database not found!');
    Application.Terminate;
  end;
  ZConnectionMain.Connect;
  ZQuery_EditConfigurations.Open;
  LoadConfigsAndSuites();
  Application.CreateForm(TForm_Suites, Form_Suites);
end;

procedure TForm_Configurations.FormShow(Sender: TObject);
begin
  LoadConfigsAndSuites();
  OnSelectionChange();
end;

procedure TForm_Configurations.LoadConfigsAndSuites();
begin
  ZQuery_Suites.Close;
  ZQuery_Suites.Open;

  Checklistbox_SuitesToUse.Clear;

  while not ZQuery_Suites.EOF do begin
    Checklistbox_SuitesToUse.Items.Add(ZQuery_Suites.FieldByName('SHORTNAME').AsString);
    ZQuery_Suites.Next;
  end;
  ZQuery_Suites.Close;
end;

procedure TForm_Configurations.B_ResetClick(Sender: TObject);
begin
  ZQuery_Insert.CancelUpdates;
  OnSelectionChange();
end;

procedure TForm_Configurations.B_SaveClick(Sender: TObject);
var
  index: integer;
begin
  ZQuery_Pairing.SQL.Text := 'DELETE FROM PAIRING WHERE COMPILER_ID = :CID';
  ZQuery_Pairing.ExecSQL;
  ZQuery_Insert.ParamByName('CID').AsInteger := ZQuery_EditConfigurations.FieldByName('COMPILER_ID').Value;
  ZQuery_Suites.Open;

  for index:=0 to Checklistbox_SuitesToUse.Count-1 do begin
    if (Checklistbox_SuitesToUse.State[index] = cbChecked) AND (ZQuery_Suites.Locate('SHORTNAME', Checklistbox_SuitesToUse.Items[index],[]))
    then begin
      ZQuery_Insert.ParamByName('ID').AsInteger:=ZSequenceGeneric.GetNextValue;
      ZQuery_Insert.ParamByName('SU_ID').AsInteger:=ZQuery_Suites.FieldByName('SUITE_ID').AsInteger;
      ZQuery_Insert.ExecSQL;
    end;
  end;
  ZQuery_Suites.Close;
end;

end.

