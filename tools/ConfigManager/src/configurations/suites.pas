unit suites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, DBGrids, Menus, ActnList, ZDataset, db;

type

  { TForm_Suites }

  TForm_Suites = class(TForm)
    DataSource_SuitesModify: TDataSource;
    DBCheckBox_Do_Rebuild: TDBCheckBox;
    DBComboBox_Delimeter_Type: TDBComboBox;
    DBEdit_Shortname: TDBEdit;
    DBEdit_Username: TDBEdit;
    DBEdit_Password: TDBEdit;
    DBEdit_Port: TDBEdit;
    DBEdit_Database: TDBEdit;
    DBEdit_Host: TDBEdit;
    DBEdit_Protocol: TDBEdit;
    DBEdit_Suitename: TDBEdit;
    DBEdit_Delimeter: TDBEdit;
    DBGrid1: TDBGrid;
    DBMemo_Properties: TDBMemo;
    DBMemo_Create: TDBMemo;
    DBMemo_Drop: TDBMemo;
    DBNavigator1: TDBNavigator;
    L_Shortname: TLabel;
    L_Delimiter: TLabel;
    L_Protocol: TLabel;
    L_Drop: TLabel;
    L_Properties: TLabel;
    L_DelimiterType: TLabel;
    L_Host: TLabel;
    L_Database: TLabel;
    L_Port: TLabel;
    L_Username: TLabel;
    L_Password: TLabel;
    L_Rebuild: TLabel;
    L_Suitename: TLabel;
    L_Create: TLabel;
    ZQuery_SuitesModify: TZQuery;
    procedure FormClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ZQuery_SuitesModifyAfterOpen(DataSet: TDataSet);
    procedure ZQuery_SuitesModifyBeforePost();
  private

  public

  end;

var
  Form_Suites: TForm_Suites;

implementation

{$R *.lfm}

uses
  main;
{ TForm_Suites }

procedure TForm_Suites.ZQuery_SuitesModifyAfterOpen(DataSet: TDataSet);
begin
  Dataset.FieldByName('SUITE_ID').visible := false;
  Dataset.FieldByName('SUITENAME').visible := false;
  Dataset.FieldByName('PROTOCOL').visible := false;
  Dataset.FieldByName('HOST').visible := false;
  Dataset.FieldByName('PORT').visible := false;
  Dataset.FieldByName('DATABASE').visible := false;
  Dataset.FieldByName('PASSWORD').visible := false;
  Dataset.FieldByName('DO_REBUILD').visible := false;
  Dataset.FieldByName('USERNAME').visible := false;
  Dataset.FieldByName('PROPERTIES').visible := false;
  Dataset.FieldByName('CREATE_SCRIPTS').visible := false;
  Dataset.FieldByName('DROP_SCRIPTS').visible := false;
  Dataset.FieldByName('DELIMITER').visible := false;
  Dataset.FieldByName('DELIMITER_TYPE').visible := false;
end;

procedure TForm_Suites.ZQuery_SuitesModifyBeforePost();
const
  forbidden_chars : Array[0..13] of Char = ' .,:;?\/*<>|"' + chr(39);
var
  index : Integer;
  count : Integer;
  hit : Integer;
begin
  if (DBEdit_Suitename.Field.Value = null) or (DBEdit_Suitename.Field.Value = '') then begin
    ShowMessage('Data not saved! Field <Suitename> must not be empty!');
    Abort;
    //exit;
  end;

  count := 0;
  hit := 0;
  for index:=0 to 13 do begin
    hit := Pos(forbidden_chars[index], DBEdit_Suitename.Field.Value);
    if hit > 0 then inc(count);
  end;

  if count > 0 then begin
    ShowMessage('Data not saved! ' + count.ToString + ' illegal characters used in <Suitename>');
    Abort;
  end
end;

procedure TForm_Suites.FormCreate(Sender: TObject);
begin
  ZQuery_SuitesModify.Open;
end;

procedure TForm_Suites.FormClose(Sender: TObject);
begin
  Form_Configurations.Show;
end;


end.

