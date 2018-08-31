unit editconfigs;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  DBGrids, ZDataset, db;

type

  { TForm_EditConfigurations }

  TForm_EditConfigurations = class(TForm)
    DataSource_EditConfigurations: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    ZQuery_EditConfigurations: TZQuery;
    procedure DBGrid1CellClick();
    procedure FormCreate(Sender: TObject);
    procedure ZQuery_EditConfigurationsAfterOpen(DataSet: TDataSet);
  private

  public

  end;

var
  Form_EditConfigurations: TForm_EditConfigurations;

implementation

{$R *.lfm}

{ TForm_EditConfigurations }
uses
  main;

procedure TForm_EditConfigurations.ZQuery_EditConfigurationsAfterOpen(DataSet: TDataSet);
begin
  Dataset.FieldByName('COMPILER_ID').visible := false;
end;

procedure TForm_EditConfigurations.FormCreate(Sender: TObject);
begin
  ZQuery_EditConfigurations.Open;
end;

procedure TForm_EditConfigurations.DBGrid1CellClick();
begin
  Form_Configurations.OnSelectionChange();
end;

end.

