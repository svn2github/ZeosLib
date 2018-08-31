unit dbedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, DBGrids, DbCtrls, ZConnection, ZDataset, ZSequence, ikshelper,
  sqldb, db, ZAbstractConnection;

type

  { TForm_DBEdit }

  TForm_DBEdit = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    FBHelper1: TFBHelper;
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
    ZSequence1: TZSequence;
    procedure FormCreate(Sender: TObject);
    procedure ZConnection1Login(Sender: TObject; var Username: string;
      var Password: string);
  private

  public

  end;

var
  Form_DBEdit: TForm_DBEdit;

implementation

{$R *.lfm}

{ TForm_DBEdit }

procedure TForm_DBEdit.FormCreate(Sender: TObject);
begin
  {
  FBHelper1.LoadConfig;
  FBHelper1.ConfigConnection(ZConnection1);
  }

  ZConnection1.Connect;
end;

procedure TForm_DBEdit.ZConnection1Login(Sender: TObject; var Username: string;
  var Password: string);
begin

end;

end.

