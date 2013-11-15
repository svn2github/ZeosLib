{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcASAResultSet;

interface

{$I ZDbc.inc}

uses
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZDbcASA, ZPlainASADriver, ZCompatibility,
  ZDbcResultSetMetadata, ZDbcASAUtils, ZMessages, ZVariant;

type

  {** Implements ASA ResultSet. }
  TZASAResultSet = class(TZAbstractResultSet)
  private
    FCachedBlob: boolean;
    FFetchStat: Integer;
    FCursorName: AnsiString;
    FStmtNum: SmallInt;
    FSqlData: IZASASQLDA;
    FParamsSqlData: IZASASQLDA;
    FUpdateSqlData: IZASASQLDA;
    FASAConnection: IZASAConnection;
    FInsert: Boolean;
    FUpdate: Boolean;
    FDelete: Boolean;
  protected
    procedure Open; override;
    procedure PrepareUpdateSQLData; virtual;
    function GetFieldValue(ColumnIndex: Integer): Variant;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(Statement: IZStatement; SQL: string;
      var StmtNum: SmallInt; CursorName: AnsiString;
      SqlData: IZASASQLDA; ParamsSqlData: IZASASQLDA;
      CachedBlob: boolean);

    function GetCursorName: AnsiString; override;
    procedure Close; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetShort(ColumnIndex: Integer): ShortInt; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TByteDynArray; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    function Last: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
    function MoveRelative(Rows: Integer): Boolean; override;
    function Previous: Boolean; override;
    function Next: Boolean; override;

    function RowUpdated: Boolean; override;
    function RowInserted: Boolean; override;
    function RowDeleted: Boolean; override;

    procedure UpdateNull(ColumnIndex: Integer); override;
    procedure UpdateBoolean(ColumnIndex: Integer; const Value: Boolean); override;
    procedure UpdateByte(ColumnIndex: Integer; const Value: Byte); override;
    procedure UpdateShort(ColumnIndex: Integer; const Value: ShortInt); override;
    procedure UpdateSmall(ColumnIndex: Integer; const Value: SmallInt); override;
    procedure UpdateInt(ColumnIndex: Integer; const Value: Integer); override;
    procedure UpdateLong(ColumnIndex: Integer; const Value: Int64); override;
    procedure UpdateFloat(ColumnIndex: Integer; const Value: Single); override;
    procedure UpdateDouble(ColumnIndex: Integer; const Value: Double); override;
    procedure UpdateBigDecimal(ColumnIndex: Integer; const Value: Extended); override;
    procedure UpdatePChar(ColumnIndex: Integer; const Value: PChar); override;
    procedure UpdateString(ColumnIndex: Integer; const Value: String); override;
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: ZWideString); override;
    procedure UpdateBytes(ColumnIndex: Integer; const Value: TByteDynArray); override;
    procedure UpdateDate(ColumnIndex: Integer; const Value: TDateTime); override;
    procedure UpdateTime(ColumnIndex: Integer; const Value: TDateTime); override;
    procedure UpdateTimestamp(ColumnIndex: Integer; const Value: TDateTime); override;
    procedure UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream); override;
    procedure UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream); override;
    procedure UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream); override;
    procedure UpdateValue(ColumnIndex: Integer; const Value: TZVariant); override;

    procedure InsertRow; override;
    procedure UpdateRow; override;
    procedure DeleteRow; override;
    procedure RefreshRow; override;
    procedure CancelRowUpdates; override;
    procedure MoveToInsertRow; override;
    procedure MoveToCurrentRow; override;

    property SQLData: IZASASQLDA read FSQLData;
  end;

  {** Implements external blob wrapper object for ASA. }
  TZASABlob = class(TZAbstractBlob)
  public
    constructor Create(const SqlData: IZASASQLDA; const ColID: Integer);
  end;

  {** Implements external clob wrapper object for ASA. }
  TZASAClob = class(TZAbstractClob)
  public
    constructor Create(const SqlData: IZASASQLDA; const ColID: Integer;
      Const ConSettings: PZConSettings);
  end;

implementation

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
 SysUtils, Math, ZFastCode, ZDbcLogging, ZPlainASAConstants, ZDbcUtils, ZEncoding;

{ TZASAResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
  @param the sql out data previously allocated
  @param the Interbase sql dialect
}
constructor TZASAResultSet.Create(Statement: IZStatement; SQL: string;
      var StmtNum: SmallInt; CursorName: AnsiString;
      SqlData: IZASASQLDA; ParamsSqlData: IZASASQLDA;
      CachedBlob: boolean);
begin
  inherited Create( Statement, SQL, nil,Statement.GetConnection.GetConSettings);

  FFetchStat := 0;
  FSqlData := SqlData;
  FCursorName := CursorName;
  FCachedBlob := CachedBlob;
  FASAConnection := Statement.GetConnection as IZASAConnection;
  FDelete := False;
  FInsert := False;
  FUpdate := False;

  FParamsSqlData := ParamsSqlData;
  FStmtNum := StmtNum;
  ResultSetType := rtScrollSensitive;
  ResultSetConcurrency := rcUpdatable;

  Open;
end;

{**
   Return field value by it index
   @param the index column 0 first, 1 second ...
   @return the field value as variant type
}
function TZASAResultSet.GetFieldValue(ColumnIndex: Integer): Variant;
begin
  CheckClosed;
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetValue( ColumnIndex - 1)
  else
    Result := FSqlData.GetValue( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetBigDecimal( ColumnIndex - 1)
  else
    Result := FSqlData.GetBigDecimal( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZASAResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  TempBytes: TByteDynArray;
  TempRaw: RawByteString;
begin
  Result := nil;
  CheckClosed;
  CheckBlobColumn(ColumnIndex);

  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
     Exit;

  case GetMetadata.GetColumnType(ColumnIndex) of
    stAsciiStream, stUnicodeStream:
      Result := TZASAClob.Create(FsqlData, ColumnIndex-1, ConSettings);
    stBinaryStream:
      Result := TZASABlob.Create(FsqlData, ColumnIndex-1);
    stBytes:
      begin
        TempBytes := GetBytes(ColumnIndex);
        Result := TZAbstractBlob.CreateWithData(Pointer(TempBytes), Length(TempBytes));
      end;
    else
    begin
      TempRaw := InternalGetString(ColumnIndex);
      Result := TZAbstractClob.CreateWithData(PAnsiChar(TempRaw), Length(TempBytes),
        ConSettings^.ClientCodePage^.CP, ConSettings);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZASAResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBoolean);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetBoolean( ColumnIndex - 1)
  else
    Result := FSqlData.GetBoolean( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stByte);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetByte( ColumnIndex - 1)
  else
    Result := FSqlData.GetByte( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>shortint</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetShort(ColumnIndex: Integer): ShortInt;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stShort);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetSmall( ColumnIndex - 1)
  else
    Result := FSqlData.GetSmall( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAResultSet.GetBytes(
  ColumnIndex: Integer): TByteDynArray;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBytes);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetBytes( ColumnIndex - 1)
  else
    Result := FSqlData.GetBytes( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDate);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetDate( ColumnIndex - 1)
  else
    Result := FSqlData.GetDate( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDouble);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetDouble( ColumnIndex - 1)
  else
    Result := FSqlData.GetDouble( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stFloat);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetFloat( ColumnIndex - 1)
  else
    Result := FSqlData.GetFloat( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stInteger);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetInt( ColumnIndex - 1)
  else
    Result := FSqlData.GetInt( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stLong);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetLong( ColumnIndex - 1)
  else
    Result := FSqlData.GetLong( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stSmall);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetSmall( ColumnIndex - 1)
  else
    Result := FSqlData.GetSmall( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
begin
  CheckClosed;
  CheckColumnConvertion( ColumnIndex, stString);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetString( ColumnIndex - 1)
  else
    Result := FSqlData.GetString( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTime);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetTime( ColumnIndex - 1)
  else
    Result := FSqlData.GetTime( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
function TZASAResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTimestamp);
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.GetTimestamp( ColumnIndex - 1)
  else
    Result := FSqlData.GetTimestamp( ColumnIndex - 1);
  LastWasNull := IsNull( ColumnIndex);
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZASAResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  if FInsert or ( FUpdate and FUpdateSQLData.IsAssigned( ColumnIndex - 1)) then
    Result := FUpdateSqlData.IsNull( ColumnIndex - 1)
  else
    Result := FSqlData.IsNull(ColumnIndex - 1);
end;

function TZASAResultSet.Last: Boolean;
begin
  if LastRowNo <> MaxInt then
    Result := MoveAbsolute( LastRowNo)
  else
    Result := MoveAbsolute( -1);
end;

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZASAResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := False;
  if (MaxRows > 0) and (Row >= MaxRows) then
    Exit;

  FASAConnection.GetPlainDriver.db_fetch( FASAConnection.GetDBHandle,
    PAnsiChar(FCursorName), CUR_ABSOLUTE, Row, FSqlData.GetData, BlockSize, CUR_FORREGULAR);
  ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, lcOther, ConSettings);

  if FASAConnection.GetDBHandle.sqlCode <> SQLE_NOTFOUND then
  begin
    RowNo := Row;
    Result := True;
    FFetchStat := 0;
    FDelete := False;
    FInsert := False;
    FUpdate := False;
  end
  else
  begin
    FFetchStat := FASAConnection.GetDBHandle.sqlerrd[2];
    if FFetchStat > 0 then
      LastRowNo := Max( Row - FFetchStat, 0);
  end;
end;

function TZASAResultSet.MoveRelative(Rows: Integer): Boolean;
begin
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) then
    Exit;
  FASAConnection.GetPlainDriver.db_fetch( FASAConnection.GetDBHandle,
    PAnsiChar( FCursorName), CUR_RELATIVE, Rows, FSqlData.GetData, BlockSize, CUR_FORREGULAR);
    ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
      FASAConnection.GetDBHandle, lcOther, ConSettings, '', SQLE_CURSOR_NOT_OPEN); //handle a known null resultset issue (cursor not open)
  if FASAConnection.GetDBHandle.sqlCode = SQLE_CURSOR_NOT_OPEN then Exit;
  if FASAConnection.GetDBHandle.sqlCode <> SQLE_NOTFOUND then
  begin
    if ( RowNo > 0) or ( RowNo + Rows < 0) then
      RowNo := RowNo + Rows;
    Result := True;
    FFetchStat := 0;
    FDelete := False;
    FInsert := False;
    FUpdate := False;
  end
  else
  begin
    FFetchStat := FASAConnection.GetDBHandle.sqlerrd[2];
    if ( FFetchStat > 0) and ( RowNo > 0) then
      LastRowNo := Max( RowNo + Rows - FFetchStat, 0);
  end;
end;

function TZASAResultSet.Previous: Boolean;
begin
  Result := MoveRelative( -1);
end;

{**
  Moves the cursor down one row from its current position.
  A <code>ResultSet</code> cursor is initially positioned
  before the first row; the first call to the method
  <code>next</code> makes the first row the current row; the
  second call makes the second row the current row, and so on.

  <P>If an input stream is open for the current row, a call
  to the method <code>next</code> will
  implicitly close it. A <code>ResultSet</code> object's
  warning chain is cleared when a new row is read.

  @return <code>true</code> if the new current row is valid;
    <code>false</code> if there are no more rows
}
function TZASAResultSet.Next: Boolean;
begin
  Result := MoveRelative( 1);
end;

{**
  Opens this recordset.
}
procedure TZASAResultSet.Open;
var
  i: Integer;
  FieldSqlType: TZSQLType;
  ColumnInfo: TZColumnInfo;
begin
  if FStmtNum = 0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  ColumnsInfo.Clear;
  for i := 0 to FSqlData.GetFieldCount - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo, FSqlData  do
    begin
      FieldSqlType := GetFieldSqlType(I);
      ColumnName := ConSettings^.ConvFuncs.ZRawToString(GetFieldName(I), ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
//      TableName := GetFieldRelationName(I);
      ColumnLabel := ColumnName;
      ColumnType := FieldSqlType;

      if FieldSqlType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
      begin
        ColumnCodePage := ConSettings^.ClientCodePage^.CP;
        case FieldSqlType of
          stString,
          stUnicodeString: Precision := GetFieldSize(FieldSqlType, ConSettings,
            GetFieldLength(I)-4, ConSettings^.ClientCodePage^.CharWidth, @ColumnDisplaySize, True);
        end;
      end
      else
        ColumnCodePage := High(Word);

      ReadOnly := False;

      if IsNullable(I) then
        Nullable := ntNullable
      else
        Nullable := ntNoNulls;
      Nullable := ntNullable;

      Scale := GetFieldScale(I);
      AutoIncrement := False;
      //Signed := False;
      CaseSensitive := False;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;
  LastRowNo := MaxInt;
  inherited Open;
end;

procedure TZASAResultSet.Close;
begin
  FSqlData := nil;
  FParamsSqlData := nil;
  FUpdateSqlData := nil;
  if FCursorName <> '' then
  begin
    FASAConnection.GetPlainDriver.db_close(FASAConnection.GetDBHandle, PAnsiChar(FCursorName));
    FCursorName := '';
  end;
  inherited Close;
end;

function TZASAResultSet.GetCursorName: AnsiString;
begin
  Result := FCursorName;
end;

function TZASAResultSet.RowUpdated: Boolean;
begin
  Result := FUpdate;
end;

function TZASAResultSet.RowInserted: Boolean;
begin
  Result := FInsert;
end;

function TZASAResultSet.RowDeleted: Boolean;
begin
  Result := FDelete;
end;

procedure TZASAResultSet.PrepareUpdateSQLData;
begin
  FUpdate := not FInsert;
  if not Assigned( FUpdateSQLData) then
  begin
    FUpdateSQLData := TZASASQLDA.Create( FASAConnection.GetPlainDriver,
      FASAConnection.GetDBHandle, FCursorName, ConSettings, FSQLData.GetFieldCount);
  end
  else if FUpdateSQLData.GetFieldCount = 0 then
    FUpdateSQLData.AllocateSQLDA( FSQLData.GetFieldCount);
end;

procedure TZASAResultSet.UpdateNull(ColumnIndex: Integer);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateNull( ColumnIndex, True);
end;

procedure TZASAResultSet.UpdateBoolean(ColumnIndex: Integer; const Value: Boolean);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateBoolean( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateByte(ColumnIndex: Integer; const Value: Byte);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateByte( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateShort(ColumnIndex: Integer; const Value: ShortInt);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateSmall( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateSmall(ColumnIndex: Integer; const Value: SmallInt);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateSmall( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateInt(ColumnIndex: Integer; const Value: Integer);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateInt( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateLong(ColumnIndex: Integer; const Value: Int64);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateLong( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateFloat(ColumnIndex: Integer; const Value: Single);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateFloat( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateDouble(ColumnIndex: Integer; const Value: Double);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateDouble( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateBigDecimal(ColumnIndex: Integer; const Value: Extended);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateBigDecimal( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdatePChar(ColumnIndex: Integer; const Value: PChar);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdatePChar( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateString(ColumnIndex: Integer; const Value: String);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateString(ColumnIndex, ZPlainString(Value));
end;

procedure TZASAResultSet.UpdateUnicodeString(ColumnIndex: Integer; const Value: ZWideString);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateString(ColumnIndex, ZPlainString(Value));
end;

procedure TZASAResultSet.UpdateBytes(ColumnIndex: Integer; const Value: TByteDynArray);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateBytes( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateDate(ColumnIndex: Integer; const Value: TDateTime);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateDate( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateTime(ColumnIndex: Integer; const Value: TDateTime);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateTime( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateTimestamp(ColumnIndex: Integer; const Value: TDateTime);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateTimestamp( ColumnIndex, Value);
end;

procedure TZASAResultSet.UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.WriteBlob( ColumnIndex, Value, stAsciiStream);
end;

procedure TZASAResultSet.UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.WriteBlob( ColumnIndex, Value, stUnicodeStream);
end;

procedure TZASAResultSet.UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.WriteBlob( ColumnIndex, Value, stBinaryStream);
end;

procedure TZASAResultSet.UpdateValue(ColumnIndex: Integer; const Value: TZVariant);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateValue( ColumnIndex, EncodeVariant( Value));
end;

procedure TZASAResultSet.InsertRow;
begin
  if Assigned( FUpdateSQLData) and FInsert then
  begin
    FASAConnection.GetPlainDriver.db_put_into( FASAConnection.GetDBHandle,
      PAnsiChar(FCursorName), FUpdateSQLData.GetData, FSQLData.GetData);
    ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
      FASAConnection.GetDBHandle, lcOther, ConSettings, 'Insert row');

    FInsert := false;
    FUpdateSQLData.FreeSQLDA;
  end;
end;

procedure TZASAResultSet.UpdateRow;
begin
  if Assigned( FUpdateSQLData) and FUpdate then
  begin
    FASAConnection.GetPlainDriver.db_update( FASAConnection.GetDBHandle,
      PAnsiChar(FCursorName), FUpdateSQLData.GetData);
    ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
      FASAConnection.GetDBHandle, lcOther, ConSettings, 'Update row:' + IntToRaw( RowNo));

    FUpdate := false;
    FUpdateSQLData.FreeSQLDA;
  end;
end;

procedure TZASAResultSet.DeleteRow;
begin
  FASAConnection.GetPlainDriver.db_delete( FASAConnection.GetDBHandle,
    PAnsiChar(FCursorName));
  ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, lcOther, ConSettings, 'Delete row:' + IntToRaw( RowNo));

  FDelete := True;
  if LastRowNo <> MaxInt then
    LastRowNo := LastRowNo - FASAConnection.GetDBHandle.sqlerrd[2];
end;

procedure TZASAResultSet.RefreshRow;
begin
  MoveRelative( 0);
end;

procedure TZASAResultSet.CancelRowUpdates;
begin
  FUpdate := false;
  if Assigned( FUpdateSQLData) then
    FUpdateSQLData.FreeSQLDA;
end;

procedure TZASAResultSet.MoveToInsertRow;
begin
  FInsert := true;
end;

procedure TZASAResultSet.MoveToCurrentRow;
begin
  FInsert := false;
  if Assigned( FUpdateSQLData) then
    FUpdateSQLData.FreeSQLDA;
end;

{ TZASABlob }

{**
  Reads the blob information by blob handle.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
}
constructor TZASABlob.Create(const SqlData: IZASASQLDA; const ColID: Integer);
var
  Buffer: Pointer;
  Len: Cardinal;
begin
  inherited Create;
  SQLData.ReadBlobToMem(ColId, Buffer, Len);
  BlobSize := Len;
  BlobData := Buffer;
end;

{ TZASAClob }
constructor TZASAClob.Create(const SqlData: IZASASQLDA; const ColID: Integer;
  Const ConSettings: PZConSettings);
var
  Buffer: Pointer;
  Len: Cardinal;
begin
  inherited CreateWithData(nil, 0, ConSettings^.ClientCodePage^.CP, ConSettings);
  InternalClear;

  SQLData.ReadBlobToMem(ColId, Buffer, Len, False);
  (PAnsiChar(Buffer)+NativeUInt(Len))^ := #0; //add leading terminator
  BlobData := Buffer;
  BlobSize := Len+1;
end;

end.
