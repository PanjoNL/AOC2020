unit uAOCUtils;

interface

uses
  inifiles, System.SysUtils, System.Generics.Collections, AOCBase, RTTI, System.Classes,
  System.Net.HttpClient, System.Net.urlclient, system.Generics.Defaults;

type AOCconfig = Record
  BaseUrl: string;
  BaseFilePath: string;
  SessionCookie: string;
  procedure LoadConfig;
End;

type TAdventOfCodeRef = class of TAdventOfCode;
type TDirection = (Up = 0, Right, Down, Left);

type AOCUtils = class
  public
    class var Config: AOCConfig;
    class function GetAdventOfCode: TList<TAdventOfCodeRef>;
    class function DayIndexFromClassName(Const aClassName: String): String;
    class procedure DoAdventOfCode(aAdventOfCodeRef: TAdventOfCodeRef);
    class procedure DownLoadPuzzleInput(var InputList: TStrings; Const DayIndex: String);
end;

type TAOCDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>)
  public
    procedure AddOrIgnoreValue(const Key: TKey; const Value: TValue);
    constructor Create(const aOnValueNoify: TCollectionNotifyEvent<TValue>); overload;
    procedure Free; overload;
end;

type
  TPosition = record
    x: integer;
    y: Integer;
    function SetIt(const aX, aY: integer): TPosition;
    function AddDelta(const aX, aY: Integer): TPosition;
    function Equals(Const Other: TPosition): Boolean;
    function Clone: TPosition;
    function ApplyDirection(Const aDirection: TDirection): TPosition;
  end;

function GCD(Number1, Number2: int64): int64;
function OccurrencesOfChar(const S: string; const C: string): integer;

implementation

procedure AOCconfig.LoadConfig;
const Config: string = 'Config';
var Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    BaseUrl := Ini.ReadString(Config, 'BaseUrl', '');
    BaseFilePath := Ini.ReadString(Config, 'BaseFilePath', '');
    SessionCookie := Ini.ReadString(Config, 'SessionCookie', '');
  finally
    Ini.Free;
  end;
end;

class function AOCUtils.GetAdventOfCode: TList<TAdventOfCodeRef>;
var
  ctx: TRttiContext;
  lType: TRttiType;
  AdventOfCode: TAdventOfCodeRef;
  Comparison: TComparison<TAdventOfCodeRef>;
begin
  result := TList<TAdventOfCodeRef>.Create;
  ctx := TRttiContext.Create;
  Writeln('Discovering advent of code');
  for lType in ctx.GetTypes do
    if (lType is TRttiInstanceType) and (TRttiInstanceType(lType).MetaclassType.InheritsFrom(TAdventOfCode))
    then
    begin
      AdventOfCode := TAdventOfCodeRef(TRttiInstanceType(lType).MetaclassType);
      if AdventOfCode.ClassName <> TAdventOfCode.ClassName then
      begin
        Writeln('Found '+ AdventOfCode.ClassName);
        Result.Add(adventOfCode);
      end;
    end;

  Comparison :=
    function(const Left, Right: TAdventOfCodeRef): Integer
    begin
      Result := StrToInt(AOCUtils.DayIndexFromClassName(Left.ClassName)) -
                StrToInt(AOCUtils.DayIndexFromClassName(Right.ClassName));
    end;
  Result.Sort(TComparer<TAdventOfCodeRef>.Construct(Comparison));
end;

class function AOCUtils.DayIndexFromClassName(Const aClassName: String): String;
var i: Integer;
begin
  i := Length('TAdventOfCodeDay');
  Result := Copy(aClassName, i + 1, Length(aClassName) - i); //
end;

class procedure AOCUtils.DoAdventOfCode(aAdventOfCodeRef: TAdventOfCodeRef);
var AdventOfCode: TAdventOfCode;
begin
  AdventOfCode := aAdventOfCodeRef.Create;
  try
    AdventOfCode.Solve;
  finally
    AdventOfCode.Free;
  end;
end;

class procedure AOCUtils.DownLoadPuzzleInput(var InputList: TStrings; Const DayIndex: String);
var HttpClient: THttpClient;
    lHeader: TNetHeader;
    Headers: TNetHeaders;
    HttpOutput: IHTTPResponse;
    Url: string;
begin
  Url := AOCUtils.Config.BaseUrl+'/day/'+DayIndex+'/input';
  WriteLn('Downloading puzzle data from ' + Url);

  HttpClient := THTTPClient.Create;
  lHeader := LHeader.Create('cookie', AOCUtils.Config.SessionCookie );
  SetLength(Headers, 1);
  Headers[0] := lHeader;
  try
    HttpOutput := HttpClient.Get(Url, nil, Headers);
    if HttpOutput.StatusCode = 200 then
      InputList.LoadFromStream(HttpOutput.ContentStream)
    else
      raise Exception.Create(HttpOutput.ContentAsString());
  finally
    HttpClient.Free;
  end;
end;

procedure TAOCDictionary<TKey,TValue>.AddOrIgnoreValue(const Key: TKey; const Value: TValue);
begin
  if not Self.ContainsKey(Key) then
    Self.Add(Key, Value);
end;

constructor TAOCDictionary<TKey,TValue>.Create(const aOnValueNoify: TCollectionNotifyEvent<TValue>);
begin
  inherited Create;
  OnValueNotify := aOnValueNoify;
end;

procedure TAOCDictionary<TKey,TValue>.Free;
begin
  Self.Clear;
  inherited Free;
end;

function TPosition.SetIt(const aX: Integer; const aY: Integer): TPosition;
begin
  x := aX;
  y := aY;
  Result := Self;
end;

function TPosition.AddDelta(const aX, aY: Integer): TPosition;
begin
  x := x + aX;
  y := y + aY;
  Result := Self;
end;

function TPosition.Equals(Const Other: TPosition): Boolean;
begin
  Result := (x = Other.x) and (y = Other.y);
end;

function TPosition.Clone: TPosition;
begin
  Result.x := Self.x;
  Result.y := Self.y;
end;

function TPosition.ApplyDirection(Const aDirection: TDirection): TPosition;
begin
  case aDirection of
    Up: AddDelta(0, -1);
    Right: AddDelta(1, 0);
    Down: AddDelta(0, 1);
    Left: AddDelta(-1, 0);
  end;
  Result := Self
end;

function GCD(Number1, Number2: int64): int64;
var Temp: int64;
begin
  if Number1 < 0 then Number1 := -Number1;
  if Number2 < 0 then Number2 := -Number2;

  repeat
    if Number1 < Number2 then
      begin
        Temp := Number1;
        Number1 := Number2;
        Number2 := Temp;
      end;

    Number1 := Number1 mod Number2;
  until (Number1 = 0);

  result := Number2;
end;

function OccurrencesOfChar(const S: string; const C: string): integer;
var
  i: Integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      inc(result);
end;


end.
