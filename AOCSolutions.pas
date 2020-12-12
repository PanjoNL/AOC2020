unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math, uAOCUtils, system.Types;

type
  TAdventOfCodeDay1 = class(TAdventOfCode)
  private
    FNumbers: TList<Integer>;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

  TPasswordValidator = function(Const int1, int2: Integer; Const Char, Password: String): boolean of object;
  TAdventOfCodeDay2 = class(TAdventOfCode)
  private
    function ValidatePasswordA(Const int1, int2: Integer; Const Char, Password: String): boolean;
    function ValidatePasswordB(Const int1, int2: Integer; Const Char, Password: String): boolean;
    function ValidatePasswords(Validate: TPasswordValidator): integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay3 = class(TAdventOfCode)
  private
    Toboggan: TDictionary<TPosition,Boolean>;
    TobogganWidth, TobogganHeight: integer;
    function TraverseToboggan(DeltaX, DeltaY: integer): Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

  TAdventOfCodeDay4 = class(TAdventOfCode)
  private
    function ProcessPassports(Const ValidateData: boolean): Integer;
    function IsValidPassport(Const ValidateData: boolean; pp: TDictionary<string,string>): Boolean;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay5 = class(TAdventOfCode)
  private
    FSeatIds: TList<Integer>;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

  TAdventOfCodeDay6 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay7 = class(TAdventOfCode)
  private
    Rules: TAOCDictionary<String, TDictionary<String,Integer>>; 
    Cache: TDictionary<string,Boolean>;

    procedure RuleValueNotify(Sender: TObject; const Item: TDictionary<String,Integer>; Action: TCollectionNotification);
    function CanContainShinyGoldBag(Const aBag: String): Boolean;
    function CountNumberOfBags(Const aBag: String): Integer;
    Const Shinygold: String = 'shinygold';
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

  TAdventOfCodeDay8 = class(TAdventOfCode)
  private
    function RunProgram(const CorruptedLineOfCode: Integer; out Accumulator: Integer): Boolean;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay9 = class(TAdventOfCode)
  private
    SolutionA: integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay10 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay11 = class(TAdventOfCode)
  private
    function TakeSeats(Const MaxAdjacent: Integer; Const UseLineOfSight: Boolean): Integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay12 = class(TAdventOfCode)
  private
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;
(*
  TAdventOfCodeDay = class(TAdventOfCode)
  private
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;
*)

implementation

{$Region 'TAdventOfCodeDay1'}
procedure TAdventOfCodeDay1.BeforeSolve;
var s: String;
begin
  FNumbers := TList<Integer>.Create;
  for s in FInput do
    FNumbers.Add(StrToInt(s));
end;

procedure TAdventOfCodeDay1.AfterSolve;
begin
  FNumbers.Free;
end;

function TAdventOfCodeDay1.SolveA: Variant;
var i: Integer;
begin
  Result := 0;
  for i in FNumbers do
    if FNumbers.Contains(2020-i) then
      Exit(i*(2020-i)); //964875
end;

function TAdventOfCodeDay1.SolveB: Variant;
var i1, i2: Integer;
begin
  Result := 0;
  for i1 in FNumbers do
    for i2 in FNumbers do
      if FNumbers.Contains(2020-i1-i2) then
        Exit(i1*i2*(2020-i1-i2)); //158661360
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay2'}
function TAdventOfCodeDay2.SolveA: Variant;
begin
  Result := ValidatePasswords(ValidatePasswordA); //628
end;

function TAdventOfCodeDay2.SolveB: Variant;
begin
  Result := ValidatePasswords(ValidatePasswordB); //705
end;

function TAdventOfCodeDay2.ValidatePasswords(Validate: TPasswordValidator): integer;
var s: string;
    Split1, split2: TStringDynArray;
begin
  Result := 0;
  for s in FInput do
  begin
    Split1 := SplitString(s, ' ');
    Split2 := SplitString(Split1[0], '-');
    if Validate(StrToInt(Split2[0]), StrToInt(Split2[1]), LeftStr(Split1[1], 1), Split1[2]) then
      Inc(Result)
  end;
end;

function TAdventOfCodeDay2.ValidatePasswordA(Const int1, int2: Integer; Const Char, Password: String): boolean;
var i: Integer;
begin
  i := OccurrencesOfChar(Password, Char);
  Result := (i >= int1) and (i <= int2);
end;

function TAdventOfCodeDay2.ValidatePasswordB(Const int1, int2: Integer; Const Char, Password: String): boolean;
begin
  result := (Password[int1] = Char) xor (Password[int2] = Char)
end;

{$ENDREGION}
{$Region 'TAdventOfCodeDay3'}
function TAdventOfCodeDay3.SolveA: Variant;
begin
  Result := TraverseToboggan(3,1); //299
end;

function TAdventOfCodeDay3.SolveB: Variant;
begin
  Result := TraverseToboggan(1,1)
          * TraverseToboggan(3,1)
          * TraverseToboggan(5,1)
          * TraverseToboggan(7,1)
          * TraverseToboggan(1,2); // 3621285278
end;

procedure TAdventOfCodeDay3.BeforeSolve;
var Position: TPosition;
    x,y: Integer;
begin
  Toboggan := TDictionary<TPosition,Boolean>.Create;
  TobogganWidth := Length(FInput[0]);
  TobogganHeight := FInput.Count -1;
  for y := 0 to TobogganHeight do
    for x := 0 to TobogganWidth do
    begin
      Position := Position.SetIt(x,y);
      Toboggan.Add(Position,  FInput[y][x+1] = '#');
    end;
end;
procedure TAdventOfCodeDay3.AfterSolve;
begin
  Toboggan.Free;
end;

function TAdventOfCodeDay3.TraverseToboggan(DeltaX, DeltaY: integer): Int64;
Var Position: TPosition;
begin
  Result := 0;
  Position := Position.SetIt(0,0);
  while Position.y <= TobogganHeight do
  begin
    Result := Result + IfThen(Toboggan[Position], 1, 0);
    Position := Position.AddDelta(DeltaX, DeltaY);
    Position.x := Position.x mod TobogganWidth
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay4'}
function TAdventOfCodeDay4.SolveA: Variant;
begin
  Result := ProcessPassports(False); //170
end;

function TAdventOfCodeDay4.SolveB: Variant;
begin
  Result := ProcessPassports(True); //103
end;

function TAdventOfCodeDay4.ProcessPassports(Const ValidateData: boolean): Integer;
var s, s2: String;
    CurrentPassport: TDictionary<string,string>;
    Split, split2: TStringDynArray;
begin
  Result := 0;

  CurrentPassport := TDictionary<string,string>.Create;

  for s in FInput do
  begin
    Split := SplitString(s, ' ');
    if Length(split) = 0 then
    begin
      if IsValidPassport(ValidateData, CurrentPassport) then
        Inc(Result);
      CurrentPassport.Clear
    end
    else
    begin
      for s2 in split do
      begin
        Split2 := SplitString(s2, ':');
        CurrentPassport.Add(Split2[0], Split2[1]);
      end;
    end;
  end;

  //Check the final password in the file
  if IsValidPassport(ValidateData, CurrentPassport) then
    Inc(Result);

  CurrentPassport.Free;
end;

function TAdventOfCodeDay4.IsValidPassport(Const ValidateData: boolean; pp: TDictionary<string,string>): Boolean;

  function _CheckHcl(aHcl: string): boolean ;
  var i: Integer;
  begin
    Result := aHcl.StartsWith('#') and (Length(aHcl) = 7);

    for i := 2 to 7 do
      if pos(aHcl[i], 'abcdef1234567890') = 0 then
        exit(false)
  end;

  function _CheckEcl(aEcl: string): Boolean;
  begin
    Result := (aEcl = 'amb')
           or (aEcl = 'blu')
           or (aEcl = 'brn')
           or (aEcl = 'gry')
           or (aEcl = 'grn')
           or (aEcl = 'hzl')
           or (aEcl = 'oth');
  end;

var temp: integer;
begin
  Result := pp.ContainsKey('byr')
        and pp.ContainsKey('iyr')
        and pp.ContainsKey('eyr')
        and pp.ContainsKey('hgt')
        and pp.ContainsKey('hcl')
        and pp.ContainsKey('ecl')
        and pp.ContainsKey('pid');

  if (Not ValidateData) or (not Result) then
    Exit;

  Result := ((TryStrToInt(pp['byr'], temp) and (temp >= 1920) and (temp <= 2002))
        and  (TryStrToInt(pp['iyr'], temp) and (temp >= 2010) and (temp <= 2020))
        and  (TryStrToInt(pp['eyr'], temp) and (temp >= 2020) and (temp <= 2030))
        and(
            (pp['hgt'].EndsWith('cm') and TryStrToInt(LeftStr(pp['hgt'], 3), temp) and (Temp >= 150) and (temp <= 193))
         or (pp['hgt'].EndsWith('in') and TryStrToInt(LeftStr(pp['hgt'], 2), temp) and (Temp >= 59) and (temp <= 76))
           )
        and (_CheckHcl(pp['hcl']))
        and (_CheckEcl(pp['ecl']))
        and (Length(pp['pid'])= 9) and (TryStrToInt(pp['pid'], temp)));
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay5'}
procedure TAdventOfCodeDay5.BeforeSolve;
var s: String;

  function FindValue(Const High: Char; index, stepsize: integer): integer;
  begin
    Result := 0;
    while stepsize > 1 do
    begin
      stepsize := Round(Stepsize/2);
      if s[index] = High then
        Inc(Result, StepSize);
      Inc(index);
    end;
  end;

begin
  FSeatIds := TList<integer>.Create;
  for s in FInput do
    FSeatIds.Add(FindValue('B', 1, 128) * 8 + FindValue('R', 8, 8));
end;

procedure TAdventOfCodeDay5.AfterSolve;
begin
  FSeatIds.Free;
end;

function TAdventOfCodeDay5.SolveA: Variant;
var i: Integer;
begin
  Result := 0;
  for i in FSeatIds do
    Result := Max(Result, i); //832
end;

function TAdventOfCodeDay5.SolveB: Variant;
var i: integer;
begin
  Result := 0;
  for i := 0 to 128*8 do
    if (not FSeatIds.Contains(i)) and FSeatIds.Contains(i+1) and FSeatIds.Contains(i-1) then
      exit(i); //517
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay6'}
function TAdventOfCodeDay6.SolveA: Variant;
var s: string;
    Letters: TDictionary<String, boolean>;
    i: Integer;
begin
  Result := 0;
  Letters := TDictionary<String, boolean>.Create;
  for s in FInput do
  begin
    if s = '' then
    begin
      Inc(Result, Letters.Count);
      Letters.Clear;
    end
    else
      for i := 1 to Length(s) do
        Letters.AddOrSetValue(s[i], true);
  end;

  Result := Result + Letters.Count; //6633
  Letters.Free;
end;


function TAdventOfCodeDay6.SolveB: Variant;
var Letters: TList<String>;

  procedure _FillLetters;
  var i: Integer;
  begin
    Letters.Clear;
    for i := Ord('a') to ord('z') do
      Letters.Add(Chr(i));
  end;

var s: string;
    i: Integer;
begin
  Result := 0;
  Letters := TList<String>.Create;

  _FillLetters;
  for s in FInput do
  begin
    if s = '' then
    begin
      Inc(Result, Letters.Count);
      _FillLetters;
    end
    else
      for i := Ord('a') to ord('z') do
        if Pos(Chr(i),s) = 0 then
          Letters.Remove(Chr(i));
  end;

  Result := Result + Letters.Count; //6633
  Letters.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay7'}
procedure TAdventOfCodeDay7.BeforeSolve;
Var Split: TStringDynArray;
    Rule: TDictionary<String, Integer>;
    s: string;
    i: Integer;
begin
  Rules := TAOCDictionary<String, TDictionary<String,Integer>>.Create(RuleValueNotify);
  Cache := TDictionary<string,Boolean>.Create;

  for s in FInput do
  begin
    Split := SplitString(s, ' ');

    Rule := TDictionary<String, Integer>.Create;
    Rules.Add(split[0]+split[1], Rule);

    if not (Split[4] = 'no') then
    begin
      i := 5;
      while i <= Length(Split) do
      begin
        Rule.Add(split[i]+split[i+1], StrToInt(Split[i-1]));
        Inc(i, 4);
      end;
    end;
  end;
end;

procedure TAdventOfCodeDay7.AfterSolve;
begin
  Rules.Free;
  Cache.Free;
end;

procedure TAdventOfCodeDay7.RuleValueNotify(Sender: TObject; const Item: TDictionary<String,Integer>; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

function TAdventOfCodeDay7.SolveA: Variant;
var s: string;
begin
  Result := 0;

  for s in Rules.Keys do
    if CanContainShinyGoldBag(s) then
      Inc(Result); //115
end;

function TAdventOfCodeDay7.SolveB: Variant;
begin
  Result := CountNumberOfBags(Shinygold); //1250
end;

function TAdventOfCodeDay7.CanContainShinyGoldBag(Const aBag: String): Boolean;
var Rule: TDictionary<String, Integer>;
    OtherBag: String;
begin
  if Cache.TryGetValue(aBag, Result) then
    Exit;

  Rule := Rules[aBag];

  Result := False;
  if Rule.ContainsKey(Shinygold) then
    Result := True
  else
  for OtherBag in Rule.Keys do
    if CanContainShinyGoldBag(OtherBag) then
    begin
      Result := True;
      Break;
    end;

  Cache.AddOrSetValue(aBag, Result);
end;

function TAdventOfCodeDay7.CountNumberOfBags(Const aBag: String): Integer;
var Rule: TDictionary<String,Integer>;
    Requierment: TPair<String,Integer>;
begin
  Result := 0;
  Rule := Rules[aBag];
  for Requierment in Rule do
    Result := Result + Requierment.Value * (1 + CountNumberOfBags(Requierment.Key));
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay8'}
function TAdventOfCodeDay8.SolveA: Variant;
var Accumulator: Integer;
begin
  RunProgram(-1, Accumulator);
  Result := Accumulator; //1521
end;

function TAdventOfCodeDay8.SolveB: Variant;
var Accumulator, i: Integer;
begin
  for i := 0 to FInput.Count-1 do
    if (Not FInput[i].StartsWith('acc')) and RunProgram(i, Accumulator) then
      Exit(Accumulator); //1016
end;

function TAdventOfCodeDay8.RunProgram(const CorruptedLineOfCode: Integer; out Accumulator: Integer): Boolean;
var InstructionPointer: Integer;
    Split: TStringDynArray;
    RunInstructions: TList<Integer>;
begin
  Result := False;
  Accumulator := 0;
  InstructionPointer := 0;
  RunInstructions := TList<Integer>.Create;
  try
    while (Not RunInstructions.Contains(InstructionPointer)) And Not Result do
    begin
      RunInstructions.Add(InstructionPointer);
      Split := SplitString(FInput[InstructionPointer], ' ');

      if CorruptedLineOfCode = InstructionPointer then
      case IndexStr(Split[0], ['jmp','nop']) of
        0: Split[0] := 'nop';
        1: Split[0] := 'jmp';
      end;

      Inc(InstructionPointer);
      case IndexStr(Split[0], ['acc','jmp']) of
        0: Inc(Accumulator, StrToInt(Split[1]));
        1: Inc(InstructionPointer, StrToInt(Split[1]) -1);
      end;

      Result := InstructionPointer >= FInput.Count;
    end;
  finally
    RunInstructions.Free;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay9'}
function TAdventOfCodeDay9.SolveA: Variant; //138879426
var PreviousNumbers: TDictionary<integer, integer>;

    function _IsValid(Target: integer): Boolean;
    var i: integer;
    begin
      Result := False;
      for i in PreviousNumbers.Values do
        if PreviousNumbers.ContainsValue(Target-i) then
          Exit(True);
    end;

var i, CurrentNumber: integer;
begin
  PreviousNumbers := TDictionary<integer, integer>.Create;
  try
    for i := 0 to FInput.Count-1 do
    begin
      CurrentNumber := StrToInt(FInput[i]);
      if i >= 25 then
      begin
        if not _IsValid(CurrentNumber) then
          Exit(CurrentNumber); //138879426
        PreviousNumbers.Remove(i-25);
      end;

      PreviousNumbers.Add(i, CurrentNumber);
    end;
  finally
    PreviousNumbers.Free;
    SolutionA := Result;
  end;
end;

function TAdventOfCodeDay9.SolveB: Variant;
var i, j, SeenMin, SeenMax, Total, CurrentNumber: Integer;
begin
  for i := 0 to FInput.Count-1 do
  begin
    Total := 0;
    SeenMax := 0;
    SeenMin := MaxInt;
    j := i;
    while Total < SolutionA do
    begin
      CurrentNumber := StrToInt(FInput[j]);
      Inc(Total, CurrentNumber);
      SeenMax := Max(SeenMax, CurrentNumber);
      SeenMin := Min(SeenMin, CurrentNumber);

      if Total = SolutionA then
        Exit(SeenMin+SeenMax); //23761694

      inc(j);
    end
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay10'}
function TAdventOfCodeDay10.SolveA: Variant;
Var Adapters: TList<Integer>;
    Jolts, Diff1, Diff3, i: Integer;
    s: string;
begin
  Jolts := 0;
  Diff1 := 0;
  Diff3 := 1;

  Adapters := TList<Integer>.Create;
  try
    for s in FInput do
      Adapters.Add(StrToInt(s));

    while Adapters.Count > 0 do
      for i := 1 to 3 do
        if Adapters.Contains(Jolts+i) then
        begin
          Inc(Jolts, i);
          Adapters.Remove(Jolts);
          case i of
            1: Inc(Diff1);
            3: Inc(Diff3);
          end;
          Break;
        end;
  finally
    Adapters.Free;
  end;

  Result := Diff1*Diff3
end;

function TAdventOfCodeDay10.SolveB: Variant;
Var Adapters: TList<Integer>;
    Routes: TDictionary<Integer, Int64>;
    s: string;
    Adapter: Integer;
    i1,i2,i3: Int64;
begin
  Adapters := TList<Integer>.Create;
  Routes := TDictionary<Integer, Int64>.Create;
  try
    Adapters.Add(0);
    for s in FInput do
      Adapters.Add(StrToInt(s));

    Adapters.Sort;
    Adapters.Reverse;
    Routes.Add(Adapters.First+3, 1);

    for Adapter in Adapters do
    begin
      Routes.TryGetValue(Adapter+1, i1);
      Routes.TryGetValue(Adapter+2, i2);
      Routes.TryGetValue(Adapter+3, i3);
      Routes.Add(Adapter, i1+i2+i3);
    end;
    Result := Routes[0];
  finally
    Adapters.Free;
    Routes.Free;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay11'}
function TAdventOfCodeDay11.SolveA: Variant;
begin
  Result := TakeSeats(4, False); //2247
end;

function TAdventOfCodeDay11.SolveB: Variant;
begin
  Result := TakeSeats(5, True); //2011
end;

type TSeateState = (aNone, aTaken, aFree);
function TAdventOfCodeDay11.TakeSeats(Const MaxAdjacent: Integer; Const UseLineOfSight: Boolean): Integer;
var Width, Height: Integer;
    Seats: Array of array of TSeateState;

  function OutOfRange(const aX, aY: integer): boolean;
  begin
    Result := (aX < 0) or (aX > width) or (aY < 0) or (aY > Height);
  end;

  function _CountadjacentSeats(const aX, aY, CutOff: integer): integer;
  Const DeltaX: array[0..7] of Integer = (-1,-1,-1,0,0,1,1,1);
        DeltaY: array[0..7] of Integer = (-1,0,1,-1,1,-1,0,1);
  var i,x,y: integer;
  begin
    Result := 0;
    for i := 0 to Length(DeltaX)-1 do
    begin
      x := aX+DeltaX[i];
      y := aY+DeltaY[i];

      if UseLineOfSight then
      while (not OutOfRange(X, Y)) and (Seats[X][Y] = aNone) do
      begin
        X := X+DeltaX[i];
        Y := Y+DeltaY[i];
      end;

      if not OutOfRange(X, Y) and (Seats[X][Y] = aTaken) then
        Inc(Result);

      if Result > CutOff then
        Exit;
    end;
  end;

var x, y: Integer;
    Seat: TPoint;
    PendingAdd, PendingDelete: TList<TPoint>;
begin
  PendingAdd := TList<TPoint>.Create;
  PendingDelete := TList<TPoint>.Create;
  Result := 0;
  try
    Width := Length(FInput[0]);
    Height := FInput.Count-1;
    SetLength(Seats, Height);
    for x := 0 to width do
    begin
      SetLength(Seats[x], height);
      for y := 0 to Height do
      case IndexStr(FInput[y][x+1], ['#', 'L']) of
        0: Seats[x][y] := aTaken;
        1: Seats[x][y] := aFree;
      else
        Seats[x][y] := aNone
      end;
    end;

    repeat
      for Seat in PendingAdd do
        Seats[Seat.X][Seat.Y] := aTaken;
      for Seat in PendingDelete do
        Seats[Seat.X][Seat.Y] := aFree;

      Result := Result + PendingAdd.Count - PendingDelete.Count;
      PendingAdd.Clear;
      PendingDelete.Clear;

      for y := 0 to Height do
        for x := 0 to width do
          Case Seats[x][y] of
            aTaken: if _CountadjacentSeats(x,y, MaxAdjacent) >= MaxAdjacent then
                      PendingDelete.Add(TPoint.Create(x,y));
            aFree:  if _CountadjacentSeats(x,y, 0) = 0 then
                      PendingAdd.Add(TPoint.Create(x,y))
          End;

    until ((PendingAdd.Count + PendingDelete.Count) = 0);
  finally
    PendingAdd.Free;
    PendingDelete.Free;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay12'}
type TFacing = (North=0, Easth=1, South=2, West=3);
function TAdventOfCodeDay12.SolveA: Variant;
Const DeltaX: array[TFacing] of integer = (1,0,-1,0);
      DeltaY: array[TFacing] of integer = (0,1,0,-1);
var Facing: TFacing;
    Position: TPoint;
    s: string;
    Number: Integer;
begin
  Facing := Easth;
  Position := TPoint.Zero;
  for s in FInput do
  begin
    Number := StrToInt(RightStr(s, Length(s)-1));
    Case IndexStr(LeftStr(s, 1), ['N','E','S','W','L','R','F']) of
      0: Position.Offset(Number, 0);
      1: Position.Offset(0, Number);
      2: Position.Offset(-Number, 0);
      3: Position.Offset(0, -Number);
      4: Facing := TFacing((Ord(Facing)+4-Number div 90) mod 4);
      5: Facing := TFacing((Ord(Facing)+Number div 90) mod 4);
      6: Position.Offset(DeltaX[Facing]*Number, DeltaY[Facing]*Number);
    End;
  end;
  Result := abs(Position.X) + abs(Position.Y); //1294
end;

function TAdventOfCodeDay12.SolveB: Variant;

  function _Rotate(point: TPoint; angle: Integer): TPoint;
  begin
    case Angle of
      0:  Result := Point;
      90: Result := TPoint.Create(-point.Y, point.X);
      180:Result := TPoint.Create(-point.X, -point.Y);
      270:Result := TPoint.Create(point.Y, -point.X);
    end;
  end;

var Position, Waypoint: TPoint;
    s: string;
    Number: Integer;
begin
  Position := TPoint.Zero;
  Waypoint := TPoint.Create(1,10);
  for s in FInput do
  begin
    Number := StrToInt(RightStr(s, Length(s)-1));
    Case IndexStr(LeftStr(s, 1), ['N','E','S','W','L','R','F']) of
      0: Waypoint.Offset(Number, 0);
      1: Waypoint.Offset(0, Number);
      2: Waypoint.Offset(-Number, 0);
      3: Waypoint.Offset(0, -Number);
      4: Waypoint := _Rotate(Waypoint, 360-Number);
      5: Waypoint := _Rotate(Waypoint, Number)   ;
      6: Position.Offset(Number*WayPoint.X, Number*Waypoint.Y);
    end;
  end;

  Result := abs(Position.X) + abs(Position.Y); //20592
end;
{$ENDREGION}


(*
//{$Region 'TAdventOfCodeDay'}
procedure TAdventOfCodeDay.BeforeSolve;
var s: String;
begin

end;

procedure TAdventOfCodeDay.AfterSolve;
begin

end;

function TAdventOfCodeDay.SolveA: Variant;
begin

end;

function TAdventOfCodeDay.SolveB: Variant;
begin

end;
//{$ENDREGION}
*)
initialization
  RegisterClasses([TAdventOfCodeDay1,TAdventOfCodeDay2,TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
    TAdventOfCodeDay6,TAdventOfCodeDay7,TAdventOfCodeDay8,TAdventOfCodeDay9, TAdventOfCodeDay10,
    TAdventOfCodeDay11,TAdventOfCodeDay12]);

end.

