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

(*
  TAdventOfCodeDay? = class(TAdventOfCode)
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
var i1, i2: Integer;
begin
  Result := 0;
  for i1 in FNumbers do
    for i2 in FNumbers do
      if i1+i2 = 2020 then
        Exit(i1*i2); //964875
end;

function TAdventOfCodeDay1.SolveB: Variant;
var i1, i2, i3: Integer;
begin
  Result := 0;
  for i1 in FNumbers do
    for i2 in FNumbers do
      if i1+i2 < 2020 then //optimization
        for i3 in FNumbers do
          if i1+i2+i3 = 2020 then
            Exit(i1*i2*i3); //158661360
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







(*
//{$Region 'TAdventOfCodeDay?'}
procedure TAdventOfCodeDay?.BeforeSolve;
var s: String;
begin

end;

procedure TAdventOfCodeDay?.AfterSolve;
begin

end;

function TAdventOfCodeDay?.SolveA: Variant;
begin

end;

function TAdventOfCodeDay?.SolveB: Variant;
begin

end;
//{$ENDREGION}
*)
initialization
  RegisterClasses([TAdventOfCodeDay1,TAdventOfCodeDay2,TAdventOfCodeDay3]);

end.

