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

  Result := ((TryStrToInt(pp['byr'], temp) and (temp >= 1920) and (temp <= 2020))
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





(*
//{$Region 'TAdventOfCodeDay'}
procedure TAdventOfCodeDay?.BeforeSolve;
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
  RegisterClasses([TAdventOfCodeDay1,TAdventOfCodeDay2,TAdventOfCodeDay3, TAdventOfCodeDay4]);

end.

