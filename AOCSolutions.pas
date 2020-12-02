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

initialization
  RegisterClasses([TAdventOfCodeDay1,TAdventOfCodeDay2]);

end.

