unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math, uAOCUtils;

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
            Exit(i1*i2*i3); //241861950
end;
{$ENDREGION}


initialization
  RegisterClasses([TAdventOfCodeDay1]);

end.

