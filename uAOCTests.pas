unit uAOCTests;

interface

uses
  System.SysUtils, Winapi.Windows,
  uAocUtils, AocSolutions, AOCBase;

type AOCTest = record
  AOCClass: TAdventOfCodeRef;
  ExpectedSolutionA, ExpectedSolutionB, OverRidenTestInput: String;
end;

type AOCTests = class
public
  Class procedure RunTests;
end;

Const AOCTestData: array[0..10] of AOCTest =
(
 (AOCClass: TAdventOfCodeDay1; ExpectedSolutionA: '964875'; ExpectedSolutionB: '158661360'),
 (AOCClass: TAdventOfCodeDay2; ExpectedSolutionA: '628'; ExpectedSolutionB: '705'),
 (AOCClass: TAdventOfCodeDay3; ExpectedSolutionA: '299'; ExpectedSolutionB: '3621285278'),
 (AOCClass: TAdventOfCodeDay4; ExpectedSolutionA: '170'; ExpectedSolutionB: '103'),
 (AOCClass: TAdventOfCodeDay5; ExpectedSolutionA: '832'; ExpectedSolutionB: '517'),
 (AOCClass: TAdventOfCodeDay6; ExpectedSolutionA: '6633'; ExpectedSolutionB: '3202'),
 (AOCClass: TAdventOfCodeDay7; ExpectedSolutionA: '115'; ExpectedSolutionB: '1250'),
 (AOCClass: TAdventOfCodeDay8; ExpectedSolutionA: '1521'; ExpectedSolutionB: '1016'),
 (AOCClass: TAdventOfCodeDay9; ExpectedSolutionA: '138879426'; ExpectedSolutionB: '23761694'),
 (AOCClass: TAdventOfCodeDay10; ExpectedSolutionA: '2176'; ExpectedSolutionB: '18512297918464'),
 (AOCClass: TAdventOfCodeDay11; ExpectedSolutionA: '2247'; ExpectedSolutionB: '2011')
);

implementation

class procedure AOCTests.RunTests;

  procedure _Check(const DisplayName, Expected, Actual: String);
  begin
    if Expected <> '' then
      if Expected <> Actual then
      begin
        WriteLn(Format('FAIL, %s Expected: %s, Actual: %s', [DisplayName, Expected, Actual]));
        Assert(False);
      end
      else
        WriteLn(Format('PASS, %s', [DisplayName]))
  end;

Var Test: AOCTest;
    AdventOfCode: TAdventOfCode;
    SolutionA, SolutionB: string;
    StartTickTest, StartTick: Int64;
begin
  Writeln('');
  StartTick := GetTickCount;
  for Test in AOCTestData do
  begin
    Writeln(Format('Running tests for %s', [Test.AOCClass.ClassName]));

    StartTickTest := GetTickCount;
    AdventOfCode := Test.AOCClass.Create;
    AdventOfCode.Test(SolutionA, SolutionB, Test.OverRidenTestInput);
    AdventOfCode.Free;

    _Check('Part a', Test.ExpectedSolutionA, SolutionA);
    _Check('Part b', Test.ExpectedSolutionB, SolutionB);
    Writeln(FormAt('Total ticks %d', [GetTickCount - StartTickTest]));
    Writeln('');
  end;

  Writeln(Format('All tests done in %d ms', [GetTickCount - StartTick]));
end;

end.
