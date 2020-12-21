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

Const AOCTestData: array[0..20] of AOCTest =
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
 (AOCClass: TAdventOfCodeDay11; ExpectedSolutionA: '2247'; ExpectedSolutionB: '2011'),
 (AOCClass: TAdventOfCodeDay12; ExpectedSolutionA: '1294'; ExpectedSolutionB: '20592'),
 (AOCClass: TAdventOfCodeDay13; ExpectedSolutionA: '3464'; ExpectedSolutionB: '760171380521445'),
 (AOCClass: TAdventOfCodeDay14; ExpectedSolutionA: '10050490168421'; ExpectedSolutionB: '2173858456958'),
 (AOCClass: TAdventOfCodeDay15; ExpectedSolutionA: '1373'; ExpectedSolutionB: '112458'),
 (AOCClass: TAdventOfCodeDay16; ExpectedSolutionA: '27898'; ExpectedSolutionB: '2766491048287'),
 (AOCClass: TAdventOfCodeDay17; ExpectedSolutionA: '286'; ExpectedSolutionB: '960'),
 (AOCClass: TAdventOfCodeDay18; ExpectedSolutionA: '209335026987'; ExpectedSolutionB: '33331817392479'),
 (AOCClass: TAdventOfCodeDay19; ExpectedSolutionA: '291'; ExpectedSolutionB: '409'),
 (AOCClass: TAdventOfCodeDay20; ExpectedSolutionA: '45443966642567'; ExpectedSolutionB: '1607'),
 (AOCClass: TAdventOfCodeDay21; ExpectedSolutionA: '2262'; ExpectedSolutionB: 'cxsvdm,glf,rsbxb,xbnmzr,txdmlzd,vlblq,mtnh,mptbpz')
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
