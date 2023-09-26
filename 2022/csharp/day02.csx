int ShiftChar(char ch, char b) => ch - b;

int Score(int res, int shape) => (res * 3) + (shape + 1);

int Part1Score(int opp, int shape) =>
  Score((shape + 4 - opp) % 3, shape);

int Part2Score(int opp, int res) =>
  Score(res, (opp + res + 2) % 3);

var inputs =
  File.ReadAllLines("2022/input/day02.txt")
      .Select(line => (ShiftChar(line[0], 'A'), ShiftChar(line[2], 'X')));

var part1 =
  inputs.Select(input => Part1Score(input.Item1, input.Item2))
        .Sum();

var part2 =
  inputs.Select(input => Part2Score(input.Item1, input.Item2))
        .Sum();

Console.WriteLine($"{part1}");
Console.WriteLine($"{part2}");
