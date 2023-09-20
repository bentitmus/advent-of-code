string[] allElves = File.ReadAllText("2022/input/day01.txt")
                        .TrimEnd()
                        .Split("\n\n");

int[] elfSums = allElves.Select(x => x.Split("\n")
                                      .Select(y => Int32.Parse(y))
                                      .Sum()
                               )
                        .Order()
                        .Reverse()
                        .Take(3)
                        .ToArray();

int part1 = elfSums[0];
int part2 = elfSums.Sum();

Console.WriteLine($"{part1}");
Console.WriteLine($"{part2}");
