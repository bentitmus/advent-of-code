int Priority(char ch) =>
    ch > 96 ? ch - 96 : (ch + 26) - 64;

var rucksacks = File.ReadAllLines("2022/input/day03.txt")
                    .Select(line => line.ToCharArray());

var part1 = rucksacks
    .Select(rucksack => Priority(
                          rucksack.Take(rucksack.Length / 2)
                                  .Intersect(rucksack.Skip(rucksack.Length / 2))
                                  .Single()))
    .Sum();

var part2 = rucksacks.Chunk(3)
                     .Select(gp => Priority(
                                     gp[0].Intersect(gp[1])
                                          .Intersect(gp[2])
                                          .Single()))
                     .Sum();

Console.WriteLine($"{part1}");
Console.WriteLine($"{part2}");
