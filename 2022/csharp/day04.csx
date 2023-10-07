(int, int, int, int) LowerFirst(int[] values) =>
  (values[0] > values[2])
    ? (values[2], values[3], values[0], values[1])
    : (values[0], values[1], values[2], values[3]);

var res = File.ReadAllLines("2022/input/day04.txt")
              .Select(line => LowerFirst(line.Split(new char[] {',', '-'})
                                             .Select(val => Int32.Parse(val)).ToArray())).ToList();

var part1 = res.Count(((int b1, int e1, int b2, int e2) t) => t.b1 == t.b2 || t.e1 >= t.e2);
var part2 = res.Count(((int b1, int e1, int b2, int e2) t) => t.e1 >= t.b2);

Console.WriteLine($"{part1}");
Console.WriteLine($"{part2}");
