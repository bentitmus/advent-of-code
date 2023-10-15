using System.Text.RegularExpressions;

var input = File.ReadAllLines("2022/input/day05.txt");
var blankLine = Array.FindIndex(input, line => line == string.Empty);

var unprocessedCrates = input[..(blankLine-1)];
var columns = (unprocessedCrates[0].Length + 1) / 4;
var crates = Enumerable.Range(0, columns)
    .Select(x => (x * 4) + 1)
    .Select(x => unprocessedCrates.Select(y => y[x]).Where(z => z != ' ').ToList()).ToArray();

var unprocessedMoves = input[(blankLine+1)..];
var moves = unprocessedMoves.Select(line => {
        var numbers = new Regex(@"\D+").Split(line).Where(z => z != string.Empty).ToArray();
        return (Int32.Parse(numbers[0]), Int32.Parse(numbers[1])-1, Int32.Parse(numbers[2])-1);
    });

string processCrates(Func<IEnumerable<char>, IEnumerable<char>> mutation)
{
    var newCrates = moves.Aggregate(crates, (crates, move) => {
        var (numToMove, fromColumn, toColumn) = move;
        var items = mutation(crates[fromColumn].Take(numToMove));
        var newFromColumn = crates[fromColumn].Skip(numToMove);
        var newToColumn = items.Concat(crates[toColumn]);
        var newCrates = crates.ToArray();
        newCrates[fromColumn] = newFromColumn.ToList();
        newCrates[toColumn] = newToColumn.ToList();
        return newCrates;
    }).Select(crate => crate[0]);
    return new System.String(newCrates.ToArray());
}

var part1 = processCrates(x => x.Reverse());
var part2 = processCrates(x => x);

Console.WriteLine($"{part1}");
Console.WriteLine($"{part2}");
