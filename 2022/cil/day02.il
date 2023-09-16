.assembly extern System.Runtime {}
.assembly extern System.Console {}
.assembly Day02 {}

.class Day02.Program
{
  .method static void Main()
  {
    .entrypoint
    .maxstack 5
    .locals init (
      [0] int32 opponent,
      [1] int32 second,
      [2] int32 part1score,
      [3] int32 part2score
    )

    // Open StreamReader to read lines of the input file
    ldstr "2022/input/day02.txt"
    call class [System.Runtime]System.IO.FileStream [System.Runtime]System.IO.File::OpenRead(string)
    dup
    call class [System.Runtime]System.Text.Encoding [System.Runtime]System.Text.Encoding::get_UTF8()
    ldc.i4.1 // true
    ldc.i4 4096
    newobj instance void [System.Runtime]System.IO.StreamReader::.ctor(class [System.Runtime]System.IO.Stream, class [System.Runtime]System.Text.Encoding, bool, int32)

LOOP: // [FileStream, StreamReader]
    // Get numbers representing the two values
    // A = 0, X = 0
    dup
    callvirt instance string [System.Runtime]System.IO.TextReader::ReadLine()
    dup
    brfalse.s END
    dup
    ldc.i4.0
    callvirt instance char [System.Runtime]System.String::get_Chars(int32)
    ldc.i4.s 65
    sub
    stloc.0
    ldc.i4.2
    callvirt instance char [System.Runtime]System.String::get_Chars(int32)
    ldc.i4.s 88
    sub
    dup
    stloc.1
    // Add the shape (second value) to the part 1 score
    dup
    ldc.i4.1
    add
    ldloc.2
    add
    stloc.2
    // Add the outcome to the part 2 score
    ldc.i4.3
    mul
    ldloc.3
    add
    stloc.3
    // Compute the game score and add to the part 1 score
    ldloc.1
    ldc.i4.4
    add
    ldloc.0
    sub
    ldc.i4.3
    rem
    ldc.i4.3
    mul
    ldloc.2
    add
    stloc.2
    // Compute the shape and add to the part 2 score
    ldloc.0
    ldloc.1
    add
    ldc.i4.2
    add
    ldc.i4.3
    rem
    ldc.i4.1
    add
    ldloc.3
    add
    stloc.3
    br.s LOOP

END: // [FileStream, StreamReader, null]
    pop
    ldloca.s 2
    call instance string [System.Runtime]System.Int32::ToString()
    call void [System.Console]System.Console::WriteLine(string)
    ldloca.s 3
    call instance string [System.Runtime]System.Int32::ToString()
    call void [System.Console]System.Console::WriteLine(string)
    // Clean-up
    callvirt instance void [System.Runtime]System.IDisposable::Dispose()
    callvirt instance void [System.Runtime]System.IDisposable::Dispose()
    ret
  }
}
