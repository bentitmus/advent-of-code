.assembly extern System.Runtime {}
.assembly extern System.Console {}
.assembly Day04 {}

.class Day04.Program
{
  .method static void ExtractValues(string input, int32& b1, int32& e1, int32& b2, int32& e2)
  {
    .maxstack 4
    .locals init (
      [0] int32 i,
      [1] int32 current,
      [2] int32 item
    )

    ldarg.0
    callvirt instance int32 [System.Runtime]System.String::get_Length()
LOOP: // [length]
    ldarg.0
    ldloc.0
    callvirt instance char [System.Runtime]System.String::get_Chars(int32)
    dup
    ldc.i4 48
    blt.s FINISHED_NUMBER
    ldc.i4 48
    sub
    ldloc.1
    ldc.i4 10
    mul
    add
    stloc.1
NEXT_LOOP: // [length]
    dup
    ldc.i4.1
    ldloc.0
    add
    dup
    stloc.0
    beq.s LAST_FINISHED_NUMBER
    br.s LOOP
FINISHED_NUMBER: // [length, char]
    pop
    ldloc.2
    switch (ITEM_0, ITEM_1, ITEM_2)
ITEM_0: // [length]
    ldarg.1
    br.s ALL_ITEMS
ITEM_1: // [length]
    ldarg.2
    br.s ALL_ITEMS
ITEM_2: // [length]
    ldarg.3
ALL_ITEMS: // [length, arg]
    ldloc.1
    stind.i8
    ldloc.2
    ldc.i4.1
    add
    stloc.2
    ldc.i4.0
    stloc.1
    br.s NEXT_LOOP
LAST_FINISHED_NUMBER: // [length]
    pop
    ldarg.s 4
    ldloc.1
    stind.i8
    ret
  }

  .method static void Main()
  {
    .entrypoint
    .maxstack 7
    .locals init (
      [0] int32 b1,
      [1] int32 e1,
      [2] int32 b2,
      [3] int32 e2,
      [4] int32 part1,
      [5] int32 part2
    )

    // Open StreamReader to read lines of the input file
    ldstr "2022/input/day04.txt"
    call class [System.Runtime]System.IO.FileStream [System.Runtime]System.IO.File::OpenRead(string)
    dup
    call class [System.Runtime]System.Text.Encoding [System.Runtime]System.Text.Encoding::get_UTF8()
    ldc.i4.1 // true
    ldc.i4 4096
    newobj instance void [System.Runtime]System.IO.StreamReader::.ctor(class [System.Runtime]System.IO.Stream, class [System.Runtime]System.Text.Encoding, bool, int32)

LOOP: // [FileStream, StreamReader]
    dup
    callvirt instance string [System.Runtime]System.IO.TextReader::ReadLine()
    dup
    brfalse.s END
    ldloca.s 0
    ldloca.s 1
    ldloca.s 2
    ldloca.s 3
    call void [Day04]Day04.Program::ExtractValues(string, int32&, int32&, int32&, int32&)
    ldloc.0
    ldloc.2
    sub
    ldloc.1
    ldloc.3
    sub
    mul
    ldc.i4.0
    ble.s CONTAINED_WITHIN
PART2: // [FileStream, StreamReader]
    ldloc.1
    ldloc.2
    sub
    ldloc.3
    ldloc.0
    sub
    mul
    ldc.i4.0
    bge.s OVERLAP
    br.s LOOP
CONTAINED_WITHIN: // [FileStream, StreamReader]
    ldloc.s 4
    ldc.i4.1
    add
    stloc.s 4
    br.s PART2
OVERLAP: // [FileStream, StreamReader]
    ldloc.s 5
    ldc.i4.1
    add
    stloc.s 5
    br.s LOOP

END: // [FileStream, StreamReader, null]
    pop
    ldloca.s 4
    call instance string [System.Runtime]System.Int32::ToString()
    call void [System.Console]System.Console::WriteLine(string)
    ldloca.s 5
    call instance string [System.Runtime]System.Int32::ToString()
    call void [System.Console]System.Console::WriteLine(string)

    // Clean-up
    callvirt instance void [System.Runtime]System.IDisposable::Dispose()
    callvirt instance void [System.Runtime]System.IDisposable::Dispose()
    ret
  }
}
