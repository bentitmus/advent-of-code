.assembly extern System.Runtime {}
.assembly extern System.Console {}
.assembly Day01 {}

.class Day01.Program
{
  .method static void Main()
  {
    .entrypoint
    .maxstack 5
    .locals init (
      [0] int32 currentAmount,
      [1] int32 top3,
      [2] int32 top2,
      [3] int32 top1,
      [4] int32 end
    )

    // Open StreamReader to read lines of the input file
    ldstr "../input/day01.txt"
    call class [System.Runtime]System.IO.FileStream [System.Runtime]System.IO.File::OpenRead(string)
    dup
    call class [System.Runtime]System.Text.Encoding [System.Runtime]System.Text.Encoding::get_UTF8()
    ldc.i4.1 // true
    ldc.i4 4096
    newobj instance void [System.Runtime]System.IO.StreamReader::.ctor(class [System.Runtime]System.IO.Stream, class [System.Runtime]System.Text.Encoding, bool, int32)

LOOP: // [FileStream, StreamReader]
    // Read in lines of a file and add them to local 0 unless it is the end of the file or a blank line
    dup
    callvirt instance string [System.Runtime]System.IO.TextReader::ReadLine()
    dup
    brfalse.s ADDELF_END
    dup
    callvirt instance int32 [System.Runtime]System.String::get_Length()
    brfalse.s ADDELF_MID
    call int32 [System.Runtime]System.Int32::Parse(string)
    ldloc.0
    add
    stloc.0
    br.s LOOP

ADDELF_END: // [FileStream, StreamReader, null]
    // We have reached the end of the file, so process this elf but set the end flag
    ldc.i4.1
    stloc.s 4
ADDELF_MID: // [FileStream, StreamReader, string]
    // Check whether the current elf is better than top3
    pop
    ldloc.0
    ldloc.1
    bgt.s BETTERTOP3

RETURN: // [FileStream, StreamReader]
    // Reset the current elf and go back to the LOOP unless the end flag is set
    ldloc.s 4
    brtrue.s END
    ldc.i4.0
    stloc.0
    br.s LOOP

BETTERTOP3: // [FileStream, StreamReader]
    // Better than top3, check whether better than top2
    ldloc.0
    dup
    ldloc.2
    bgt.s BETTERTOP2
    stloc.1
    br.s RETURN

BETTERTOP2: // [FileStream, StreamReader, currentAmount]
    // Better than top2, check whether better than top1
    ldloc.2
    stloc.1
    dup
    ldloc.3
    bgt.s BETTERTOP1
    stloc.2
    br.s RETURN

BETTERTOP1: // [FileStream, StreamReader, currentAmount]
    // Better than top1
    ldloc.3
    stloc.2
    stloc.3
    br.s RETURN

END: // [FileStream, StreamReader]
    // We have reached the end of the file and processed all elves
    // Print the top elf
    ldloca.s 3
    dup
    call instance string [System.Runtime]System.Int32::ToString()
    call void [System.Console]System.Console::WriteLine(string)
    // Print the sum of the top three elves
    ldloc.1
    ldloc.2
    add
    ldloc.3
    add
    stloc.3
    call instance string [System.Runtime]System.Int32::ToString()
    call void [System.Console]System.Console::WriteLine(string)
    // Clean-up
    callvirt instance void [System.Runtime]System.IDisposable::Dispose()
    callvirt instance void [System.Runtime]System.IDisposable::Dispose()
    ret
  }
}
