.assembly extern System.Runtime {}
.assembly extern System.Console {}
.assembly Day01 {}

// This code fails if the last elf is one of the best
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
      [3] int32 top1
    )

    ldstr "../input/day01.txt"
    call class [System.Runtime]System.IO.FileStream [System.Runtime]System.IO.File::OpenRead(string) // [FileStream]
    dup
    call class [System.Runtime]System.Text.Encoding [System.Runtime]System.Text.Encoding::get_UTF8()
    ldc.i4.1 // true
    ldc.i4 4096
    newobj instance void [System.Runtime]System.IO.StreamReader::.ctor(class [System.Runtime]System.IO.Stream, class [System.Runtime]System.Text.Encoding, bool, int32) // [FileStream, StreamReader]

LOOP: // [FileStream, StreamReader]
    dup // S3
    callvirt instance string [System.Runtime]System.IO.TextReader::ReadLine() // S3
    dup // S4
    brfalse.s END // S3
    dup // S4
    callvirt instance int32 [System.Runtime]System.String::get_Length() // S4
    brfalse.s NEWELF // S3
    call int32 [System.Runtime]System.Int32::Parse(string) // S3
    ldloc.0 // S4
    add // S3
    stloc.0 // S2
    br.s LOOP

NEWELF: // [FileStream, StreamReader, '']
    pop
    ldloc.0
    ldloc.1
    bgt.s BETTERELF // [FileStream, StreamReader]

RETURN: // S2
    ldc.i4.0 // S3
    stloc.0 // S2
    br.s LOOP // S2

BETTERELF: // S2
    ldloc.0 // S3
    dup // S4
    ldloc.2 // S5
    bgt.s BETTERELF2 // S3
    stloc.1 // S2
    br.s RETURN // S2

BETTERELF2: // S3
    ldloc.2 // S4
    stloc.1 // S3
    dup // S4
    ldloc.3 // S5
    bgt.s BETTERELF3 // S3
    stloc.2 // S2
    br.s RETURN // S2

BETTERELF3: // S3
    ldloc.3 // S4
    stloc.2 // S3
    stloc.3 // S2
    br.s RETURN // S2

END: // [FileStream, StreamReader, null]
    // Clean-up
    pop // S2
    ldloca.s 3 // S3
    dup // S4
    call instance string [System.Runtime]System.Int32::ToString() // S5
    call void [System.Console]System.Console::WriteLine(string) // S3
    ldloc.1 // S4
    ldloc.2 // S5
    add // S4
    ldloc.3 // S5
    add // S4
    stloc.3 // S3
    call instance string [System.Runtime]System.Int32::ToString() // S4
    call void [System.Console]System.Console::WriteLine(string) // S2
    callvirt instance void [System.Runtime]System.IDisposable::Dispose() // S1
    callvirt instance void [System.Runtime]System.IDisposable::Dispose() // S0
    ret
  }
}
