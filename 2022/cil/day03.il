.assembly extern System.Runtime {}
.assembly extern System.Console {}
.assembly Day03 {}

// Look-up table for base 2 logarithm assuming the input is one-hot
// Each 64-bit entry is a consecutive power of two, so the index gives
// the base 2 logarithm
.data log2lut_data = bytearray (
  01 00 00 00 00 00 00 00 02 00 00 00 00 00 00 00
  04 00 00 00 00 00 00 00 08 00 00 00 00 00 00 00
  10 00 00 00 00 00 00 00 20 00 00 00 00 00 00 00
  40 00 00 00 00 00 00 00 80 00 00 00 00 00 00 00
  00 01 00 00 00 00 00 00 00 02 00 00 00 00 00 00
  00 04 00 00 00 00 00 00 00 08 00 00 00 00 00 00
  00 10 00 00 00 00 00 00 00 20 00 00 00 00 00 00
  00 40 00 00 00 00 00 00 00 80 00 00 00 00 00 00
  00 00 01 00 00 00 00 00 00 00 02 00 00 00 00 00
  00 00 04 00 00 00 00 00 00 00 08 00 00 00 00 00
  00 00 10 00 00 00 00 00 00 00 20 00 00 00 00 00
  00 00 40 00 00 00 00 00 00 00 80 00 00 00 00 00
  00 00 00 01 00 00 00 00 00 00 00 02 00 00 00 00
  00 00 00 04 00 00 00 00 00 00 00 08 00 00 00 00
  00 00 00 10 00 00 00 00 00 00 00 20 00 00 00 00
  00 00 00 40 00 00 00 00 00 00 00 80 00 00 00 00
  00 00 00 00 01 00 00 00 00 00 00 00 02 00 00 00
  00 00 00 00 04 00 00 00 00 00 00 00 08 00 00 00
  00 00 00 00 10 00 00 00 00 00 00 00 20 00 00 00
  00 00 00 00 40 00 00 00 00 00 00 00 80 00 00 00
  00 00 00 00 00 01 00 00 00 00 00 00 00 02 00 00
  00 00 00 00 00 04 00 00 00 00 00 00 00 08 00 00
  00 00 00 00 00 10 00 00 00 00 00 00 00 20 00 00
  00 00 00 00 00 40 00 00 00 00 00 00 00 80 00 00
  00 00 00 00 00 00 01 00 00 00 00 00 00 00 02 00
  00 00 00 00 00 00 04 00 00 00 00 00 00 00 08 00
)

.class Day03.Program
{
  // Type for fixing the size of the initialisation field for the log2lut
  .class explicit sealed nested private log2lut_type extends [System.Runtime]System.ValueType
  {
    .pack 1
    .size 416 // 52 * 64 bits
  }
  .field static initonly valuetype Day03.Program/log2lut_type log2lut_init at log2lut_data

  // Field for the LUT
  .field static int64[] log2lut

  .method static void InitLog2Lut()
  {
    .maxstack 3
    ldc.i4.s 52
    newarr [System.Runtime]System.Int64
    dup
    ldtoken field valuetype Day03.Program/log2lut_type Day03.Program::log2lut_init
    call void [System.Runtime]System.Runtime.CompilerServices.RuntimeHelpers::InitializeArray(class [System.Runtime]System.Array, valuetype [System.Runtime]System.RuntimeFieldHandle)
    stsfld int64[] Day03.Program::log2lut
    ret
  }

  // Look-up the log2 for a given one hot number less than or equal to 2^51
  // Use binary chop which should be six or fewer loop interations
  .method static int32 Log2OneHot(int64 oneHotNumber)
  {
    .maxstack 3
    .locals init (
      [0] int32 lower,
      [1] int32 upper,
      [2] int32 current
    )

    ldc.i4.s 51
    stloc.1

LOG2_LOOP: // []
    ldsfld int64[] Day03.Program::log2lut
    ldloc.0
    ldloc.1
    add
    ldc.i4.2
    div
    dup
    stloc.2
    ldelem.i8
    dup
    ldarg.0
    beq.s LOG2_BE
    ldarg.0
    bgt.s LOG2_BGT
    ldloc.2
    ldc.i4.1
    add
    stloc.0
    br.s LOG2_LOOP

LOG2_BGT: // []
    ldloc.2
    ldc.i4.1
    sub
    stloc.1
    br.s LOG2_LOOP

LOG2_BE: // [log2lut[current]]
    pop
    ldloc.2
    ret
  }

  .method static int64 CharToOneHot(int64 input)
  {
    .maxstack 4
    ldc.i8 1
    ldarg.0
    ldc.i8 97
    sub
    dup
    ldc.i8 0
    bge.s CTOH_END
    ldc.i8 58
    add
CTOH_END: // [1, value]
    shl
    ret
  }

  .method static void StringToCompartments(string input, int64& c1, int64& c2)
  {
    .maxstack 2
    .locals init (
      [0] int32 i,
      [1] int32 max,
      [2] int64 current,
      [3] bool  second
    )

    ldarg.0
    callvirt instance int32 [System.Runtime]System.String::get_Length()
    ldc.i4.2
    div
    stloc.1
CHAR_LOOP: // []
    ldarg.0
    ldloc.0
    callvirt instance char [System.Runtime]System.String::get_Chars(int32)
    call int64 [Day03]Day03.Program::CharToOneHot(int64)
    ldloc.2
    or
    stloc.2
    ldloc.0
    ldc.i4.1
    add
    dup
    stloc.0
    ldloc.1
    blt.s CHAR_LOOP

    ldloc.3
    brtrue.s CHAR_DONE
    ldc.i4.1
    stloc.3
    ldloc.1
    ldc.i4.2
    mul
    stloc.1
    ldarg.1
    ldloc.2
    stind.i8
    ldc.i4.0
    stloc.2
    br.s CHAR_LOOP

CHAR_DONE: // []
    ldarg.2
    ldloc.2
    stind.i8
    ret
  }

  .method static void Main()
  {
    .entrypoint
    .maxstack 5
    .locals init (
      [0] int64 c1,
      [1] int64 c2,
      [2] int64 others,
      [3] int32 count,
      [4] int32 part1,
      [5] int32 part2
    )

    // Initialise LUT
    call void [Day03]Day03.Program::InitLog2Lut()

    // Open StreamReader to read lines of the input file
    ldstr "2022/input/day03.txt"
    call class [System.Runtime]System.IO.FileStream [System.Runtime]System.IO.File::OpenRead(string)
    dup
    call class [System.Runtime]System.Text.Encoding [System.Runtime]System.Text.Encoding::get_UTF8()
    ldc.i4.1 // true
    ldc.i4 4096
    newobj instance void [System.Runtime]System.IO.StreamReader::.ctor(class [System.Runtime]System.IO.Stream, class [System.Runtime]System.Text.Encoding, bool, int32)

BEGIN_THREE: // [FileStream, StreamReader]
    ldc.i8 1
    ldc.i8 52
    shl
    ldc.i8 1
    sub
    stloc.2

LOOP: // [FileStream, StreamReader]
    dup
    callvirt instance string [System.Runtime]System.IO.TextReader::ReadLine()
    dup
    brfalse.s END
    ldloca.s 0
    ldloca.s 1
    call void [Day03]Day03.Program::StringToCompartments(string, int64&, int64&)

    // Part 1 score
    ldloc.0
    ldloc.1
    and
    call int32 [Day03]Day03.Program::Log2OneHot(int64)
    ldc.i4.1
    add
    ldloc.s 4
    add
    stloc.s 4

    // Part 2 score
    ldloc.0
    ldloc.1
    or
    ldloc.2
    and
    stloc.2

    // Determine whether to compute the part 2 score
    ldloc.3
    ldc.i4.1
    add
    ldc.i4.3
    rem
    dup
    stloc.3
    ldc.i4.0
    bgt.s LOOP

    // Compute the part 2 score (every third line)
    ldloc.2
    call int32 [Day03]Day03.Program::Log2OneHot(int64)
    ldc.i4.1
    add
    ldloc.s 5
    add
    stloc.s 5
    br.s BEGIN_THREE

END: // [FileStream, StreamReader, string]
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
