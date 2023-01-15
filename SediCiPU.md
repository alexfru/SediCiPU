# SediCiPU

sedici - 16 in Italian

Specification version: 0.9

CC BY 2023 Alexey Frunze  
This text is licensed under a [Creative Commons Attribution
4.0 International license.](https://creativecommons.org/)

## 1 Intro

As a child I played a lot of computer games on 8-bit micro
computers and I wrote some of my first computer programs on and
for them. So, for me there's a great deal of good memories
associated with microcomputers based on the intel 8080 CPU and
its closest "successor", the Zylog Z80 CPU.

However, such 8-bit CPUs were greatly underpowered due to overly
simplistic and non-orthogonal ISAs, lacking many useful
instructions. This made it difficult to write and generate
efficient code for them.

For example, a quarter the i8080's opcode space is occupied by
the "mov REG, reg" instructions (roughly 8 regs * another
8 regs = 64 opcodes) while another quarter is occupied by the
"ALU reg" instructions (8 ALU operations * 8 regs = 64 opcodes).
That is, of the 256 opcodes (all possible values of the first byte
of an instruction), one half is occupied by just these two things.

The Z80 retains nearly perfect software compatibility with the
i8080 and introduces some noticeable improvements, but they are
largely limited by said compatibility and cost. It's hard not to
see that the idea behind those improvements was making the most
bang for the buck. For instance, the additional registers can't be
conveniently used together with the old ones and some basic
instructions for loads, stores, moves and ALU operations (on
16-bit and signed integers) still aren't there.

But I feel like we could do better, while keeping the spirit and
charm of these CPUs.


### 1.1 New ISA

Here I present to you an ISA that feels familiar and is capable of
most of the same things that the i8080's and the Z80's could do,
yet better, in fewer instructions and less code overall, thanks to
a redesign from the ground up.

There are two main goals in the ISA:
- It must be 16-bit (not stuck somewhere between 8 and 16 bits
  like the aforementioned 8-bit CPUs).
- It must be more orthogonal and less accumulator-centric.

This and a few additional instructions make the ISA more suitable
for programming in assembly and languages like Pascal and C.

The ISA has a minimum configuration and a maximum configuration.
The minimum configuration doesn't have registers r4 through r7 nor
does it have some other enhancements/extensions, however, unlike
the Z80 extensions over the i8080, the maximum configuration is a
systematic and quite regular extension of the minimum
configuration. We will first consider the minimum configuration.


## 2 The mini

The CPU is little-endian and addresses a total of 64KB of combined
code and data memory (that is, they are in the same address
space). Two-byte loads/stores are allowed at odd addresses.

The CPU can also access an I/O space of 256 8-bit ports.

There are 3 16-bit general-purpose registers, r0 (AKA acc, the
accumulator) through r2, and 16-bit r3 (AKA sp, the stack
pointer), a somewhat less general-purpose register.

Naturally, there's a program counter register, 16-bit pc.

There's also another 16-bit register, flc, which has two parts:
- the flags (in bits 15 through 8) and
- the loop counter (AKA lc; in bits 7 through 0).

The flags are (in bits 15 through 8 of flc):

    15 14 13 12 11 10  9  8
     0  0  I  O  S  Z  P  C
     |  |  |  |  |  |  |  |
     |  |  |  |  |  |  |  carry (borrow) flag
     |  |  |  |  |  |  parity flag (0=odd parity, 1=even parity)
     |  |  |  |  |  zero result flag
     |  |  |  |  sign (negative result) flag
     |  |  |  overflow (signed) flag
     |  |  interrupts enabled flag
     |  reserved
     reserved


### 2.1 Instruction set

All instructions are 1 to 3 bytes in length. Bytes 2 and 3
typically encode only the immediates such as constants, offsets
and addresses (8-bit or 16-bit).

Note, this is mostly a load/store ISA in that it has very few
instructions that read memory, modify the read value and write it
back to memory (AKA RMW instruction).

Without further ado, here's the opcode map:

              x0/x4/x8/xC          x1/x5/x9/xD          x2/x6/xA/xE          x3/x7/xB/xF
    0x:
            movb r0, (r0)        movb r1, (r0)        movb r2, (r0)               pop r0
            movb r0, (r1)        movb r1, (r1)        movb r2, (r1)               pop r1
      movb r0, (r2+simm8)  movb r1, (r2+simm8)  movb r2, (r2+simm8)               pop r2
       movb r0, (sp+imm8)   movb r1, (sp+imm8)   movb r2, (sp+imm8)              pop flc

    1x:
            movw r0, (r0)        movw r1, (r0)        movw r2, (r0)                cpl c
            movw r0, (r1)        movw r1, (r1)        movw r2, (r1)                  ret
      movw r0, (r2+simm8)  movw r1, (r2+simm8)  movw r2, (r2+simm8)  movw sp, (r2+simm8)
       movw r0, (sp+imm8)   movw r1, (sp+imm8)   movw r2, (sp+imm8)                  nop

    2x:
            mov r0, simm8        movb (r0), r1        movb (r0), r2              push r0
            movb (r1), r0        mov r1, simm8        movb (r1), r2              push r1
      movb (r2+simm8), r0  movb (r2+simm8), r1        mov r2, simm8              push r2
       movb (sp+imm8), r0   movb (sp+imm8), r1   movb (sp+imm8), r2             push flc

    3x:
            adj r0, simm8        movw (r0), r1        movw (r0), r2                clr c
            movw (r1), r0        adj r1, simm8        movw (r1), r2                set c
      movw (r2+simm8), r0  movw (r2+simm8), r1        adj r2, simm8  movw (r2+simm8), sp
       movw (sp+imm8), r0   movw (sp+imm8), r1   movw (sp+imm8), r2        adj sp, simm8

    4x:
                       di           mov r0, r1           mov r0, r2           mov r0, sp
               mov r1, r0                   ei           mov r1, r2           mov r1, sp
               mov r2, r0           mov r2, r1                 reti           mov r2, sp
               mov sp, r0           mov sp, r1           mov sp, r2                  hlt

    5x:
                   sxt r0          xchg r0, r1          xchg r0, r2               cpl r1
               cmp r1, r0               sxt r1          xchg r1, r2               cpl r2
               cmp r2, r0           cmp r2, r1               sxt r2               neg r1
            cmp r0, imm16        cmp r1, imm16        cmp r2, imm16               neg r2

    6x:
           jnc/jgeu simm8         jc/jlu simm8            jgu simm8           jleu simm8
                jns simm8             js simm8        jnz/jne simm8         jz/je  simm8
               jges simm8            jls simm8            jgs simm8           jles simm8
                jno simm8             jo simm8            jmp simm8       djnz lc, simm8

    7x:
         movb r0, (imm16)     movb r1, (imm16)     movb r2, (imm16)          call simm16
         movw r0, (imm16)     movw r1, (imm16)     movw r2, (imm16)     movw sp, (imm16)
         movb (imm16), r0     movb (imm16), r1     movb (imm16), r2           jmp simm16
         movw (imm16), r0     movw (imm16), r1     movw (imm16), r2     movw (imm16), sp

    8x:
      movb r0, (r0+imm16)  movb r0, (r1+imm16)  movb r0, (r2+imm16)  movb r0, (sp+imm16)
      movw r0, (r0+imm16)  movw r0, (r1+imm16)  movw r0, (r2+imm16)  movw r0, (sp+imm16)
               push imm16  movb (r1+imm16), r0  movb (r2+imm16), r0  movb (sp+imm16), r0
     j<cc>/djnz lc,simm12  movw (r1+imm16), r0  movw (r2+imm16), r0  movw (sp+imm16), r0

    9x:
         movb (r0), simm8     movb (r1), simm8 movb (r2+simm8), si8 movb (sp+imm8), sim8
         movw (r0), simm8     movw (r1), simm8 movw (r2+simm8), si8 movw (sp+imm8), sim8
              "memb" (r0)          "memb" (r1)    "memb" (r2+simm8)     "memb" (sp+imm8)
              "memw" (r0)          "memw" (r1)    "memw" (r2+simm8)     "memw" (sp+imm8)

    Ax:
                rr r0, r1            rl r0, r1           crr r0, r1           crl r0, r1
                sr r0, r1            sl r0, r1           asr r0, r1         abcdc r0, r1
       adr r0, (r0+imm16)   adr r0, (r1+imm16)   adr r0, (r2+imm16)   adr r0, (sp+imm16)
            mov r0, imm16        mov r1, imm16        mov r2, imm16        mov sp, imm16

    Bx:
               add r0, r0           add r0, r1           add r0, r2           add r0, sp
               add r1, r0           add r1, r1           add r1, r2           add r1, sp
               add r2, r0           add r2, r1           add r2, r2           add r2, sp
               any r0, r0           any r0, r1           any r0, r2                 bkpt

    Cx:
               adc r0, r0           adc r0, r1           adc r0, r2               cpl r0
               sbb r0, r0           sbb r0, r1           sbb r0, r2               neg r0
               sub r0, r1           and r0, r1            or r0, r1           xor r0, r1
               sub r0, r2           and r0, r2            or r0, r2           xor r0, r2

    Dx:
               sub r1, r0           and r1, r0            or r1, r0           xor r1, r0
               sub r1, r2           and r1, r2            or r1, r2           xor r1, r2
               sub r2, r0           and r2, r0            or r2, r0           xor r2, r0
               sub r2, r1           and r2, r1            or r2, r1           xor r2, r1

    Ex:
            add r0, imm16        and r0, imm16         or r0, imm16        xor r0, imm16
            add r1, imm16        and r1, imm16         or r1, imm16        xor r1, imm16
            add r2, imm16        and r2, imm16         or r2, imm16        xor r2, imm16
            any r0, imm16        any r1, imm16        any r2, imm16        sub r0, imm16

    Fx:
            adc r0, imm16        sbb r0, imm16         inb r0, imm8        outb r0, imm8
     SHIFT7 r0, imm4; etc         mov lc, imm8           mov lc, r0           mov r0, lc
                  call r0               jmp r0           any r1, r2         cntlz r1, r0
                     exts                 sur1                 sur2                 sur3

The vast majority of these 256 opcodes in the minimal
configuration uniquely identifies each instruction and its
operands (except immediate values). The few exceptions are:

-   SHIFT7 r0, imm4

    The actual shift operation is encoded together with the shift
    count in the immediate byte.
    The opcode for these shift instructions is also shared with a
    few other instructions since the immediate byte has enough
    space to uniquely encode all of them:
    - bko
    - bkc
    - swi imm4
    - mov r0, \<cc\>

-   "j\<cc\> simm12" and "djnz lc, simm12"

    The actual operation (the conditional codes) is encoded in the
    4 most significant bits of the 16-bit immediate. The jump
    distance is therefore reduced from 16 possible to 12 bits.

-   "memb/memw" class of instructions operating on memory

    These instructions may have one or two immediate bytes,
    depending on the address register used to access the memory.
    The second (or only) immediate byte encodes the operation to
    be performed on the memory operand. The opcode and the first
    of the two immediates (if there are two immediates) identifies
    the location of the memory operand and its size (byte or
    word).

I think it's OK to have a few such exceptions in the otherwise
clean encoding scheme, where one opcode encodes exactly one
instruction.

The last 4 opcodes (exts, sur1, sur2, sur3) are reserved for the
maximum configuration of the ISA. They serve as instruction
prefixes, hence often lengthening instructions by one byte. They
allow us to add a few more enhancement/extension instructions and
introduce 4 more registers, r4 through r7. The maximum ISA is
going to be discussed much later.


#### 2.1.1 Memory loads and stores: movb, movw

movb loads or stores an 8-bit byte from/to data memory.

movw loads or stores a 16-bit word from/to data memory.

Memory operands:

-   byte/word at r0, r1, r2+simm8, sp+imm8

    This is the most universal form, which is also used in
    "memb/memw" instructions and for storing constants.

-   byte/word at imm16

-   byte/word at reg+imm16 when loading and storing r0

Some examples:

-   movb r0, (r1)

    loads a byte from memory at address contained in r1 into the
    least significant byte of r0, zeroes the most significant byte
    of r0.

-   movw r1, (imm16)

    loads r1 with a word from memory at specified address, imm16.

-   movb (sp+imm8), r2

    stores the least significant byte of r2 to memory at address
    "sp + zero-extended 8-bit immediate".

-   movw (r2+simm8), r1

    stores r1 as a word to memory at address
    "r2 + sign-extended 8-bit immediate".

-   movw r0, (r1+imm16)

    loads r0 with a word from memory at address
    "r1 + 16-bit immediate".

-   movb (sp+imm8), simm8

    stores an 8-bit immediate to memory at address
    "sp + zero-extended 8-bit immediate"  
    (note, these are two different immediates).

-   movw (sp+imm8), simm8

    sign-extends an 8-bit immediate to 16 bits and stores those to
    memory at address "sp + zero-extended 8-bit immediate"  
    (note, these are two different immediates).


#### 2.1.2 Stack loads and stores: pop, push

-   pop reg

    loads the specified register from memory at address contained
    in sp and then increments sp by 2.

-   push reg

    decrements sp by 2 and stores the specified register to memory
    at the new address contained in sp.

-   push imm16

    decrements sp by 2 and stores a 16-bit immediate to memory at
    the new address contained in sp.


#### 2.1.3 Carry flag manipulation: cpl c, clr c, set c

These instructions complement, clear and set the carry flag in the
flags register respectively.


#### 2.1.4 Non-memory data moves: mov, xchg

There are multiple variants of the mov instruction:

-   mov reg, imm16

    Loads a 16-bit immediate into the specified register.

-   mov reg, simm8

    Sign-extends an 8-bit immediate to 16 bits and loads it into
    the specified register.

-   mov REG, reg

    Copies the contents of register reg to register REG.

-   mov lc, imm8

    Loads an 8-bit immediate into the loop counter register.

-   mov lc, r0

    Loads the loop counter register from the least significant
    byte of r0.

-   mov r0, lc

    Loads r0 with the zero-extended value of the loop counter
    register.

-   mov r0, \<cc\>

    examines the flags register and if the specified condition is
    true (note, both parity flag conditions available in this
    instruction; see the conditions in the descriptions of the
    jump instructions), r0 is set to 1, otherwise it's cleared to 0.

Lastly,

-   xchg REG, reg

    exchanges the values in the specified registers.


#### 2.1.5 Small register adjustment: adj

-   adj reg, simm8

    sign-extends an 8-bit immediate to 16 bits and adds it to the
    specified register. The flags are updated as in the regular
    addition instruction (add), except adj preserves the carry flag.


#### 2.1.6 Interrupt control: di, ei, reti, hlt

-   di

    disables external/hardware interrupts and clears the
    interrupts enabled flag in the flags register.

-   ei

    enables external/hardware interrupts and sets the interrupts
    enabled flag in the flags register.

-   reti

    terminates execution of a hardware or software interrupt
    service routine by popping the return address from the stack
    into pc and popping the saved flc register value into flc, in
    that order.

-   hlt

    halts execution until there's an external/hardware interrupt.
    The interrupts must have been enabled for the halt instruction
    to be able to resume execution upon interrupt.


#### 2.1.7 Single-register ALU operations: sxt, cpl, neg

-   sxt reg

    sign-extends the least significant byte of the specified
    register to the most significant byte of the register. Does
    not modify the flags.

-   cpl reg

    complements (as 1's complement) the value in the specified
    register. Does not modify the flags.

-   neg reg

    negates (as 2's complement) the value in the specified
    register. Modifies the following flags based on the result:
    overflow, sign, zero, parity. The carry flag is preserved.


#### 2.1.8 Jumps, nop

Unconditional jumps:

-   nop

    advances pc to the next instruction (pc = pc + 1)

-   jmp simm16

    advances pc to the address of the immediately following
    instruction plus a 16-bit immediate (pc = pc + 3 + simm16)

-   jmp simm8

    advances pc to the address of the immediately following
    instruction plus an 8-bit immediate sign-extended to 16 bits
    (pc = pc + 2 + sign-extended simm8)

-   jmp r0

    loads pc with the address contained in r0

Conditional jumps:

-   j\<cc\> simm8

    examines the flags register and if the specified condition is
    true (parity flag conditions unavailable in this instruction),
    jumps just like "jmp simm8", otherwise continues to the
    immediately following instruction

-   j\<cc\> simm12

    examines the flags register and if the specified condition is
    true (unlike the above shorter variant of the instruction, one
    parity condition is available to check: np=po/flags.PF==0),
    advances pc to the address of the immediately following
    instruction plus a 12-bit immediate sign-extended to 16 bits
    (pc = pc + 3 + sign-extended simm12), otherwise continues to
    the immediately following instruction

-   djnz lc, simm8

    decrements the 8-bit loop counter register by 1 and if lc
    doesn't become 0, jumps like "jmp simm8", otherwise continues
    to the immediately following instruction; does not modify the
    flags

-   djnz lc, simm12

    decrements the 8-bit loop counter register by 1 and if lc
    doesn't become 0, advances pc to the address of the
    immediately following instruction plus a 12-bit immediate
    sign-extended to 16 bits (pc = pc + 3 + sign-extended simm12),
    otherwise continues to the immediately following instruction;
    does not modify the flags

Condition codes:

-   Carry/borrow (AKA unsigned overflow) and unsigned comparison:
    - nc=geu (cccc= 0; flags.CF==0)
    -  c=lu  (cccc= 1; flags.CF==1)
    -    gu  (cccc= 2; flags.CF==0 && flags.ZF==0)
    -    leu (cccc= 3; flags.CF==1 || flags.ZF==1)

-   Sign (negative or non-negative result):
    - ns     (cccc= 4; flags.SF==0)
    -  s     (cccc= 5; flags.SF==1)

-   Zero or equality:
    - nz=ne  (cccc= 6; flags.ZF==0)
    -  z= e  (cccc= 7; flags.ZF==1)

-   Signed comparison:
    - ges    (cccc= 8; flags.SF==flags.OF)
    -  ls    (cccc= 9; flags.SF!=flags.OF)
    -  gs    (cccc=10; flags.SF==flags.OF && flags.ZF==0)
    - les    (cccc=11; flags.SF!=flags.OF || flags.ZF==1)

-   Overflow (signed):
    - no     (cccc=12; flags.OF==0)
    -  o     (cccc=13; flags.OF==1)

-   Parity:
    - np=po  (cccc=14; flags.PF==0; AKA odd parity)
    -  p=pe  (cccc=15; flags.PF==1; AKA even parity; available in
      the "mov r0, \<cc\>" instruction only)


#### 2.1.9 Subroutines: call, ret

-   call simm16

    pushes the address of the immediately following instruction
    (pc + 3) to the stack and then advances pc to the address of
    the immediately following instruction plus a 16-bit immediate
    (pc = pc + 3 + simm16).

-   call r0

    pushes the address of the immediately following instruction
    (pc + 1) to the stack and then loads pc with the address
    contained in r0.

-   ret

    pops a return address from the stack into pc.


#### 2.1.10 Shifts: cntlz, rr, rl, crr, crl, sr, sl, asr

-   cntlz r1, r0

    counts the leading zero bits in r0 and stores the count in r1.
    The zero flag is set to indicate whether the count is zero or
    not. The carry flag is set to indicate that the count is 16 or
    not. The overflow, sign and parity flags are cleared.

The shift instructions come in two forms:

-   SHIFT r0, r1

    The instruction shifts r0 by 0 to 15 positions right or left
    with the shift count taken from the 4 least significant bits
    of r1.

-   SHIFT r0, imm4

    The instruction shifts r0 by 1 to 15 positions right or left
    with the shift count taken from the immediate byte.

The shift instructions are:
-   rr: 16-bit rotate right
-   rl: 16-bit rotate left
-   crr: 17-bit rotate right, the carry flag is rotated too
-   crl: 17-bit rotate left, the carry flag is rotated too
-   sr: 16-bit logical (zero-extending) shift right
-   sl: 16-bit shift left
-   asr: 16-bit arithmetic (sign-extending) shift right

The shift instructions update the sign, zero and parity flags as
usual (see the section describing the regular ALU operations like
add). However, the overflow flag is unspecified. The carry flag
gets whatever is shifted out of the register.


#### 2.1.11 Address formation: adr

-   adr r0, (reg+imm16)

    stores in r0 the sum of the specified register and a 16-bit
    immediate. Does not modify the flags.


#### 2.1.12 Regular ALU: add, adc, sub, sbb, cmp, and, any, or, xor

These all are 16-bit operations and the flags are manipulated
accordingly to this 16-bit size. That is, the sign bit is bit 15
of the register containing the result, the zero and parity flags
take into account all 16 bits of the result register, the
carry/borrow-out is from bit 15 (the sign bit) and signed overflow
is computed for 16-bit addition, subtraction and comparison.

The regular ALU instructions come in two forms:
-   ALU REG, reg
-   ALU reg, imm16

The ALU instructions should be easily recognizable, they are:
-   add
-   adc: the carry flag is added in
-   sub
-   sbb: the carry/borrow flag is subtracted in
-   cmp: like the sub instruction, but modifies only the flags
-   and
-   any: like the and instruction, but modifies only the flags
-   or
-   xor

The ALU instructions update the sign, zero and parity flags as
usual:
-   The sign flag gets a copy of the most significant bit of the
    result in the register.
-   The zero flag is set to indicate whether all bits of the
    result register are zeroes.
-   The parity flag is set to 1 if the result register contains
    an even number of bits set to 1, otherwise it's cleared to 0.
  
The add, subtract and compare instructions set the overflow and
carry/borrow flag as appropriate to indicate signed 16-bit
overflow (or not) and carry/borrow-out (or none). The bitwise
operations (and, any, or, xor) clear the overflow and carry flags.


#### 2.1.13 Decimal addition: abcdc

-   abcdc r0, r1

    adds a pair of 4-digit packed BCD values with carry-in/out.
    The zero and parity flags are set as usual. The carry flag is
    set to indicate whether the sum exceeds decimal 9999. The
    overflow and sign flags are cleared. The results are
    unspecified if any of the two addends isn't a proper 4-digit
    packed BCD value.

Examples:
-   r0=0x9998 + r1=0x0001 + flags.CF=0 -> flags.CF=0, r0=0x9999
-   r0=0x9998 + r1=0x0000 + flags.CF=1 -> flags.CF=0, r0=0x9999
-   r0=0x9998 + r1=0x0001 + flags.CF=1 -> flags.CF=1, r0=0x0000
-   r0=0x9999 + r1=0x9999 + flags.CF=0 -> flags.CF=1, r0=0x9998
-   r0=0x9999 + r1=0x9999 + flags.CF=1 -> flags.CF=1, r0=0x9999


#### 2.1.14 Port I/O: inb, outb

There are two basic instructions:

-   inb r0, imm8

    Reads a byte from the I/O port identified by an 8-bit
    immediate into the least significant byte of r0 and zeroes the
    most significant byte of r0.

-   outb r0, imm8

    Writes the least significant byte of r0 to the port identified
    by an 8-bit immediate.


#### 2.1.15 Software interrupts: bkpt, swi, bko, bkc

Each of the instructions invoking a software interrupt handler
first pushes the flc register onto the stack and then disables
external/hardware interrupts as if by the di instruction.

After that each of the instructions pushes the return address to
the stack and jumps to the interrupt handler. The handler is
expected to terminate with the reti instruction.

Software interrupt instructions:

-   bkpt

    Invokes the breakpoint handler. The return address is of the
    bkpt instruction.

-   swi imm4

    Invokes one of 16 software interrupt handlers identified by
    imm4. The return address is of the instruction immediately
    following the swi instruction.

-   bko

    Invokes the signed overflow handler if the overflow flag is
    set. The return address is of the instruction immediately
    following the bko instruction.

-   bkc

    Invokes the unsigned overflow handler if the carry flag is
    set. The return address is of the instruction immediately
    following the bkc instruction.


#### 2.1.16 "memb/memw" instructions

There are two forms of these instructions:

-   ALU r0, mem

    Loads a byte/word from memory into an internal temporary
    register (with zero or sign extension for loaded bytes) and
    then performs a 16-bit binary ALU operation on the destination
    register, r0, and the source temporary. The memory operand can
    be at r1, r2+simm8, sp+imm8.

-   INC/DEC mem

    Increments/decrements the value in a byte/word of memory by 1
    or 2. The memory operand can be at r0, r1, r2+simm8, sp+imm8.

ALU r0, mem instructions with 16-bit mem:
-   add
-   adc
-   sub
-   sbb
-   cmp
-   and
-   any
-   or 
-   xor
-   rr
-   rl
-   crr
-   crl
-   sr
-   sl
-   asr

"ALU r0, mem" instructions with zero-extended 8-bit mem are
suffixed with "z", e.g. orz.

"ALU r0, mem" instructions with sign-extended 8-bit mem are
suffixed with "s", e.g. adds.

The ALU instructions update the flags as usual.

INC/DEC mem instructions, incrementing/decrementing by 1:
-   incb
-   incw
-   decb
-   decw

INC/DEC mem instructions, incrementing/decrementing by 2:
-   dincb
-   dincw
-   ddecb
-   ddecw

The INC/DEC instructions update the flags as the regular addition
instruction (add), except the INC/DEC preserves the carry flag. In
byte increments/decrements the flags are updated as if the byte
was loaded into the most significant byte of an initially zeroed
16-bit register and then 256 or 512 was added to or subtracted
from the register as usual.


### 2.2 Code examples

#### 2.2.1 Example: 16-bit multiplication

    ; r0 = r1 * r2
    ; destroyed: r1, lc
        mov  r0, 0
        mov  lc, 16
    Lrepeat:
        add  r0, r0
        add  r1, r1
        jnc  Lskip
        add  r0, r2
    Lskip:
        djnz lc, Lrepeat


#### 2.2.2 Example: widening unsigned 16-bit multiplication

    ; r0:r1 = r0 * r2
    ; destroyed: lc
        mov  r1, 0
        mov  lc, 16
    Lrepeat:
        add  r1, r1
        adc  r0, r0
        jnc  Lskip
        add  r1, r2
        adc  r0, 0
    Lskip:
        djnz lc, Lrepeat


#### 2.2.3 Example: unsigned 16-bit division and modulo

    ; r0 = r1 % r2
    ; r1 = r1 / r2
    ; destroyed: lc
        mov  r0, 0
        mov  lc, 16
    Lrepeat:
        add  r1, r1
        adc  r0, r0
        cmp  r2, r0
        jgu  Lskip
        sub  r0, r2
        adj  r1, 1
    Lskip:
        djnz lc, Lrepeat


#### 2.2.4 Example: 32-bit shift left

    ; r0:r2 <<= r1 ; 0<=r1<=31
        cmp  r1, 16
        jc   Lunder16
        mov  r0, r2
        sl   r0, r1
        mov  r2, 0
        jmp  Lend
    Lunder16:
        xor  r0, r2
        sl   r0, r1
        xchg r0, r2
        rl   r0, r1
        xor  r2, r0
        sr   r0, r1
        sl   r0, r1
        xchg r0, r2
    Lend:


#### 2.2.5 Example: 32-bit logical (zero-extending) shift right

    ; r0:r2 >>= r1 ; 0<=r1<=31
        cmp  r1, 16
        jc   Lunder16
        sr   r0, r1
        mov  r2, r0
        mov  r0, 0
        jmp  Lend
    Lunder16:
        xor  r2, r0
        rr   r0, r1
        xchg r0, r2
        sr   r0, r1
        xor  r0, r2
        xchg r0, r2
        sl   r0, r1
        sr   r0, r1
    Lend:


#### 2.2.6 Example: 32-bit arithmetic (sign-extending) shift right

    ; r0:r2 >>= r1 ; 0<=r1<=31
        cmp  r1, 16
        jc   Lunder16
        asr  r0, r1
        mov  r2, r0
        asr  r0, 15
        jmp  Lend
    Lunder16:
        xor  r2, r0
        rr   r0, r1
        xchg r0, r2
        sr   r0, r1
        xor  r0, r2
        xchg r0, r2
        sl   r0, r1
        asr  r0, r1
    Lend:


#### 2.2.7 Example: integer binary to decimal conversion

    ; in: r2=16-bit integer, e.g. 0xFFFF
    ; out: r2:r0=packed BCD integer, e.g. 0x0006:0x5535
    ; destroyed: r1, lc
        mov   r0, 0
        mov   lc, 14
    L1:
        add   r2, r2
        mov   r1, r0
        abcdc r0, r1
        djnz  lc, L1

        mov   lc, 2
    L2:
        xchg  r0, r2
        adc   r0, r0
        xchg  r0, r2
        mov   r1, r0
        abcdc r0, r1
        djnz  lc, L2

        xchg  r0, r2
        adc   r0, r0
        xchg  r0, r2


#### 2.2.8 Example: efficient comparison with zero

We can always use the cmp instruction to compare a register with
zero but with a 16-bit immediate it takes 3 bytes to encode,
which is kinda long. With most of largely worthless operand
combinations eliminated from the ALU instructions, there doesn't
seem much left as we can't and/or/any an arbitrary register with
itself anymore. Here are some alternatives shorter than 3 bytes.

        ; 1-byte comparison, r0 only
        any  r0, r0
        ; followed by one of:
        j(n)z   ; jump if r0 == 0 (r0 != 0)
        j(n)s   ; jump if r0 < 0 (r0 >= 0)
        jl(e)s  ; jump if r0 < 0 (r0 <= 0)
        jg(e)s  ; jump if r0 > 0 (r0 >= 0)

        ; 2-byte comparison, r0-sp
        adj  r1, 0
        ; followed by one of:
        j(n)z   ; jump if r1 == 0 (r1 != 0)
        j(n)s   ; jump if r1 < 0 (r1 >= 0)
        jl(e)s  ; jump if r1 < 0 (r1 <= 0)
        jg(e)s  ; jump if r1 > 0 (r1 >= 0)

        ; 1-byte comparison, r0-r2, mutating
        neg  r2
        j(n)z   ; jump if r2 == 0 (r2 != 0)

        ; 1-byte comparison, r0-r2, mutating
        add  r1, r1
        j(n)c   ; jump if r1 < 0 (r1 >= 0)


## 3 The maxi

The maximum ISA configuration unlocks all the CPU features that
have been designed to make the CPU even more powerful and
useful.

The maxi offers the following enhancements over the mini:
- additional registers r4 through r7 (they are selected instead
  of registers r0...r3 by using the "sur" instruction prefixes;
  the sur prefixes increase the instruction length by one byte,
  making the longest instruction 4 bytes long;
  "sur"=select upper reg(s), pronounced as sir or sure)
- hardware multiplier and divider and the instructions to use
  them
- block instructions to quickly search and copy blocks of data
- separate code and data memory address spaces option (64K+64K)
  and instructions to copy code and data between the two
- I/O space extended to 16 bits of address and allows 16-bit
  reads and writes, all via additional instructions


### 3.1 Complete instruction set

The opcode map presented earlier is in fact derived directly
from the following instruction encoding scheme, which is more
detailed and nuanced.

In said scheme every block of opcodes is going to be presented
somewhat like

    bits-to-encode how-many-(sub)opcodes instruction ; details
                                         instruction ; details
                                         instruction ; details

"bits-to-encode" include the variable parts of the opcode, which
often consist of a register number (or two register numbers) and
the "w" bit that tells the operand size (byte vs word), etc.
So, if there are 3 "bits-to-encode" (e.g. a 2-bit register
number and the "w" bit), the block is going to cost 2\*\*3=8
opcodes.

If a register is encoded in the opcode (as "rr" or "RR"),
it takes only 2 bits. The sur prefixes effectively carry a third
bit of a register number. With up to 2 registers used by a
regular instruction, we therefore need precisely 3 different sur
prefixes (sur1, sur2 and sur3). They provide a 3rd register
number bit for one register of the two, the other register or
both registers. In absence of the sur prefixes one can think
that the 3rd bits of register numbers are zeroes.

Note also, if a register is not explicitly encoded by its
number in the opcode, which is often the case with e.g. r0 (and
sometimes r1 and r2), the sur prefixes are still applicable
and so we effectively get a second accumulator (r0->r4) and a
second shift count register (r1->r5).

So, here goes, it may be a little messy, though...

    w.RR.rr  32 movb/movw rr, (RR)       ; when rr!=sp && RR%4<r2
                movb/movw rr, (RR+simm8) ; when rr!=sp && RR%4==r2
                movb/movw rr, (RR+imm8)  ; when rr!=sp && RR%4==sp
                pop RR                   ; when rr==sp && w==0 && RR!=sp
                pop flc                  ; when rr==sp && w==0 && RR==sp
                cpl c                    ; when rr==sp && w!=0 && RR%4==r0
                ret                      ; when rr==sp && w!=0 && RR%4==r1
                movw sp, (RR+simm8)      ; when rr==sp && w!=0 && RR%4==r2
                nop                      ; when rr==sp && w!=0 && RR==sp

    Expanded via the sur prefixes:
    "movb rr, (RR+imm)":
      RR\rr           0             1             2             3 |            4             5             6             7
      0       movb r0,m     movb r1,m     movb r2,m        pop r0 |    movb r4,m     movb r5,m     movb r6,m     movb r7,m
      1       movb r0,m     movb r1,m     movb r2,m        pop r1 |    movb r4,m     movb r5,m     movb r6,m     movb r7,m
      2       movb r0,m     movb r1,m     movb r2,m        pop r2 |    movb r4,m     movb r5,m     movb r6,m     movb r7,m
      3       movb r0,m     movb r1,m     movb r2,m       pop flc |    movb r4,m     movb r5,m     movb r6,m     movb r7,m
      ------------------------------------------------------------+-------------------------------------------------------
      4       movb r0,m     movb r1,m     movb r2,m        pop r4 |    movb r4,m     movb r5,m     movb r6,m     movb r7,m
      5       movb r0,m     movb r1,m     movb r2,m        pop r5 |    movb r4,m     movb r5,m     movb r6,m     movb r7,m
      6       movb r0,m     movb r1,m     movb r2,m        pop r6 |    movb r4,m     movb r5,m     movb r6,m     movb r7,m
      7       movb r0,m     movb r1,m     movb r2,m        pop r7 |    movb r4,m     movb r5,m     movb r6,m     movb r7,m
    and "movw rr, (RR+imm)":
      RR\rr           0             1             2             3 |            4             5             6             7
      0       movw r0,m     movw r1,m     movw r2,m         cpl c |    movw r4,m     movw r5,m     movw r6,m     movw r7,m
      1       movw r0,m     movw r1,m     movw r2,m           ret |    movw r4,m     movw r5,m     movw r6,m     movw r7,m
      2       movw r0,m     movw r1,m     movw r2,m     movw sp,m |    movw r4,m     movw r5,m     movw r6,m     movw r7,m
      3       movw r0,m     movw r1,m     movw r2,m           nop |    movw r4,m     movw r5,m     movw r6,m     movw r7,m
      ------------------------------------------------------------+-------------------------------------------------------
      4       movw r0,m     movw r1,m     movw r2,m           ??? |    movw r4,m     movw r5,m     movw r6,m     movw r7,m
      5       movw r0,m     movw r1,m     movw r2,m           ??? |    movw r4,m     movw r5,m     movw r6,m     movw r7,m
      6       movw r0,m     movw r1,m     movw r2,m     movw sp,m |    movw r4,m     movw r5,m     movw r6,m     movw r7,m
      7       movw r0,m     movw r1,m     movw r2,m           ??? |    movw r4,m     movw r5,m     movw r6,m     movw r7,m
    Rationales:
    - there's little use in loading the least significant byte of
      sp from memory, so that space is used for the pop
      instructions instead
    - sp rarely needs to be loaded from memory, so most of the
      options for doing so are taken away and the recovered opcode
      space is used for cpl, ret, nop


    w.RR.rr  32 movb/movw (RR), rr        ; when rr!=sp && RR%4<r2
                movb/movw (RR+simm8), rr  ; when rr!=sp && RR%4==r2
                movb/movw (RR+imm8), rr   ; when rr!=sp && RR%4==sp
                push RR                   ; when rr==sp && w==0 && RR!=sp
                push flc                  ; when rr==sp && w==0 && RR==sp
                mov rr, simm8             ; when rr==RR && w==0 && RR!=sp
                adj rr, simm8             ; when rr==RR && w!=0 ; doesn't affect flags.CF
                clr c                     ; when rr==sp && w!=0 && RR%4==r0
                set c                     ; when rr==sp && w!=0 && RR%4==r1
                movw (RR+simm8), sp       ; when rr==sp && w!=0 && RR%4==r2

    Expanded via the sur prefixes:
    "movb (RR+imm), rr":
      RR\rr           0             1             2             3 |            4             5             6             7
      0    mov r0,simm8     movb m,r1     movb m,r2       push r0 |    movb m,r4     movb m,r5     movb m,r6     movb m,r7
      1       movb m,r0  mov r1,simm8     movb m,r2       push r1 |    movb m,r4     movb m,r5     movb m,r6     movb m,r7
      2       movb m,r0     movb m,r1  mov r2,simm8       push r2 |    movb m,r4     movb m,r5     movb m,r6     movb m,r7
      3       movb m,r0     movb m,r1     movb m,r2      push flc |    movb m,r4     movb m,r5     movb m,r6     movb m,r7
      ------------------------------------------------------------+-------------------------------------------------------
      4       movb m,r0     movb m,r1     movb m,r2       push r4 | mov r4,simm8     movb m,r5     movb m,r6     movb m,r7
      5       movb m,r0     movb m,r1     movb m,r2       push r5 |    movb m,r4  mov r5,simm8     movb m,r6     movb m,r7
      6       movb m,r0     movb m,r1     movb m,r2       push r6 |    movb m,r4     movb m,r5  mov r6,simm8     movb m,r7
      7       movb m,r0     movb m,r1     movb m,r2       push r7 |    movb m,r4     movb m,r5     movb m,r6  mov r7,simm8
    and "movw (RR+imm), rr":
      RR\rr           0             1             2             3 |            4             5             6             7
      0     adj r0,sim8     movw m,r1     movw m,r2         clr c |    movw m,r4     movw m,r5     movw m,r6     movw m,r7
      1       movw m,r0   adj r1,sim8     movw m,r2         set c |    movw m,r4     movw m,r5     movw m,r6     movw m,r7
      2       movw m,r0     movw m,r1   adj r2,sim8     movw m,sp |    movw m,r4     movw m,r5     movw m,r6     movw m,r7
      3       movw m,r0     movw m,r1     movw m,r2   adj sp,sim8 |    movw m,r4     movw m,r5     movw m,r6     movw m,r7
      ------------------------------------------------------------+-------------------------------------------------------
      4       movw m,r0     movw m,r1     movw m,r2           ??? |  adj r4,sim8     movw m,r5     movw m,r6     movw m,r7
      5       movw m,r0     movw m,r1     movw m,r2           ??? |    movw m,r4   adj r5,sim8     movw m,r6     movw m,r7
      6       movw m,r0     movw m,r1     movw m,r2     movw m,sp |    movw m,r4     movw m,r5   adj r6,sim8     movw m,r7
      7       movw m,r0     movw m,r1     movw m,r2           ??? |    movw m,r4     movw m,r5     movw m,r6   adj r7,sim8
    Rationales:
    - there's little use in storing the least significant byte of
      sp to memory, so that space is used for the push
      instructions instead
    - sp rarely needs to be stored to memory, so most of the
      options for doing so are taken away and the recovered opcode
      space is used for clr, set, adj
    - it is quite unlikely to need to store an address of
      something (e.g. a data structure) within that same something
      (especially, if only a part of the address is being stored),
      hence the space for "movb/movw (reg+imm8), reg" (where both
      regs are the same) is reclaimed and reused for
      "mov reg, simm8" and "adj reg, simm8"


      RR.rr  16 mov RR, rr ; when RR!=rr
                di         ; when RR==rr && rr==0
                ei         ; when RR==rr && rr==1
                reti       ; when RR==rr && rr==2
                hlt        ; when RR==rr && rr==3

    Expanded via the sur prefixes:
      RR\rr  0    1    2    3 |   4    5    6    7
      0     di  mov  mov  mov | mov  mov  mov  mov
      1    mov   ei  mov  mov | mov  mov  mov  mov
      2    mov  mov reti  mov | mov  mov  mov  mov
      3    mov  mov  mov  hlt | mov  mov  mov  mov
      ------------------------+-------------------
      4    mov  mov  mov  mov | ???  mov  mov  mov
      5    mov  mov  mov  mov | mov  ???  mov  mov
      6    mov  mov  mov  mov | mov  mov  ???  mov
      7    mov  mov  mov  mov | mov  mov  mov  ???
    Rationales:
    - it's quite useless to move from a register to that same
      register, so the space is reused by di, ei, reti, hlt


      RR.rr  16 xchg RR, rr   ; when RR<rr && rr!=sp && RR!=sp
                cmp RR, rr    ; when RR>rr && rr!=sp && RR!=sp
                sxt rr        ; when RR==rr && rr!=sp ; doesn't affect flags
                cpl r1        ; when rr==sp && RR==0 ; doesn't affect flags
                cpl r2        ; when rr==sp && RR==1 ; doesn't affect flags
                neg r1        ; when rr==sp && RR==2 ; affects flags
                neg r2        ; when rr==sp && RR==3 ; affects flags
                cmp rr, imm16 ; when RR==sp && rr<sp

    Expanded via the sur prefixes:
      RR\rr           0             1             2             3 |            4             5             6             7
      0          sxt r0    xchg r0,r1    xchg r0,r2        cpl r1 |   xchg r0,r4    xchg r0,r5    xchg r0,r6    xchg r0,r7
      1       cmp r1,r0        sxt r1    xchg r1,r2        cpl r2 |   xchg r1,r4    xchg r1,r5    xchg r1,r6    xchg r1,r7
      2       cmp r2,r0     cmp r2,r1        sxt r2        neg r1 |   xchg r2,r4    xchg r2,r5    xchg r2,r6    xchg r2,r7
      3      cmp r0,i16    cmp r1,i16    cmp r2,i16        neg r2 |   cmp r4,i16    cmp r5,i16    cmp r6,i16           ???
      ------------------------------------------------------------+-------------------------------------------------------
      4       cmp r4,r0     cmp r4,r1     cmp r4,r2        cpl r5 |       sxt r4    xchg r4,r5    xchg r4,r6    xchg r4,r7
      5       cmp r5,r0     cmp r5,r1     cmp r5,r2        cpl r6 |    cmp r5,r4        sxt r5    xchg r5,r6    xchg r5,r7
      6       cmp r6,r0     cmp r6,r1     cmp r6,r2        neg r5 |    cmp r6,r4     cmp r6,r5        sxt r6    xchg r6,r7
      7       cmp r7,r0     cmp r7,r1     cmp r7,r2        neg r6 |    cmp r7,r4     cmp r7,r5     cmp r7,r6        sxt r7
    Rationales:
    - in "xchg RR, rr" we exploit symmetry and redundancy by
      requiring RR<rr, which lets us stick "cmp RR, rr" into
      the half of the space
    - in "cmp RR, rr" we exploit symmetry and redundancy by
      requiring RR>rr, which lets us stick "sxt rr" into the
      diagonal space
    - exchanging or comparing sp is rarely needed, so we reuse
      the space where RR=sp and rr=sp for cpl, neg and
      "cmp reg, imm16"


       cccc  16 j<cc> simm8    ; includes signed conditions, excludes parities
                jmp simm8      ; when jnp=jpo/flags.PF==0
                djnz lc, simm8 ; when  jp=jpe/flags.PF==1 ; doesn't affect flags
    Rationales:
    - we rarely need to check the parity flag, so the two
      opcodes are reused for unconditional jmp and djnz;
      the longer variant of j<cc> can check the parity flag


       w.rr   8 movb/movw rr, (imm16) ; when rr!=sp || w!=0
                call simm16           ; when rr==sp && w==0


       w.rr   8 movb/movw (imm16), rr  ; when rr!=sp || w!=0
                jmp simm16             ; when rr==sp && w==0


       w.rr   8 movb/movw acc, (RR+imm16)


       w.rr   8 movb/movw (RR+imm16), acc ; when RR!=acc
                push imm16                ; when RR==acc && w==0
                j<cc>/djnz lc, simm12     ; when RR==acc && w!=0 ; imm16=cccc.simm12 ; djnz lc instead of jp=jpe/flags.PF==1

    Expanded via the sur prefixes:
    "movb/movw (RR+imm16), acc":
    acc|w\RR          0             1             2             3 |            4             5             6             7
      0 0    push imm16     movb m,r0     movb m,r0     movb m,r0 |    movb m,r0     movb m,r0     movb m,r0     movb m,r0
      0 1 j<cc>/djnz 12     movw m,r0     movw m,r0     movw m,r0 |    movw m,r0     movw m,r0     movw m,r0     movw m,r0
      ------------------------------------------------------------+-------------------------------------------------------
      4 0     movb m,r4     movb m,r4     movb m,r4     movb m,r4 |          ???     movb m,r4     movb m,r4     movb m,r4
      4 1     movw m,r4     movw m,r4     movw m,r4     movw m,r4 |          ???     movw m,r4     movw m,r4     movw m,r4
    Rationales:
    - it is quite unlikely to need to store an address of
      something (e.g. a data structure) within that same something
      (especially, if only a part of the address is being stored),
      and there's a rare need to initialize an array with ever
      increasing numbers, hence the space for
      "movb/movw (reg+imm16), reg" (where both regs are the same)
      is reclaimed and reused for "push imm16", "j<cc> simm12" and
      "djnz lc, simm12"
    - we can get away with only being able to check the parity
      flag for zero and so we free the (sub)opcode space where
      we'd otherwise have the conditional jump checking the parity
      flag for non-zero and reuse that space for djnz


       w.rr   8 movb/movw (r0|r1|r2+simm8|sp+imm8), simm8 ; first (s)imm8 for address (if applicable),
                                                          ; last simm8 is value to store


       w.rr   8 "memb/memw" or rather one of the following two:
                - "OPz/OPs/OP acc, (?|r1|r2+simm8|sp+imm8)" ; first (s)imm8 for address (if applicable),
                - "INC/DEC (r0|r1|r2+simm8|sp+imm8)"        ; last imm8=s.00.ooooo

                OPz and OPs load a byte (w==0) and zero-extend it (s==0) or sign-extend it (s!=0),
                whereas OP loads a word (w!=0, s==0)
                  0<=ooooo<=15:
                    add, adc, sub, sbb, cmp, and, or, xor,
                    rr, rl, crr, crl, sr, sl, asr, any
                  16<=ooooo<=20: (available only as part of exts-2 mul/div extension)
                    divu, divs, modu, mods, mul

                  21<=ooooo<=27:
                    reserved

                INC/DEC loads a byte/word (s==0), increments/decrements it by 1 or 2, stores the result back
                  28<=ooooo<=29:
                    incb/incw/decb/decw  (r0|r1|r2+simm8|sp+imm8) ; doesn't affect flags.CF ; RMW instr!!!
                  30<=ooooo<=31:
                    dincb/dincw/ddecb/ddecw  (r0|r1|r2+simm8|sp+imm8) ; doesn't affect flags.CF ; RMW instr!!!


        ooo   8 SHIFT7 acc, r1 ; when ooo!=7 ; ooo: rr, rl, crr, crl, sr, sl, asr
                abcdc acc, r1  ; when ooo==7 ; add a pair of 4-digit packed BCD values with carry-in/out
         rr   4 adr acc, (RR+imm16) ; doesn't affect flags
         rr   4 mov rr, imm16

         rr   4 add acc, rr
         rr   4 add r1, rr
         rr   4 add r2, rr
         rr   4 any acc, rr ; when rr!=sp
                bkpt        ; when rr==sp

         rr   4 adc acc, rr  ; when rr!=sp
                cpl acc      ; when rr==sp ; doesn't affect flags
         rr   4 sbb acc, rr ; when rr!=sp
                neg acc     ; when rr==sp ; affects flags
              1 sub acc, r1
              1 and acc, r1
              1 or  acc, r1
              1 xor acc, r1
              1 sub acc, r2
              1 and acc, r2
              1 or  acc, r2
              1 xor acc, r2

              1 sub r1, acc
              1 and r1, acc
              1 or  r1, acc
              1 xor r1, acc
              1 sub r1, r2
              1 and r1, r2
              1 or  r1, r2
              1 xor r1, r2
              1 sub r2, acc
              1 and r2, acc
              1 or  r2, acc
              1 xor r2, acc
              1 sub r2, r1
              1 and r2, r1
              1 or  r2, r1
              1 xor r2, r1

              1 add acc, imm16
              1 and acc, imm16
              1 or  acc, imm16
              1 xor acc, imm16
              1 add r1, imm16
              1 and r1, imm16
              1 or  r1, imm16
              1 xor r1, imm16
              1 add r2, imm16
              1 and r2, imm16
              1 or  r2, imm16
              1 xor r2, imm16
              1 any acc, imm16
              1 any r1, imm16
              1 any r2, imm16
              1 sub acc, imm16

              1 adc acc, imm16
              1 sbb acc, imm16
              1 inb acc, imm8
              1 outb acc, imm8
              1 SHIFT7 acc, imm4          ; when ooo!=7 && 1<=imm4<=15     ; imm8=0.ooo.imm4
                bko        ; when acc==r0 ; when ooo==0 && imm4==0         ; imm8=0.ooo.0000
                bkc        ; when acc==r0 ; when ooo==1 && imm4==0         ; imm8=0.ooo.0000
                5 ???                     ; when 2<=ooo<=6 && imm4==0      ; imm8=0.ooo.0000
                swi imm4   ; when acc==r0 ; when ooo==7                    ; imm8=0.ooo.imm4
                7 imm4 ???                ; when ooo!=7                    ; imm8=1.ooo.imm4
                mov acc, <cc> ; imm4=cccc ; when ooo==7 ; flags unaffected ; imm8=1.ooo.imm4
              1 mov lc, imm8
              1 mov lc, acc
              1 mov acc, lc
              1 call acc
              1 jmp acc
              1 any r1, r2
              1 cntlz r1, acc
              1 exts-0 (imm8=0000.oooo):
                  sub r0, r4 ; sur is invalid here
                  and r0, r4 ; ditto
                  or  r0, r4 ; ditto
                  xor r0, r4 ; ditto
                  sub r1, r5 ; ...
                  and r1, r5
                  or  r1, r5
                  xor r1, r5
                  sub r2, r6
                  and r2, r6
                  or  r2, r6
                  xor r2, r6
                  sub r4, r0
                  and r4, r0
                  or  r4, r0
                  xor r4, r0
                exts-1 (imm8=0001.oooo):
                  sub r5, r1
                  and r5, r1
                  or  r5, r1
                  xor r5, r1
                  sub r6, r2
                  and r6, r2
                  or  r6, r2
                  xor r6, r2
                  any r1, r5
                  any r2, r6
                  6 ???
                exts-2 (imm8=0010.oooo):
                  divu acc, r1 ; unsigned div: acc = acc / r1 ; affects flags???
                  divs acc, r1 ;   signed div: acc = acc / r1 ; affects flags???
                  modu acc, r1 ; unsigned mod: acc = acc % r1 ; affects flags???
                  mods acc, r1 ;   signed mod: acc = acc % r1 ; affects flags???
                  divmodu acc, r1 ; unsigned div: acc = acc / r1, r1 = acc % r1 ; affects flags???
                  divmods acc, r1 ;   signed div: acc = acc / r1, r1 = acc % r1 ; affects flags???
                  mulu acc, r1 ; widening unsigned mul: r1:acc = acc * r1 ; affects flags???
                  muls acc, r1 ; widening   signed mul: r1:acc = acc * r1 ; affects flags???
                  mul acc, r1 ; non-widening mul: acc *= r1 ; affects flags
                  7 ???
                exts-3 (imm8=0011.oooo):
                  in   b/w acc, r1 ; all 64K ports addressable by r1
                  out  b/w acc, r1 ; all 64K ports addressable by r1
                  movc b/w acc, (r1) ; mov from code
                  movc b/w (r1), acc ; mov to code
                  8 ???
                exts-4 (imm8=0100.oooo):
                  finde  b/w ; r0=count, r1=val,  r2=src,  carry=direction (0=lower then higher;1=higher then lower)
                  findne b/w ; r0=count, r1=val,  r2=src,  carry=direction
                  findm  b/w ; r0=count, r1=src1, r2=src2, carry=direction
                  findnm b/w ; r0=count, r1=src1, r2=src2, carry=direction
                  bmvfc  b/w ; r0=count, r1=src,  r2=dst,  carry=direction ; mov from code to data
                  bmvtc  b/w ; r0=count, r1=src,  r2=dst,  carry=direction ; mov from data to code
                  bmov   b/w ; r0=count, r1=src,  r2=dst,  carry=direction
                  bfill  b/w ; r0=count, r1=val,  r2=dst,  carry=direction
              1 sur1 ; prefix to select one register operand from
                       the upper registers, r4...r7,
                       instead of the lower registers, r0...r3;
                       implicit acc=r0 can be reselected as r4 (likewise, r1->r5, etc)
              1 sur2 ; prefix to select another register operand from
                       the upper registers, r4...r7,
                       instead of the lower registers, r0...r3;
                       implicit acc=r0 can be reselected as r4 (likewise, r1->r5, etc)
              1 sur3 ; prefix to select both register operands from
                       the upper registers, r4...r7,
                       instead of the lower registers, r0...r3;
                       implicit acc=r0 can be reselected as r4 (likewise, r1->r5, etc)
      total 256


#### 3.1.1 Sur prefixes

When applied to a register operand, a sur prefix makes these
trivial transformations:

    r0 -> r4
    r1 -> r5
    r2 -> r6
    sp -> r7

When applied to a memory operand, a sur prefix makes these
trivial transformations:

    (r0)            -> (r4)
    (r1)            -> (r5)
    (r2+simm8)      -> (r6+simm8)
    (sp+imm8)       -> (r7+imm8)
    (r0...sp+imm16) -> (r4...r7+imm16)

Note that the r7 register cannot be accessed by the vast
majority of the ALU instructions even when a sur prefix is used.
The r7 register is therefore best to be used to address data
(for example, it could be used as a frame pointer).

In the preceding encoding scheme it may not be entirely clear
which instruction register is affected by which sur prefix.
Let's clarify the association between them.

The rules, applied sequentially, are:
- If a register in an instruction is marked or encoded as "RR",
  it is affected by sur2 and sur3. If there's another register
  in the instruction, it's affected by sur1 and sur3.
- If there are two registers and none of them is marked as "RR",
  the destination/result register is affected by sur2 and sur3
  and the other register is affected by sur1 and sur3.
  In a few instructions there isn't a clear destination register
  because none of r0 through r7 is modified. In this case the
  first register listed in the instruction mnemonic is
  considered the destination register. Examples of such
  instructions: cmp, any, outb/outw.
- Finally, if there's just one register in an instruction,
  whether explicitly encoded as "rr" or implicit (e.g. r0, r1 or
  r2), it's affected by sur1 and sur3.

Note that some instructions are not supposed to be sur-prefixed:
- exts-0 and exts-1 already perform the function of the sur
  prefixes
- block instructions in exts-4 each use 3 implicit registers
  (count, address/value, address), but the sur prefixes can only
  affect 2 of them


#### 3.1.2 Block instructions

Block instructions let us quickly copy data between different
regions of memory and quickly find/match data in sequences of
memory bytes or words.

For brevity let's call these memory bytes or words elements.

Each block instruction takes the number of elements to process
from the r0 register. No more than this number of elements will be
processed. The operation can complete earlier if there's another
termination condition that becomes true. 0 is a valid element
count with nothing being copied or found/matched.

When a block instruction completes, the r0 register contains the
number of elements (or pairs of elements, if appropriate) not yet
examined. The address register (or two, if there are two) is
incremented or decremented by the element size as many times as
many elements (or pairs, if appropriate) have been examined.
IOW, one may execute the find/match instruction after a successful
find/match again to get another successful find/match or until r0
becomes 0.

The copy and search direction is controlled by the carry flag.
Namely, the memory addresses increment by the element size after
each element is processed if the carry flag is 0. They decrement
if the carry flag is 1.

-   bfillb/bfillw

    Fills data memory (starting at address in r2) with a byte/word
    value (in r1).

-   bmovb/bmovw

    Copies bytes/words between different regions of data memory.
    r1 contains the source address, while r2 contains the
    destination address.

-   bmvfcb/bmvfcw

    Copies bytes/words from code to data memory.
    r1 contains the source address, while r2 contains the
    destination address.

-   bmvtcb/bmvtcw

    Copies bytes/words from data to code memory.
    r1 contains the source address, while r2 contains the
    destination address.

-   findeb/findew

    Finds a data memory byte/word (starting at address in r2) that
    is equal to a given value (in r1). The zero flag is set to 1
    if a byte/word found, cleared to 0 otherwise.

-   findneb/findnew

    Finds a data memory byte/word (starting at address in r2) that
    is not equal to a given value (in r1). The zero flag is set to
    1 if a byte/word found, cleared to 0 otherwise.

-   findmb/findmw

    Finds a pair of matching bytes/words in two data memory
    regions (starting at addresses in r1 and r2). The zero flag is
    set to 1 if a matching byte/word pair found, cleared to 0
    otherwise.

-   findnmb/findnmw

    Finds a pair of mismatching bytes/words in two data memory
    regions (starting at addresses in r1 and r2). The zero flag is
    set to 1 if a mismatching byte/word pair found, cleared to 0
    otherwise.

Block instructions are interruptible and the intermediate state
is observable (remaining element count, incremented/decremented
address(es), flags).

Pseudo code for find(n)eb/find(n)ew operation:

    1. if flags.IF != 0 and an external/hardware interrupt is
       requested/pending
         invoke the appropriate ISR
    2. flags.ZF = 0
    3. if r0 == 0
         goto 8
    4. if findeb/findew
         if data memory element at address r2 == r1
           flags.ZF = 1
       else if findneb/findnew
         if data memory element at address r2 != r1
           flags.ZF = 1
    5. if flags.CF == 0
         r2 = r2 + element size
       else
         r2 = r2 - element size
    6. r0 = r0 - 1
    7. if flags.ZF == 0
         goto 1
    8. advance pc to the next instruction

Note that the most significant byte of r1 is ignored in find(n)eb.

Pseudo code for find(n)mb/find(n)mw operation:

    1. if flags.IF != 0 and an external/hardware interrupt is
       requested/pending
         invoke the appropriate ISR
    2. flags.ZF = 0
    3. if r0 == 0
         goto 8
    4. if findmb/findmw
         if data memory element at address r1 ==
            data memory element at address r2
           flags.ZF = 1
       else if findnmb/findnmw
         if data memory element at address r1 !=
            data memory element at address r2
           flags.ZF = 1
    5. if flags.CF == 0
         r1 = r1 + element size
         r2 = r2 + element size
       else
         r1 = r1 - element size
         r2 = r2 - element size
    6. r0 = r0 - 1
    7. if flags.ZF == 0
         goto 1
    8. advance pc to the next instruction


## 4 Instruction set summaries

### 4.1 Summary of ALU instructions and operands (w/o sur prefix)

    instruction | r0,mem | RR,rr         | rr,imm       | other
    add           +        r0-r2,r0-sp     r0-r2,imm16
    adc,sbb       +        r0,r0-r2           r0,imm16
    sub           +        r0-r2,!=r0-r2      r0,imm16
    and,or,xor    +        r0-r2,!=r0-r2   r0-r2,imm16
    cmp           +        r1-r2,>r0-r1    r0-r2,imm16
    any           +        r0-r2,!=r0-r2   r0-r2,imm16
    any                    r0,r0
    adj                                    r0-sp,simm8
    adr                                                   r0,rr,imm16
    rr,rl         +        r0,r1           r0,imm4
    crr,crl       +        r0,r1           r0,imm4
    sr,sl,asr     +        r0,r1           r0,imm4
    abcdc                  r0,r1
    cntlz                  r1,r0
    mul           +        r0,r1
    div<s/u>      +        r0,r1
    mod<s/u>      +        r0,r1
    mul<s/u>               r0,r1
    divmod<s/u>            r0,r1
    The memory operand can be at r1, r2+simm8, sp+imm8.

    instruction   | rr    | mem | <cc>
    cpl             r0-r2
    neg             r0-r2
    sxt             r0-r2
    inc/dec b/w             +
    dinc/ddec b/w           +
    clr c                         c
    set c                         c
    cpl c                         c
    The memory operand can be at r0, r1, r2+simm8, sp+imm8.

    Notes:
    - regs,!=regs requires different register operands
    - regs,>regs requires 1st reg's number higher than 2nd's (e.g.
      "r1,r0" or "r2,r1" or "r2,r0", etc)


### 4.2 Summary of data transfers (w/o sur prefix)

    mov   rr, simm8 ; rr!=sp
    mov   rr, imm16
    mov   RR, rr    ; RR!=rr
    xchg  RR, rr    ; RR<rr, RR!=sp, rr!=sp

    mov   lc, imm8
    mov   lc, r0
    mov   r0, lc

    mov   r0, <cc>  ; includes np=po/flags.PF==0, p=pe/flags.PF==1

    push  imm16
    push  rr        ; rr!=sp
    push  flc
    pop   rr        ; rr!=sp
    pop   flc

    movb/movw rr, (r0|r1)    ; rr!=sp
    movb/movw rr, (r2+simm8) ; rr!=sp when movb
    movb/movw rr, (sp+imm8)  ; rr!=sp
    movb/movw rr, (imm16)
    movb/movw r0, (rr+imm16)

    movb/movw (r0), rr       ; rr!=sp,r0
    movb/movw (r1), rr       ; rr!=sp,r1
    movb/movw (r2+simm8), rr ; rr!=sp when movb ; rr!=r2
    movb/movw (sp+imm8), rr  ; rr!=sp
    movb/movw (imm16), rr
    movb/movw (rr+imm16), r0 ; rr!=r0
    movb/movw (r0|r1|r2+simm8|sp+imm8), simm8

    movcb/movcw r0, (r1)
    movcb/movcw (r1), r0

    inb       r0, imm8
    outb      r0, imm8
    inb/inw   r0, r1
    outb/outw r0, r1

    bfillb/bfillw
    bmovb/bmovw
    bmvfcb/bmvfcw
    bmvtcb/bmvtcw


### 4.3 Summary of control transfers (w/o sur prefix)

    nop
    jmp     simm8
    jmp     simm16
    jmp     r0
    j<cc>   simm8 ; cc excludes both parity flag checks
    j<cc>   simm12 ; cc excludes only p=pe/flags.PF==1
    djnz    lc, simm8
    djnz    lc, simm12

    call    simm16
    call    r0
    ret

    bkpt
    swi imm4
    bko
    bkc
    reti
    hlt
    di
    ei


### 4.4 Summary of flag-manipulating instructions

                              IF OF SF ZF PF CF
    clr c                                     0
    set c                                     1
    cpl c                                    rw
    add,sub,cmp                   w  w  w  w  w
    adc,sbb                       w  w  w  w rw
    abcdc                         0  0  w  w rw
    adr
    adj,neg,inc,dec,dinc,ddec     w  w  w  w
    and,any,or,xor                0  w  w  w  0
    sxt/cpl rr
    cntlz                         0  0  w  0  w
    rr,rl,sl,sr,asr               ?  w  w  w  w
    crr,crl                       ?  w  w  w rw
    mul                           ?  w  w  w  ?
    mulu,muls                     ?  ?  ?  ?  ?
    divu,divs                     ?  ?  ?  ?  ?
    modu,mods                     ?  ?  ?  ?  ?
    divmodu,divmods               ?  ?  ?  ?  ?
    j<cc>;mov r0, <cc>            r  r  r  r  r
    bkpt;swi n                r0  r  r  r  r  r
    bko                     crc0  r cr cr cr cr
    bkc                     crc0 cr cr cr cr  r
    reti                       w  w  w  w  w  w
    di                         0
    ei                         1
    push flc                   r  r  r  r  r  r
    pop flc                    w  w  w  w  w  w
    bmvfc,bmvtc,bmov,bfill                    r
    finde,findne,findm,findnm           w     r

    Legend:
    - r: reads
    - w: writes
    - 0: writes 0
    - 1: writes 1
    - ?: unspecified if modified and how
    - cx: conditionally x (x: reads, writes)


## 5 System details (unfinished)

- reset state:
  - pc = ?
  - sp = ?
  - flc = 0 (that is, interrupts disabled)
- interrupt table:
  - location/format or vector addresses: undecided
  - vectors (16), in no particular order:
    - invalid opcode
    - breakpoint
    - signed overflow
    - unsigned overflow
    - NMI
    - HW ints
    - SW ints
- supervisor vs user mode: only supervisor at the moment
- page tables: none at the moment

