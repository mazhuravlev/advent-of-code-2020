module Day08Tests

open FsUnit.Xunit
open Xunit
open Day08

type Day08Tests() =

    [<Fact>]
    let ``can parse instruction with positive argument`` () =
        Assert.Equal(Instruction(Nop, 0), parseInstruction "nop +0")
        Assert.Equal(Instruction(Jmp, 3), parseInstruction "jmp +3")
        Assert.Equal(Instruction(Acc, 9), parseInstruction "acc +9")

    [<Fact>]
    let ``can parse instruction with negative argument`` () =
        Assert.Equal(Instruction(Nop, -1), parseInstruction "nop -1")
        Assert.Equal(Instruction(Jmp, -3), parseInstruction "jmp -3")
        Assert.Equal(Instruction(Acc, -9), parseInstruction "acc -9")
