// <copyright file="BrainfuckInterpreterTests.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

using FluentAssertions;

namespace Brainfuck.Tests;

/// <summary>
/// Tests the Brainfuck interpreter.
/// </summary>
public class BrainfuckInterpreterTests
{
    /// <summary>
    /// Generate a combination of program, input and expected output.
    /// </summary>
    /// <returns>A combination of program, input and expected output.</returns>
    public static IEnumerable<object[]> GenerateProgramAndExpectedOutput()
    {
        yield return new object[]
        {
            // Sample from https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md#hello-world-example
            "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.",
            string.Empty,
            "Hello World!\n",
        };

        yield return new object[]
        {
            ",.",
            "x",
            "x",
        };

        // ROT13
        // https://brainfuck.org/rot13.b
        yield return new object[]
        {
            """
            ,
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>++++++++++++++<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>>+++++[<----->-]<<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>++++++++++++++<-
            [>+<-[>+<-[>+<-[>+<-[>+<-
            [>++++++++++++++<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>>+++++[<----->-]<<-
            [>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
            [>++++++++++++++<-
            [>+<-]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
            ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.[-]<,]
            """,
            "stefano\0",
            "fgrsnab",
        };

        // wc
        // https://brainfuck.org/wc.b
        yield return new object[]
        {
            """
            >>>+>>>>>+>>+>>+[<<],[
                -[-[-[-[-[-[-[-[<+>-[>+<-[>-<-[-[-[<++[<++++++>-]<
                    [>>[-<]<[>]<-]>>[<+>-[<->[-]]]]]]]]]]]]]]]]
                <[-<<[-]+>]<<[>>>>>>+<<<<<<-]>[>]>>>>>>>+>[
                    <+[
                        >+++++++++<-[>-<-]++>[<+++++++>-[<->-]+[+>>>>>>]]
                        <[>+<-]>[>>>>>++>[-]]+<
                    ]>[-<<<<<<]>>>>
                ],
            ]+<++>>>[[+++++>>>>>>]<+>+[[<++++++++>-]<.<<<<<]>>>>>>>>]
            """,
            "Hello world\nthis is me\0",
            "\t1\t5\t22\n",
        };
    }

    /// <summary>
    /// Test that a set of programs behave as expected.
    /// </summary>
    /// <param name="program">The program to test.</param>
    /// <param name="input">The console input.</param>
    /// <param name="expectedOutput">The expected output.</param>
    [Theory]
    [MemberData(nameof(GenerateProgramAndExpectedOutput))]
    public void ProgramTests(string program, string input, string expectedOutput)
    {
        int inputPosition = 0;
        var actualOutput = string.Empty;
        var interpreter = new BrainfuckInterpreter(
            program,
            cin: () => (byte)input[inputPosition++],
            cout: b => actualOutput = actualOutput + (char)b);
        interpreter.Run();
        actualOutput.Should().Be(expectedOutput);
    }
}