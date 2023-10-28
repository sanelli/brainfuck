// <copyright file="BrainfuckInterpreter.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

namespace Brainfuck;

/// <summary>
/// The brainfuck interpreter.
/// </summary>
public sealed class BrainfuckInterpreter
{
    private readonly string program;
    private readonly int tapeSize;
    private readonly Dictionary<int, int> jumpTable;
    private readonly Func<byte> cin;
    private readonly Action<byte> cout;

    /// <summary>
    /// Initializes a new instance of the <see cref="BrainfuckInterpreter"/> class.
    /// </summary>
    /// <param name="program">The program to be run.</param>
    /// <param name="tapeSize">The size of the tape.</param>
    /// <param name="cin">The function to input a character.</param>
    /// <param name="cout">The function to output a character.</param>
    public BrainfuckInterpreter(string program, int tapeSize = 30000, Func<byte>? cin = null, Action<byte>? cout = null)
    {
        this.program = program;
        this.tapeSize = tapeSize;
        this.jumpTable = this.ComputeJumpTable();
        this.cin = cin ?? DefaultCin;
        this.cout = cout ?? DefaultCout;
    }

    /// <summary>
    /// Run the interpreter.
    /// </summary>
    public void Run()
    {
        var pointer = this.tapeSize;
        var tape = new byte[2 * this.tapeSize];
        for (var programCounter = 0; programCounter < this.program.Length; ++programCounter)
        {
            switch (this.program[programCounter])
            {
                case '>':
                    ++pointer;
                    break;
                case '<':
                    --pointer;
                    break;
                case '+':
                    ++tape[pointer];
                    break;
                case '-':
                    --tape[pointer];
                    break;
                case '.':
                    this.cout(tape[pointer]);
                    break;
                case ',':
                    tape[pointer] = this.cin();
                    break;
                case '[':
                    if (tape[pointer] == 0)
                    {
                        programCounter = this.jumpTable[programCounter] - 1;
                    }

                    break;
                case ']':
                    if (tape[pointer] != 0)
                    {
                        programCounter = this.jumpTable[programCounter] - 1;
                    }

                    break;
            }
        }
    }

    private static byte DefaultCin()
    {
        try
        {
            var key = Console.ReadKey();
            return (byte)key.KeyChar;
        }
        catch (InvalidOperationException)
        {
            return (byte)Console.Read();
        }
    }

    private static void DefaultCout(byte b)
    {
        Console.Write((char)b);
    }

    private Dictionary<int, int> ComputeJumpTable()
    {
        var table = new Dictionary<int, int>();
        var stack = new Stack<int>();
        for (int programCounter = 0; programCounter < this.program.Length; ++programCounter)
        {
            if (this.program[programCounter] == '[')
            {
                stack.Push(programCounter);
            }
            else if (this.program[programCounter] == ']')
            {
                if (stack.Count == 0)
                {
                    throw new BrainFuckException($"The jump character at {programCounter} does not have a matching open jump.");
                }

                var open = stack.Pop();
                table.Add(open, programCounter);
                table.Add(programCounter, open);
            }
        }

        // Check all jumps are resolved
        if (stack.Count > 0)
        {
            throw new BrainFuckException($"The jumps [{string.Join(", ", stack)}] does not have a closing jump.");
        }

        return table;
    }
}