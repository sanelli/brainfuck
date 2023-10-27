// <copyright file="Program.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

using System.Text;

if (args.Length < 1)
{
    Console.Error.WriteLine("Usage: dotnet brainfuck <FILENAME>");
    return;
}

const int tapeSize = 30000;
var tape = new byte[2 * tapeSize];
var pointer = tapeSize;

int programCounter;
var program = File.ReadAllText(args[0], Encoding.ASCII);
var stack = new Stack<int>();

// Identify matching [ and ]
var jumps = new Dictionary<int, int>();
for (programCounter = 0; programCounter < program.Length; ++programCounter)
{
    if (program[programCounter] == '[')
    {
        stack.Push(programCounter);
    }
    else if (program[programCounter] == ']')
    {
        if (stack.Count == 0)
        {
            Console.Error.WriteLine($"The jump character at {programCounter} does not have a matching open jump.");
            return;
        }

        var open = stack.Pop();
        jumps.Add(open, programCounter);
        jumps.Add(programCounter, open);
    }
}

// Check all jumps are resolved
if (stack.Count > 0)
{
    Console.Error.WriteLine($"The jumps [{string.Join(", ", stack)}] does not have a closing jump.");
    return;
}

// Run the program
programCounter = 0;
while (programCounter < program.Length)
{
    switch (program[programCounter])
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
            Console.Write((char)tape[pointer]);
            break;
        case ',':
            try
            {
                var key = Console.ReadKey();
                tape[pointer] = (byte)key.KeyChar;
            }
            catch (InvalidOperationException)
            {
                tape[pointer] = (byte)Console.Read();
            }

            break;
        case '[':
            if (tape[pointer] == 0)
            {
                programCounter = jumps[programCounter] - 1;
            }

            break;
        case ']':
            if (tape[pointer] != 0)
            {
                programCounter = jumps[programCounter] - 1;
            }

            break;
    }

    ++programCounter;
}