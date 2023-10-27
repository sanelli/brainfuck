// <copyright file="Program.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

using System.Text;

using Brainfuck;

if (args.Length < 1)
{
    WriteError("Usage: dotnet brainfuck <FILENAME>");
    return 1;
}

try
{
    var program = File.ReadAllText(args[0], Encoding.ASCII);
    var interpreter = new BrainfuckInterpreter(program);
    interpreter.Run();

    return 0;
}
catch (Exception e)
{
    WriteError(e.Message);
    return 1;
}

static void WriteError(string message)
{
    Console.ForegroundColor = ConsoleColor.Red;
    Console.WriteLine(message);
    Console.ResetColor();
}