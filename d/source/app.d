import std.stdio;
import std.getopt;
import std.string;

import interpreter;

void main(string[] args)
{
	string filename;
	try
	{
		auto arguments = getopt(args,
			std.getopt.config.required, "filename|f", "The path of the file containing the brainfuck program", &filename);

		if (arguments.helpWanted)
		{
			defaultGetoptPrinter("Some information about the program.", arguments.options);
		}

		auto intrp = Interpreter.from_file(filename);
		intrp.run();
	}
	catch (std.getopt.GetOptException e)
	{
		"Usage: %s --filename <filename>".format(args[0]).writeln;
	}
}
