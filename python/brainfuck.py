#!/usr/bin/python3

import sys
from BrainfuckInterpreter import *
from getch import *

def cout(b : int) -> None:
    print(chr(b), end='')

def cin() -> int:
    v = getch()
    print(v, end = '')
    return ord(v)

def main(argv:list[str]) -> int:
    if(len(argv) < 1):
        return 1
    
    file = open(argv[0], mode='r')
    program = file.read()
    file.close()

    try:
        interpreter = BrainfuckInterpreter(program, DefaultTapeSize, cin, cout)
        interpreter.run()
        return 0
    except Exception as e:
        print(e)
        return 1

if __name__ == "__main__":
    returnCode = main(sys.argv[1::])
    exit(returnCode)