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

def main(argv):
    if(len(argv) < 1):
        return 1
    
    file = open(argv[0], mode='r')
    program = file.read()
    file.close()

    try:
        interpreter = BrainfuckInterpreter(program, DefaultTapeSize, cin, cout)
        interpreter.run()
    except Exception as e:
        print(e)

if __name__ == "__main__":
    returnCode = main(sys.argv[1::])
    exit(returnCode)