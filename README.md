# brainfuck
Collection of brainfuck interpreters written in various languages.

## Prerequisities
Prerequisites can be validated by running the `Check.ps1` script, possibly with a list of languages to validate. If the parameter `-Languages` is not provided then all supported languages are cheked. In case of failures will provide a set of links to follow to help with the installation.
```powershell
./Scripts/Check.ps1
./Scripts/Check.ps1 -Languages:$("csharp", "python", "cpp")
```

### [C#](https://github.com/sanelli/brainfuck/tree/main/csharp)
- dotnet 8

### [Python](https://github.com/sanelli/brainfuck/tree/main/python)
- python 3.11.5

### [C++](https://github.com/sanelli/brainfuck/tree/main/cpp)
- CMake 3.27
- C++ 20 compiler (the script does not validate this as too many variations exists)

### [Go](https://github.com/sanelli/brainfuck/tree/main/go)
- Go 1.21.3

### [Pascal](https://github.com/sanelli/brainfuck/tree/main/pascal)
- Free pascal 3.2

## Compile and test
```powershell
./Scripts/Build.ps1
./Scripts/RunHello.ps1
./Scripts/Test.ps1
```

Each script accepts a `-Language` parameter with a list of languages:
```powershell
./Scripts/Build.ps1 -Languages:$("csharp", "python", "cpp")
./Scripts/RunHello.ps1 -Languages:$("csharp", "cpp")
./Scripts/Test.ps1 -Languages:$("cpp")
```

## Screenshots

<img width="682" alt="image" src="https://github.com/sanelli/brainfuck/assets/2866041/636f62c0-0b56-426a-b4ed-f8edb18dcb96">
