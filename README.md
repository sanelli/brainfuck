# brainfuck
Collection of brainfuck interpreters written in various languages.

## Prerequisities
### [C#](https://github.com/sanelli/brainfuck/tree/main/csharp)
- dotnet 7

### [Python](https://github.com/sanelli/brainfuck/tree/main/python)
- python 3.11.5

### [C++](https://github.com/sanelli/brainfuck/tree/main/cpp)
- CMake 3.27
- C++ 20 compiler

### [Go](https://github.com/sanelli/brainfuck/tree/main/go)
- Go 1.21.3

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