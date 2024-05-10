param([string[]]$Languages = $("csharp", "python", "cpp", "go", "pascal", "ada", "c", "d"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet test ./Brainfuck.Tests -c:Release
    $Success = $Success -and $?
    Pop-Location
}

if ("python" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Python ==="
    Push-Location ./python
    python3 test.py
    $Success = $Success -and $?
    Pop-Location
}

if ("cpp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== C++ ==="
    Push-Location ./cpp
    ./brainfuck-tests
    $Success = $Success -and $?
    Pop-Location
}

if ("go" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Go ==="
    Push-Location ./go
    go test ./interpreter/
    $Success = $Success -and $?
    Pop-Location
}

if ("pascal" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Pascal ==="
    Push-Location ./pascal
    ./BrainfuckTest
    $Success = $Success -and $?
    Pop-Location
}

if ("ada" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Ada ==="
    Push-Location ./ada/test
    alr run
    $Success = $Success -and $?
    Pop-Location
}

if ("c" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== C ==="
    Push-Location ./c
    ./brainfuck-tests
    $Success = $Success -and $?
    Pop-Location
}

if ("d" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== D ==="
    Push-Location ./d
    dub test
    $Success = $Success -and $?
    Pop-Location
}

if ($Success) {
    Write-Host "`n`nTest successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`n`nTest failed" -ForegroundColor:Red
    exit 1
}