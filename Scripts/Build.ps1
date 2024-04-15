param([string[]]$Languages = $("csharp", "python", "cpp", "go", "pascal", "ada"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet build ./Brainfuck -c:Release
    $Success = $Success -and $?
    dotnet build ./Brainfuck.Tests -c:Release
    $Success = $Success -and $?
    Pop-Location
}

if ("python" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Python ==="
    Push-Location ./python
    $Success = $Success -and $?
    Pop-Location
}

if ("cpp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== C++ ==="
    Push-Location ./cpp
    cmake .
    $Success = $Success -and $?
    cmake --build .
    $Success = $Success -and $?
    Pop-Location
}

if ("go" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Go ==="
    Push-Location ./go
    go build
    Pop-Location
}

if ("pascal" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Pascal ==="
    Push-Location ./pascal
    fpc Brainfuck.pas
    $Success = $Success -and $?
    fpc BrainfuckTest.pas
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("ada" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Ada ==="
    Push-Location ./ada
    alr build
    Pop-Location
    Write-Host ""
}

if ($Success) {
    Write-Host "`n`nBuild successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`n`nBuild failed" -ForegroundColor:Red
    exit 1
}