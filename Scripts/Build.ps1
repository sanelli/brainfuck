param([string[]]$Languages = $("csharp", "python", "cpp", "go"))

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

if ($Success) {
    Write-Host "`n`nBuild successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`n`nBuild failed" -ForegroundColor:Red
    exit 1
}