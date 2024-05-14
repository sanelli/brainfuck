param([string[]]$Languages = $("csharp", "python", "cpp", "go", "pascal", "ada", "c", "d", "rust"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet run --project ./Brainfuck -c:Release --no-build ../samples/hello-world-short.bf
    $Success = $Success -and $?
    Pop-Location
}

if ("python" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Python ==="
    Push-Location ./python
    python3 ./brainfuck.py ../samples/hello-world-short.bf
    $Success = $Success -and $?
    Pop-Location
}

if ("cpp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== C++ ==="
    Push-Location ./cpp
    ./brainfuck ../samples/hello-world-short.bf
    $Success = $Success -and $?
    Pop-Location
}

if ("go" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Go ==="
    Push-Location ./go
    ./brainfuck ../samples/hello-world-short.bf
    $Success = $Success -and $?
    Pop-Location
}

if ("pascal" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Pascal ==="
    Push-Location ./pascal
    ./Brainfuck ../samples/hello-world-short.bf
    $Success = $Success -and $?
    Pop-Location
}

if ("ada" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Ada ==="
    Push-Location ./ada
    ./bin/brainfuck ../samples/hello-world-short.bf
    $Success = $Success -and $?
    Pop-Location
}

if ("c" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== C ==="
    Push-Location ./c
    ./brainfuck ../samples/hello-world-short.bf
    $Success = $Success -and $?
    Pop-Location
}

if ("d" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== D ==="
    Push-Location ./d
    ./brainfuck --filename ../samples/hello-world-short.bf
    $Success = $Success -and $?
    Pop-Location
}

if ("rust" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Rust ==="
    Push-Location ./rust
    cargo run -q -- ../samples/hello-world-short.bf
    $Success = $Success -and $?
    Pop-Location
}


if ($Success) {
    Write-Host "`n`nRun successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`n`nRun failed" -ForegroundColor:Red
    exit 1
}
