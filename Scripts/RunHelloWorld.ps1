$Success = $true

Write-Host -ForegroundColor:"Yellow" "=== C# ==="
Push-Location ./csharp
dotnet run --project ./Brainfuck -c:Release --no-build ../samples/hello-world-short.bf
$Success = $Success -and $?
Pop-Location

Write-Host -ForegroundColor:"Yellow" "`n=== Python ==="
Push-Location ./python
python3 ./brainfuck.py ../samples/hello-world-short.bf
$Success = $Success -and $?
Pop-Location

Write-Host -ForegroundColor:"Yellow" "`n=== C++ ==="
Push-Location ./cpp
./brainfuck ../samples/hello-world-short.bf
$Success = $Success -and $?
Pop-Location

if($Success)
{
    Write-Host "`n`nRun successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`n`nRun failed" -ForegroundColor:Red
    exit 1
}