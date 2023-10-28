$Success = $true

Write-Host -ForegroundColor:"Yellow" "=== C# ==="
Push-Location ./csharp
dotnet test ./Brainfuck.Tests -c:Release
$Success = $Success -and $?
Pop-Location

Write-Host -ForegroundColor:"Yellow" "`n=== Python ==="
Push-Location ./python
python3 test.py
$Success = $Success -and $?
Pop-Location

Write-Host -ForegroundColor:"Yellow" "`n=== C++ ==="
Push-Location ./cpp
./brainfuck-tests
$Success = $Success -and $?
Pop-Location

if($Success)
{
    Write-Host "`n`nTest successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`n`nTest failed" -ForegroundColor:Red
    exit 1
}