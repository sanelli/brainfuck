param([string[]]$Languages = $("csharp", "python", "cpp", "go", "pascal", "ada"))

function Invoke-Checks() {
    $Success = $true

    if ("csharp" -in $Languages) {
        Write-Host -ForegroundColor:"Yellow" "=== C# ==="
        $Exists = Confirm-CommandExists "dotnet"
        $CorrectVersion = Confirm-DotnetVersion "8"
        $LocalSuccess = $Exists -and $CorrectVersion
        $Success = $Success -and $LocalSuccess
        Write-Succes $LocalSuccess "C#"
        if (-not $LocalSuccess) {
            Write-Host -ForegroundColor:"Red" "Visit https://dotnet.microsoft.com/en-us/download for installation instructions"
        }
    }

    if ("python" -in $Languages) {
        Write-Host -ForegroundColor:"Yellow" "=== Python ==="
        $Exists = Confirm-CommandExists "python3"
        $CorrectVersion = Confirm-Pythong3Version 3 11
        $LocalSuccess = $Exists -and $CorrectVersion
        $Success = $Success -and $LocalSuccess
        Write-Succes $LocalSuccess "Python"
        if (-not $LocalSuccess) {
            Write-Host -ForegroundColor:"Red" "Visit https://www.python.org/downloads for installation instructions"
        }
    }

    if ("cpp" -in $Languages) {
        Write-Host -ForegroundColor:"Yellow" "=== C++ ==="
        $Exists = Confirm-CommandExists "cmake"
        $CorrectVersion = Confirm-CMakeVersion 3 27
        $LocalSuccess = $Exists -and $CorrectVersion
        $Success = $Success -and $LocalSuccess
        Write-Succes $LocalSuccess "C++"
        if (-not $LocalSuccess) {
            Write-Host -ForegroundColor:"Red" "Visit https://cmake.org/download/ for installation instructions"
        }
    }

    if ("go" -in $Languages) {
        Write-Host -ForegroundColor:"Yellow" "=== Go ==="
        $Exists = Confirm-CommandExists "go"
        $CorrectVersion = Confirm-GoVersion 1 21
        $LocalSuccess = $Exists -and $CorrectVersion
        $Success = $Success -and $LocalSuccess
        Write-Succes $LocalSuccess "go"
        if (-not $LocalSuccess) {
            Write-Host -ForegroundColor:"Red" "Visit https://go.dev/doc/install for installation instructions"
        }
    }

    if ("pascal" -in $Languages) {
        Write-Host -ForegroundColor:"Yellow" "=== Pascal ==="
        $Exists = Confirm-CommandExists "fpc"
        $CorrectVersion = Confirm-PascalVersion 3 2
        $LocalSuccess = $Exists -and $CorrectVersion
        $Success = $Success -and $LocalSuccess
        Write-Succes $LocalSuccess "Pascal"
        if (-not $LocalSuccess) {
            Write-Host -ForegroundColor:"Red" "Visit https://www.freepascal.org/ for installation instructions"
        }
    }

    if ("ada" -in $Languages) {
        Write-Host -ForegroundColor:"Yellow" "=== Ada ==="
        $Exists = Confirm-CommandExists "alr"
        $CorrectVersion = Confirm-AdaVersion 2 0
        $LocalSuccess = $Exists -and $CorrectVersion
        $Success = $Success -and $LocalSuccess
        Write-Succes $LocalSuccess "Ada"
        if (-not $LocalSuccess) {
            Write-Host -ForegroundColor:"Red" "Visit https://alire.ada.dev for installation instructions"
        }
    }

    if ($Success) {
        Write-Host "`n`nAll compilers available!" -ForegroundColor:Green
        exit 0
    }
    else {
        Write-Host "`n`nNot all compilers are available" -ForegroundColor:Red
        exit 1
    }
}

function Write-Succes([bool]$Success, [string]$Language) {
    if ($Success) {
        Write-Host "The required command(s) exist(s) for $Language" -ForegroundColor:Green
    }
    else {
        Write-Host "The required command(s) does/do NOT exist for $Language" -ForegroundColor:Red
    }
}

function Confirm-CommandExists($command) {
    Get-Command $command 2>&1 >$null
    if (-not $?) {
        Write-Host "Cannot find executable '$command'" -ForegroundColor:Red
        return $false
    }

    return $true
}

function Confirm-DotnetVersion($expectedVersion) {

    $sdks = dotnet --list-sdks

    foreach ($sdk in $sdks) {
        $major = $sdk.Split(".")[0]
        if ($major -eq $expectedVersion) {
            return $true
        }
    }
  
    Write-Host "Require dotnet version $expectedVersion but version $version found." -ForegroundColor:Red
    return $false;
}

function Confirm-Pythong3Version([int]$expectedMajorVersion, [int]$expectedMinorVersion) {

    $pythonVersion = python3 --version
    $version = $pythonVersion -split " "
    $versionMajorMinor = $version[1] -split "\."
    $versionMajor = [System.Int32]::Parse($versionMajorMinor[0].Trim())
    $versionMinor = [System.Int32]::Parse($versionMajorMinor[1].Trim())

    $Success = ($versionMajor -gt $expectedMajorVersion) -or ($versionMajor -eq $expectedMajorVersion) -and ($versionMinor -ge $expectedMinorVersion)

    if (-not $Success) {
        Write-Host "Require Python3 version >= $expectedMajorVersion.$expectedMinorVersion but version $version found." -ForegroundColor:Red
    }
    return $Success;
}

function Confirm-CMakeVersion([int]$expectedMajorVersion, [int]$expectedMinorVersion) {

    $cmakeVersion = cmake --version
    $version = $cmakeVersion -split " "

    $versionMajorMinor = $version[2] -split "\."
    $versionMajor = [System.Int32]::Parse($versionMajorMinor[0].Trim())
    $versionMinor = [System.Int32]::Parse($versionMajorMinor[1].Trim())

    $Success = ($versionMajor -gt $expectedMajorVersion) -or ($versionMajor -eq $expectedMajorVersion) -and ($versionMinor -ge $expectedMinorVersion)

    if (-not $Success) {
        Write-Host "Require cmake version >= $expectedMajorVersion.$expectedMinorVersion but version $version found." -ForegroundColor:Red
    }
    return $Success;
}

function Confirm-GoVersion([int]$expectedMajorVersion, [int]$expectedMinorVersion) {

    $goVersion = go version
    $version = $goVersion -split " "
    $versionMajorMinor = $version[2].substring(2) -split "\."
    $versionMajor = [System.Int32]::Parse($versionMajorMinor[0].Trim())
    $versionMinor = [System.Int32]::Parse($versionMajorMinor[1].Trim())

    $Success = ($versionMajor -gt $expectedMajorVersion) -or ($versionMajor -eq $expectedMajorVersion) -and ($versionMinor -ge $expectedMinorVersion)

    if (-not $Success) {
        Write-Host "Require go version >= $expectedMajorVersion.$expectedMinorVersion but version $version found." -ForegroundColor:Red
    }
    return $Success;
}

function Confirm-PascalVersion([int]$expectedMajorVersion, [int]$expectedMinorVersion)
{
    $pascalVersion = $(fpc -h | Select-String -Pattern "Free Pascal Compiler Version").Line.Split()[4]
    $versionMajorMinor = $pascalVersion -split "\."
    $versionMajor = [System.Int32]::Parse($versionMajorMinor[0].Trim())
    $versionMinor = [System.Int32]::Parse($versionMajorMinor[1].Trim())

    $Success = ($versionMajor -gt $expectedMajorVersion) -or ($versionMajor -eq $expectedMajorVersion) -and ($versionMinor -ge $expectedMinorVersion)

    if (-not $Success) {
        Write-Host "Require fpc version >= $expectedMajorVersion.$expectedMinorVersion but version $version found." -ForegroundColor:Red
    }
    return $Success;
}

function Confirm-AdaVersion([int]$expectedMajorVersion, [int]$expectedMinorVersion) {

    $adaVersion = alr --version
    $version = $adaVersion -split " "
    $versionMajorMinor = $version[1] -split "\."
    $versionMajor = [System.Int32]::Parse($versionMajorMinor[0].Trim())
    $versionMinor = [System.Int32]::Parse($versionMajorMinor[1].Trim())

    $Success = ($versionMajor -gt $expectedMajorVersion) -or ($versionMajor -eq $expectedMajorVersion) -and ($versionMinor -ge $expectedMinorVersion)

    if (-not $Success) {
        Write-Host "Require cmake version >= $expectedMajorVersion.$expectedMinorVersion but version $version found." -ForegroundColor:Red
    }
    return $Success;
}

Invoke-Checks