#!/usr/bin/env pwsh
##############################################################################################################

Function Show-Usage {
    "
Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build   Build program
" | Out-Host
}

Function Request-File {
    While ($Input.MoveNext()) {
        $VAR = @{
            Uri = $Input.Current
            OutFile = (Split-Path -Path $Input.Current -Leaf).Split('?')[0]
        }
        Invoke-WebRequest @VAR
        Return $VAR.OutFile
    }
}

Function Install-Program {
    While ($Input.MoveNext()) {
        Switch ((Split-Path -Path $Input.Current -Leaf).Split('.')[-1]) {
            'msi' {
                & msiexec /passive /package $Input.Current | Out-Null
            }
            Default {
                & ".\$($Input.Current)" /SP- /VERYSILENT /SUPPRESSMSGBOXES /NORESTART | Out-Null
            }
        }
        Remove-Item $Input.Current
    }
}

Function Build-Project {
    @(
        @{
            Cmd = 'lazbuild'
            Url = 'https://fossies.org/windows/misc/lazarus-3.6-fpc-3.2.2-win64.exe'
            Path = "C:\Lazarus"
        }
    ) | Where-Object { ! (Test-Path -Path $_.Path) } |
        ForEach-Object {
            $_.Url | Request-File | Install-Program
            $Env:PATH+=";$($_.Path)"
            (Get-Command $_.Cmd).Source | Out-Host
        }
    If (Test-Path -Path '.gitmodules') {
        & git submodule update --init --recursive --force --remote | Out-Host
        ".... [[$($LastExitCode)]] git submodule update" | Out-Host
    }
    $Env:Src = 'lazpaint'
    $Env:Use = 'use'
    $Env:Pkg = 'use\components.txt'
    If (Test-Path -Path $Env:Use) {
        If (Test-Path -Path $Env:Pkg) {
            Get-Content -Path $Env:Pkg |
                Where-Object {
                    ! (Test-Path -Path "$($Env:Use)\$($_)") &&
                    ! (& lazbuild --verbose-pkgsearch $_ ) &&
                    ! (& lazbuild --add-package $_)
                } | ForEach-Object {
                    Return @{
                        Uri = "https://packages.lazarus-ide.org/$($_).zip"
                        Path = "$($Env:Use)\$($_)"
                        OutFile = (New-TemporaryFile).FullName
                    }
                } | ForEach-Object -Parallel {
                    Invoke-WebRequest -OutFile $_.OutFile -Uri $_.Uri
                    Expand-Archive -Path $_.OutFile -DestinationPath $_.Path
                    Remove-Item $_.OutFile
                    Return ".... download $($_.Uri)"
                } | Out-Host
        }
        (Get-ChildItem -Filter '*.lpk' -Recurse -File –Path $Env:Use).FullName |
            ForEach-Object {
                & lazbuild --add-package-link $_ | Out-Null
                Return ".... [$($LastExitCode)] add package link $($_)"
            } | Out-Host
    }
    If (Test-Path -Path $Env:Src) {
        Exit (
            (Get-ChildItem -Filter '*.lpi' -Recurse -File –Path $Env:Src).FullName |
                Sort-Object |
                ForEach-Object {
                    $error = 0
                    $Output = (& lazbuild --build-all --recursive --no-write-project --build-mode='release' $_)
                    $Result = @(".... [$($LastExitCode)] build project $($_)")
                    If ($LastExitCode -eq 0) {
                        $Result += $Output | Select-String -Pattern 'Linking'
                    } Else {
                        $error = 1
                        $Result += $Output | Select-String -Pattern 'Error:', 'Fatal:'
                    }
                    $Result | Out-Host
                    Return $error
                } | Measure-Object -Sum
        ).Sum
    }
}

Function Switch-Action {
    $ErrorActionPreference = 'stop'
    Set-PSDebug -Strict #-Trace 1
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    If ($args.count -gt 0) {
        Switch ($args[0]) {
            'build' {
                Build-Project
            }
            Default {
                Show-Usage
            }
        }
    } Else {
        Show-Usage
    }
}

##############################################################################################################
Switch-Action @args | Out-Null
