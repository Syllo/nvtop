# NVTOP Windows Launcher (PowerShell)
# Adds DLL directory to PATH before launching nvtop.exe

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$env:PATH = "$ScriptDir;$env:PATH"
& "$ScriptDir\nvtop.exe" $args
exit $LASTEXITCODE
