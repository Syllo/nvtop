# PowerShell Profile Addition for NVTOP
# Add this to your PowerShell profile for easy nvtop access

# To edit your profile, run:
#   notepad $PROFILE

# Add these lines to your profile:
# ============================================

# NVTOP Quick Launch Function
function nvtop {
    <#
    .SYNOPSIS
        Launch NVTOP in WSL2
    .DESCRIPTION
        Runs NVTOP GPU monitoring tool in WSL2 environment
    .EXAMPLE
        nvtop
        # Launches nvtop with default settings
    .EXAMPLE
        nvtop -h
        # Shows nvtop help
    #>
    param(
        [Parameter(ValueFromRemainingArguments = $true)]
        [string[]]$Arguments
    )
    
    if ($Arguments) {
        wsl nvtop $Arguments
    }
    else {
        wsl nvtop
    }
}

# Set up tab completion for nvtop
Register-ArgumentCompleter -CommandName nvtop -ScriptBlock {
    param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameters)
    
    $completions = @(
        '-d', '--delay',
        '-v', '--version',
        '-h', '--help',
        '-c', '--config-file',
        '-p', '--no-plot',
        '-P', '--no-processes',
        '-r', '--reverse-abs',
        '-C', '--no-color',
        '-f', '--freedom-unit',
        '-i', '--gpu-info',
        '-E', '--encode-hide',
        '-s', '--snapshot'
    )
    
    $completions | Where-Object { $_ -like "$wordToComplete*" } | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
    }
}

# Alias for easy access
Set-Alias -Name gpumon -Value nvtop

Write-Host "NVTOP aliases loaded!" -ForegroundColor Green
Write-Host "  - Run 'nvtop' to launch GPU monitor" -ForegroundColor Cyan
Write-Host "  - Run 'gpumon' as an alternative" -ForegroundColor Cyan
