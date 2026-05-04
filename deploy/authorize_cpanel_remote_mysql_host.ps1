[CmdletBinding(DefaultParameterSetName = "Token")]
param(
    [string]$CpanelHost = "servernew.syokdc.com",
    [int]$CpanelPort = 2083,
    [string]$CpanelUser = "mytrusth",
    [string]$RemoteHost = "74.220.52.251",
    [string]$Note = "Render pineguard-api Singapore outbound host",
    [Parameter(ParameterSetName = "Token")]
    [string]$ApiToken = $env:CPANEL_API_TOKEN,
    [Parameter(ParameterSetName = "Password")]
    [switch]$UsePassword
)

$ErrorActionPreference = "Stop"

function ConvertTo-PlainText([securestring]$Value) {
    $credential = [System.Net.NetworkCredential]::new("", $Value)
    return $credential.Password
}

function Invoke-CpanelUapi([string]$Function, [hashtable]$Query, [hashtable]$Headers) {
    $builder = [System.UriBuilder]::new("https", $CpanelHost, $CpanelPort, "/execute/Mysql/$Function")
    $queryParts = [System.Collections.Generic.List[string]]::new()
    foreach ($key in $Query.Keys) {
        $encodedKey = [System.Web.HttpUtility]::UrlEncode($key)
        $encodedValue = [System.Web.HttpUtility]::UrlEncode([string]$Query[$key])
        $queryParts.Add("$encodedKey=$encodedValue")
    }
    $builder.Query = [string]::Join("&", $queryParts)
    Invoke-RestMethod -Uri $builder.Uri.AbsoluteUri -Headers $Headers -Method Get -TimeoutSec 30
}

Add-Type -AssemblyName System.Web

if ([string]::IsNullOrWhiteSpace($CpanelHost)) {
    throw "CpanelHost is required."
}
if ([string]::IsNullOrWhiteSpace($CpanelUser)) {
    throw "CpanelUser is required."
}
if ([string]::IsNullOrWhiteSpace($RemoteHost)) {
    throw "RemoteHost is required."
}

$headers = @{}
if ($UsePassword) {
    $password = ConvertTo-PlainText (Read-Host "cPanel password for $CpanelUser" -AsSecureString)
    $basic = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes("${CpanelUser}:$password"))
    $headers.Authorization = "Basic $basic"
} else {
    if ([string]::IsNullOrWhiteSpace($ApiToken)) {
        throw "ApiToken is required. Set CPANEL_API_TOKEN or pass -ApiToken."
    }
    $headers.Authorization = "cpanel ${CpanelUser}:$ApiToken"
}

$addHost = Invoke-CpanelUapi -Function "add_host" -Query @{ host = $RemoteHost } -Headers $headers
if ($addHost.result.status -ne 1) {
    throw "Failed to add cPanel Remote MySQL host: $($addHost | ConvertTo-Json -Depth 6)"
}

$noteResult = $null
if (-not [string]::IsNullOrWhiteSpace($Note)) {
    $noteResult = Invoke-CpanelUapi -Function "add_host_note" -Query @{
        host = $RemoteHost
        note = $Note
    } -Headers $headers
}

[pscustomobject]@{
    CpanelHost = $CpanelHost
    CpanelUser = $CpanelUser
    RemoteHost = $RemoteHost
    AddHostStatus = $addHost.result.status
    NoteStatus = if ($noteResult) { $noteResult.result.status } else { $null }
}
