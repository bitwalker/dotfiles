@{
 GroupName = 'Command'
 Name = 'Case-Correct Command Parameter Names'
 Description = 'Replaces Command Parameters With Correct Casing'
 Tooltip = @'
PowerShell commands parameter names are not case-sensitive, but casing can improve readability.

This unit will look up all command parameters and replace incorrectly cased
or incomplete command parameter names with full correct casing parameter names.
'@
 Icon = 'image.png'
 Script = 'CommandParameter.ps1'
 RecommendedOrder = 20
 GUID = '5c70c737-e1d9-40b4-aa7e-a5e40e73c206'
 GroupGUID = '7a5eef2e-8c5d-4acd-81f2-20cbdfe36e89'
}