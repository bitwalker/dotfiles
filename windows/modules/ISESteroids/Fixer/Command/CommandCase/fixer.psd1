@{
 GroupName = 'Command'
 Name = 'Case-Correct Command Names'
 Description = 'Replaces Command Names With Correct Casing'
 Tooltip = @'
PowerShell commands are not case-sensitive, but casing can improve readability.

This unit will look up all commands and replace incorrectly cased
command names with correct casing.
'@
 Icon = 'image.png'
 Script = 'CommandCase.ps1'
 RecommendedOrder = 15
 GUID = 'f608bacb-0f2b-444f-ac0d-158e73f172ee'
 GroupGUID = 'e44420d0-2782-40ee-a533-ea2f39b9f48e'
}