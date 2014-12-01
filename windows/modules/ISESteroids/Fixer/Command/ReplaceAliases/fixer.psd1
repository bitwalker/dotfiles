@{
 GroupName = 'Command'
 Name = 'Turns Aliases Into Commands'
 Description = 'Replaces aliases with their command names.'
 Tooltip = @'
Command aliases help you work interactively with PowerShell.
Aliases are typically shorter than cmdlet names, or they can mimick historic commands like "dir" or "ls".
Aliases should never be used in scripts. This unit will turn all aliases into real commands because:
- Aliases can change
- Aliases may not be present on another system
- Aliases can be confusing to read
'@
 Icon = 'image.png'
 Script = 'ReplaceAliases.ps1'
 RecommendedOrder = 5
 GUID = 'c9946b58-e589-4bf8-a567-6a984ccee558'
 GroupGUID = '501d0af6-7778-4486-95fb-845b3a8833c0'
}