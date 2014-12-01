@{
 GroupName = 'Script Block'
 Name = 'Align Braces (Except One-Liner)'
 Description = 'Places Braces On Individual Lines'
 Tooltip = @'
Scriptblocks contain PowerShell code and are delimited by braces.
Scriptblocks are part of many PowerShell structures such as functions
or conditional statements such as "If" or "Switch".
This unit will
- move start and end braces in separate lines for better alignment
This unit will not change scriptblock one-liners (start and end brace on same line).
'@
 Icon = 'image.png'
 Script = 'ScriptBlockAlignBracesSome.ps1'
 RecommendedOrder = 10
 Guid = '902fe7bf-bda2-4a5f-9045-9349e08e71f2'
 GroupGUID = '40c1fc72-674e-4727-aee2-80a53ead2e0f'
}