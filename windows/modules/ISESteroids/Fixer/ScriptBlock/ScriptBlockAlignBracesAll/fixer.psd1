@{
 GroupName = 'Script Block'
 Name = 'Align Braces (Including One-Liner)'
 Description = 'Places Braces On Individual Lines'
 Tooltip = @'
Scriptblocks contain PowerShell code and are delimited by braces.
Scriptblocks are part of many PowerShell structures such as functions
or conditional statements such as "If" or "Switch".
This unit will
- move start and end braces in separate lines for better alignment
This unit will also convert one-liners (start and end brace on same line).
'@
 Icon = 'image.png'
 Script = 'ScriptBlockAlignBracesAll.ps1'
 RecommendedOrder = 10
 GUID = 'db88158d-0abe-4d43-b3dd-8b0a789e523f' 
 GroupGUID = '40c1fc72-674e-4727-aee2-80a53ead2e0f'
}