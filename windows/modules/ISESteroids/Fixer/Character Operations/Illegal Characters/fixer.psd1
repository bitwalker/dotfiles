@{
 GroupName = 'Character Operations'
 Name = 'Convert Illegal Characters'
 Description = 'Replaces illegal characters with legal characters.'
 IgnoreSyntaxErrors = $true
 Icon = 'illegalchar.png'
 Script = 'IllegalCharacters.ps1'
 RecommendedOrder = 1
 GUID = '7b521d9c-038b-46ca-9f93-115b5def6dc0'
 GroupGUID = 'b2d8b3dd-244c-48cf-8197-06b606543243'
 Tooltip = @'
When you copy and paste PowerShell code from the Internet or help files,
the code may accidentally contain "typographic" characters like typographic
hyphens or other Unicode characters that look like regular characters but are 
different.
This unit will identify and replace the most common illegal characters.
'@


}