@{
 GroupName = 'Code Quality Improvement'
 Name = 'Replaces Double Quotes'
 Description = 'Turns double quotes into single quotes where appropriate.'
 Tooltip = @'
Use double quotes to expand variables and escape sequences in strings.
Use single quotes when you do not want PowerShell to change the text in any way.
This unit will turnall double-quoted strings into single quoted text if:
- there are no variables in the text that could be expanded
- there are no escape sequences in the text
'@
 Icon = 'image.png'
 Script = 'DoubleQuotes.ps1'
 RecommendedOrder = 3
 GUID = '1a3e919c-5581-45af-98a3-8c1bc4f86edc'
 GroupGUID = '494ff452-e59a-40ea-ae5e-b4d3cfe07c12'
}