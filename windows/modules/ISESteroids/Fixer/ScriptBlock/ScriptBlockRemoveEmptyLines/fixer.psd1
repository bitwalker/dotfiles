@{
 GroupName = 'Script Block'
 Name = 'Remove Empty Lines At Start/End'
 Description = 'Removes blank lines at the beginning and end'
 Tooltip = @'
This unit removes any empty line
- between the opening brace and the content
- between the end of content and the closing brace
This unit will not remove blank lines inside the
scriptblock content.
'@
 Icon = 'image.png'
 Script = 'ScriptBlockRemoveEmptyLines.ps1'
 RecommendedOrder = 4
 GUID = 'afd184bc-8576-4317-8f0c-f0dc1b8bd41b'
 GroupGUID = '858174a6-de86-4e48-8c7d-5f415f754f31'
}