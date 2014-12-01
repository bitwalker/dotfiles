@{
 GroupName = 'Reformat Structures'
 Name = 'Replaces Semicolons with Line Breaks'
 Description = 'Replaces semicolons with line breaks.'
 Tooltip = @'
Semicolons can be used to combine multiple commands in one line. 
This is typically done in interactive code. In scripts, each command should be written in separate lines.
This unit will
- Remove Semicolons at Line Ends
- Replace Semicolons with Line Breaks where needed
'@
 Icon = 'image.png'
 Script = 'RemoveSemicolon.ps1'
 RecommendedOrder = 2
 GUID = 'fcf65566-4433-4034-92da-1bdef7206280'
 GroupGUID = '0cc23cda-31ee-4790-9706-49498c963b17'
}