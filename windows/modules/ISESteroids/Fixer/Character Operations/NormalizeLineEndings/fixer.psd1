@{
 GroupName = 'Character Operations'
 Name = 'Normalize Line Endings'
 Description = 'Replaces irregular line endings with CRLF.'
 IgnoreSyntaxErrors = $true
 Tooltip = @'
Typically, line endings are marked by two characters: 
- CR (carriage return, ASCII 13)
- and LF (line feed, ASCII 10)
When you paste code from other sources (like web pages), sometimes line
endings are irregular, consisting of only CR or only LF or reverse order.
While the ISE editor can display most irregular line endings correctly,
other editors may not. 
Also, since irregular line endings may consist of less or more than 2 characters,
when you parse your code, you may get irregular results because of offset shifts.
This unit normalizes line endings and turns irregular
line endings into a consistent CRLF pair.
'@
 #Icon = 'image.png'
 Script = 'NormalizeLineEndings.ps1'
 RecommendedOrder = 1
 GUID = 'd5c7c2cc-28bc-405a-ab5f-12f7d7408f39'
 GroupGUID = '5b664b42-0f95-4737-a554-c6adea9c6aa9'

}