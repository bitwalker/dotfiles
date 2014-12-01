@{
 GroupName = 'Character Operations'
 Name = 'Fix Whitespace Following Backtick'
 Description = 'Remove Whitespace that follow a backtick'
 IgnoreSyntaxErrors = $false
 Tooltip = @'
When you use a backtick for line continuation, there must not follow any code in the same line after the backtick.
If there are spaces following the backtick, then they are hard to recognize and will lead to unexpected results.
This unit identifies these situations and removes any whitespace following a line continuation backtick.
'@
 #Icon = 'image.png'
 Script = 'BacktickSpace.ps1'
 RecommendedOrder = 1
 GUID = 'd7c58cec-46b5-4b04-925f-123315350ce3'
 GroupGUID = '272339ac-7e52-4fa5-9413-b8ae5063cb09'

}