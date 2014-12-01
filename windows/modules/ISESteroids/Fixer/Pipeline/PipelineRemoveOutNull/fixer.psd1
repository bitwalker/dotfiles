@{
 GroupName = 'Pipeline'
 Name = 'Remove Out-Null'
 Description = 'Replaces Out-Null With $null-Assignment'
 Tooltip = @'
To get rid of unwanted command output, you can pipe results to Out-Null,
or you can assign the results to the special $null variable.

Both approaches do the same, but piping to Out-Null is more than
40x slower.

This unit replaces Out-Null with a $null assignment.
'@
 Icon = 'image.png'
 Script = 'PipelineRemoveOutNull.ps1'
 RecommendedOrder = 10
 GUID = '8c3cd455-045b-4c63-b2fd-960480cac3bc'
 GroupGUID = '5a1c4cc4-dd7c-4e2e-b9e3-4b7cbcda05a0'
}