@{
 GroupName = 'Pipeline'
 Name = 'Linebreak After Pipe'
 Description = 'Adds A LineBreak After Each Pipeline Symbol'
 Tooltip = @'
Pipeline commands can become very long, depending on how many
commands participate in a pipeline.

This unit will break up long pipeline statements by adding
linebreaks after each pipeline ("|") operator.
'@
 Icon = 'image.png'
 Script = 'PipelineBreakAfterPipe.ps1'
 RecommendedOrder = 5
 GUID = '3b046488-ac92-4d23-966b-5003f11a1105'
 GroupGUID = 'd3bfbb35-e540-4ed3-b0c9-a3407b77e74c'
}