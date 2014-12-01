@{
 GroupName = 'Command'
 Name = 'Convert Positional Parameters'
 Description = 'Adds Parameter Names To Unnamed Arguments'
 Tooltip = @'
Command arguments can be positional if the parameter supports position.
A positional argument is defined by its position and data type.
Positional parameters are hard to read and error-prone because they
are not clearly assigned to a particular parameter.

This unit will match positional arguments to the most appropriate
named parameter. If there is ambiguity, no changes take place.

Ambiguity can occur when the argument cannot be identified at
design time. For example, if the argument is a variable, then it is
not possible to determine the content of this variable during design
time. If the parameter binding depends on data type, binding cannot 
occur during design time and is omitted.
'@
 Icon = 'image.png'
 Script = 'CommandPositionalParameter.ps1'
 RecommendedOrder = 20
 GUID = 'a8ee451a-cbd8-4e1f-8655-aebf5f18bd84'
 GroupGUID = '5cc2f7dc-bd2b-4147-8260-d7ce0df0df37'
}