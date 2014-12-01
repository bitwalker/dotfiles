@{
 GroupName = 'Variables'
 Name = 'Case-Correct Variable Names'
 Description = 'Replaces variables with initial casing.'
 Tooltip = @'
Variable names are generally not case-sensitive.
Still, a quality script uses consistent casing for variables. This improves readability.

This unit "normalizes" variable name casing. It takes the first instance of a variable.
It then searches for all other variables with the same name.
All other variables are renamed so that casing matches the variable name casing of the
first variable instance.

Simply adjust casing in the first instance of a variable, then run this unit.
'@
 Icon = 'image.png'
 Script = 'NormalizeVariables.ps1'
 RecommendedOrder = 10
 GUID = '9543db9a-afbd-43da-99bc-e4ecf98a41af'
 GroupGUID = 'ad1e7958-0340-461f-a741-253b9d1d0b87'
}