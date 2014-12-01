@{
 GroupName = 'Reformat Structures'
 Name = 'Aligns Key-Value-Pairs in Hashtables'
 Description = 'puts each key-value pair into an individual line and aligns assignment operators.'
 Tooltip = @'
Hashtables can contain one or more key-value pairs. Key-value pairs can be defined in one line (using semicolons to separate them), or one per line.
This fixer will:
- place each key-value pair on an individual line, adding line breaks where needed
- align assignment operators to make keys and values align nicely
'@
 Icon = 'image.png'
 Script = 'ReformatHashtable.ps1'
 RecommendedOrder = 2
 GUID = '25efc521-d658-4876-866b-5207239b2113'
 GroupGUID = 'a6299653-8d78-4a47-88b8-3a1d4c1ca0e7'
}