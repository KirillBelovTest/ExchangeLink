(* ::Package:: *)

BeginPackage["ExchangeLink`Tools`Settings`"]


ClearAll["`*"]


ExchangeLinkSettings::usage = 
"ExchangeLinkSettings[]
ExchangeLinkSettings[file]"


Begin["`Private`"]


$directory = ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]]


getEchangeLinkSettings[date_, file_] := 
getEchangeLinkSettings[date, file] = 
Which[
    FileExistsQ[file], 
        Get[file], 
    FileExistsQ[FileNameJoin[{$directory, file}]], 
        Get[FileNameJoin[{$directory, file}]], 
    FileExistsQ[FileNameJoin[{$HomeDirectory, file}]], 
        Get[FileNameJoin[{$HomeDirectory, file}]]
]


ExchangeLinkSettings[file: (_String | _File): ".ExchangeLink"] := 
getEchangeLinkSettings[Today, file]


End[]


EndPackage[]