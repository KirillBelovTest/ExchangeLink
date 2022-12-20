(* ::Package:: *)

(* ::Chapter:: *)
(*Settings*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`ExchangeLink`Settings`"]


(* ::Section:: *)
(*Usings*)


Get["KirillBelov`ExchangeLink`Tools`"]


(* ::Section:: *)
(*Clear all*)


ClearAll["`*"]


(* ::Section:: *)
(*Names*)


$ExchangeLinkDirectory::usage = 
"$ExchangeLinkDirectory \
project directory"


$ExchangeLinkSettings::usage = 
"$ExchangeLinkSettings \
this variable stores settings for all available exchanges in one place. \
For each exchange, you can define in it the API address \
and secret keys for access to trading, wallet, etc"


(* ::Section:: *)
(*Private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Advances messages*)


$ExchangeLinkSettings::notfound = 
"File with ExchangeLink Settings not found in any paths: \n`1`"


(* ::Section:: *)
(*Implementation*)


$ExchangeLinkDirectory = 
ParentDirectory[DirectoryName[$InputFileName]]


$ExchangeLinkSettings := 
Cache[Get[$settingsFile], $cacheTime]


$ExchangeLinkSettings /: 
Set[$ExchangeLinkSettings[exchange_String, key_String], value_] := 
Module[{settings, temp = CreateFile[]}, 
	If[FileExistsQ[$settingsFile], 
		settings = Get[$settingsFile], 
		settings = <||>
	]; 
	
	If[Not[KeyExistsQ[settings, exchange]], 
		settings[exchange] = <||>		
	]; 
	
	settings[exchange, key] = value; 
	
	Export[$settingsFile, settings, "WL"]; 
	Encode[$settingsFile, temp, MachineID -> $MachineID]; 
	DeleteFile[$settingsFile]; 
	CopyFile[temp, $settingsFile]; 
	DeleteFile[temp]; 
	
	Block[{$cacheTime = 1}, $ExchangeLinkSettings]; 
	
	Return[value]
]


(* ::Section:: *)
(*Internal functions*)


$cacheTime = 60 * 60


$settingsFile = 
FileNameJoin[{$HomeDirectory, ".ExchangeLinkSettings"}]


(* ::Section:: *)
(*End private context*)


End[] (*`Private`*)


(* ::Section:: *)
(*End package*)


EndPackage[] (*KirillBelov`ExchangeLink`Settings`*)
