(* ::Package:: *)

BeginPackage["KirillBelov`ExchangeLink`Internal`Tools`"]


Timestamp::usage = 
"Timestamp[] current Unix time in milliseconds 
Timestamp[dateObject] specific Unix time in milliseconds"


OptionNames::usage = 
"OptionNames[symbols] \
returns the names of the options of the passed function in quotes"


Cache::usage = 
"Cache[expr] stores the result of the expr for 60 seconds
Cache[expr, period] stores the result of the expr for the specified period"


Begin["`Private`"]


(* Timestamp *)


Timestamp[date_DateObject?DateObjectQ] := 
Round[1000 * (UnixTime[date] + FractionalPart[AbsoluteTime[date]])]


Timestamp[] := 
Timestamp[Now]


(* options *)


OptionNames[symbols__Symbol] := 
Map[If[StringQ[#], "\"" <> # <> "\"", ToString[#]]&] @ 
Keys @ 
Flatten @ 
Map[Options] @ 
{symbols}


(* code completion *)


AddCodeCompletion[function_Symbol][args___] := 
Module[{completionType}, 
	completionType = {args} /. {
		None -> 0, 
		"AbsoluteFileName" -> 2, 
		"RelativeFileName" -> 3, 
		"Color" -> 4, 
		"PackageName" -> 7, 
		"DirectoryName" -> 8, 
		"InterpreterType" -> 9
	}; 
	(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&)[SymbolName[function] -> completionType]
]


(* cache *)


SetAttributes[Cache, HoldFirst]


Cache[expr_, date_DateObject] := (
	Cache[expr, {"Date"}] = date; 
	Cache[expr, date] = expr
)


Cache[expr_, period_Integer: 60] := 
Module[{time = AbsoluteTime[], roundedTime, previouseTime = Cache[expr, {"Date"}]}, 
	roundedTime = DateObject[Round[time, period]]; 
	If[DateObjectQ[previouseTime] && roundedTime != previouseTime, 
		Cache[expr, previouseTime] =.; 
		previouseTime = roundedTime; 
		Cache[expr, {"Date"}] = previouseTime; 
		Cache[expr, previouseTime] = expr, 
		
		previouseTime = roundedTime;
	]; 
	Return[Cache[expr, previouseTime]]
]


End[] (*`Private`*)


EndPackage[] (*KirillBelov`ExchangeLink`Internal`Tools`*)
