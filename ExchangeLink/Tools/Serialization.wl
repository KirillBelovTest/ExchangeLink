(* ::Package:: *)

BeginPackage["ExchangeLink`Tools`Serialization`"]


ClearAll["`*"]


JSONDeserialize::usage = 
"JSONDeserialize[json]"


URLQueryFormat::usage = 
"URLQueryFormat[expr]"


Begin["`Private`"]


SetAttributes[nativeDeserialize, Listable]


nativeDeserialize[token_] := 
token


nativeDeserialize[unixTime_Integer] /; 
unixTime > 1.2*10^12 := 
FromUnixTime[unixTime/1000]


nativeDeserialize[number_String] /; 
StringMatchQ[number, NumberString] := 
ToExpression[number]


JSONDeserialize[json_String] := 
nativeDeserialize[ImportString[json, "RawJSON"]]


JSONDeserialize[response_HTTPResponse] := 
JSONDeserialize[response["Body"]]


URLQueryFormat[token_] := 
token


URLQueryFormat[date_DateObject] := 
1000 * UnixTime[date] + Round[1000 * FractionalPart[Last[DateList[date]]]]


End[]


EndPackage[]