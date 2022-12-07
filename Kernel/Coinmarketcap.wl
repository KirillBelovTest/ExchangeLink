(* ::Package:: *)

(* ::Title:: *)
(*Coinmarketcap*)


(* ::Section:: *)
(*info block*)


(* :Title: Coinmarketcap *)
(* :Context: CryptoWatcher`DataCollector`Coinmarketcap` *)
(* :Version: 0.0.1 *)
(* :Author: Kirill Belov *)
(* :Description: *)
(* :Discussion: *)


(* ::Section:: *)
(*package*)


BeginPackage["KirillBelov`ExchangeLink`Coinmarketcap`"]


(* ::Section:: *)
(*name clearing*)


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*declaration of public variables*)


collectCoinmarketcapData::usage = 
"collectCoinmarketcapData[\"ticker\"]\n\
collectCoinmarketcapData[\"ticker\", \"currency\"]\n\
collectCoinmarketcapData[\"ticker\", \"limit\" -> limit]"


representCoinmarketcapData::usage = 
"representCoinmarketcapData[data, \"format\"]"


saveCoinmarketcapData::usage = 
"saveCoinmarketcapData[data, \"db folder\"]"


extractCoinmarketcapData::usage = 
"extractCoinmarketcapData[\"db folder\", date]\n\
extractCoinmarketcapData[\"currency\", \"db folder\", date]"


(* ::Section:: *)
(*private context*)


Begin["`Private`"]


(* ::Section:: *)
(*internal variables*)


$coinmarketcapURL = 
"https://api.coinmarketcap.com/v1"


$convertList = 
{
	"AUD", "BRL", "CAD", "CHF", "CLP", "CNY", 
	"CZK", "DKK", "EUR", "GBP", "HKD", "HUF", 
	"IDR", "ILS", "INR", "JPY", "KRW", "MXN", 
	"MYR", "NOK", "NZD", "PHP", "PKR", "PLN", 
	"RUB", "SEK", "SGD", "THB", "TRY", "TWD", "ZAR"
}


$currencyList := $currencyList = 
<|"id" -> #["id"], "name" -> #["name"], "symbol" -> #["symbol"]|>& /@ 
collectCoinmarketcapData["ticker", "limit" -> 0]


(* ::Section:: *)
(*internal functions*)


(* ::Subsubsection:: *)
(*secondary functions*)


numberStringQ[numstring_String] := 
StringMatchQ[numstring, NumberString]

numberStringQ[___] := 
False


SetAttributes[stringToNumber, Listable]


stringToNumber[string_] := 
If[numberStringQ[string], ToExpression[string], string]


currencyQ[currency_String] := 
MemberQ[ToLowerCase[Flatten[Values[$currencyList]]], ToLowerCase[currency]]


currencyAlias[currency_String?currencyQ, key: ("id" | "name" | "symbol"): "id"] := 
SelectFirst[$currencyList, MemberQ[ToLowerCase[#], ToLowerCase[currency]]&][key]


(* ::Section:: *)
(*internal patterns*)


currencyDataPattern[] := 
<|Rule[_String, (_String | _Integer | _Real | Null)].. |>


formatPattern[] := 
("md" | "MD" | "wl" | "WL")


(* ::Section:: *)
(*public functions implementation*)


(* ::Subsubsection:: *)
(*collectCoinmarketcapData*)


(* ::Text:: *)
(*error message*)


collectCoinmarketcapData::reqerr = 
"Error during request to: `1`\n\
response status code: `2`\n\n`3`"


(* ::Text:: *)
(*options of the collector*)


Options[collectCoinmarketcapData] := 
{"start" -> 0, "limit" -> 100, "convert" -> "USD"}


(* ::Text:: *)
(*getting list of tickers*)


collectCoinmarketcapData[type_String, OptionsPattern[]] /; 
StringMatchQ[type, "ticker", IgnoreCase -> True] := 
Block[{
	start = OptionValue["start"], 
	limit = OptionValue["limit"], 
	convert = OptionValue["convert"], 
	request, response, 
	status, body, data
}, 
	request = URLBuild[
		{$coinmarketcapURL, type, ""}, 
		{"start" -> start, "limit" -> limit, "convert" -> convert}
	]; 
	response = URLRead[request]; 
	status = response["StatusCode"];
	body = response["Body"]; 
	If[status != 200, 
		Message[collectCoinmarketcapData::reqerr, request, status, body]; 
		Return[Null], 
		
	(*Else*)
		data = ImportString[body, "RawJSON"]; 
		Return[Map[stringToNumber, data]]
	]
]


(* ::Text:: *)
(*getting one ticker*)


collectCoinmarketcapData[type_String, currency_String, OptionsPattern[]] /; 
StringMatchQ[type, "ticker", IgnoreCase -> True] && currencyQ[currency] := 
Block[{
	id = currencyAlias[currency], 
	convert = OptionValue["convert"], 
	request, response, 
	status, body, data
}, 
	request = URLBuild[
		{$coinmarketcapURL, type, id, ""}, 
		{"convert" -> convert}
	]; 
	response = URLRead[request]; 
	status = response["StatusCode"];
	body = response["Body"]; 
	If[status != 200, 
		Message[collectCoinmarketcapData::reqerr, request, status, body]; 
		Return[Null], 
		
	(*Else*)
		data = ImportString[body, "RawJSON"]; 
		Return[stringToNumber[First[data]]]
	]
]


(* ::Subsubsection:: *)
(*representCoinmarketcapData*)


(* ::Text:: *)
(*function for the representation of the assotiations*)


representCoinmarketcapData[data: currencyDataPattern[], format: formatPattern[]: "MD"] := 
Block[{num, link, tag, bold, italic, date}, 
	Column[ Row /@ { 
		{link["coinmarketcap.com", "https://coinmarketcap.com/currencies", data["id"]]}, 
		{tag[data["name"]], " ", tag[data["symbol"]]}, 
		{"price: ", bold["\:20bf"], bold[num[data["price_btc"]]]}, 
		{"price: ", bold["$"], bold[num[data["price_usd"]]]}, 
		If[KeyExistsQ[data, "price_rur"], 
			{"price: ", bold["\:20bd"], bold[num[data["price_rur"]]]}, Nothing], 
		{"market capital: ", bold["$"], bold[num[data["market_cap_usd"]]]}, 
		If[KeyExistsQ[data, "price_rur"], 
			{"market capital: ", bold["\:20bd"], bold[num[data["market_cap_rur"]]]}, Nothing], 
		{"available supply: ", bold[num[data["available_supply"]]]}, 
		{"change last hour: ", bold[num[data["percent_change_1h"]]], bold["%"]}, 
		{"change last day: ", bold[num[data["percent_change_24h"]]], bold["%"]}, 
		{"change last week: ", bold[num[data["percent_change_7d"]]], bold["%"]}, 
		{"updated: ", date[data["last_updated"]]}, 
		{" - - - - - - - "}
	}] /. {
		tag -> toTag[format], 
		link -> toLink[format], 
		num -> toNum[format], 
		italic -> toItalic[format], 
		bold -> toBold[format], 
		date -> toDate[format]
	}
]


(* ::Subsubsection:: *)
(*saveCoinmarketcapData*)


saveCoinmarketcapData[data: (currencyDataPattern[] | {currencyDataPattern[] ..}), folder: _String: $coinmarketcapDB] := 
Block[{dbPath, dbUnit},
	dbPath = FileNameJoin[{folder, "coinmarketcap.com"}];
	If[Not[FileExistsQ[dbPath]], CreateDirectory[dbPath]]; 
	Switch[data, 
		_List, 
			dbUnit = DateString[Today, {"all", " ", "Year", "-", "Month", "-", "Day", ".cryptdat"}], 
		_Association, 
			dbUnit = DateString[Today, {data["id"], " ", "Year", "-", "Month", "-", "Day", ".cryptdat"}]
	]; 
	PutAppend[Compress[data], FileNameJoin[{dbPath, dbUnit}]]; 
	Return[FileNameJoin[{dbPath, dbUnit}]]
]


(* ::Subsubsection:: *)
(*extractCoinmarketcapData*)


(* ::Text:: *)
(*messages*)


extractCoinmarketcapData::notexst = 
"This file/directory is not exists: `1`"


(* ::Text:: *)
(*extracting all data or one currency*)


extractCoinmarketcapData[currency: (_String?(currencyQ) | "all"), dbfolder_String?DirectoryQ, date_DateObject] := 
Block[{dbpath, dbunit, data}, 
	dbpath = FileNameJoin[{dbfolder, "coinmarketcap.com"}]; 
	If[Not[DirectoryQ[dbpath]], Message[extractCoinmarketcapData::notexst, dbpath]; Return[Null]]; 
	dbunit = DateString[date, {If[currency == "all", "all", currencyAlias[currency]], " ", "Year", "-", "Month", "-", "Day", ".cryptdat"}]; 
	If[Not[FileExistsQ[dbpath]], Message[extractCoinmarketcapData::notexst, FileNameJoin[{dbpath, dbunit}]]; Return[Null]]; 
	data = If[Length[#] == 1, First[#], #]&[Uncompress /@ ReadList[FileNameJoin[{dbpath, dbunit}]]]; 
	Return[data]; 
]


(* ::Text:: *)
(*extracting all data*)


extractCoinmarketcapData[dbfolder_String?DirectoryQ, date_DateObject] := 
extractCoinmarketcapData["all", dbfolder, date]


(* ::Section:: *)
(*end private*)


End[] (*`Private`*)


(* ::Section:: *)
(*from change protection*)


Protect["`*"]


(* ::Section:: *)
(*end*)


EndPackage[] (*KirillBelov`ExchangeLink`Coinmarketcap`*)
