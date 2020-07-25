(* ::Package:: *)


BeginPackage["ExchangeLink`Coinbase`"]


CoinbaseTime::usage = 
"CoinbaseTime[]"


CoinbaseCurrencies::usage = 
"CoinbaseCurrencies[]"


CoinbaseProducts::usage = 
"CoinbaseProducts[]
CoinbaseProducts[productId]"


CoinbaseProductOrderBook::usage = 
"CoinbaseProductOrderBook[productId]"


CoinbaseProductTicker::usage = 
"CoinbaseProductTicker[productId]"


CoinbaseProductTrades::usage = 
"CoinbaseProductTrades[productId]"


CoinbaseProductCandles::usage = 
"CoinbaseProductCandles[productId, opts]"


CoinbaseProduct24Stats::usage = 
"CoinbaseProduct24Stats[productId]"


$CoinbaseCurrencies::usage = 
"Cache with currencies that are actual today"

$CoinbaseProducts::usage = 
"Cache with products that are actual today"


Begin["`Private`"] (* Begin Private Context *) 


$apiurl = "https://api.pro.coinbase.com"


Coinbase::reqerr = 
"Error during request to: `1`\ncode: `2`\nresponse body: `3`\n\n`4`\n`5`"


coinbaseMarketData[method_String, path: {__String}, query: {___Rule}: {}] := 
	Block[{url, request, response, status, body, json}, 
		url = URLBuild[Join[{$apiurl}, path], query]; 
		request = HTTPRequest[url]; 
		response = URLRead[request]; 
		status = response["StatusCode"]; 
		If[status =!= 200, Message[Coinbase::reqerr, url, status, request["Body"], request, response]; Return[]]; 
		body = response["Body"];
		json = ImportString[ExportString[body, "Text"], "RawJSON"];
		Return[json]
	]


productIdQ[productId_String] := 
	MemberQ[$CoinbaseProducts[[All, "id"]], productId]


$coinbaseCache = <||>


CoinbaseTime[] := 
	Query[{"iso" -> Function[time, DateObject[time, TimeZone -> 0]]}] @ 
	coinbaseMarketData["GET", {"time"}]


$CoinbaseCurrencies := Block[{}, 
	If[!AssociationQ[$coinbaseCache], $coinbaseCache = <||>]; 
	If[Length[$coinbaseCache] > 1, $coinbaseCache = KeySelect[$coinbaseCache, # === Today&]]; 
	If[Length[$coinbaseCache] =!= 1, $coinbaseCache = <|Today -> <||>|>]; 
	If[!KeyExistsQ[$coinbaseCache[Today], "CoinbaseCurrencies"], $coinbaseCache[Today, "CoinbaseCurrencies"] = CoinbaseCurrencies[]]; 
	$coinbaseCache[Today, "CoinbaseCurrencies"]
]


CoinbaseCurrencies[] := 
	If[AssociationQ[$coinbaseCache] && KeyExistsQ[$coinbaseCache, Today] && KeyExistsQ[$coinbaseCache[Today], "CoinbaseCurrencies"], 
		$coinbaseCache[Today, "CoinbaseCurrencies"] = Query[All, {"min_size" -> ToExpression, "max_precision" -> ToExpression}] @ coinbaseMarketData["GET", {"currencies"}], 
		Query[All, {"min_size" -> ToExpression, "max_precision" -> ToExpression}] @ coinbaseMarketData["GET", {"currencies"}]
	]


$CoinbaseProducts := Block[{}, 
	If[!AssociationQ[$coinbaseCache], $coinbaseCache = <||>]; 
	If[Length[$coinbaseCache] > 1, $coinbaseCache = KeySelect[$coinbaseCache, # === Today&]]; 
	If[Length[$coinbaseCache] =!= 1, $coinbaseCache = <|Today -> <||>|>]; 
	If[!KeyExistsQ[$coinbaseCache[Today], "CoinbaseProducts"], $coinbaseCache[Today, "CoinbaseProducts"] = CoinbaseProducts[]]; 
	$coinbaseCache[Today, "CoinbaseProducts"]
]


CoinbaseProducts[] := 
	If[AssociationQ[$coinbaseCache] && KeyExistsQ[$coinbaseCache, Today] && KeyExistsQ[$coinbaseCache[Today], "CoinbaseProducts"], 
		$coinbaseCache[Today, "CoinbaseProducts"] = 
		Query[All, {4 -> ToExpression, 5 -> ToExpression, 6 -> ToExpression, 7 -> ToExpression, 9 -> ToExpression, 10 -> ToExpression}] @ coinbaseMarketData["GET", {"products"}], 
		Query[All, {4 -> ToExpression, 5 -> ToExpression, 6 -> ToExpression, 7 -> ToExpression, 9 -> ToExpression, 10 -> ToExpression}] @ coinbaseMarketData["GET", {"products"}]
	]
	


CoinbaseProducts[productId_String?productIdQ] := 
	Query[{"base_min_size" -> ToExpression, "base_max_size" -> ToExpression, "quote_increment" -> ToExpression, "base_increment" -> ToExpression, "min_market_funds" -> ToExpression, "max_market_funds" -> ToExpression}] @ 
	coinbaseMarketData["GET", {"products", productId}]


Options[CoinbaseProductOrderBook] = {"level" -> 3}


CoinbaseProductOrderBook[productId_String?productIdQ, opts: OptionsPattern[]] := 
	coinbaseMarketData["GET", {"products", productId, "book"}, {"level" -> OptionValue["level"]}]


CoinbaseProductTicker[productId_String?productIdQ] := 
	Query[{1 -> ToExpression, 2 -> ToExpression, 3 -> ToExpression, 4 -> Function[time, DateObject[time, TimeZone -> 0]], 5 -> ToExpression, 6 -> ToExpression, 7 -> ToExpression}] @ 
	coinbaseMarketData["GET", {"products", productId, "ticker"}]


CoinbaseProductTrades[productId_String?productIdQ] := 
	Query[All, {"time" -> Function[time, DateObject[time, TimeZone -> 0]], "price" -> ToExpression, "size" -> ToExpression}] @ 
	coinbaseMarketData["GET", {"products", productId, "trades"}]


Options[CoinbaseProductCandles] = {
	"start" :> DatePlus[Now, -1], 
	"end" :> Now, 
	"granularity" -> 300
}


CoinbaseProductCandles[productId_String?productIdQ, opts: OptionsPattern[]] := 
	Query[All, {"time" -> Function[time, FromUnixTime[time, TimeZone -> 0]]}] @ 
	Map[AssociationThread[{"time", "low", "high", "open", "close", "volume"} -> #]&] @ 
	coinbaseMarketData["GET", {"products", productId, "candles"}, {
		"start" -> DateString[OptionValue["start"], "ISODateTime"], 
		"end" -> DateString[OptionValue["end"], "ISODateTime"], 
		"granularity" -> OptionValue["granularity"]
	}]


CoinbaseProduct24Stats[productId_String?productIdQ] := 
	Map[ToExpression] @ 
	coinbaseMarketData["GET", {"products", productId, "stats"}]


End[] (* End Private Context *)


EndPackage[]