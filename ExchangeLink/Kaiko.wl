(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["ExchangeLink`Kaiko`"]


Kaiko::usage = 
"just a symbol"


KaikoAssets::usage = 
"KaikoAssets[]"


KaikoExchanges::usage = 
"KaikoExchanges[]"


KaikoInstruments::usage = 
"KaikoInstruments[]"


KaikoTradeDataHist


KaikoTradeDataRecent


KaikoTradeDataSpot


KaikoCandlesHist


KaikoCandlesRecent


Begin["`Private`"] (* Begin Private Context *) 


(* ::Section:: *)
(*Reference Data API*)


$kaikoReferenceDataAPI = "https://reference-data-api.kaiko.io"


kaikoPublicExec[path: {__String}, query: {___Rule}: {}] := 
	Block[{url, request, response, status, body, json}, 
		url = URLBuild[Flatten[{$kaikoReferenceDataAPI, "v1", path}], query]; 
		request = HTTPRequest[url]; 
		response = URLRead[request]; 
		body = response["Body"]; 
		json = ImportString[ExportString[body, "Text"], "RawJSON"]; 
		Return[json]
	]


KaikoAssets[] := 
	kaikoPublicExec[{"assets"}]["data"]


KaikoExchanges[] := 
	Query[All, <|
		"code" -> "code", 
		"name" -> "name", 
		"assetClass" -> "asset_class", 
		"assetClasses" -> "asset_classes"
	|>] @ 
	kaikoPublicExec[{"exchanges"}]["data"]


KaikoInstruments[] := 
	Query["data", All, <|
		"kaikoLegacyExchangeSlug" -> "kaiko_legacy_exchange_slug",
		"tradeStartTime" -> "trade_start_time",
		"tradeEndTime" -> "trade_end_time",
		"code" -> "code",
		"exchangeCode" -> "exchange_code",
		"exchangePairCode" -> "exchange_pair_code",
		"baseAsset" -> "base_asset",
		"quoteAsset" -> "quote_asset",
		"kaikoLegacySymbol" -> "kaiko_legacy_symbol",
		"class" -> "class",
		"tradeStartTimestamp" -> "trade_start_timestamp",
		"tradeEndTimestamp" -> "trade_end_timestamp",
		"tradeCount" -> "trade_count",
		"tradeCompressedSize" -> "trade_compressed_size"
	|>] @ 
	kaikoPublicExec[{"instruments"}]


kaikoAuthExec[]


(* ::Section:: *)
(*Tools*)


toSnakecase[any_] := any
toSnakecase[camelcase_String] := StringReplace[camelcase, Thread[CharacterRange["A", "Z"] -> Map[StringJoin["_", #]&] @ CharacterRange["a", "z"]]]


toISO[digit_?NumericQ] := digit
toISO[date_?DateObjectQ] := DateString[TimeZoneConvert[date, 0], {"Year", "-", "Month", "-", "Day", "T", "Hour", ":", "Minute", ":", "SecondExact", "Z"}]
toISO[string_String] := string


getOptions[function_Symbol, opts: OptionsPattern[{}]] := 
	Normal @ Map[toISO] @ KeyMap[toSnakecase] @ Association @ FilterRules[DeleteCases[Flatten[{opts}], _[_, Automatic | Null | None]], Options[function]]


(* ::Section:: *)
(*Market Data API*)


Kaiko::notscs = 
"Error during request to `1`"


Options[kaikoMarketDataAPI] = {
	"auth" :> <|"APIKey" -> "f00edcvicw9g1bq4v887zhmwjgmwsngv"|>
}


$kaikoMarketDataAPI = "https://us.market-api.kaiko.io/"


kaikoMarketDataAPI[v_String, path__String, query: {___Rule}: {}, OptionsPattern[]] := 
	Block[{url, request, key, response, body, json}, 
		url = URLBuild[{$kaikoMarketDataAPI, v, "data", path}, query]; 
		key = OptionValue["auth"]["APIKey"]; 
		request = HTTPRequest[url, <|
			"Headers" -> {"X-Api-Key" -> key}
		|>]; 
		response = URLRead[request]; 
		body = response["Body"]; 
		json = ImportString[body, "RawJSON"]; 
		If[json[] === "", 
			Return[json], 
			Message[Kaiko::notscs, url]
		]
	]


Options[KaikoTradeDataHist] = {
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"continuationToken" -> Automatic, 
	"pageSize" -> Automatic
}


KaikoTradeDataHist[exchangeCode_String, instrumentClass_String, instrumentCode_String, opts: OptionsPattern[{KaikoTradeDataHist, kaikoMarketDataAPI}]] := 
	Query["data", All, <|
		"timestamp" -> Function[FromUnixTime[#timestamp/1000]], 
		"tradeId" -> Function[ToExpression[#["trade_id"]]], 
		"price" -> Function[ToExpression[#price]], 
		"amount" -> Function[ToExpression[#amount]], 
		"takerSideSell" -> Function[#["taker_side_sell"]]
	|>] @ 
	kaikoMarketDataAPI["v2", "trades.v1", "exchanges", exchangeCode, instrumentClass, instrumentCode, "trades", getOptions[KaikoTradeDataHist, opts], getOptions[kaikoMarketDataAPI, opts]]


Options[KaikoTradeDataRecent] = {
	"limit" -> Automatic
}


KaikoTradeDataRecent[exchangeCode_String, instrumentClass_String, instrumentCode_String, opts: OptionsPattern[{KaikoTradeDataRecent, kaikoMarketDataAPI}]] := 
	Query["data", All, <|
		"timestamp" -> Function[FromUnixTime[#timestamp/1000]], 
		"tradeId" -> Function[ToExpression[#["trade_id"]]], 
		"price" -> Function[ToExpression[#price]], 
		"amount" -> Function[ToExpression[#amount]], 
		"takerSideSell" -> Function[#["taker_side_sell"]]
	|>] @ 
	kaikoMarketDataAPI["v1", "trades.v1", "exchanges", exchangeCode, instrumentClass, instrumentCode, "trades", "recent", getOptions[KaikoTradeDataRecent, opts], getOptions[kaikoMarketDataAPI, opts]]


Options[KaikoTradeDataSpot] = {
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"continuationToken" -> Automatic, 
	"pageSize" -> Automatic, 
	"sort" -> Automatic, 
	"interval" -> Automatic
}


KaikoTradeDataSpot[opts: OptionsPattern[{KaikoTradeDataSpot, kaikoMarketDataAPI}]] := 
	kaikoMarketDataAPI["v2", "trades.v1", "exchanges", "spots", "recent", getOptions[KaikoTradeDataSpot, opts], getOptions[kaikoMarketDataAPI, opts]]


Options[KaikoCandlesHist] = {
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"continuationToken" -> Automatic, 
	"pageSize" -> Automatic, 
	"interval" -> Automatic, 
	"sort" -> Automatic
}; 


KaikoCandlesHist[exchangeCode_String, instrumentClass_String, instrumentCode_String, opts: OptionsPattern[{KaikoCandlesHist, kaikoMarketDataAPI}]] := 
	Query["data", All, <|
		"timestamp" -> Function[FromUnixTime[#timestamp/1000]], 
		"open" -> "open" /* ToExpression, 
		"high" -> "high" /* ToExpression, 
		"low" -> "low" /* ToExpression, 
		"close" -> "close" /* ToExpression, 
		"volume" -> "volume" /* ToExpression
	|>] @ 
	kaikoMarketDataAPI["v2", "trades.v1", "exchanges", exchangeCode, instrumentClass, instrumentCode, "aggregations", "ohlcv", getOptions[KaikoCandlesHist, opts], getOptions[kaikoMarketDataAPI, opts]]


(* ::Section:: *)
(*End*)


End[] (* End Private Context *)


EndPackage[]
