(* ::Package:: *)

(* ::Section:: *)
(*Begin package*)


BeginPackage["ExchangeLink`Bitfinex`"]


(* ::Section:: *)
(*Clear names*)


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Public names declaration*)


BitfinexPlatformStatus


BitfinexTickers


BitfinexTicker


BitfinexTrades


BitfinexBook


BitfinexStats


BitfinexCandles


BitfinexConfigs


BitfinexStatus


BitfinexLiquidationFeed


BitfinexLeaderBoards


Begin["`Private`"] 


(* ::Section:: *)
(*Internal variables*)


$BitfinexPairs := 
	$bitfinexPairs


$bitfinexPairs := $bitfinexPairs = 
	Flatten[URLExecute["https://api-pub.bitfinex.com/v2/conf/pub:list:pair:exchange"]]


$BitfinexCurrencies := 
	$bitfinexCurrencies


$bitfinexCurrencies := $bitfinexCurrencies = 
	Flatten[URLExecute["https://api-pub.bitfinex.com/v2/conf/pub:list:currency"]]


(* ::Section:: *)
(*Internal functions*)


symbolQ[_] := 
	True


toSymbol[symbol_String] /; MemberQ[$BitfinexPairs, symbol] := 
	"t" <> ToUpperCase[symbol]


toSymbol[symbol_String] /; MemberQ[$BitfinexCurrencies, symbol] := 
	"f" <> ToUpperCase[symbol]


toSymbol[symbols: {__String}] := 
	StringRiffle[toSymbol /@ symbols, ","]


fromTicker[array_List] := 
	Block[{ticker = Association @@ Thread[{
		"symbol", "bid", "bidSize", 
		"ask", "askSize", "dailyChange", 
		"daylyChangeRelative", 
		"lastPrice", "volume", "high", "low"} -> array]}, 
		ticker["symbol"] = StringDrop[ticker["symbol"], 1]; 
		ticker
	]


fromFund[array_List] := 
	Block[{fund = Association @@ Thread[{"symbol", 
			"frr", "bid", "bidPeriod", "bidSize", 
			"ask", "askPeriod", "askSize", "dailyChange", 
			"dailyChangeRelative", "lastPrice", "volume", 
			"high", "low", "placeholder", "placeholder2", 
			"frrAmountAvailable"} -> array]}, 
		fund["symbol"] = StringDrop[fund["symbol"], 1];
		fund
	]


(* ::Section:: *)
(*Common public API method*)


$bitfinexPublicEndpoint = 
	"https://api-pub.bitfinex.com"


bitfinexPublicEndpointExec[method_String, path: {__String}, params: {(_String -> _)...}] := 
	Block[{url, query, request, response, body, data}, 
		query = DeleteDuplicatesBy[DeleteCases[params, _[_, None | Automatic | Null]], First]; 
		url = URLBuild[Flatten[{$bitfinexPublicEndpoint, "v2", path}], query]; 
		request = HTTPRequest[url, <|
			Method -> method
		|>]; 
		response = URLRead[request]; 
		body = response["Body"]; 
		data = ImportString[body, "RawJSON"]; 
		Return[data];
	]


(* ::Section:: *)
(*Public endpoints implementation*)


(* ::Text:: *)
(*Platform Status*)


Options[BitfinexPlatformStatus] = 
	{
		"operative" -> 1
	}


SyntaxInformation[BitfinexPlatformStatus] = 
	{
		"ArgumentsPattern" -> {OptionsPattern[]}, 
		"OptionNames" -> {"\"operative\""}
	}


BitfinexPlatformStatus[OptionsPattern[]] := 
	Block[{status = bitfinexPublicEndpointExec["GET", 
		{"platform", "status"}, 
		{"operative" -> OptionValue["operative"]}
	]}, If[MatchQ[status, {1}], True, False]]


(* ::Text:: *)
(*Tickers*)


SyntaxInformation[BitfinexTickers] = 
	{
		"ArgumentsPattern" -> {_.}
	}


BitfinexTickers[] := 
	Block[{result = bitfinexPublicEndpointExec["GET", 
		{"tickers"}, 
		{"symbols" -> "ALL"}
	], tickers, funds}, 
		tickers = Map[fromTicker, Select[result, StringMatchQ[#[[1]], "t" ~~ __]&]];
		funds = Map[fromFund, Select[result, StringMatchQ[#[[1]], "f" ~~ __]&]];
		
		<|"funds" -> funds, "tickers" -> tickers|>
	]


BitfinexTickers[symbol_String?symbolQ] := 
	Block[{result = bitfinexPublicEndpointExec["GET", 
		{"tickers"}, 
		{"symbols" -> toSymbol[symbol]}], tickers, funds}, 
		tickers = Map[fromTicker, Select[result, StringMatchQ[#[[1]], "t" ~~ __]&]];
		funds = Map[fromFund, Select[result, StringMatchQ[#[[1]], "f" ~~ __]&]];
		
		<|"funds" -> funds, "tickers" -> tickers|>
	];


BitfinexTickers[symbols: {__String?symbolQ}] := 
	Block[{result = bitfinexPublicEndpointExec["GET", {"tickers"}, 
		{"symbols" -> toSymbol[symbols]}], tickers, funds}, 
		tickers = Map[fromTicker, Select[result, StringMatchQ[#[[1]], "t" ~~ __]&]];
		funds = Map[fromFund, Select[result, StringMatchQ[#[[1]], "f" ~~ __]&]];
		
		<|"funds" -> funds, "tickers" -> tickers|>
	];


(* ::Text:: *)
(*Ticker*)


SyntaxInformation[BitfinexTicker] = 
	{
		"ArgumentsPattern" -> {_}
	}


BitfinexTicker[symbol_String?symbolQ] := 
	Block[{result = bitfinexPublicEndpointExec["GET", {"ticker", toSymbol[symbol]}, {}]}, 
		Which[
			MemberQ[$BitfinexCurrencies, symbol], fromFund[Prepend[result, "f" <> symbol]], 
			MemberQ[$BitfinexPairs, symbol], fromTicker[Prepend[result, "t" <> symbol]]
		]
	];


(* ::Text:: *)
(*Trades*)


Options[BitfinexTrades] = 
	{
		"limit" -> Automatic, 
		"start" -> Automatic, 
		"end" -> Automatic, 
		"sort" -> Automatic
	}


SyntaxInformation[BitfinexTrades] = 
	{
		"ArgumentsPattern" -> {_, OptionsPattern[]}, 
		"OptionNames" -> {"\"limit\"", "\"start\"", "\"end\"", "\"sort\""}
	}


BitfinexTrades[symbol_String?symbolQ, opts: OptionsPattern[]] := 
	Block[{result = bitfinexPublicEndpointExec["GET", 
			{"trades", toSymbol[symbol], "hist"}, 
			DeleteCases[Flatten[{opts}], _[_, Automatic]]
		]
	}, 
		result
	]


(* ::Section:: *)
(*Common auth API method*)


$bitfinexAuthenticatedEndpoint = 
	"https://api.bitfinex.com"


(* ::Section:: *)
(*End private context*)


End[] (*`Private`*)


(* ::Section:: *)
(*Protection*)


Protect["`*"]


(* ::Section:: *)
(*End package*)


EndPackage[] (*ExchangeLink`Bitfinex`*)
