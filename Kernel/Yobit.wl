(* ::Package:: *)

(* ::Chapter:: *)
(*Yobit*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`ExchangeLink`Yobit`", {
	"KirillBelov`ExchangeLink`Settings`", 
	"KirillBelov`WebSocketJLink`"
}];


(* ::Section:: *)
(*Import references*)


Get["KirillBelov`ExchangeLink`HMAC`"]; 
Get["KirillBelov`ExchangeLink`Tools`"]; 


(* ::Section:: *)
(*Clear names*)


ClearAll["`*"];


(* ::Section:: *)
(*Public names*)


yobitInfo::usage =
"yobitInfo[]";


yobitTicker::usage =
"yobitTicker[pair]\n" <> 
"yobitTicker[{pairs}]";


yobitDepth::usage =
"yobitDepth[pair]\n" <> 
"yobitDepth[{pairs}, \"limit\" -> limit]";


yobitTrades::usage =
"yobitTrades[pair]\n" <> 
"yobitTrades[{pairs}, \"limit\" -> limit]";


yobitGetInfo::usage =
"yobitGetInfo[]";


yobitTrade::usage =
"yobitTrade[pair, type, rate, amount]";


yobitActiveOrders::usage =
"yobitActiveOrders[pair]";


yobitOrderInfo::usage =
"yobitOrderInfo[orderID]";


yobitCancelOrder::usage =
"yobitCancelOrder[orderID]";


yobitTradeHistory::usage =
"yobitTradeHistory[from, count, fromID, endID, order, since, end, pair]";


yobitGetDepositAddress::usage =
"yobitGetDepositAddress[coinName, needNew]";


yobitWithdrawCoinsToAddress::usage =
"yobitWithdrawCoinsToAddress[coinName, amount, address]";


yobitCreateYobicode::usage =
"yobitCreateYobicode[currency, amount]";


yobitRedeemYobicode::usage =
"yobitRedeemYobicode[coupon]";


(* ::Section:: *)
(*Private context*)


Begin["`Private`"]


(* ::Section:: *)
(*internal variables*)


(* ::Text:: *)
(*URLs*)


$yobitAPI =
	"https://yobit.net/api/3";


$yobitTAPI =
	"https://yobit.net/tapi/";


(* ::Section:: *)
(*Internal functions*)


requestToYobitAPI::reqerr =
"Error during request to: `1`";


requestToYobitAPI[___] := 
Null;


requestToYobitAPI[(method_String | PatternSequence[method_String, pairs: (_String | {__String})] | PatternSequence[method_String, pairs: (_String | {__String}), parameters: {__Rule}])] :=
	Block[{
		url, request, response,
		status, body, data, result
	},
		url = URLBuild[{$yobitAPI, method, pairs}, parameters];
		request = HTTPRequest[url];
		Check[response = URLRead[request, FollowRedirects -> True], Message[requestToYobitAPI::reqerr, url]; Return[Null]];

		status = response["StatusCode"];
		If[status != 200, Message[requestToYobitAPI::reqerr, url]; Return[Null]];

		body = response["Body"];

		Check[data = ImportString[body, "JSON"], Message[requestToYobitAPI::reqerr, url]; Return[Null]];

		result = Replace[data, List[rules__Rule] :> Association[rules], {0, -1}];

		Return[result]
	];


requestToYobitTAPI::argx = 
	"called with arguments [`1`]; expected list of rules or association"


requestToYobitTAPI::reqerr =
	"Error during request to: `1`";


Options[requestToYobitTAPI] = 
{
	"key" :> iniRead["yobit", "user_key"], 
	"secret" :> iniRead["yobit", "user_secret"]
};


requestToYobitTAPI[args___] := 
	(Message[requestToYobitTAPI::argx, StringRiffle[Map[ToString, {args}]], ", "]; Null);


requestToYobitTAPI[rules: (List | Association)[__Rule], OptionsPattern[]] :=
Block[{
	postParams, keyValueJoin, request, 
	key = OptionValue["key"], 
	secret = OptionValue["secret"], 
	response, status, body, data, result
},
	keyValueJoin = StringTemplate["`key`=`value`"][<|"key" -> #1, "value" -> #2|>]&;
	postParams = StringRiffle[KeyValueMap[keyValueJoin, Replace[rules, List[rs__Rule] :> Association[rs]]], "&"];
	request = HTTPRequest[$yobitTAPI,
		<|
			Method -> "POST",
			"ContentType" -> "application/x-www-form-urlencoded",
			"Headers" -> {
				"Key" -> key,
				"Sign" -> hmac[postParams, secret, "SHA512"]
			},
			"Body" -> postParams
		|>
	];
		
	Check[response = URLRead[request], Message[requestToYobitTAPI::reqerr, request]; Return[Null]];
	status = response["StatusCode"]; 
	body = response["Body"]; 
	
	If[status != 200, Message[requestToYobitTAPI::reqerr, request]; Return[Null]]; 
		
	Check[
		data = ImportString[body, "JSON"];
		result = Replace[data, List[rs__Rule] :> Association[rs], {0, -1}];, 
			
		Message[requestToYobitTAPI::reqerr, request]; Return[Null]
	]; 
		
	Return[result]; 
];


(* ::Section:: *)
(*Market data implementation*)


yobitInfo[] :=
requestToYobitAPI["info"];


yobitTicker[pair_String] :=
requestToYobitAPI["ticker", pair];


yobitTicker[pairs: {__String}] :=
requestToYobitAPI["ticker", StringRiffle[pairs, "-"]];


Options[yobitDepth] :=
	{"limit" -> 150};


yobitDepth[pair_String, OptionsPattern[]] :=
	requestToYobitAPI["depth", pair, {"limit" -> OptionValue["limit"]}];


yobitDepth[pairs: {__String}, OptionsPattern[]] :=
	requestToYobitAPI["depth", StringRiffle[pairs, "-"], {"limit" -> OptionValue["limit"]}];


(* ::Text:: *)
(*trades*)


Options[yobitTrades] :=
	{"limit" -> 150};


yobitTrades[pair_String, OptionsPattern[]] :=
	requestToYobitAPI["trades", pair, {"limit" -> OptionValue["limit"]}];


yobitTrades[pairs: {__String}, OptionsPattern[]] :=
	requestToYobitAPI["trades", StringRiffle[pairs, "-"], {"limit" -> OptionValue["limit"]}];


(* ::Subsubsection:: *)
(*Privat Trade API*)


(* ::Text:: *)
(*getInfo*)


yobitGetInfo[options: OptionsPattern[requestToYobitTAPI]] := 
	requestToYobitTAPI[
		{
			"method" -> "getInfo", 
			"nonce" -> nonce["yobit", OptionValue["key"]]
		},
		options
	];


(* ::Text:: *)
(*Trade*)


yobitTrade[pair_String, type: "buy" | "sell", rate_?NumericQ, amount_?NumericQ, options: OptionsPattern[requestToYobitTAPI]] :=
	requestToYobitTAPI[
		{
			"method" -> "Trade",
			"pair" -> pair, 
			"type" -> type, 
			"rate" -> ToString[DecimalForm[N[SetAccuracy[rate, 9]]]], 
			"amount" -> ToString[DecimalForm[N[SetAccuracy[amount, 9]]]], 
			"nonce" -> nonce["yobit", OptionValue["key"]]
		},
		options
	];


(* ::Text:: *)
(*ActiveOrders*)


yobitActiveOrders::srverr := 
	"`1`"


yobitActiveOrders[pair_String, options: OptionsPattern[requestToYobitTAPI]] := 
	Module[{
		result
	}, 
		result = Check[requestToYobitTAPI[
			{
				"method" -> "ActiveOrders",
				"pair" -> pair,
				"nonce" -> nonce["yobit", OptionValue["key"]]
			},
			options
		], Message[yobitActiveOrders::srverr, "external error"]; Return[result]]; 
		
		Which[
			Not[AssociationQ[result]],
				Message[yobitActiveOrders::srverr, "broken data"], 
			Not[KeyExistsQ[result, "success"]], 
				Message[yobitActiveOrders::srverr, "unexpected data"], 
			result["success"] =!= 1, 
				Message[yobitActiveOrders::srverr, result["error"]]
		]; 
		
		Return[result]
	]


(* ::Text:: *)
(*OrderInfo*)


yobitOrderInfo[orderID_String, options: OptionsPattern[requestToYobitTAPI]] := 
	requestToYobitTAPI[
		{
			"method" -> "OrderInfo", 
			"order_id" -> orderID, 
			"nonce" -> nonce["yobit", OptionValue["key"]]
		}, 
		options
	];


(* ::Text:: *)
(*CancelOrder*)


yobitCancelOrder[orderID_String, options: OptionsPattern[requestToYobitTAPI]] := 
	requestToYobitTAPI[
		{
			"method" -> "CancelOrder", 
			"order_id" -> orderID, 
			"nonce" -> nonce["yobit", OptionValue["key"]]
		}, 
		options
	];


(* ::Text:: *)
(*TradeHistory*)


yobitTradeHistory[from_, count_, fromID_, endID_, order_, since_, end_, pair_, options: OptionsPattern[requestToYobitTAPI]] := 
	requestToYobitTAPI[
		{
			"method" -> "TradeHistory", 
			"count" -> count, 
			"from_id" -> fromID,
			"end_id" -> endID,
			"order" -> order, 
			"since" -> since, 
			"end" -> end, 
			"pair" -> pair, 
			"nonce" -> nonce["yobit", OptionValue["key"]]
		}, 
		options
	];
	
	
yobitTradeHistory[pair_, options: OptionsPattern[requestToYobitTAPI]] := 
	requestToYobitTAPI[
		{
			"method" -> "TradeHistory", 
			"pair" -> pair, 
			"nonce" -> nonce["yobit", OptionValue["key"]]
		}, 
		options
	];


(* ::Text:: *)
(*GetDepositAddress*)


yobitGetDepositAddress[coinName_, needNew_, options: OptionsPattern[requestToYobitTAPI]] := 
	requestToYobitTAPI[
		{
			"method" -> "GetDepositAddress", 
			"coinName" -> coinName, 
			"need_new" -> needNew, 
			"nonce" -> nonce["yobit", OptionValue["key"]]
		}, 
		options
	];


(* ::Text:: *)
(*WithdrawCoinsToAddress*)


yobitWithdrawCoinsToAddress[coinName_, amount_, address_, options: OptionsPattern[requestToYobitTAPI]] := 
	requestToYobitTAPI[
		{
			"method" -> "WithdrawCoinsToAddress",
			"coinName" -> coinName,
			"amount" -> amount, 
			"address" -> address, 
			"nonce" -> nonce["yobit", OptionValue["key"]]
		},
		options
	];


(* ::Text:: *)
(*CreateYobicode*)


yobitCreateYobicode[currency_, amount_, options: OptionsPattern[requestToYobitTAPI]] := 
	requestToYobitTAPI[
		{
			"method" -> "CreateYobicode",
			"currency" -> currency,
			"amount" -> amount,
			"nonce" -> nonce["yobit", OptionValue["key"]]
		}, 
		options
	];


(* ::Text:: *)
(*RedeemYobicode*)


yobitRedeemYobicode[coupon_, options: OptionsPattern[requestToYobitTAPI]] :=
	requestToYobitTAPI[
		{
			"method" -> "RedeemYobicode",
			"coupon" -> coupon,
			"nonce" -> nonce["yobit", OptionValue["key"]]
		},
		options
	];


(* ::Subsubsection::Closed:: *)
(*Helper functions*)


Options[yobitUserTrade] := 
	{"logger" -> Print}


yobitUserTrade[method: ("buy" | "sell"), pair_String, part_Real: 0.999, options: OptionsPattern[{yobitUserTrade, requestToYobitTAPI}]] := 
	Module[{
		logger = OptionValue["logger"], 
		ticker, getInfo, 
		currencyRight, 
		currencyLeft, price, amount, 
		message, result
	}, 
		ticker = yobitTicker[pair][pair]; 
		getInfo = yobitGetInfo[FilterRules[{options}, Options[requestToYobitTAPI]]]; 
		
		If[Not[MatchQ[ticker, yobitPatternTickerData[]]], logger["ticker - not executed"]; Return[Null]]; 
		If[getInfo["success"] =!= 1, logger["getInfo - not executed"]; Return[Null]]; 
		
		{currencyLeft, currencyRight} = StringSplit[pair, "_"]; 
		
		Switch[method, 
			"buy", 
				price = ticker["sell"]; 
				amount = getInfo["return", "funds", currencyRight] / price, 
				
			"sell", 
				price = ticker["buy"]; 
				amount = getInfo["return", "funds", currencyLeft]
		]; 
		
		logger[ToString[Row[{method, " ", currencyLeft, "; price ", DecimalForm[price], "; amount ", DecimalForm[amount]}]]]; 
		yobitTrade[pair, method, price, amount * part]
	];


(* ::Subsubsection:: *)
(*Helper Patterns*)


yobitPatternTickerData[] := 
	<|
		"sell" -> (_Real | _Integer), 
		"avg" -> (_Real | _Integer), 
		"high" -> (_Real | _Integer), 
		"low" -> (_Real | _Integer), 
		"vol_cur" -> (_Real | _Integer), 
		"vol" -> (_Real | _Integer), 
		"buy" -> (_Real | _Integer), 
		"last" -> (_Real | _Integer), 
		"updated" -> _Integer
	|>;


yobitPatternTicker[] := 
	With[{patternTickerData = yobitPatternTickerData[]}, <|
		Rule[
			_String, 
			patternTickerData
		].. 
	|>];


yobitPatternTickerListData[] := 
	With[{
		patternTickerData = yobitPatternTickerData[]
	}, 
		<|
			Rule[
				_Integer, 
				patternTickerData
			].. 
		|>
	];


yobitPatternTickerList[] := 
	With[{
		patternTickerData = yobitPatternTickerData[]
	}, 
		<|
			Rule[
				_String, 
				<|
					Rule[
						_Integer, 
						patternTickerData
					].. 
				|>
			].. 
		|>
	];


yobitPatternGetInfo[] := 
	With[{
		patternGetInfoData = yobitPatternGetInfoData[]
	}, 
		<|"return" -> patternGetInfoData, "success" -> 1|>
	]


yobitPatternGetInfoData[] := 
	With[{
		patternRightsData = yobitPatternRightsData[], 
		patternFundsInclOrdersData = yobitPatternFundsInclOrdersData[], 
		patternFundsData = yobitPatternFundsData[]
	}, 
		<|
			"rights" -> patternRightsData, 
			"funds_incl_orders" -> patternFundsInclOrdersData, 
			"funds" -> patternFundsData, 
			"transaction_count" -> _Integer, 
			"open_orders" -> _Integer, 
			"server_time" -> _Integer
		|>
	]


yobitPatternRightsData[] := 
	<|"info" -> _Integer, "trade" -> _Integer, "deposit" -> _Integer, "withdraw" -> _Integer|>


yobitPatternFundsInclOrdersData[] := 
	<|(_String -> _Integer | _Real)..|>


yobitPatternFundsData[] := 
	<|(_String -> _Integer | _Real)..|>


yobitPatternOrder[] := 
	<|"return"-><|(_String|_Integer)-><|(_String->_String | _Integer | _Real)..|>|>,"success"->1|>


(* ::Section::Closed:: *)
(*end private*)


End[]; (*`Private`*)


(* ::Section::Closed:: *)
(*from change protection*)


Protect["`*"];


(* ::Section::Closed:: *)
(*end*)


EndPackage[]; (*CryptoWatcher`ExchangeLink`Yobit`*)
