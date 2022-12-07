(* ::Package:: *)

(* ::Section:: *)
(*package*)


BeginPackage["KirillBelov`ExchangeLink`Exmo`"]; 


Get["CryptoWatcher`Project`", Path -> ParentDirectory[DirectoryName[$InputFileName]]]; 
Get["Tools`", Path -> DirectoryName[$InputFileName]]; 


(* ::Section:: *)
(*public names declaration*)


exmoTrades::usage = 
"exmoTrades[pair]
exmoTrades[{pairs}]"; 


exmoOrderBook::usage = 
"exmoOrderBook[pair]
exmoOrderBook[{pairs}]
exmoOrderBook[{pairs}, limit -> n]"; 


exmoTicker::usage = 
"exmoTicker[]"; 


exmoPairSettings::usage = 
"exmoPairSettings[]"; 


exmoCurrency::usage = 
"exmoCurrency[]"; 


exmoUserInfo::usage = 
"exmoUserInfo[]"; 


exmoOrderCreate::usage = 
"exmoOrderCreate[pair, quantity, price, type]";


exmoOrderCancel::usage = 
"exmoOrderCancel[orderID]";


exmoUserOpenOrders::usage = 
"exmoUserOpenOrders[]";


exmoUserTrades::usage = 
"exmoUserTrades[pair]";


(* ::Text:: *)
(*patterns*)


exmoPatternTicker::usage = 
	"exmoPatternTicker[]";


(* ::Section:: *)
(*private context*)


Begin["`Private`"];


(* ::Section:: *)
(*internal variables*)


$exmoApiUrl := 
	"https://api.exmo.com/v1"; 


(* ::Section:: *)
(*internal functions*)


requestToExmoPublic[method_String, params___Rule] := 
	Module[{
		request, response, 
		body
	},
		request = HTTPRequest[URLBuild[{$exmoApiUrl, method, ""}, {params}]]; 
		response = URLRead[request]; 
		body = response["Body"]; 
		Return[toNum[ImportString[body, "RawJSON"]]]
	];


Options[requestToExmoAuthenticated] = 
	{
		"key" :> iniRead["exmo", "user_key"], 
		"secret" :> iniRead["exmo", "user_secret"]
	};


requestToExmoAuthenticated[method_String, params: <|Rule[_String, _].. |>, OptionsPattern[]] := 
	Module[{
		request, response, 
		postparams
	}, 
	
		postparams = 
			StringRiffle[KeyValueMap[#1 <> "=" <> ToString[#2]&, params], "&"]; 
	
		request = HTTPRequest[
			URLBuild[{$exmoApiUrl, method}], 
			<|
				Method -> "POST", 
				"ContentType" -> "application/x-www-form-urlencoded", 
				"Headers" -> 
					{ 
						"Key" -> OptionValue["key"], 
						"Sign" -> HMAC[postparams, OptionValue["secret"], "SHA512"]
					}, 
				"Body" -> postparams
			|>
		]; 
		
		response = URLRead[request]; 
		
		toNum[ImportString[response["Body"], "RawJSON"]]
	
	];


(* ::Section:: *)
(*patterns*)


exmoOrderTypes[] := 
	(
		"buy" | 
		"sell" | 
		"market_buy" | 
		"market_sell" | 
		"market_buy_total" | 
		"market_sell_total"
	);


exmoPatternTicker[] := 
	<|
		Rule[
			_String, 
			<|
				"buy_price" -> _Integer | _Real, 
				"sell_price" -> _Integer | _Real, 
				"last_trade" -> _Integer | _Real, 
				"high" -> _Integer | _Real, 
				"low" -> _Integer | _Real, 
				"avg" -> _Integer | _Real, 
				"vol" -> _Integer | _Real, 
				"vol_curr" -> _Integer | _Real, 
				"updated" -> _Integer
			|>
		].. 
	|>;


(* ::Section:: *)
(*public functions*)


(* ::Text:: *)
(*public api*)


exmoTrades[pair_String] := 
	requestToExmoPublic["trades", "pair" -> ToUpperCase[pair]];


exmoTrades[pairs: {__String}] := 
	requestToExmoPublic["trades", "pair" -> ToUpperCase[StringRiffle[pairs, ","]]];


Options[exmoOrderBook] := 
	{"limit" -> 100};


exmoOrderBook[pair_String, OptionsPattern[]] := 
	requestToExmoPublic["order_book", "pair" -> ToUpperCase[pair], "limit" -> OptionValue["limit"]];


exmoOrderBook[pairs: {__String}, OptionsPattern[]] := 
	requestToExmoPublic["order_book", "pair" -> ToUpperCase[StringRiffle[pairs, ","]], "limit" -> OptionValue["limit"]];


exmoTicker[] := 
	requestToExmoPublic["ticker"];


exmoPairSettings[] := 
	requestToExmoPublic["pair_settings"];


exmoCurrency[] := 
	requestToExmoPublic["currency"];


(* ::Text:: *)
(*authenticated api*)


exmoUserInfo[options: OptionsPattern[requestToExmoAuthenticated]] := 
	requestToExmoAuthenticated["user_info", <|"nonce" -> nonce["exmo", OptionValue["key"]]|>, options];


exmoOrderCreate[pair_String, quantity: (_Integer | _Real), price: (_Integer | _Real), type: exmoOrderTypes[], options: OptionsPattern[requestToExmoAuthenticated]] := 
	requestToExmoAuthenticated[
		"order_create", 
		<|
			"pair" -> ToUpperCase[pair], 
			"quantity" -> ToString[DecimalForm[quantity]], 
			"price" -> ToString[DecimalForm[price]], 
			"type" -> type, 
			"nonce" -> nonce["exmo", OptionValue["key"]]
		|>, 
		options
	];


exmoOrderCancel[orderID: (_String | _Integer), options: OptionsPattern[requestToExmoAuthenticated]] := 
	requestToExmoAuthenticated[
		"order_cancel", 
		<|
			"order_id" -> ToString[orderID], 
			"nonce" -> nonce["exmo", OptionValue["key"]]
		|>, 
		options
	];


exmoUserOpenOrders[options: OptionsPattern[requestToExmoAuthenticated]] := 
	requestToExmoAuthenticated[
		"user_open_orders", 
		<|
			"nonce" -> nonce["exmo", OptionValue["key"]]
		|>, 
		options
	];


Options[exmoUserTrades] = 
	{"offset" -> 0, "limit" -> 100};


exmoUserTrades[pair_String, options: OptionsPattern[{exmoUserTrades, requestToExmoAuthenticated}]] := 
	requestToExmoAuthenticated[
		"user_open_orders", 
		<|
			"pair" -> pair, 
			"offset" -> ToString[OptionValue["offset"]], 
			"limit" -> ToString[OptionValue["limit"]], 
			"nonce" -> nonce["exmo", OptionValue["key"]]
		|>, 
		FilterRules[{options}, Options[requestToExmoAuthenticated]]
	];


(* ::Section:: *)
(*end private*)


End[];


(* ::Section:: *)
(*end*)


EndPackage[];
