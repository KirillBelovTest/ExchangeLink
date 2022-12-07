(* ::Package:: *)

(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`ExchangeLink`Coinbase`", { 
    "KirillBelov`ExchangeLink`Settings`", 
    "KirillBelov`WebSocketJLink`"
}]


Get["KirillBelov`ExchangeLink`HMAC`"]
Get["KirillBelov`ExchangeLink`Tools`"]


(* ::Section:: *)
(*Coinbase websocket market streams*)


CoinbaseChannelCreate::usage = 
"CoinbaseChannelCreate[]"


CoinbaseChannelSubscribe::usage = 
"CoinbaseChannelSubscribe[connection, channel]
CoinbaseChannelSubscribe[connection, {channels}]
CoinbaseChannelSubscribe[connection, channels, producsId]
CoinbaseChannelSubscribe[connection, channels, {producsIds}]"


CoinbaseChannelUnsubscribe::usage = 
"CoinbaseChannelUnsubscribe[]"


Begin["`Private`"]


Options[CoinbaseChannelCreate] = {
	"Settings" :> $ExchangeLinkSettings, 
	"Deserializer" :> coinbaseDeserialize, 
	"EventHandler" :> Print
}


SyntaxInformation[CoinbaseChannelCreate] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[CoinbaseChannelCreate]
}


CoinbaseChannelCreate[opts: OptionsPattern[{}]] := 
Module[{settings, webSocketEndpoint, deserializer, eventHandler}, 
	settings = OptionValue[CoinbaseChannelCreate, opts, "Settings"]; 
	deserializer = OptionValue[CoinbaseChannelCreate, opts, "Deserializer"]; 
	eventHandler = OptionValue[CoinbaseChannelCreate, opts, "EventHandler"]; 
	webSocketEndpoint = settings["Coinbase", "WebSocketEndpoint"]; 
	WebSocketConnect[webSocketEndpoint, "Deserializer" -> deserializer, "EventHandler" -> eventHandler]
]


Options[CoinbaseChannelSubscribe] = {
	"Serializer" :> coinbaseSerialize
}


CoinbaseChannelSubscribe[connection_WebSocketConnectionObject, productIds_, channels_, opts: OptionsPattern[{}]] := 
Module[{frame, serializer}, 
	serializer = OptionValue[CoinbaseChannelSubscribe, {opts}, "Serializer"]; 
	frame = <|
		"type" -> "subscribe", 
		"product_ids" -> productIds, 
		"channels" -> channels
	|>; 
	WebSocketSend[connection, frame, "Serializer" -> serializer]
]


CoinbaseHeartbeatChannel[productIds: _String | {__String}, 
	opts: OptionsPattern[{CoinbaseChannelCreate, CoinbaseChannelSubscribe}]] := 
Module[{connection}, 
	connection = CoinbaseChannelCreate[opts]; 
	CoinbaseChannelSubscribe[connection, Flatten[{productIds}]]
]


End[] (*`Private`*)


EndPackage[] (*KirillBelov`ExchangeLink`Coinbase`*)
