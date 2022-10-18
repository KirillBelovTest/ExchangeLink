(* ::Package:: *)

(* ::Chapter:: *)
(*ExchangeLink - Coinbase*)


BeginPackage["ExchangeLink`CoinbaseChannels`", {"WebSocketJLink`"}]


CoinbaseChannelSubscribe::usage = 
"CoinbaseChannelSubscribe[channels] - new connection
CoinbaseChannelSubscribe[channels, productIds] - new connection with specific produc ids
CoinbaseChannelSubscribe[connection, channels] - for the existing connection
CoinbaseChannelSubscribe[connection, channels, productIds] - for the existing connection"

CoinbaseChannelUnsubscribe::usage = 
"CoinbaseChannelUnsubscribe[connection, channels]"


CoinbaseHeartbeatChannel::usage = 
"CoinbaseHeartbeatChannel[productIds]
CoinbaseHeartbeatChannel[connection, productIds]"


CoinbaseStatusChannel::usage = 
"CoinbaseStatusChannel[]
CoinbaseStatusChannel[connection]"


CoinbaseTickerChannel::usage = 
"CoinbaseTickerChannel[productIds]
CoinbaseTickerChannel[connection, productIds]"


CoinbaseTickerBatchChannel::usage = 
"CoinbaseTickerBatchChannel[productIds]
CoinbaseTickerBatchChannel[connection, productIds]"


CoinbaseLevel2Channel::usage = 
"CoinbaseLevel2Channel[productIds]
CoinbaseLevel2Channel[connection, productIds]"


CoinbaseLevel2BatchChannel::usage = 
"CoinbaseLevel2BatchChannel[productIds]
CoinbaseLevel2BatchChannel[connection, productIds]"


Begin["`Private`"]


$webSocketEndpoint = 
"wss://ws-feed.exchange.coinbase.com"


CoinbaseChannelSubscribe[channels_List] := 
Module[{frame, connection}, 
	connection = WebSocketConnect[$webSocketEndpoint]; 
	frame = ExportString[<|
		"type" -> "subscribe", 
		"channels" -> channels
		|>, "RawJSON"];
	WebSocketSend[connection, frame]; 
	Return[connection]
]


CoinbaseChannelSubscribe[channels_List, productIds_List] := 
Module[{frame, connection}, 
	connection = WebSocketConnect[$webSocketEndpoint]; 
	frame = ExportString[<|
		"type" -> "subscribe", 
		"product_ids" -> productIds, 
		"channels" -> channels
		|>, "RawJSON"];
	WebSocketSend[connection, frame]; 
	Return[connection]
]


CoinbaseChannelSubscribe[connection_WebSocketConnectionObject, channels_List] := 
Module[{frame}, 
	frame = ExportString[<|
		"type" -> "subscribe", 
		"channels" -> channels
		|>, "RawJSON"];
	WebSocketSend[connection, frame]; 
	Return[connection]
]


CoinbaseChannelSubscribe[connection_WebSocketConnectionObject, channels_List, productIds_List] := 
Module[{frame}, 
	frame = ExportString[<|
		"type" -> "subscribe", 
		"product_ids" -> productIds, 
		"channels" -> channels
		|>, "RawJSON"];
	WebSocketSend[connection, frame]; 
	Return[connection]
]


CoinbaseChannelUnsubscribe[connection_WebSocketConnectionObject, channels_List] := 
Module[{frame}, 
	frame = ExportString[<|
		"type" -> "unsubscribe", 
		"channels" -> channels
		|>, "RawJSON"];
	WebSocketSend[connection, frame]; 
	Return[connection]
]


CoinbaseHeartbeatChannel[productIds: {__String}] := 
Module[{channels}, 
	channels = {<|"name" -> "heartbeat", 
				"product_ids" -> ToUpperCase /@ productIds|>}; 
	CoinbaseChannelSubscribe[channels]
]


CoinbaseHeartbeatChannel[connection_WebSocketConnectionObject, productIds: {__String}] := 
Module[{channels}, 
	channels = {<|"name" -> "heartbeat", 
				"product_ids" -> ToUpperCase /@ productIds|>}; 
	CoinbaseChannelSubscribe[connection, channels]
]


CoinbaseStatusChannel[] := 
Module[{channels}, 
	channels = {<|"name" -> "status"|>}; 
	CoinbaseChannelSubscribe[channels]
]


CoinbaseStatusChannel[connection_WebSocketConnectionObject] := 
Module[{channels}, 
	channels = {<|"name" -> "status"|>}; 
	CoinbaseChannelSubscribe[connection, channels]
]


CoinbaseTickerChannel[productIds_List] := 
Module[{channels}, 
	channels = {"ticker"}; 
	CoinbaseChannelSubscribe[channels, productIds]
]


CoinbaseTickerChannel[connection_WebSocketConnectionObject, productIds_List] := 
Module[{channels}, 
	channels = {"ticker"}; 
	CoinbaseChannelSubscribe[connection, channels, productIds]
]


CoinbaseTickerBatchChannel[productIds_List] := 
Module[{channels}, 
	channels = {"ticker_batch"}; 
	CoinbaseChannelSubscribe[channels, productIds]
]


CoinbaseTickerBatchChannel[connection_WebSocketConnectionObject, productIds_List] := 
Module[{channels}, 
	channels = {"ticker_batch"}; 
	CoinbaseChannelSubscribe[connection, channels, productIds]
]


CoinbaseLevel2Channel[productIds_List] := 
Module[{channels}, 
	channels = {"level2"}; 
	CoinbaseChannelSubscribe[channels, productIds]
]


CoinbaseLevel2Channel[connection_WebSocketConnectionObject, productIds_List] := 
Module[{channels}, 
	channels = {"level2"}; 
	CoinbaseChannelSubscribe[connection, channels, productIds]
]


CoinbaseLevel2BatchChannel[productIds_List] := 
Module[{channels}, 
	channels = {"level2_batch"}; 
	CoinbaseChannelSubscribe[channels, productIds]
]


CoinbaseLevel2BatchChannel[connection_WebSocketConnectionObject, productIds_List] := 
Module[{channels}, 
	channels = {"level2_batch"}; 
	CoinbaseChannelSubscribe[connection, channels, productIds]
]


End[] (*`Private`*)


EndPackage[] (*ExchangeLink`Coinbase`*)
