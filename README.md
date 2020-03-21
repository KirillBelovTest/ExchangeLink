# ExchangeLink

## Introduction

In this section Jon has to write about the project. 

More about the project - goals

- general architecture
- API implementation of top exchanges
- all available methods
- using native types (for example converting UTC to DateObject)

## Using

1. Open any notebook
2. Evaluate code: <code>PacletInstall["<path/to/file.paclet>"]</code> <!--not uploaded-->
3. Evaluate code: <code>Get["ExchangeLink`"]</code>
4. All functions are available

## Binance

### Information

After initialization Binance API methods are available in the current notebook. 
You can see list of functions/methods using following command:

```wolfram
?ExchangeLink`Binance`*
```

>|                             |                          |                          |
>| --------------------------- | ------------------------ | ------------------------ |
>| **BinanceAccountInfo**      | **BinanceOCOrderCancel** | **BinanceOrderTest**     |
>| **BinanceAggTrades**        | **BinanceOCOrderCreate** | **BinancePing**          |
>| **BinanceAveragePrice**     | **BinanceOCOrderGet**    | **BinancePrice**         |
>| **BinanceBookTicker**       | **BinanceOCOrdersAll**   | **BinanceSell**          |
>| **BinanceBuy**              | **BinanceOCOrdersNow**   | **BinanceTicker**        |
>| **BinanceDepth**            | **BinanceOrderCancel**   | **BinanceTime**          |
>| **BinanceExchangeInfo**     | **BinanceOrderCreate**   | **BinanceTrades**        |
>| **BinanceHistoricalTrades** | **BinanceOrderGet**      | **$BinanceExchangeInfo** |
>| **BinanceKlines**           | **BinanceOrdersAll**     |                          |
>| **BinanceMyTrades**         | **BinanceOrdersNow**     |                          |

### General data

General information from Binance server.

Check that server is available and get server time. This need if you want use trade API. Server time used in the creating of e-sign.

```wolfram
BinancePing[]
BinanceTime[]
```

><||>  
><|"serverTime" -> DateObject[<...>]|>

Information about order limits, precision. List of all pairs available on Binance and current statuses

```wolfram
BinanceExchangeInfo[]
```

><| 
>   "timezone" -> "UTC", 
>   ..., 
>   "symbols" -> { 
>       <|"symbol" -> "ETHBTC", ..., "filters" -> {...}|>, 
>       ... 
>   } 
>|> 

ExchangeInfo cache 

```wolfram
Dataset @ 
Query[All, <|
	"symbol" -> "symbol", 
	"minPrice" -> "filters" /* 1 /* 2, 
	"maxPrice" -> "filters" /* 1 /* 3, 
	"tickSize" -> "filters" /* 1 /* 4, 
	"minQty" -> "filters" /* 3 /* 2, 
	"maxQty" -> "filters" /* 3 /* 3, 
	"stepSize" -> "filters" /* 3 /* 4|>] @ 
$BinanceExchangeInfo["symbols"]
```

>| symbol  | minPrice | maxPrice | tickSize | minQty  | maxQty  | stepSize |
>| ------- | -------- | -------- | -------- | ------- | ------- | -------- |
>| ETHBTC  | 1.e-6    | 100000.  | 1.e-6    | 0.001   | 100000. | 0.001    |
>| LTCBTC  | 1.e-6    | 100000.  | 1.e-6    | 0.01    | 100000. | 0.01     |
>| BNBBTC  | 1.e-7    | 100000.  | 1.e-7    | 0.01    | 100000. | 0.01     |
>| NEOBTC  | 1.e-6    | 100000.  | 1.e-6    | 0.01    | 100000. | 0.01     |
>| QTUMETH | 1.e-6    | 1000.    | 1.e-6    | 0.01    | 9.e7    | 0.01     |
>| EOSETH  | 1.e-6    | 1000.    | 1.e-6    | 0.01    | 9.e7    | 0.01     |
>| SNTETH  | 1.e-8    | 1000.    | 1.e-8    | 1.      | 9.e7    | 1.       |
>| BNTETH  | 1.e-6    | 1000.    | 1.e-6    | 0.01    | 9.e7    | 0.01     |
>| BCCBTC  | 1.e-6    | 100000.  | 1.e-6    | 0.001   | 100000. | 0.001    |
>| GASBTC  | 1.e-7    | 1000.    | 1.e-7    | 0.01    | 9.e7    | 0.01     |
>| BNBETH  | 1.e-6    | 1000.    | 1.e-6    | 0.01    | 9.e7    | 0.01     |
>| BTCUSDT | 0.01     | 1.e6     | 0.01     | 1.e-6   | 9000.   | 1.e-6    |
>| ETHUSDT | 0.01     | 1.e6     | 0.01     | 0.00001 | 9000.   | 0.00001  |
>| HSRBTC  | 1.e-6    | 100000.  | 1.e-6    | 0.01    | 1.e7    | 0.01     |
>| OAXETH  | 1.e-7    | 100000.  | 1.e-7    | 1.      | 9.e7    | 1.       |
>| DNTETH  | 1.e-8    | 1000.    | 1.e-8    | 1.      | 9.e7    | 1.       |
>| MCOETH  | 1.e-6    | 1000.    | 1.e-6    | 0.01    | 9.e7    | 0.01     |
>| ICNETH  | 1.e-7    | 100000.  | 1.e-7    | 1.      | 9.e7    | 1.       |
>| MCOBTC  | 1.e-7    | 1000.    | 1.e-7    | 0.01    | 9.e7    | 0.01     |
>| WTCBTC  | 1.e-7    | 1000.    | 1.e-7    | 0.01    | 9.e7    | 0.01     |

### Market data

Current order book

```wolfram
depth = BinanceDepth["BTCUSDT"];
bids = depth["bids"]; 
asks = depth["asks"]; 
min = Min[bids[[All, 2]] ~ Join ~ asks[[All, 2]]]; 
bidNormalizaer = Round[bids[[All, 2]] / min];
askNormalizaer = Round[asks[[All, 2]] / min];
```

```wolfram
PairedHistogram[
	Flatten[Table[ConstantArray[bids[[i, 1]], bidNormalizaer[[i]]], {i, 1, Length@bidNormalizaer}]], 
	Flatten[Table[ConstantArray[asks[[i, 1]], askNormalizaer[[i]]], {i, 1, Length@askNormalizaer}]], 
	ImageSize -> Large, Frame -> True, 
	ChartLabels -> {
		Placed[{"BIDS", "ASKS"}, Above],
		Placed[{"r1", "r2"}, Tooltip]
	}, 
	LabelingFunction -> (If[!(#1 == 0), Placed[#1*min, Center], None]&), 
	FrameTicks -> {None, Automatic}
]
```

![](https://wolfr.am/LdqAAGTu)

## Contributing

After uploading to github

