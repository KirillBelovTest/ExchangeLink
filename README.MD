# ExchangeLink

## Introduction

Hi Wolfram Community! 

We are going to try to see if we can do a community project.
ExchangeLink is an attempt to cover the top 10 crypto exchanges 
with Wolfram Language connectivity.

Binance is the first exchange library and will serve as an example.

There are several ways to contribute:

1. Code Editting
	
	* Suggestions on better function names and coding style. 
	* Documentation
	* Tests
	* Examples

2. Create New Libraries
	
	* Follow the Binance Library Example. 

3. Suggestions
	* New Exchange Libraries (see list)
	* Examples using the exchange libraries

We look forward to seeing how this project goes!

Current Suggestions of exchanges:

Data Services: 
1. Kaiko

Exchanges:

1. Binance (.com, .us)
2. Bitfinex
3. Coinbase
4. Kraken

Futures: 
1. Bitmex 
2. Deribit

## Using

1. Open any notebook
2. Evaluate code: `PacletInstall["https://github.com/ExchangeLink/ExchangeLink/releases/download/v0.0.1/ExchangeLink-0.0.1.paclet"]`
3. Evaluate code: <code>Get["ExchangeLink`"]</code>
4. All functions are available

### API Keys

If you want to use your account and trade API endpoint, you must create your own API Key and API sercret key. 

Use following way to do this:

1. Go and login to [binance.com](https://www.binance.com)
2. Click to the menu item "API Managment" in your account actions menu  
![](https://wolfr.am/LA5pbcHc)
3. Click to the button "Create"  
![](https://wolfr.am/LA6N8OcM)
4. Follow the instructions of Binance. You have to confirm process in your email

After the previous operation you have to copy the keys and save them in a special variable - 
*$ExchangeLinkConfig*. 

You must execute the same code but with *your* domain and api/secret keys: 

```wolfram
$ExchangeLinkConfig["Binance", "Domain"] = "binance.com" (*also binance.us is available*) 
$ExchangeLinkConfig["Binance", "APIKey"] = "JqbsI8dNvn4wQ2tQ00bzChAdIiKVxas98ZMQPxO7Iykr0zrp32mPMOBO2tFbrdw5" 
$ExchangeLinkConfig["Binance", "SecretKey"] = "nwVaV8d25Btl9PqSJgks0IUBtZMj44xIdJY2wHceAcGqH3zpagEx9R6mn1G7rbbo"
```

You can view all config data:

```wolfram
$ExchangeLinkConfig
```

## Information

After the initialization of Binance API methods are available in the current notebook 
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

## General data

General information from the Binance server.

Check that the server is available and get the server time. 
This is needed if you want to use the trade API. 
The server time is used in the creation of the e-sign.

```wolfram
BinancePing[]
BinanceTime[]
```

><||>  
><|"serverTime" -> DateObject[<...>]|>

Information about order limits and precision. 
List of all the trading pairs available on Binance and the current statuses. 
This is the information you need, if you want to create your own orders. 
Binance has restrictions on precision of prices or quantities in orders. 

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

ExchangeInfo cache. `BinanceExchangeInfo[]` is nearly constant data. 
It changes about one time per week. So you can save the current value of this variable 
in the session and you can use it without another request to Binance. The value that is saved in memory 
exists for one day. After time 00:00 `$BinanceExchangeInfo` will be updated. 


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

## Market data

Current order book

```wolfram
depth = BinanceDepth["BTCUSDT"];
bids = depth["bids"]; 
asks = depth["asks"]; 
min = Min[bids[[All, 2]] ~ Join ~ asks[[All, 2]]]; 
bidNormalizaer = Round[bids[[All, 2]] / min];
askNormalizaer = Round[asks[[All, 2]] / min];
```

Representation of the order book using `Histogram`:

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

>![](https://wolfr.am/LdqAAGTu)

Last trades for the pair - order size, time and other

```wolfram
BinanceTrades["BTCUSDT"]
```

>{<|"id" -> 275374890, price -> 6232.51, qty -> quoteQty -> 267.026, time -> DateObject[<..>], ...|>, ...}

## Market prices

Get the current price of Bitcoin

```wolfram
BinancePrice["BTCUSDT"]
```

><|"symbol" -> "BTCUSDT", "price" -> 6231.45|>

More information about a cryptocurrency pair. The result has information about the 24h change, 
volume and last prices

```wolfram
BinanceTicker["BTCUSDT"]
```

><|"symbol" -> "BTCUSDT", "priceChange" -> -287.64, 
> "priceChangePercent" -> -4.405, "weightedAvgPrice" -> 6207.84, 
> "prevClosePrice" -> 6529.23, "lastPrice" -> 6242.19, 
> "lastQty" -> 0.150057, "bidPrice" -> 6241.39, "bidQty" -> 0.083302, 
> "askPrice" -> 6243.39, "askQty" -> 1.71051, "openPrice" -> 6529.83, 
> "highPrice" -> 6714.53, "lowPrice" -> 5670., "volume" -> 182195., 
> "quoteVolume" -> 1.1310340689753455*10^9, 
> "openTime" -> 
>  DateObject<>, "closeTime" -> 
>  DateObject<>, "firstId" -> 274022160, "lastId" -> 275354648, 
> "count" -> 1332489|>

And for all currency pairs

```wolfram
BinanceTicker[]
```

>{<|"symbol" -> "ETHBTC", "priceChange" -> -0.00007, 
> "priceChangePercent" -> -0.327, "weightedAvgPrice" -> 0.0213236, 
> "prevClosePrice" -> 0.021413, "lastPrice" -> 0.021351, 
> "lastQty" -> 1.204, "bidPrice" -> 0.021351, <...>, 
> "volume" -> 312333., "quoteVolume" -> 6660.05, 
> "openTime" -> 
>  DateObject<>, "closeTime" -> 
>  DateObject<>, "firstId" -> 169520504, "lastId" -> 169718743, 
> "count" -> 198240|>, < .. 767 ..>}

## Chart

Get historical data from Binance

* "BTCUSDT" - cryptocurrency pair
* "15m" - time interval of the one "candle". 
  Available intervals: 1m, 3m, 5m, 15m, 30m, 1h, 2h, 4h, 6h, 12h, 1d
* "limit" -> 96 - number of candles. Max value is 1000, default - 500

Interactive chart with different indicators

```wolfram
InteractiveTradingChart[
	Query[All, {1, {2, 3, 4, 5, 6}}] @ BinanceKlines["BTCUSDT", "15m", "limit" -> 192], 
	ImageSize -> Large, PlotTheme -> "Marketing"
]
```

>![](https://wolfr.am/LdrufmFh)

## Account

Non zero balances

```wolfram
SortBy[#free&] @ 
Select[#free > 0&] @ 
BinanceAccountInfo[]["balances"]
```

>{<|"asset" -> "BTC", "free" -> 1.0*10^6, "locked" -> 0.0|>}

History of all trades

```wolfram
BinanceMyTrades["BTCUSDT"]
```

>{<|"symbol" -> "BTCUSDT", "id" -> 17056810, "orderId" -> 44470524, 
>  "orderListId" -> -1, "price" -> 8750., "qty" -> 0.013617, 
>  "quoteQty" -> 119.149, "commission" -> 0.0357446, 
>  "commissionAsset" -> "USDT", 
>  "time" -> 
>   DateObject[{2018, 2, 14, 8, 7, 40.198}, "Instant", "Gregorian", 
>    4.], "isBuyer" -> False, "isMaker" -> True, 
>  "isBestMatch" -> True|>, ...}

History of all orders. 
Like all trades but in these statistics you can see what orders were canceled. 

```wolfram
BinanceOrdersAll["BTCUSDT"]
```

><|"symbol" -> "BTCUSDT", "orderId" -> 44470524, "orderListId" -> -1, 
> "clientOrderId" -> "ItBhjKwekNUlg8wRiLrzFT", "price" -> 8750., 
> "origQty" -> 0.013617, "executedQty" -> 0.013617, 
> "cummulativeQuoteQty" -> 119.149, "status" -> "FILLED", 
> "timeInForce" -> "GTC", "type" -> "LIMIT", "side" -> "SELL", 
> "stopPrice" -> 0., "icebergQty" -> 0., 
> "time" -> 
>  DateObject[{2018, 2, 14, 8, 6, 50.655}, "Instant", "Gregorian", 4.],
>  "updateTime" -> 
>  DateObject[{2018, 2, 14, 8, 7, 40.198}, "Instant", "Gregorian", 4.],
>  "isWorking" -> True, "origQuoteOrderQty" -> 0.|>

## Trading

One of the important parts of this guide - trading. 
The point of Binance is to trade cryptocurrency!

You can create simple order using following. 

First - get the current price of BTC:
```wolfram
price = BinancePrice["BTCUSDT"]["price"]
```

>6808.69

And try to sell BTC 10% more expensive than our original purchase:

```wolfram
BinanceSell["BTCUSDT", 0.001, price * 1.1]
```

><|"symbol" -> "BTCUSDT", "orderId" -> 1739082090, "orderListId" -> -1,
>  "clientOrderId" -> "LbwGlsBinxRtNRsabubXCR", 
> "transactTime" -> 
>  DateObject[{2020, 4, 5, 23, 10, 35.547}, "Instant", "Gregorian", 
>   4.], "price" -> 7487.77., "origQty" -> 0.001, "executedQty" -> 0., 
> "cummulativeQuoteQty" -> 0., "status" -> "NEW", 
> "timeInForce" -> "GTC", "type" -> "LIMIT", "side" -> "SELL", 
> "fills" -> {}|>

Now you can check that the order was created. 

Try to get all opened orders:

```wolfram
orders = BinanceOrdersNow["BTCUSDT"]
```

Get order informoation first: 

```wolfram
BinanceOrderGet["BTCUSDT", orders[[1, "orderId"]]]
```

><|"symbol" -> "BTCUSDT", "orderId" -> 1739082090, "orderListId" -> -1,
>  "clientOrderId" -> "LbwGlsBinxRtNRsabubXCR", "price" -> 7487.77., 
> "origQty" -> 0.001, "executedQty" -> 0., 
> "cummulativeQuoteQty" -> 0., "status" -> "NEW", 
> "timeInForce" -> "GTC", "type" -> "LIMIT", "side" -> "SELL", 
> "stopPrice" -> 0., "icebergQty" -> 0., 
> "time" -> 
>  DateObject[{2020, 4, 5, 23, 10, 35.547}, "Instant", "Gregorian", 
>   4.], "updateTime" -> 
>  DateObject[{2020, 4, 5, 23, 10, 35.547}, "Instant", "Gregorian", 
>   4.], "isWorking" -> True, "origQuoteOrderQty" -> 0.|>

If you changed the decision, then you can cancel the order:

```wolfram
BinanceOrderCancel["BTCUSDT", orders[[1, "orderId"]]]
```

><|"symbol" -> "BTCUSDT", 
> "origClientOrderId" -> "LbwGlsBinxRtNRsabubXCR", 
> "orderId" -> 1739082090, "orderListId" -> -1, 
> "clientOrderId" -> "x5uDpwDlIcnHn2rdTcZgnR", "price" -> 7484.77., 
> "origQty" -> 0.001, "executedQty" -> 0., 
> "cummulativeQuoteQty" -> 0., "status" -> "CANCELED", 
> "timeInForce" -> "GTC", "type" -> "LIMIT", "side" -> "SELL"|>

## Contributing

You can use all available ways to contribute to this project. 

### 1. Join the team.

We will glad to see you in ExchangeLink team. Please contact with us. 

### 2. Standard github flow

More about this way on the [official github help page](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/github-flow)

### 3. Contact with the team

If you have any suggestions or questions, you can always write us an email. We will consider any offer and try to answer all your questions.

## Contacts

**Jon Woodard** ([@Lenley](https://github.com/Lenley)) - jonlbwoodard@gmail.com  
**Kirill Belov** ([@KirillBelovTest](https://github.com/KirillBelovTest)) - KirillBelovTest@gmail.com
