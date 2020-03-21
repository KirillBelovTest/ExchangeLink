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



## Contributing

After uploading to githab

