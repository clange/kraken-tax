[How to interpret trades history fields](https://support.kraken.com/hc/en-us/articles/360001184886-How-to-interpret-Trades-history-fields)

Assumption: history has been exported with at least the following fields:
* txid
* pair
* time
* type
* price
* cost
* fee
* vol

## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).
