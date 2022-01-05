import com.github.tototoshi.csv._
import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter,DateTimeFormatterBuilder}
import java.time.temporal.{ChronoField,Temporal}
import scala.collection.immutable.{ListMap,SeqMap}

/** We currently assume that always the same fiat currency is involved (hard-coded to EUR) */
val onlyFiatCurrency = "EUR"

/** Record type according to https://docs.scala-lang.org/scala3/book/types-structural.html */
class Record(elems: (String, Any)*) extends Selectable:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)

/** State of gains and taxation */
type State = Record {
  val assets: Map[Currency, SeqMap[Temporal, Purchase]]
  val sumFees: BigDecimal
}

/** One purchase of an asset (and what's left of it) */
class Purchase(
  // TODO do we need to (redundantly) store currency and date here as well, or rather just in "state"?
  /** the amount purchased */
  amountPurchased: BigDecimal,
  /** the amount still available (taking into account sales) */
  amountLeft: BigDecimal,
  /** the cost of purchase */
  cost: BigDecimal,
  /** the fee paid for purchasing */
  fee: BigDecimal):
  /** For a new purchase, the amount left is the same as the amount purchased. */
  def this(amountPurchased: BigDecimal, cost: BigDecimal, fee: BigDecimal) =
    this(amountPurchased, amountPurchased, cost, fee)

  /** Only print the amount left if it is different from (i.e., lower than) the amount purchased. */
  override def toString(): String =
    val left =
      if (amountLeft < amountPurchased) then
        s" – $amountLeft left"
      else
        ""
    s"$amountPurchased purchased for $onlyFiatCurrency $cost ($onlyFiatCurrency $fee fee)$left"

/** Explicitly typed wrapper for a currency name */
case class Currency(name: String):
  override def toString(): String =
    name

/** Types of transactions */
enum TransactionType:
  case buy, sell

  /** Buying has a positive sign in computations, selling has a negative sign */
  def sign =
    this match
      case TransactionType.buy => 1
      case TransactionType.sell => -1

/** All relevant information about a transaction */
type Transaction = Record {
  /** part of "pair" */
  val currency: Currency
  /** "time" */
  val time: Temporal
  /** "type" */
  val typ: TransactionType
  /** "price" */
  val price: BigDecimal
  /** "cost" */
  val cost: BigDecimal
  /** "fee" */
  val fee: BigDecimal
  /** "vol" */
  val vol: BigDecimal
}

/** Custom class for exceptions that occur during transaction processing */
case class TransactionException(
  private val message: String = "", 
  private val cause: Throwable = None.orNull)
extends Exception(message, cause)

/** From the */
def sellFIFO(purchases: SeqMap[Temporal, Purchase], time: Temporal, volume: BigDecimal, cost: BigDecimal): SeqMap[Temporal, Purchase] =
  /* FIXME implement FIFO algorithm for selling:
   *
   * while vol > 0
   *   reduce volume of first (list head) purchase of currency
   *   determine cost of purchasing that amount (proportionate if > 0 remains) (*)
   *   determine (proportionate) fee of purchasing that amount (*)
   *   continue with next purchase of same currency
   *
   * gain = sumSold - sumPurchaseCost
   *
   * overallFee = sumPurchaseFee + sumSaleFee
   *
   * taxableGain = gain - overallFee
   * (*) for taxation, only take into account purchases with a holding period of <= a year
   */
  purchases

/** Given the current state, process a transaction and return the new state */
def processTx(st: State, tx: Transaction): State =
  // printf("on %s: %s %s %s at %s (%s + %s)\n", tx.time, tx.typ, tx.vol, tx.currency, tx.price, tx.cost, tx.fee)
  Record(
    "assets" -> (st.assets +
      (tx.currency ->
        (tx.typ match
          case TransactionType.buy =>
            // the following default to the empty Map
            st.assets(tx.currency)
              + (tx.time -> Purchase(
                amountPurchased = tx.vol,
                cost            = tx.cost,
                fee             = tx.fee))
          case TransactionType.sell =>
            if st.assets.contains(tx.currency) then
              // TODO implement not just the change to the assets, but also compute the tax
              sellFIFO(
                purchases = st.assets(tx.currency),
                time   = tx.time,
                volume = tx.vol,
                cost   = tx.cost)
            else
              throw TransactionException("trying to sell an asset of which we don't have any")
      ))),
    "sumFees" -> (st.sumFees + tx.fee))
    .asInstanceOf[State]

/** Old-style currency pair, e.g., XETHZEUR */
val currencyREXZ = s"X(\\p{Lu}+)Z$onlyFiatCurrency".r
/** Currency pair, e.g., DOTEUR */
val currencyRE = s"(\\p{Lu}+)$onlyFiatCurrency".r

/** Extracts the crypto currency out of the given "pair" string. */
def extractCryptoCurrency(pair: String): Currency =
  pair match
    case currencyREXZ(currency) => Currency(currency)
    case currencyRE(currency) => Currency(currency)

/** Main program */
@main def main: Unit = 
  // Open CSV export file for reading
  val reader = CSVReader.open("trades.csv")
  // initialize zero State
  val st = Record(
    "assets" -> Map
      .empty[Currency, SeqMap[Temporal, Purchase]]
      .withDefaultValue(ListMap.empty[LocalDateTime, Purchase]),
    "sumFees" -> BigDecimal(0))
    .asInstanceOf[State]
  // initialize date/time formatter
  val df = DateTimeFormatterBuilder()
    .appendPattern("uuuu-MM-dd HH:mm:ss")
    .appendFraction(ChronoField.NANO_OF_SECOND, 3, 4, true)
    .toFormatter()
  // read all rows from the CSV
  val finalSt = reader.iteratorWithHeaders
  // parse one CSV record into a Transaction
    .map(
      tx =>
        Record(
          "currency" -> extractCryptoCurrency(tx("pair")),
          "time"     -> LocalDateTime.parse(tx("time"), df), // TODO actually UTC, but should be converted to local timezone to be precise w.r.t. tax years
          "typ"      -> TransactionType.valueOf(tx("type")),
          "price"    -> BigDecimal(tx("price")),
          "cost"     -> BigDecimal(tx("cost")),
          "fee"      -> BigDecimal(tx("fee")),
          "vol"      -> BigDecimal(tx("vol"))
        ).asInstanceOf[Transaction])
    .foldLeft(st)(processTx)
  println(finalSt.assets)
  printf("Sum of fees: %s\n", finalSt.sumFees)
  reader.close()
