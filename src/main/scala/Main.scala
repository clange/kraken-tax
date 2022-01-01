import com.github.tototoshi.csv._
import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter,DateTimeFormatterBuilder}
import java.time.temporal.{ChronoField,Temporal}

/** Record type according to https://docs.scala-lang.org/scala3/book/types-structural.html */
class Record(elems: (String, Any)*) extends Selectable:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)

/** State of gains and taxation */
type State = Record {
  val assets: Map[Currency, BigDecimal]
  /* TODO actually, for each currency, maintain a List of each _purchase_, including:
   * * the currency
   * * the amount purchased
   * * the amount still available (taking into account sales)
   * * the cost paid
   * * the fee paid
   * * the date/time of purchase
   */
  val sumFees: BigDecimal
}

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

/** Given the current state, process a transaction and return the new state */
def processTx(st: State, tx: Transaction): State =
  // printf("on %s: %s %s %s at %s (%s + %s)\n", tx.time, tx.typ, tx.vol, tx.currency, tx.price, tx.cost, tx.fee)
  Record(
    "assets" -> (
      if (st.assets.contains(tx.currency))
        st.assets + 
          (tx.currency -> (st.assets(tx.currency) + tx.typ.sign * tx.vol))
        /* TODO add a new data structure for each purchase, as documented above for State.assets – run `tx.typ match` first */
        /* TODO implement FIFO algorithm for selling:
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
      else
        tx.typ match
          case TransactionType.buy => st.assets + (tx.currency -> tx.vol)
          /* TODO actually construct a more complex purchase data structure, as documented above for State.assets */
          case TransactionType.sell => throw TransactionException("trying to sell an asset of which we don't have any")
      ),
    "sumFees" -> (st.sumFees + tx.fee))
    .asInstanceOf[State]

/** Old-style currency pair, e.g., XETHZEUR */
val currencyREXZ = """X(\p{Lu}+)ZEUR""".r
/** Currency pair, e.g., DOTEUR */
val currencyRE = """(\p{Lu}+)EUR""".r

/** Extracts the crypto currency out of the given "pair" string.
 * We currently assume that always the same fiat currency is involved (hard-coded to EUR) */
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
    "assets" -> Map.empty[Currency, BigDecimal],
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
          "time" -> LocalDateTime.parse(tx("time"), df), // TODO actually UTC, but should be converted to local timezone
          "typ" -> TransactionType.valueOf(tx("type")),
          "price" -> BigDecimal(tx("price")),
          "cost" -> BigDecimal(tx("cost")),
          "fee" -> BigDecimal(tx("fee")),
          "vol" -> BigDecimal(tx("vol"))
        ).asInstanceOf[Transaction])
    .foldLeft(st)(processTx)
  println(finalSt.assets)
  printf("Sum of fees: %s\n", finalSt.sumFees)
  reader.close()
