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
  // val currency: String
  val sumFees: BigDecimal
}

/** Types of transactions */
enum TransactionType:
  case buy, sell

/** All relevant information about a transaction */
type Transaction = Record {
  /** part of "pair" */
  val currency: String
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

/** Given the current state, process a transaction and return the new state */
def processTx(st: State, tx: Transaction): State =
  printf("on %s: %s %s %s at %s (%s + %s)\n", tx.time, tx.typ, tx.vol, tx.currency, tx.price, tx.cost, tx.fee)
  tx.typ match
    case TransactionType.sell => Record("sumFees" -> (st.sumFees + tx.fee)).asInstanceOf[State]
    case TransactionType.buy => st

/** Old-style currency pair, e.g., XETHZEUR */
val currencyREXZ = """X(\p{Lu}+)ZEUR""".r
/** Currency pair, e.g., DOTEUR */
val currencyRE = """(\p{Lu}+)EUR""".r

/** Extracts the crypto currency out of the given "pair" string.
 * We currently assume that always the same fiat currency is involved (hard-coded to EUR) */
def extractCryptoCurrency(pair: String): String =
  pair match
    case currencyREXZ(currency) => currency
    case currencyRE(currency) => currency

/** Main program */
@main def main: Unit = 
  // Open CSV export file for reading
  val reader = CSVReader.open("trades.csv")
  // initialize zero State
  val st = Record("sumFees" -> BigDecimal(0)).asInstanceOf[State]
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
  printf("%s\n", finalSt.sumFees)
  reader.close()
