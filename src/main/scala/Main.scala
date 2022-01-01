import com.github.tototoshi.csv._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal

// https://docs.scala-lang.org/scala3/book/types-structural.html
class Record(elems: (String, Any)*) extends Selectable:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)

type State = Record {
  val currency: String
  val lastPrice: BigDecimal
}

enum TransactionType:
  case buy, sell

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

def processTx(st: State, tx: Transaction): State =
  st

@main def main: Unit = 
  // TODO initialize zero State
  val reader = CSVReader.open("trades.csv")
  val df = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSSS")
  val it = reader.iteratorWithHeaders.map(
    tx =>
      Record(
        "currency" -> tx("pair"),
        "time" -> LocalDateTime.parse(tx("time"), df), // TODO actually UTC, but should be converted to local timezone
        "typ" -> TransactionType.valueOf(tx("type")),
        "price" -> BigDecimal(tx("price")),
        "cost" -> BigDecimal(tx("cost")),
        "fee" -> BigDecimal(tx("fee")),
        "vol" -> BigDecimal(tx("vol"))
    ).asInstanceOf[Transaction])
  // TODO foldLeft processTx over the collection
  val tx = it.next
  printf("on %s: %s %s %s at %s (%s + %s)\n", tx.time, tx.typ, tx.vol, tx.currency, tx.price, tx.cost, tx.fee)
  reader.close()
