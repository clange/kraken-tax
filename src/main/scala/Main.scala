import com.github.tototoshi.csv._
import java.time.LocalDateTime

// https://docs.scala-lang.org/scala3/book/types-structural.html
class Record(elems: (String, Any)*) extends Selectable:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)

type State = Record {
  val currency: String
  val lastPrice: BigDecimal
}

object TransactionType extends Enumeration:
  val buy, sell = Value

type Transaction = Record {
  /** part of "pair" */
  val currency: String
  /** "time" */
  val time: String // TODO LocalDateTime
  /** "type" */
  val typ: String // TODO TransactionType.Value
  /** "price" */
  val price: String // TODO BigDecimal
  /** "cost" */
  val cost: String // TODO BigDecimal
  /** "fee" */
  val fee: String // TODO BigDecimal
  /** "vol" */
  val vol: String // TODO BigDecimal
}

def processTx(st: State, tx: Transaction): State =
  st

@main def main: Unit = 
  // TODO initialize zero State
  val reader = CSVReader.open("trades.csv")
  // Map(Foo -> a, Bar -> b, Baz -> c)
  val it = reader.iteratorWithHeaders.map(
    tx =>
      Record( // TODO parse dates and numbers
        "currency" -> tx("pair"),
        "time" -> tx("time"),
        "typ" -> tx("type"),
        "price" -> tx("price"),
        "cost" -> tx("cost"),
        "fee" -> tx("fee"),
        "vol" -> tx("vol")
    ).asInstanceOf[Transaction])
  // TODO foldLeft processTx over the collection
  val tx = it.next
  printf("%s: %s\n", tx.currency, tx.price)
  reader.close()
