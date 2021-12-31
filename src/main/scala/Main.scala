import com.github.tototoshi.csv._

// https://docs.scala-lang.org/scala3/book/types-structural.html
class Record(elems: (String, Any)*) extends Selectable:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)

type State = Record {
  val currency: String
  val lastPrice: BigDecimal
}

type Transaction = Record {
  val currency: String
  val price: BigDecimal
}

def processTx(st: State, tx: Transaction): State =
  st

@main def main: Unit = 
  val reader = CSVReader.open("trades.csv")
  val it = reader.iteratorWithHeaders
  val tx = it.next
  printf("%s: %s\n", tx("pair"), tx("price"))
  reader.close()
