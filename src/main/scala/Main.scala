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
class State(
  /** the assets being held at the moment */
  val assets: Map[Currency, SeqMap[Temporal, Purchase]],
  /** the sum of all gains so far */
  val sumGains: BigDecimal,
  /** the sum of all taxable gains so far */
  val sumTaxableGains: BigDecimal,
  /** the sum of all fees incurred so far */
  val sumFees: BigDecimal):
  /** initial state before processing any transactions */
  def this() =
    this(
      assets = Map
        .empty[Currency, SeqMap[Temporal, Purchase]]
        .withDefaultValue(ListMap.empty[Temporal, Purchase]),
      sumGains = BigDecimal(0),
      sumTaxableGains = BigDecimal(0),
      sumFees = BigDecimal(0))

/** One purchase of an asset (and what's left of it) */
class Purchase(
  // TODO do we need to (redundantly) store currency and date here as well, or rather just in "state"?
  /** the amount purchased */
  val amountPurchased: BigDecimal,
  /** the amount still available (taking into account sales) */
  val amountLeft: BigDecimal,
  /** the cost of purchase */
  val cost: BigDecimal,
  /** the fee paid for purchasing */
  val fee: BigDecimal):
  /** For a new purchase, the amount left is the same as the amount purchased. */
  def this(amountPurchased: BigDecimal, cost: BigDecimal, fee: BigDecimal) =
    this(amountPurchased, amountPurchased, cost, fee)

  /** Only print the amount left if it is different from (i.e., lower than) the amount purchased. */
  override def toString(): String =
    val left =
      if amountLeft < amountPurchased then
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
  val time: LocalDateTime
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

/** From the available purchases of an asset (non-empty), execute a sale, starting with those purchased first.
  * Execute a (partial) sale, starting with the first purchase of an asset, then (if anything is left) continuing with the subsequent purchases of the same asset. */
def sellFIFO(purchases: SeqMap[Temporal, Purchase], time: Temporal, timeMinus1Year: Temporal, volume: BigDecimal, cost: BigDecimal): (SeqMap[Temporal, Purchase], BigDecimal, BigDecimal) =
  val (firstPurchaseTime, firstPurchase) = purchases.head
  val nextPurchases = purchases.tail
  val firstPurchaseAmountReduced = firstPurchase.amountLeft - volume
  /* FIXME implement FIFO tax computation:
   *
   * while vol > 0
   *   ✓ reduce volume of first (list head) purchase of currency
   *   determine cost of purchasing that amount (proportionate if > 0 remains) (*)
   *   determine (proportionate) fee of purchasing that amount (*)
   *   continue with next purchase of same currency
   *
   * gain = sumSold - sumPurchaseCost
   *
   * overallFee = sumPurchaseFee + sumSaleFee
   *
   * taxableGain = gain - overallFee
   * (*) for taxation, only take into account sales of purchases with a holding period of <= a year
   */
  firstPurchaseAmountReduced match
    case x if x > 0 =>
      // if the first purchase has not been sold completely, just reduce its amount ...
      (ListMap(firstPurchaseTime -> Purchase(
        firstPurchase.amountPurchased,
        firstPurchaseAmountReduced,
        firstPurchase.cost,
        firstPurchase.fee))
      // ... and leave the rest of the list unchanged.
        ++ nextPurchases,
        BigDecimal(0), // FIXME
        BigDecimal(0)) // FIXME
    case 0 =>
      // if the first purchase has been sold exactly, just return the remaining ones.
      (nextPurchases,
        BigDecimal(0), // FIXME
        BigDecimal(0)) // FIXME
    case x if x < 0 =>
      // if a greater amount of the asset has been sold than purchased first, then continue processing the remaining ones
      if !nextPurchases.isEmpty then
        sellFIFO(nextPurchases, time, timeMinus1Year, -firstPurchaseAmountReduced, /* FIXME we might have to reduce this */ cost)
      else
        throw TransactionException("trying to sell more of an asset than we had left")

/** Given the current state, process a transaction and return the new state */
def processTx(st: State, tx: Transaction): State =
  // printf("on %s: %s %s %s at %s (%s + %s)\n", tx.time, tx.typ, tx.vol, tx.currency, tx.price, tx.cost, tx.fee)
  tx.typ match
    case TransactionType.buy =>
      State(assets      = (st.assets +
        (tx.currency ->
          // the following defaults to the empty Map
          (st.assets(tx.currency)
          + (tx.time -> Purchase(
            amountPurchased = tx.vol,
            cost            = tx.cost,
            fee             = tx.fee))))),
        sumGains        = st.sumGains,
        sumTaxableGains = st.sumTaxableGains,
        sumFees         = st.sumFees + tx.fee)
    case TransactionType.sell =>
      if !st.assets(tx.currency).isEmpty then
        val (assetsOfCurrency, sumGains, sumTaxableGains) = sellFIFO(
          purchases      = st.assets(tx.currency),
          time           = tx.time,
          timeMinus1Year = tx.time.minusYears(1),
          volume         = tx.vol,
          cost           = tx.cost)
        State(assets = (st.assets + (tx.currency -> assetsOfCurrency)),
          sumGains = sumGains,
          sumTaxableGains = sumTaxableGains,
          sumFees = st.sumFees + tx.fee)
      else
        throw TransactionException("trying to sell an asset of which we don't have any")

/** Old-style currency pair, e.g., XETHZEUR */
val currencyREXZ = s"X(\\p{Lu}+)Z$onlyFiatCurrency".r
/** Currency pair, e.g., DOTEUR */
val currencyRE = s"(\\p{Lu}+)$onlyFiatCurrency".r

/** Extracts the crypto currency out of the given "pair" string. */
def extractCryptoCurrency(pair: String): Currency =
  pair match
    case currencyREXZ(currency) => Currency(currency)
    case currencyRE(currency) => Currency(currency)

/** default date/time formatter */
val dateTimeFormat = DateTimeFormatterBuilder()
  .appendPattern("uuuu-MM-dd HH:mm:ss")
  .appendFraction(ChronoField.NANO_OF_SECOND, 3, 4, true)
  .toFormatter()

/** Main program */
@main def main: Unit = 
  // Open CSV export file for reading
  val reader = CSVReader.open("trades.csv")
  // read all rows from the CSV
  val finalState = reader.iteratorWithHeaders
  // parse one CSV record into a Transaction
    .map(
      tx =>
        Record(
          "currency" -> extractCryptoCurrency(tx("pair")),
          "time"     -> LocalDateTime.parse(tx("time"), dateTimeFormat), // TODO actually UTC, but should be converted to local timezone to be precise w.r.t. tax years
          "typ"      -> TransactionType.valueOf(tx("type")),
          "price"    -> BigDecimal(tx("price")),
          "cost"     -> BigDecimal(tx("cost")),
          "fee"      -> BigDecimal(tx("fee")),
          "vol"      -> BigDecimal(tx("vol"))
        ).asInstanceOf[Transaction])
    .foldLeft(new State)(processTx)
  println(finalState.assets)
  printf("Sum of fees: %s\n", finalState.sumFees)
  reader.close()
