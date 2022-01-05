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

/** Execute a (partial) sale, starting with the first purchase of an asset, then (if anything is left) continuing with the subsequent purchases of the same asset. */
def sellFIFOStep(firstPurchaseTime: Temporal, firstPurchase: Purchase, nextPurchases: SeqMap[Temporal, Purchase], time: Temporal, timeMinus1Year: Temporal, volume: BigDecimal, cost: BigDecimal): SeqMap[Temporal, Purchase] =
  val firstPurchaseAmountReduced = firstPurchase.amountLeft - volume
  if firstPurchaseAmountReduced > 0 then
    // if the first purchase has not been sold completely, just reduce its amount ...
    ListMap(firstPurchaseTime -> Purchase(
      firstPurchase.amountPurchased,
      firstPurchaseAmountReduced,
      firstPurchase.cost,
      firstPurchase.fee))
    // ... and leave the rest of the list unchanged.
      ++ nextPurchases
  else if firstPurchaseAmountReduced == 0 then
    // if the first purchase has been sold exactly, just return the remaining ones.
    nextPurchases
  else
    // if a greater amount of the asset has been sold than purchased first, then continue processing the remaining ones
    if !nextPurchases.isEmpty then
      val (newFirstPurchaseTime, newFirstPurchase) = nextPurchases.head
      val newNextPurchases = nextPurchases.tail
      sellFIFOStep(newFirstPurchaseTime, newFirstPurchase, newNextPurchases, time, timeMinus1Year, -firstPurchaseAmountReduced, /* FIXME we might have to reduce this */ cost)
    else
      throw TransactionException("trying to sell more of an asset than we had left")

/** From the available purchases of an asset (non-empty), execute a sale, starting with those purchased first. */
def sellFIFO(purchases: SeqMap[Temporal, Purchase], time: LocalDateTime, volume: BigDecimal, cost: BigDecimal): SeqMap[Temporal, Purchase] =
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
  val (firstPurchaseTime, firstPurchase) = purchases.head
  val nextPurchases = purchases.tail
  val timeMinus1Year = time.minusYears(1)
  sellFIFOStep(firstPurchaseTime, firstPurchase, nextPurchases, time, timeMinus1Year, volume, cost)

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
            if !st.assets(tx.currency).isEmpty then
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

/** default date/time formatter */
val dateTimeFormat = DateTimeFormatterBuilder()
  .appendPattern("uuuu-MM-dd HH:mm:ss")
  .appendFraction(ChronoField.NANO_OF_SECOND, 3, 4, true)
  .toFormatter()

/** initial state before processing any transactions */
val zeroState = Record(
  "assets" -> Map
    .empty[Currency, SeqMap[Temporal, Purchase]]
    .withDefaultValue(ListMap.empty[LocalDateTime, Purchase]),
  "sumFees" -> BigDecimal(0))
  .asInstanceOf[State]

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
    .foldLeft(zeroState)(processTx)
  println(finalState.assets)
  printf("Sum of fees: %s\n", finalState.sumFees)
  reader.close()
