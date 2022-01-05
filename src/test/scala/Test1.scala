import org.junit.Test
import org.junit.Assert.*

import java.time.LocalDateTime
import java.time.temporal.Temporal
import scala.collection.immutable.{ListMap,SeqMap}

class PurchaseAmount(override val amountPurchased: BigDecimal)
extends Purchase(
  amountPurchased,
  cost = 0, // irrelevant for this test
  fee  = 0  // irrelevant for this test
)

val dateLate = LocalDateTime.parse("9999-12-31 23:59:59.9999", dateTimeFormat)

def sellFIFOVolume(purchases: SeqMap[Temporal, Purchase], volume: BigDecimal): SeqMap[Temporal, Purchase] =
  sellFIFO(
    purchases,
    time   = dateLate, // irrelevant for this test
    volume = volume,
    cost   = 0         // irrelevant for this test
  )

class Test1:
  val date1    = LocalDateTime.parse("2021-12-11 00:17:59.1546", dateTimeFormat)
  val date2    = LocalDateTime.parse("2021-12-20 05:57:51.4673", dateTimeFormat)
  val date3    = LocalDateTime.parse("2021-12-20 06:00:26.6124", dateTimeFormat)

  @Test def sellFIFO_SinglePurchaseCompletely(): Unit =
    val purchasesBefore = ListMap[Temporal, Purchase](date1 // irrelevant for this test
      -> PurchaseAmount(1))
    val purchasesAfter = sellFIFOVolume(purchasesBefore, 1)
    assertTrue(purchasesAfter.isEmpty)

  @Test def sellFIFO_SinglePurchasePartially(): Unit =
    val purchasesBefore = ListMap[Temporal, Purchase](date1 // irrelevant for this test
      -> PurchaseAmount(2))
    val purchasesAfter = sellFIFOVolume(purchasesBefore, 1)
    assertEquals(purchasesAfter.head._2.amountLeft, 1)

  @Test def sellFIFO_FirstAndPartOfSecondPurchase(): Unit =
    val purchasesBefore = ListMap[Temporal, Purchase](
      date1 -> PurchaseAmount(1),
      date2 -> PurchaseAmount(2)
    )
    val purchasesAfter = sellFIFOVolume(purchasesBefore, 2)
    assertEquals(purchasesAfter.head._2.amountLeft, 1)

  @Test def sellFIFO_FirstAndPartOfSecondButNotThird(): Unit =
    val purchasesBefore = ListMap[Temporal, Purchase](
      date1 -> PurchaseAmount(1),
      date2 -> PurchaseAmount(2),
      date3 -> PurchaseAmount(3))
    val purchasesAfter = sellFIFOVolume(purchasesBefore, 2)
    assertEquals(purchasesAfter.tail.head._2.amountLeft, 3)

  @Test def sellFIFO_FirstAndSecondAndPartOfThird(): Unit =
    val purchasesBefore = ListMap[Temporal, Purchase](
      date1 -> PurchaseAmount(1),
      date2 -> PurchaseAmount(2),
      date3 -> PurchaseAmount(3))
    val purchasesAfter = sellFIFOVolume(purchasesBefore, 4)
    assertEquals(purchasesAfter.head._2.amountLeft, 2)
