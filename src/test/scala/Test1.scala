import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
import org.scalatest.matchers.should.Matchers._

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

def sellFIFOVolume(purchases: SeqMap[Temporal, Purchase], volume: BigDecimal): (SeqMap[Temporal, Purchase], BigDecimal, BigDecimal, BigDecimal) =
  sellFIFO(
    purchases,
    time           = dateLate, // irrelevant for this test
    timeMinus1Year = dateLate, // irrelevant for this test
    volume         = volume,
    cost           = 0         // irrelevant for this test
  )

class Test1 extends AnyFunSpec with Matchers:
  val date1    = LocalDateTime.parse("2021-12-11 00:17:59.1546", dateTimeFormat)
  val date2    = LocalDateTime.parse("2021-12-20 05:57:51.4673", dateTimeFormat)
  val date3    = LocalDateTime.parse("2021-12-20 06:00:26.6124", dateTimeFormat)

  describe("The list of purchases") {
    describe("after a single purchase has been sold completely") {
      it("should be empty") {
        val purchasesBefore = ListMap[Temporal, Purchase](date1 // irrelevant for this test
          -> PurchaseAmount(1))
        val purchasesAfter = sellFIFOVolume(purchasesBefore, 1)
        purchasesAfter._1 shouldBe empty
      }
    }

    describe("after a single purchase has been sold partially") {
      it("should have some amount left from that purchase") {
        val purchasesBefore = ListMap[Temporal, Purchase](date1 // irrelevant for this test
          -> PurchaseAmount(2))
        val purchasesAfter = sellFIFOVolume(purchasesBefore, 1)
        purchasesAfter._1.head._2.amountLeft shouldBe 1
      }
    }

    describe("after the first purchase has been sold completely, and part of the second purchase,") {
      it("should have some amount left from the latter") {
        val purchasesBefore = ListMap[Temporal, Purchase](
          date1 -> PurchaseAmount(1),
          date2 -> PurchaseAmount(2)
        )
        val purchasesAfter = sellFIFOVolume(purchasesBefore, 2)
        purchasesAfter._1.head._2.amountLeft shouldBe 1
      }

      it("should have everything left from a third purchase") {
        val purchasesBefore = ListMap[Temporal, Purchase](
          date1 -> PurchaseAmount(1),
          date2 -> PurchaseAmount(2),
          date3 -> PurchaseAmount(3))
        val purchasesAfter = sellFIFOVolume(purchasesBefore, 2)
        purchasesAfter._1.tail.head._2.amountLeft shouldBe 3
      }
    }

    describe("after the first and second purchases have been sold completely, and part of the third purchase,") {
      it("should have some amount left from the latter") {
        val purchasesBefore = ListMap[Temporal, Purchase](
          date1 -> PurchaseAmount(1),
          date2 -> PurchaseAmount(2),
          date3 -> PurchaseAmount(3))
        val purchasesAfter = sellFIFOVolume(purchasesBefore, 4)
        purchasesAfter._1.head._2.amountLeft shouldBe 2
      }
    }
  }
