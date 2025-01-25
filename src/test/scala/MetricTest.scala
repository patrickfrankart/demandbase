import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.funsuite.AnyFunSuite

import java.time.LocalDateTime

class MetricTest extends AnyFunSuite {
  val epsilon = 1e-4f
  implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(epsilon)

  test("sum"){
    assert(Metric.sum(Array(Metric(1, LocalDateTime.now()), Metric(2, LocalDateTime.now()))) == 3)
  }
  test("average"){
    // === operator uses implicit tolerant equality from above for floating-point comparison
    assert(Metric.average(Array(Metric(4, LocalDateTime.now()), Metric(2, LocalDateTime.now()))) === 3.0)
  }
  test("greater than"){
    assert(Metric.greaterThan(
      Array(Metric(4, LocalDateTime.now()), Metric(2, LocalDateTime.now())), 3)
        .length == 1)
    assert(Metric.greaterThan(
      Array(Metric(4, LocalDateTime.now()), Metric(2, LocalDateTime.now())), LocalDateTime.of(
        2007, 1, 1, 1, 1, 1)).length == 2)
    assert(Metric.greaterThan(
      Array(Metric(4, LocalDateTime.now(), Label.ACTIVE), Metric(2, LocalDateTime.now())), Label.INACTIVE)
      .length == 1)
  }
  test("greater than or equal"){
    assert(Metric.greaterThanOrEqual(
      Array(Metric(4, LocalDateTime.now()), Metric(2, LocalDateTime.now()), Metric(3, LocalDateTime.now())), 3)
      .length == 2)
    assert(Metric.greaterThanOrEqual(
      Array(Metric(4, LocalDateTime.now()), Metric(2, LocalDateTime.now()), Metric(3, LocalDateTime.now())), LocalDateTime.of(
        2007, 1, 1, 1, 1, 1)).length == 3)
    assert(Metric.greaterThanOrEqual(
      Array(Metric(4, LocalDateTime.now(), Label.ACTIVE), Metric(2, LocalDateTime.now()), Metric(3, LocalDateTime.now(), Label.INACTIVE)), Label.INACTIVE)
      .length == 2)
  }
  test("less than"){
    assert(Metric.lessThan(
      Array(
        Metric(4, LocalDateTime.of(2007, 1, 1, 1, 1, 1)),
        Metric(2, LocalDateTime.of(2008, 1, 1, 1, 1, 1))),
      3).length == 1)
    assert(Metric.lessThan(
      Array(
        Metric(4, LocalDateTime.of(2007, 1, 1, 1, 1, 1)),
        Metric(2, LocalDateTime.of(2008, 1, 1, 1, 1, 1))),
      LocalDateTime.now()).length == 2)
    assert(Metric.lessThan(
      Array(Metric(4, LocalDateTime.now(), Label.ACTIVE), Metric(2, LocalDateTime.now())), Label.INACTIVE)
      .length == 1)
  }
  test("less than or equal"){
    assert(Metric.lessThanOrEqual(
      Array(Metric(4, LocalDateTime.now()), Metric(2, LocalDateTime.now()), Metric(3, LocalDateTime.now())), 3)
      .length == 2)
    assert(Metric.lessThanOrEqual(
      Array(
        Metric(4, LocalDateTime.of(2007, 1, 1, 1, 1, 1)),
        Metric(2, LocalDateTime.of(2008, 1, 1, 1, 1, 1))),
      LocalDateTime.of(2008, 1, 1, 1, 1, 1)).length == 2)
    assert(Metric.lessThanOrEqual(
      Array(Metric(4, LocalDateTime.now(), Label.ACTIVE), Metric(2, LocalDateTime.now()), Metric(3, LocalDateTime.now(), Label.INACTIVE)), Label.INACTIVE)
      .length == 2)
  }
  test("equal"){
    assert(Metric.equal(
      Array(
        Metric(4, LocalDateTime.of(2007, 1, 1, 1, 1, 1)),
        Metric(2, LocalDateTime.of(2008, 1, 1, 1, 1, 1))),
      2).length == 1)
    assert(Metric.equal(
      Array(
        Metric(4, LocalDateTime.of(2007, 1, 1, 1, 1, 1)),
        Metric(2, LocalDateTime.of(2008, 1, 1, 1, 1, 1))),
      LocalDateTime.of(2008, 1, 1, 1, 1, 1)).length == 1)
  }
  test("hour group"){
    assert(Metric.hourGroup(
      Array(
        Metric(4, LocalDateTime.of(2007, 1, 1, 3, 1, 1)),
        Metric(2, LocalDateTime.of(2008, 1, 1, 6, 1, 1))),
      3).length == 1)
  }
  test("minute group"){
    assert(Metric.minuteGroup(
      Array(
        Metric(4, LocalDateTime.of(2007, 1, 1, 3, 3, 1)),
        Metric(2, LocalDateTime.of(2008, 1, 1, 6, 7, 1))),
      3).length == 1)
  }
  test("day group"){
    assert(Metric.dayGroup(
      Array(
        Metric(4, LocalDateTime.of(2007, 1, 9, 3, 3, 1)),
        Metric(2, LocalDateTime.of(2008, 1, 15, 6, 7, 1))),
      15).length == 1)
  }
  test("label group"){
    assert(Metric.labelGroup(
      Array(
        Metric(4, LocalDateTime.of(2007, 1, 9, 3, 3, 1), Label.ACTIVE),
        Metric(2, LocalDateTime.of(2008, 1, 15, 6, 7, 1), Label.ACTIVE)),
      Label.ACTIVE).length == 2)
  }
}
