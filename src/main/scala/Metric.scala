import java.time.LocalDateTime

case class Metric(value: Int, timestamp: LocalDateTime, label: Label = Label.UNLABELLED)

enum Label{case ACTIVE, INACTIVE, UNLABELLED}

object Metric {

  // Terminal operations. These functions calculate an aggregate and need to be the last member of chained
  // transformations.
  def sum(input: Array[Metric]): Int = {
    input.map(_.value).reduceLeft( (a: Int, b: Int) => a + b)
  }

  def average(input: Array[Metric]): Float = {
    input.map(_.value).sum.toFloat / input.map(_.value).size
  }

  // Filtering operations. These functions return a subset of the input, and can be chained to compose complex
  // transformations.
  def greaterThan(input: Array[Metric], value: Int): Array[Metric] = {
    input.filter(m => m.value > value)
  }

  def greaterThan(input: Array[Metric], timestamp: LocalDateTime): Array[Metric] = {
    input.filter(m => m.timestamp.isAfter(timestamp))
  }

  def greaterThan(input: Array[Metric], label: Label): Array[Metric] = {
    input.filter(m => m.label.ordinal > label.ordinal)
  }

  def greaterThanOrEqual(input: Array[Metric], value: Int): Array[Metric] = {
    input.filter(m => m.value >= value)
  }

  def greaterThanOrEqual(input: Array[Metric], timestamp: LocalDateTime): Array[Metric] = {
    input.filter(m => (m.timestamp.isAfter(timestamp) || m.timestamp.equals(timestamp)))
  }

  def greaterThanOrEqual(input: Array[Metric], label: Label): Array[Metric] = {
    input.filter(m => m.label.ordinal >= label.ordinal)
  }

  def lessThan(input: Array[Metric], value: Int): Array[Metric] = {
    input.filter(m => m.value < value)
  }

  def lessThan(input: Array[Metric], timestamp: LocalDateTime): Array[Metric] = {
    input.filter(m => m.timestamp.isBefore(timestamp))
  }

  def lessThan(input: Array[Metric], label: Label): Array[Metric] = {
    input.filter(m => m.label.ordinal < label.ordinal)
  }

  def lessThanOrEqual(input: Array[Metric], value: Int): Array[Metric] = {
    input.filter(m => m.value <= value)
  }

  def lessThanOrEqual(input: Array[Metric], timestamp: LocalDateTime): Array[Metric] = {
    input.filter(m => (m.timestamp.isBefore(timestamp) || m.timestamp.equals(timestamp)))
  }

  def lessThanOrEqual(input: Array[Metric], label: Label): Array[Metric] = {
    input.filter(m => m.label.ordinal <= label.ordinal)
  }

  def equal(input: Array[Metric], value: Int): Array[Metric] = {
    input.filter(m => m.value == value)
  }

  def equal(input: Array[Metric], timestamp: LocalDateTime): Array[Metric] = {
    input.filter(m => m.timestamp.equals(timestamp))
  }

  // Grouping operations. These functions return a subset of the input, and can be chained to compose complex
  // transformations.
  def hourGroup(input: Array[Metric], hour: Int): Array[Metric] ={
    input.filter(m => m.timestamp.getHour == hour)
  }

  def minuteGroup(input: Array[Metric], minute: Int): Array[Metric] ={
    input.filter(m => m.timestamp.getMinute == minute)
  }

  def dayGroup(input: Array[Metric], day: Int):Array[Metric] ={
    input.filter(m => m.timestamp.getDayOfMonth == day)
  }

  def labelGroup(input: Array[Metric], label: Label): Array[Metric] = {
    input.filter(m => m.label == label)
  }
}
