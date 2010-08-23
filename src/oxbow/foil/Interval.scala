package oxbow.foil
import java.util.concurrent.TimeUnit

object Interval {
  def apply(duration : Long, unit : TimeUnit) = new Interval(duration, unit)
}

/**
 * The interval class represents a similar concept as the JSR310 Duration class: it is a length of time in a given
 * TimeUnit. Implicit conversions mean that instances can be created directly from Ints and Longs:
 * {{{
 *  2.minutes
 *  234.nanos
 *  //etc
 * }}}
 * The class is used in conjunction with the foil typeclasses and can affect Instants:
 * {{{
 *   25.minutes before myInstant
 *   24.hours.inTheFuture
 *   //etc
 * }}}
 * The class also contains utilities for performing certain familiar operations that are more normally restricted to taking
 * a Long MILLISECOND value:
 * {{{
 *    20.seconds.sleep()
 *    1.minutes.waitOn(aMutex)
 *    2.micros.joinTo(aThread)
 * }}}
 * We can also use its conversions between units, either to get another duration, or a raw Long value:
 * {{{
 *    val durLong = 2.minutes.toMicros
 *    val ivlNanos = 20.millis.asNanos
 * }}}
 * You can also perform simple multiplicative arithmetic on an interval, perhaps for backing off in a retry loop@
 * {{{
 * val ivl = 5.seconds
 * //DO 10 attempts, INCREASING WAIT LINEARLY
 * (1 to 10) foreach { n => try { attempt } catch { case _ => (ivl * n).sleep() } }
 * }}}
 */
class Interval(val duration : Long, val unit : TimeUnit) {
  def unary_- = Interval(-duration, unit)

  def *(factor : Long) = new Interval(factor * duration, unit)

  def as(otherUnit : TimeUnit) = Interval(otherUnit.convert(duration, unit), otherUnit)
  def asNanos = as(TimeUnit.NANOSECONDS)
  def asMicros = as(TimeUnit.MICROSECONDS)
  def asMillis = as(TimeUnit.MILLISECONDS)
  def asSeconds = as(TimeUnit.SECONDS)
  def asMinutes = as(TimeUnit.MINUTES)
  def asHours = as(TimeUnit.HOURS)
  def asDays = as(TimeUnit.DAYS)

  def toNanos = unit toNanos duration
  def toMicros = unit toMicros duration
  def toMillis = unit toMillis duration
  def toSeconds = unit toSeconds duration
  def toMinutes = unit toMinutes duration
  def toHours = unit toHours duration
  def toDays = unit toDays duration

  def earlierThan[I : InstantLike](i : I) : I = implicitly[InstantLike[I]].minus(i, this)
  def before[I : InstantLike](i : I) : I = earlierThan(i)
  def ago[I : InstantLike] : I = earlierThan(implicitly[InstantLike[I]].now)

  def laterThan[I : InstantLike](i : I) : I = implicitly[InstantLike[I]].plus(i, this)
  def after[I : InstantLike](i : I) : I = laterThan(i)
  def inTheFuture[I : InstantLike] : I = laterThan(implicitly[InstantLike[I]].now)

  @throws(classOf[InterruptedException])
  def sleep() = unit sleep duration

  import util.control.Exception._
  /**
   *   Sleep for the duration of this interval allowing the catcher to define the control structure
   */
  def sleep(catcher : Catch[Unit]) : Unit  = catcher apply sleep()

  /**
   * Wait on the reference for the duration of this interval. Will throw IllegalMonitorStateException if the lock is
   * not held
   */
  @throws(classOf[InterruptedException])
  def waitOn(any : AnyRef) : Unit  = unit.timedWait(any, duration)

  /**
   * Wait on the reference for the duration of this interval allowing the catcher to define control structure.
   */
  def waitOn(any : AnyRef, catcher : Catch[Unit]) : Unit = catcher apply waitOn(any)

  /**
   * Join to the given thread for the duration of this interval
   */
  @throws(classOf[InterruptedException])
  def joinTo(t : Thread) : Unit = unit.timedJoin(t, duration)

  /**
   * Join to the given thread for the duration of this interval allowing the control to be defined by the catcher
   */
  def joinTo(t : Thread, catcher : Catch[Unit]) : Unit  = catcher apply joinTo(t)

}

trait Intervals {
  implicit def int2intervalfactory(i : Int) = new RichLong(i.toLong)
  implicit def long2intervalfactory(l : Long) = new RichLong(l)

  class RichLong(l : Long) {
    def nanos = Interval(l, TimeUnit.NANOSECONDS)
    def micros = Interval(l, TimeUnit.MICROSECONDS)
    def millis = Interval(l, TimeUnit.MILLISECONDS)
    def seconds = Interval(l, TimeUnit.SECONDS)
    def minutes = Interval(l, TimeUnit.MINUTES)
    def hours = Interval(l, TimeUnit.HOURS)
    def days = Interval(l, TimeUnit.DAYS)
  }

}