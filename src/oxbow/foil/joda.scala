package oxbow.foil

import java.util.TimeZone
import java.util.concurrent.TimeUnit
import org.joda.time.base.AbstractDuration
import org.joda.time._

/**
 * Mixing in the JODA typeclass instances will allow you to seamlessly interop between the *foil* scheduling DSL and JODA.
 * For example:
 * {{{
 *     schedule(println("foo")) onceAt (new LocalTime(13, 0))
 *     schedule(println("foo")) onceIn (new Duration(5000)) //equivalent to
 *     schedule(println("foo")) onceIn 5.seconds
 *
 *     val midnight = new LocalTime(0, 0).toDateTimeToday.toInstant
 *     schedule(println("Bar")) startingIn 2.hours thenEvery 5.minutes until midnight
 * }}}
 */

/**
 * This is a lower-priority implicit conversion for use by those who are using the Joda YearMonthDay and TimeOfDay
 * classes
 */
trait JodaLowPriority {
  implicit val JodaYMDDateLike = new DateLike[YearMonthDay] {
    def plus(x: YearMonthDay, days: Int) = x.plusDays(days)

    def now(zone: TimeZone) = new YearMonthDay(DateTimeZone.forTimeZone(zone))
  }

  implicit val JodaTODTimeLike = new TimeLike[TimeOfDay, YearMonthDay, Instant] {
    def instant(t: TimeOfDay, d: YearMonthDay, zone: TimeZone) = t.toDateTimeToday(DateTimeZone.forTimeZone(zone)).withDate(d.getYear, d.getMonthOfYear, d.getDayOfMonth).toInstant

    def next(t: TimeOfDay, zone: TimeZone) = {
      val now = new TimeOfDay(DateTimeZone.forTimeZone(zone))
      (if (now.compareTo(t) < 0)
        t.toDateTimeToday(DateTimeZone.forTimeZone(zone))
      else
        t.toDateTimeToday(DateTimeZone.forTimeZone(zone)).plusDays(1)).toInstant
    }

    def now(zone: TimeZone) = new TimeOfDay(DateTimeZone.forTimeZone(zone))

    def compare(x: TimeOfDay, y: TimeOfDay) = x compareTo y
  }
}

/**
 * Mixin this trait and take advantage of typeclass instances for the JODA date time library
 */
trait Joda16 extends Intervals with JodaLowPriority {

  implicit def duration2interval(d : AbstractDuration) = d.toDuration.getMillis.millis
  implicit def interval2duration(i : Interval) = new Duration(i.toMillis)

  implicit val JodaDateLike = new DateLike[LocalDate] {
    def plus(x: LocalDate, days: Int) = x.plusDays(days)

    def now(zone: TimeZone) = new LocalDate(DateTimeZone.forTimeZone(zone))
  }

  implicit val JodaInstantLike = new InstantLike[Instant] {
    def fromEpochMillis(l: Long) = new Instant(l)

    def millisSinceEpoch(x: Instant) = x.getMillis

    def now = new Instant

    def delay(x: Instant, other: Instant)(unit: TimeUnit) = Interval(unit.convert(other.getMillis - x.getMillis, TimeUnit.MILLISECONDS), unit)

    def plus(x: Instant, interval: Interval) = x.plus(interval.toMillis)
  }

  implicit val JodaTimeLike = new TimeLike[LocalTime, LocalDate, Instant] {
    def instant(t: LocalTime, d: LocalDate, zone: TimeZone) = t.toDateTimeToday(DateTimeZone.forTimeZone(zone)).withDate(d.getYear, d.getMonthOfYear, d.getDayOfMonth).toInstant

    def next(t: LocalTime, zone: TimeZone) = {
      val now = new LocalTime(DateTimeZone.forTimeZone(zone))
      (if (now.compareTo(t) < 0)
        t.toDateTimeToday(DateTimeZone.forTimeZone(zone))
      else
        t.toDateTimeToday(DateTimeZone.forTimeZone(zone)).plusDays(1)).toInstant
    }

    def now(zone: TimeZone) = new LocalTime(DateTimeZone.forTimeZone(zone))

    def compare(x: LocalTime, y: LocalTime) = x compareTo y
  }
}