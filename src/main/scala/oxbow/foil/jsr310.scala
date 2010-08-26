package oxbow.foil

import java.util.concurrent.TimeUnit
import javax.time.{Duration, Instant}
import javax.time.calendar._
import java.util.{TimeZone => TZ}

/**
 * Typeclass instances for the JSR310 javax.time library, as of version 0.6.2. This will enable code lkike this to be written:
 * {{{
 *    schedule(println("Foo")) onceAtNext LocalTime.MIDDAY
 *
 *    val t = Clock.systemDefaultZone.dateTimeToMinute.withHour(15).withMinute(35)
 *    schedule(println("Bar")) startingAt t thenEvery 10.seconds untilNext LocalTime.MIDNIGHT
 * }}}
 *
 */
trait JSR310 {
  private val MaxLong = BigInt(java.lang.Long.MAX_VALUE)

  /**
   * Can override this method if we wish to define a different way of providing zone conversions
   */
  protected[this] def zoneFor(zone : TZ) = TimeZone.of(zone.getID)
  private[this] def zonedClock(zone : TZ) = Clock.system(zoneFor(zone))

  implicit def interval2duration(i : Interval) = Duration.of(i.duration, i.unit)
  implicit def duration2interval(d : Duration) = Interval(d.toMillisLong, TimeUnit.MILLISECONDS)

  implicit val JSR310DateLike = new DateLike[LocalDate] {
    def plus(x: LocalDate, days: Int) = x.plusDays(days)

    def now(zone: TZ) = zonedClock(zone).today()
  }

  implicit val JSR310InstantLike = new InstantLike[Instant] {
    def fromEpochMillis(l: Long) = Instant.ofEpochMillis(l)

    def millisSinceEpoch(x: Instant) = x.toEpochMillisLong

    def now = Clock.systemDefaultZone.instant()

    def delay(x: Instant, other: Instant)(unit: TimeUnit) = {

      def attemptNanos : Option[Interval] = {
        val nanos = new BigInt(other.toEpochNanos.subtract(x.toEpochNanos))
        if (nanos <= MaxLong)
          Some(Interval(nanos.longValue, TimeUnit.NANOSECONDS))
        else
          None
      }

      def fromMillis = Interval(other.toEpochMillisLong - x.toEpochMillisLong, TimeUnit.MILLISECONDS)

      (unit match {
        case TimeUnit.NANOSECONDS | TimeUnit.MICROSECONDS => attemptNanos getOrElse fromMillis
        case _                                            => fromMillis
      }) as unit
    }

    def plus(x: Instant, interval: Interval) = x.plus(interval.duration, interval.unit)
  }

  implicit val JSR310TimeLike = new TimeLike[LocalTime, LocalDate, Instant] {
    def instant(t: LocalTime, d: LocalDate, zone: TZ) = d.atTime(t).atZone(zoneFor(zone)).toInstant

    def now(zone: TZ) = zonedClock(zone).time

    def compare(x: LocalTime, y: LocalTime) = x compareTo y
  }

  implicit val JSR310ZonedTimeLike = new ZonedTimeLike[OffsetTime, LocalDate, Instant] {
    def instant(t: OffsetTime, d: LocalDate, zone: TZ) = JSR310TimeLike.instant(t.toLocalTime, d, zone)

    def now(zone: TZ) = zonedClock(zone).offsetTime

    def compare(x: OffsetTime, y: OffsetTime) = x.toLocalTime compareTo y.toLocalTime

    //MAY NOT WORK!
    def zone(z: OffsetTime) = TZ.getTimeZone(z.getOffset.getID)
  }
}
