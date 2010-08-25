package oxbow.foil

import java.util.concurrent.TimeUnit
import oxbow.saturn.TimeOfDay
import java.util.TimeZone

trait Saturn extends Intervals {
  import oxbow.saturn.{Date, Instant}
  implicit val SaturnInstantLike = new InstantLike[Instant] {
    def fromEpochMillis(l: Long) = Instant(l)

    def millisSinceEpoch(x: Instant) = x.millis

    def now = Instant.systemTime

    def delay(x: Instant, y: Instant)(unit: TimeUnit) = Interval((x delayTo y)(unit), unit)

    def plus(x: Instant, interval: Interval) = x + (interval.duration, interval.unit)
  }

  implicit val SaturnDateLike = new DateLike[Date] {

    import oxbow.saturn.Date

    def plus(x: Date, days: Int) = x plus days

    def now(zone: TimeZone) = Date.systemDate(zone)

  }

  implicit val SaturnTimeLike = new TimeLike[TimeOfDay, Date, Instant] {

    def compare(x : TimeOfDay, y : TimeOfDay) : Int = x compare y

    def now(zone: TimeZone) = TimeOfDay.systemTime(zone)

    def instant(x: TimeOfDay, d: Date, zone : TimeZone) = d.toInstant(x)(zone)

    def next(t: TimeOfDay, zone: TimeZone) = t nextIn zone

  }
}