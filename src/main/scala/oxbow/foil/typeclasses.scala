package oxbow.foil

import java.util.TimeZone


/**
 * In order to integrate a datetime library with this scheduling DSL, it suffices only to supply three typeclass
 * implementations, DateLike, TimeLike and InstantLike:
 *
 *  - A DateLike is supposed to represent a year-month-day with no timezone
 *  - A InstantLike is supposed to represent an immutable Instant in time (zone-agnostic)
 *  - A TimeLike is supposed to represent a time of day
 */
trait DateLike[X] {

  /**
   * Return the date instance which represents "now" in the given time zone
   */
  def now(zone : TimeZone = TimeZone.getDefault) : X

  /**
   * Return the date instance which represents the number of days _after_ x
   */
  def plus(x : X, days : Int) : X
  def minus(x : X, days : Int) : X = plus(x, -days)
}

import java.util.concurrent.TimeUnit
import java.util.{Calendar, Date}

/**
 * An InstantLike is a typeclass for a store of _Instant_, which is a zone-agnostic instant-in-time
 */
trait InstantLike[X] {
  /**
   * Return the instant which represents the given interval _after_ x
   */
  def plus(x : X, interval : Interval) : X
  def minus(x : X, interval : Interval) : X = plus(x, -interval)

  /**
   * Return the delay (interval) which, when added to x would give y:
   * {{{
   *  plus(x, delay(x, y)) == y
   * }}}
   *
   */
  def delay(x: X, y : X)(unit : TimeUnit) : Interval

  /**
   * Return the instant which represents "now"
   */
  def now : X

  /**
   * Return the supplied instant as the standard _epoch millis_
   */
  def millisSinceEpoch(x : X) : Long
  /**
   * Construct instant from the standard _epoch millis_
   */
  def fromEpochMillis(l : Long) : X
}

/**
 * A TimeLike is a typeclass for a store of _Time Of Day_, which is a zone-agnostic time of day. An Instant is parametrized
 * on all 3 types, for Instant, Date and Time
 */
trait TimeLike[T, D, I] {
  /**
   * Return -1 if x is earlier than y, 0 if they represent the same time and +1 if x is after y
   */
  def compare(x : T, y : T) : Int

  /**
   * Return a time which represents now in the supplied time zone
   */
  def now(zone : TimeZone = TimeZone.getDefault ) : T

  /**
   * Return the Instant which represents the soonest instant with the given time of day in the supplied time zone
   */
  def next(t : T, zone : TimeZone = TimeZone.getDefault) : I

  /**
   * Return the instant which represents the given time of day on the given date in the given zone
   */
  def instant(t : T, d : D, zone : TimeZone = TimeZone.getDefault) : I
}

trait ZonedTimeLike[Z, D, I] extends TimeLike[Z, D, I] {
  def zone(z : Z) : TimeZone

  def nowZoned(z : Z) = now(zone(z))

  def nextZoned(z : Z) = next(z, zone(z))

  def instantZoned(z : Z, d : D) = instant(z, d, zone(z))

}
