package oxbow.foil

import java.util.concurrent.{ScheduledFuture, TimeUnit}
import java.util.TimeZone

/**
 * Mixing in this trait will give the user scheduling functionality. For example:
 * {{{
 *    schedule(println("Hello") startingIn 2.minutes thenEvery 5.seconds untilNext Midnight
 * }}}
 */
trait Schedules {
  /**
   * Return a schedule for the given "thunk"
   */
  def schedule(f : => Unit) = new Schedule(f)

  implicit def scheduledfuture2limitedfuture(s : ScheduledFuture[_]) = new LimitedScheduledFuture(s)
}

/**
 * A limited scheduled future is a pimped class which allows us to control execution of some periodic schedule which we
 * may wish to cancel at some later point
 */
class LimitedScheduledFuture(self : ScheduledFuture[_]) extends Schedules with Intervals {

  /**
   * Return the start time of the future in the Instant class defined by the library user. For example, if you prefer to
   * use the Joda library:
   * {{{
   *   val starts : Option[org.joda.time.Instant] = fut.startTime
   * }}}
   * Returns None if the invocation has already begun
   */
  def startTime[I : InstantLike] : Option[I] = self.getDelay(TimeUnit.MILLISECONDS) match {
    case i if i > 0   => Some(i.millis after implicitly[InstantLike[I]].now)
    case _            => None
  }

  /**
   * Return the delay until the first execution of this future as an Interval
   */
  def delay(unit : TimeUnit) : Option[Interval] = self.getDelay(unit) match {
    case i if i > 0 => Some(Interval(i, unit))
    case _          => None
  }

  /**
   * Cancel this future at the next instant in the supplied time zone at the supplied time. For example:
   * {{{
   *    schedule(println("Foo")) immediatelyThenEvery 5.seconds untilNext Midnight
   * }}}
   */
  def untilNext[I,T](t : T, zone : TimeZone = TimeZone.getDefault)(implicit ev : TimeLike[T,_,I], ev2 : InstantLike[I], strategy : Scheduler) = until(ev.next(t, zone))

  /**
   * Cancel this future at the next instant which represents the zoned time. For example:
   * {{{
   *    val midnight = OffsetTime.of(0, 0, ZoneOffset.of(2))
   *    schedule(println("Foo")) immediatelyThenEvery 5.seconds untilNextZoned midnight
   * }}}
   */
  def untilNextZoned[I,Z](t : Z)(implicit ev : ZonedTimeLike[Z,_,I], ev2 : InstantLike[I], strategy : Scheduler) = untilNext(t, ev.zone(t))

  /**
   * Cancel this future at the supplied instant. For example:
   * {{{
   *    val midnight = Instant.next(Midnight, zone)
   *    schedule(println("Foo")) immediatelyThenEvery 5.seconds until midnight
   * }}}
   */
  def until[I](i : I)(implicit ev : InstantLike[I], strategy : Scheduler) : ScheduledFuture[_] = {
    schedule(self.cancel(false)) onceAt i
    self
  }


  /**
   * Cancel this future after the supplied interval. For example:
   * {{{
   *    schedule(println("Foo")) immediatelyThenEvery 5.seconds forTheNext 2.minutes
   * }}}
   */
  def forTheNext[I](interval : Interval)(implicit ev : InstantLike[I], strategy : Scheduler) : ScheduledFuture[_] = {
    import math._
    val delayToStart = delay(interval.unit).getOrElse(Interval(0L, interval.unit))
    until(interval after (delayToStart after ev.now))
  }
}

/**
 * The scheduler strategy can be provided by the user of the framework at the point where a strategy for executing a
 * function is required
 *
 */
trait Scheduler {
  /**
   * Execute the work immediately
   */
  def execute(f : => Unit)

  /**
   * Execute the work with a given delay
   */
  def schedule(f : => Unit, delay : Long, unit : TimeUnit) : ScheduledFuture[_]

  /**
   * Execute the work with a given delay and period
   */
  def schedule(f : => Unit, delay : Long, period : Long, unit : TimeUnit) : ScheduledFuture[_]
}

/**
 * A nano scheduler is required if we wish to explicitly use nano-second precision for scheduling
 */
trait NanoScheduler {
  /**
   * Schedule the execution with the given delay in nanos
   */
  def schedule(f : => Unit, delay : Long) : ScheduledFuture[_]
  /**
   * Schedule the execution with the given delay and period in nanos
   */
  def schedule(f : => Unit, delay : Long, period : Long) : ScheduledFuture[_]
}

/**
 * A Schedule represents a holder for a "thunk" which will then be scheduled for execution, possibly immediately, with a delay
 * or even periodic
 */
class Schedule(f : => Unit) extends Intervals {
  import Predef.{implicitly => ??}

  /**
   * Execute this thunk immediately using the supplied strategy
   */
  def now(implicit strategy : Scheduler) = strategy.execute(f)

  def onceAtNext[T,I](t : T, zone : TimeZone = TimeZone.getDefault)(implicit ev : TimeLike[T, _, I], ev2 : InstantLike[I], strategy : Scheduler) : ScheduledFuture[_] = onceAt(ev.next(t, zone))
  def onceAtNextZoned[Z,I](t : Z)(implicit ev : ZonedTimeLike[Z, _, I], ev2 : InstantLike[I], strategy : Scheduler) : ScheduledFuture[_] = onceAtNext(t, ev.zone(t))

  /**
   * Execute this thunk at the supplied instant using the supplied strategy. The type of instant supplied is flexible but
   * there must be a "typeclass conversion" to InstantLike (a context-bound). Example usage:
   * {{{
   *   schedule(println("Hello")) onceAt inst
   * }}}
   */
  def onceAt[I](i : I)(implicit ev : InstantLike[I], strategy : Scheduler) : ScheduledFuture[_] = strategy.schedule(f, ??[InstantLike[I]].delay(??[InstantLike[I]].now, i)(TimeUnit.MILLISECONDS).duration, TimeUnit.MILLISECONDS)

  /**
   * Execute this thunk after the supplied interval using the supplied strategy. There must be a "typeclass conversion"
   * to InstantLike (a context-bound). Example usage:
   * {{{
   *   schedule(println("Hello")) onceAfter 10.minutes
   * }}}
   */
  def onceIn[I](interval : Interval)(implicit ev : InstantLike[I], strategy : Scheduler) : ScheduledFuture[_] = onceAt(interval after ??[InstantLike[I]].now)

  /**
   * Return a repeating (periodic) schedule whereby the first invocation will occur immediately. Nothing will actually be
   * executed upon return from this method. See RepeatingSchedule
   */
  def immediately[I : InstantLike] = new RepeatingSchedule[I] {
    protected def start = None

    protected def fun = () => f
  }

  /**
   * Equivalent to {{immediately thenEvery interval}}
   */
  def immediatelyThenEvery[I](interval : Interval)(implicit ev : InstantLike[I], strategy : Scheduler) : ScheduledFuture[_] = immediately thenEvery interval

  def startingAtNext[T,I](t : T, zone : TimeZone = TimeZone.getDefault)(implicit ev : TimeLike[T,_,I], ev2 : InstantLike[I]) = startingAt(ev.next(t,zone))

  def startingAtNextZoned[Z,I](t : Z)(implicit ev : ZonedTimeLike[Z,_,I], ev2 : InstantLike[I]) = startingAt(ev.next(t, ev.zone(t)))

  /**
   * Return a repeating (periodic) schedule whereby the first invocation will after the given interval. Nothing will actually be
   * executed upon return from this method. See RepeatingSchedule
   */
  def startingIn[I : InstantLike](interval : Interval) = startingAt(interval inTheFuture)

  /**
   * Return a repeating (periodic) schedule whereby the first invocation will occur at the given instant. Nothing will actually be
   * executed upon return from this method. See RepeatingSchedule
   */
  def startingAt[I : InstantLike](i : I) = new RepeatingSchedule[I] {
    protected def start = Some(i)

    protected def fun = () => f
  }

  def dailyAtZoned[Z,I](t : Z, immediatelyIfAfter : Boolean = true)(implicit ev : ZonedTimeLike[Z,_,I], ev2 : InstantLike[I], strategy : Scheduler) : ScheduledFuture[_] = dailyAt(t, ev.zone(t), immediatelyIfAfter)
  /**
   * Execute the thunk daily at the supplied time, running it immediately iff this time has passed today (in the supplied zone)
   * and the immediatelyIfAfter parameter is set to true (default value is true).
   */
  def dailyAt[T, I](t : T, zone : TimeZone = TimeZone.getDefault, immediatelyIfAfter : Boolean = true)(implicit ev : TimeLike[T,_,I], ev2 : InstantLike[I], strategy : Scheduler) : ScheduledFuture[_] = {
    if ((ev.compare(ev.now(zone), t) > 0) && immediatelyIfAfter)
      now

    startingAtNext(t, zone) thenEvery 1.days
  }
}

/**
 * This class represents a repeating, or periodic, scheduled invocation of a "thunk". Invocation will only begin
 * after the period is specified by invoking the appropriate method on this instance
 */
sealed abstract class RepeatingSchedule[I : InstantLike] {
  protected def fun : () => Unit
  protected def start : Option[I]

  /**
   * Subsequent invocations of this schedule should occur with the given interval. The returned ScheduledFuture is the
   * scheduled future representing the invocation and can be cancelled
   */
  def thenEvery(interval : Interval)(implicit strategy : Scheduler) : ScheduledFuture[_] = {
    val i = implicitly[InstantLike[I]]
    strategy.schedule(fun(), start.map(s => i.delay(i.now, s)(TimeUnit.MILLISECONDS).toMillis).getOrElse(0L), interval.toMillis, TimeUnit.MILLISECONDS)
  }

  /**
   * Equivalent to thenEvery
   */
  def withPeriod(interval: Interval)(implicit strategy : Scheduler) = thenEvery(interval)

  /**
   * Subsequent invocations of this schedule should occur with the given interval in nanos. The returned ScheduledFuture is the
   * scheduled future representing the invocation and can be cancelled
   */
  def thenEveryNanos(nanos : Long)(implicit strategy : NanoScheduler) : ScheduledFuture[_] = {
    val i = implicitly[InstantLike[I]]
    strategy.schedule(fun(), start.map(s => i.delay(i.now, s)(TimeUnit.NANOSECONDS).toNanos).getOrElse(0L), nanos)
  }
}

