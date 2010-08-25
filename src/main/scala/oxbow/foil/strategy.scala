package oxbow.foil

import java.util.concurrent.{Executors, TimeUnit}

trait ScalaActorStrategy {
  import scala.actors.{Scheduler => ASched}
  protected lazy val sched = Executors.newSingleThreadScheduledExecutor

  implicit val Strategy = new Scheduler {
    def schedule(f: => Unit, delay: Long, period: Long, unit: TimeUnit) = sched.scheduleAtFixedRate(new Runnable {
      def run = ASched.execute(f)
    }, delay, period, unit)

    def schedule(f: => Unit, delay: Long, unit: TimeUnit) = sched.schedule(new Runnable {
      def run = ASched.execute(f)
    }, delay, unit)

    def execute(f: => Unit) = ASched.execute(f)
  }
}

trait JavaUtilConcurrentStrategy {
  protected lazy val sched = Executors.newSingleThreadScheduledExecutor
  protected[this] implicit def thunk2runnable(f : => Unit) = new Runnable {
    def run = f
  }
  protected[this] def schedule0(f: => Unit, delay: Long, unit: TimeUnit) = sched.schedule(f, delay, unit)

  protected[this] def execute0(f: => Unit) = sched.execute(f)
}

trait FixedDelayStrategy extends JavaUtilConcurrentStrategy{
  implicit val Strategy = new Scheduler {
    def schedule(f: => Unit, delay: Long, period: Long, unit: TimeUnit) = sched.scheduleWithFixedDelay(f, delay, period, unit)

    def schedule(f: => Unit, delay: Long, unit: TimeUnit) = schedule0(f, delay, unit)

    def execute(f: => Unit) = execute0(f)
  }
}
trait FixedRateStrategy extends JavaUtilConcurrentStrategy{
  implicit val Strategy = new Scheduler {
    def schedule(f: => Unit, delay: Long, period: Long, unit: TimeUnit) = sched.scheduleAtFixedRate(f, delay, period, unit)

    def schedule(f: => Unit, delay: Long, unit: TimeUnit) = schedule0(f, delay, unit)

    def execute(f: => Unit) = execute0(f)
  }
}