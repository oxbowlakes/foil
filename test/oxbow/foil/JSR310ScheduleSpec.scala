package oxbow.foil

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import javax.time.calendar.{ZoneOffset, TimeZone, Clock}

class JSR310ScheduleSpec extends Spec with ShouldMatchers {
  describe("Schedule") {
    it("should be able to do stuff") {

      object X extends Schedules with Intervals with JSR310 with FixedRateStrategy
      import X._

      val i = Clock.system(TimeZone.of(ZoneOffset.ofHours(1))).offsetTime.plusMinutes(1)
      schedule(println("Foo")) onceAtNextZoned i
      ()
    }
  }
}