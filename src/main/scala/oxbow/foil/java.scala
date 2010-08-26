package oxbow.foil

import java.util.concurrent.TimeUnit
import compat.Platform
import java.util.TimeZone

trait JavaUtilCalendarFields {
  import java.util.Calendar
  object fields extends (TimeUnit => Int) {
    def apply(tu : TimeUnit)  = tu match {
      case TimeUnit.NANOSECONDS => error("Unsupported")
      case TimeUnit.MICROSECONDS => error("Unsupported")
      case TimeUnit.MILLISECONDS => Calendar.MILLISECOND
      case TimeUnit.SECONDS => Calendar.SECOND
      case TimeUnit.MINUTES => Calendar.MINUTE
      case TimeUnit.HOURS => Calendar.HOUR
      case TimeUnit.DAYS => Calendar.DATE
    }
  }
}

trait JavaDates extends JavaUtilCalendarFields with Intervals {
  import java.util.{Date, Calendar}
  implicit val DateInstantLike = new InstantLike[Date] {
    def plus(value : Date, interval: Interval) = {
      val cal = Calendar.getInstance
      cal.setTime(value)
      cal.add(fields(interval.unit), interval.duration.toInt)
      cal.getTime
    }

    def now = new Date

    def delay(value : Date, other : Date)(unit : TimeUnit) = (other.getTime - value.getTime).millis.as(unit)

    def millisSinceEpoch(x: Date) = x.getTime

    def fromEpochMillis(l: Long) = new Date(l)
  }

  @deprecated("Use Calendar if you must use a java.util class!")
  implicit val DateDateLike = new DateLike[Date] {
    import java.util.Calendar
    def plus(x: Date, days: Int) = {
      val cal = Calendar.getInstance
      cal.setTime(x)
      cal.add(Calendar.DATE, days)
      cal.getTime
    }

    def now(zone: TimeZone) = Calendar.getInstance(zone).getTime
  }

  @deprecated("Use Calendar if you must use a java util class - if you are using Date as a holder of time information you have bigger problems!")
  implicit val DateTimeLike = new TimeLike[Date, Date, Date] {
    import java.util.Calendar
    def instant(t: Date, d: Date, zone: TimeZone) = {
      val cal = Calendar.getInstance(zone)
      cal.set(Calendar.HOUR_OF_DAY, t.getHours)
      cal.set(Calendar.MINUTE, t.getMinutes)
      cal.set(Calendar.SECOND, t.getSeconds)
      cal.set(Calendar.MILLISECOND, 0)

      cal.set(Calendar.YEAR, d.getYear)
      cal.set(Calendar.MONTH, d.getMonth)
      cal.set(Calendar.DAY_OF_MONTH, d.getDay)
      cal.getTime
    }

    def next(t: Date, zone: TimeZone) = {
      val now = Calendar.getInstance(zone)
      val date =
        (if (t.getHours > now.get(Calendar.HOUR_OF_DAY) || t.getMinutes > now.get(Calendar.MINUTE) || t.getSeconds > now.get(Calendar.SECOND))
          now
        else
          { now.add(Calendar.DATE, 1) ; now }).getTime
      date.setHours(t.getHours)
      date.setMinutes(t.getMinutes)
      date.setSeconds(t.getSeconds)
      date
    }

    def now(zone: TimeZone) = Calendar.getInstance(zone).getTime

    def compare(x: Date, y: Date) = {
      var cf = x.getHours compare y.getHours
      if (cf == 0) {
        cf = x.getMinutes compare y.getMinutes
        if (cf == 0)
          cf = x.getSeconds compare y.getSeconds
      }
      cf
    }
  }
}

trait JavaCalendars extends JavaUtilCalendarFields with Intervals {
  import java.util.{Calendar, Date}
  implicit val CalendarInstantLike = new InstantLike[Date] {
    def plus(value : Date, interval: Interval) = {
      val cal = Calendar.getInstance
      cal.setTime(value)
      cal.add(fields(interval.unit), interval.duration.toInt)
      cal.getTime
    }

    def now = new Date

    def millisSinceEpoch(x: Date) = x.getTime

    def fromEpochMillis(l: Long) = new Date(l)

    def delay(value : Date, other : Date)(unit : TimeUnit) = (other.getTime - value.getTime).millis.as(unit)

  }

  implicit val CalendarDateLike = new DateLike[Calendar] {
    def plus(x: Calendar, days: Int) = { val cal = x.clone.asInstanceOf[Calendar]; cal.add(Calendar.DATE, days); cal }

    def now(zone: TimeZone) = Calendar.getInstance(zone)
  }

  implicit val CalendarTimeLike = new TimeLike[Calendar, Calendar, Date] {

    def compare(x : Calendar, y: Calendar) : Int = {
      Array(Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND).view map (field => x.get(field).compare(y.get(field))) find (_ != 0) getOrElse(0)
    }

    def now(zone: TimeZone) = Calendar.getInstance(zone)

    def instant(t: Calendar, d: Calendar, zone: TimeZone) = {
      val cal = Calendar.getInstance(zone)
      Array(Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND) foreach (field => cal.set(field, t.get(field)))
      Array(Calendar.YEAR, Calendar.MONTH, Calendar.DAY_OF_MONTH) foreach (field => cal.set(field, d.get(field)))
      cal.getTime
    }

  }
}