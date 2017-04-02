package leon.utils.logging

import org.apache.logging.log4j.{ Logger => Logger4J, Level }

class ScalaLog4jLogger(logger: Logger4J) extends Logger {

  protected[this] def getMyClass = this.getClass
  
  def log(msg: => String, lvl: Level) =
    if (logger.isEnabled(lvl)) logger.log(lvl, msg)
    
  import Level._
    
  def severe(msg: => String)(implicit hasLogger: HasLogger) =
    log(msg, FATAL)
//    logger.error("FATAL:\n" + msg)

  def error(msg: => String)(implicit hasLogger: HasLogger) = log(msg, ERROR)
  
  def warning(msg: => String)(implicit hasLogger: HasLogger) = log(msg, WARN)  
     
  def info(msg: => String)(implicit hasLogger: HasLogger) = log(msg, INFO)
   
  def fine(msg: => String)(implicit hasLogger: HasLogger) = log(msg, DEBUG)
   
  def finer(msg: => String)(implicit hasLogger: HasLogger)  = log(msg, DEBUG)
   
  def finest(msg: => String)(implicit hasLogger: HasLogger) = log(msg, TRACE)
     
  def entering(method: => String, arguments: Any*)(implicit hasLogger: HasLogger) = {
    if (logger.isEnabled(TRACE)) {
      val argumentStrings = arguments map (_.toString)
      if ((argumentStrings map (_.size) sum) <= 80)
    	  log("Entering " + method + " with " + arguments.mkString(", "), TRACE)
  	  else
    	  log("Entering " + method + " with:\n" + arguments.mkString("\n"), TRACE)
    }
  }
     
  def exiting(method: => String, result: => String)(implicit hasLogger: HasLogger) =
	  log("Exiting " + method + " with " + result, TRACE)

}