package leon.utils.logging

import org.apache.logging.log4j.{ Logger => Logger4J }

class ScalaLog4jLogger(logger: Logger4J) extends Logger {

  protected[this] def getMyClass = this.getClass
    
  def severe(msg: => String)(implicit hasLogger: HasLogger) = logger.error("FATAL:\n" + msg)

  def error(msg: => String)(implicit hasLogger: HasLogger) = logger.error(msg)
  
  def warning(msg: => String)(implicit hasLogger: HasLogger) = logger.warn(msg)  
     
  def info(msg: => String)(implicit hasLogger: HasLogger) = logger.info(msg)
   
  def fine(msg: => String)(implicit hasLogger: HasLogger) = logger.debug(msg)
   
  def finer(msg: => String)(implicit hasLogger: HasLogger)  = logger.debug(msg)
   
  def finest(msg: => String)(implicit hasLogger: HasLogger) = logger.trace(msg)
     
  def entering(method: => String, arguments: Any*)(implicit hasLogger: HasLogger) =
	  logger.trace("Entering " + method + " with " + arguments.mkString(", "))
     
  def exiting(method: => String, result: => String)(implicit hasLogger: HasLogger) =
	  logger.trace("Exiting " + method + " with " + result)

}