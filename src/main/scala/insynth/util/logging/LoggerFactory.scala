package insynth.util.logging

import scala.collection.mutable.{ Map => MutableMap }
import com.typesafe.scalalogging.log4j.Logger
import org.apache.logging.log4j.LogManager

/** 
 * Factory for producing loggers
 */
object LoggerFactory {
  
  val logDirectory = "log"
  
  val loggerMap: MutableMap[String, Logger] = MutableMap.empty
  
  final def newLogger(className: String) =
//	if (className.contains("insynth.reconstruction.stream.DebugOrderedStreamFactory"))
	  Logger(LogManager.getLogger(className))
//    else
//    if (className.contains("insynth.util.streams"))
//    	(Filter.Finest, new SimpleFormatter(className) with ConsoleLogger)
//  	else
//    if (className.contains("Transformer2"))
//    	(Filter.Fine, new SimpleFormatter(className) with ConsoleLogger)
//  	else
//    if (className.contains("Extractor"))
//    	(Filter.Fine, new SimpleFormatter(className) with ConsoleLogger)
//  	else
//    if (className.contains("insynth"))
//    	(Filter.Info, new SimpleFormatter(className) with ConsoleLogger)
//  	else
//    if (className.contains("insynth.InSynth"))
//    	(Filter.Finest, new SimpleFormatter(className) with ConsoleLogger)
//  	else
//	if (className.contains("lesynth.Synthesizer"))
//    	(Filter.Fine, new SimpleFormatter(className) with ConsoleLogger)
//    else
//    if (className.contains("lesynth.RelaxedVerifier"))
//    	(Filter.Finest, new SimpleFormatter(className) with ConsoleLogger)
//    else 
//    	(Filter.Off, new SimpleFormatter(className) with ConsoleLogger)

}
