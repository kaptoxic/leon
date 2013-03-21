package lesynth

import leon.purescala.Definitions.{ FunDef, VarDecl, Program, ObjectDef }

trait Report {
  def summaryString: String
  
  def isSuccess: Boolean
}

class EmptyReport extends Report {
  import Report._
  
  implicit val width = 70
  
  override def summaryString =
    infoHeader +
    ("║ %-" + width + "s ║\n").format("No solution found for this synthesis problem.") +
    infoFooter
    
  override def isSuccess = false
}

class FullReport(val function: FunDef, val totalTime: Long/*, innerVerificationReport */) extends Report {

  import Report._
  
  implicit val width = 70
  
  override def summaryString : String = {
    infoHeader +
    function.toString.split("\n").map {
    	("║ %-" + (width - 2) + "s ║\n").format(_)
    }.mkString +
    infoSep +
    ("║ Total time: %" + (width - 15) + ".2fs ║\n").format(totalTime.toDouble/1000) +
    infoFooter
  }

  override def isSuccess = true
}

object Report {
  //def emptyReport : Report = new Report(Nil)

  def fit(str : String, maxLength : Int) : String = {
    if(str.length <= maxLength) {
      str
    } else {
      str.substring(0, maxLength - 1) + "…"
    }
  }

  def infoSep(implicit width: Int)    : String = "╟" + ("┄" * width) + "╢\n"
  def infoFooter(implicit width: Int) : String = "╚" + ("═" * width) + "╝"
  def infoHeader(implicit width: Int) : String = ". ┌─────────┐\n" +
                                    "╔═╡ Summary ╞" + ("═" * (width - 12)) + "╗\n" +
                                    "║ └─────────┘" + (" " * (width - 12)) + "║\n"

}
