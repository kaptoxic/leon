package leon
package codegen

/* These parameters are passed along when generated code is evaluated.
 * Be very careful if you change the defaults: the generated code is 
 * instrumented to assume these variables if `null` is passed as a
 * monitor, because this is faster than reading the parameters each time.
 * That means, if you change these defaults, you should go and update
 * `CodeGeneration.scala` as well to reflect these changes.
 */
case class CodeGenEvalParams(
  val checkContracts : Boolean = false,
  val maxFunctionInvocations : Int = -1  // negative means infinity
)

object CodeGenEvalParams {
  def default = CodeGenEvalParams()
}
