package leon
package scife
package wolfram

import scala.util.Random

import org.scalatest._

class QuerierTest extends FunSuite {
  
  val appid = "QVKRYR-QJATYL67J4"
  
  val querier = new Querier(appid)

//  test("reduce") {
//    
//    val query = "Reduce[v >= s && v <= e && x >= s && x <= e && x *2  < v, {v, x}, Integers]"
//    
//    querier.request(query)
//    
//  }

}
