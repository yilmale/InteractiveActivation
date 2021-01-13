import iac._
import iac.Declarations._
import scalAgent._

object IACMain extends App {
  println("Test....")

  var n = Narrative {
    evidence("e", 0.2)
    evidence("e1", 1.0)
    belief("a", 0.5)
    belief("b", 0.2)
    belief("c", 0.4)
  } subjectTo {
    explain("b", "e", 0.5)
    explain(List("a", "c"), "e1", 0.5)
    contradict("a", "b", 1)
  }

  println(n.activations)
  println(n.constraintMatrix)

  n.simulate()
  println(n.activations)

  println("---------------------")

  IACDSLTest()

  IACUtilities()

}
