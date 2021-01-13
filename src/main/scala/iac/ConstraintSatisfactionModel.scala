package iac

import breeze.linalg._

trait IANode
trait IAEdge
case class BaseNode(name: String, activation: Double = 0.1) extends IANode

trait InteractiveActivation[Node,Edge] {
  val DEFAULT = 0.01
  val decay : Double  = 0.3
  val min : Double = -1.0
  val max : Double = 1.0
  val rest : Double = 0.0
  val maxIterations : Int = 200
  var N : Int
  var delta : Double = 0.02
  val deltaThreshold = 0.01
  var r : scala.util.Random = scala.util.Random
  type InputVector = DenseVector[Double]
  type ActivationVector = DenseVector[Double]
  type ConstraintMatrix = DenseMatrix[Double]
  var activations: ActivationVector
  var initialState: ActivationVector
  var constraintMatrix : ConstraintMatrix
  def reset() : Unit
  def update : Int => ActivationVector
  def computeActivations() :  ActivationVector
  def simulate() : Unit
}


abstract class ConstraintSatisfactionModel extends InteractiveActivation[IANode,IAEdge] {
  var narrative : Narrative

  override def reset(): Unit = {
    (0 until activations.length)
      .foreach(i => activations(i) = initialState(i))
  }

  override def update : Int => ActivationVector = {j => {
    var net = (constraintMatrix(::, j)  *:* activations) reduce (_ + _)
    var adelta : Double = 0.0
    if (net > 0) {
      adelta = (max - activations(j))*net - (decay*(activations(j)-rest))
      activations(j)=Math.min(1,activations(j)+adelta)
    }
    else {
      adelta = (activations(j)-min)*net - (decay*(activations(j)-rest))
      activations(j) = Math.max(-1, activations(j)+adelta)
    }
    activations
  }}

  override def computeActivations() : ActivationVector = {
    val A = shuffle(DenseVector.tabulate[Int](N){i => i})
    (0 until activations.length)
      .foreach(i => if (!narrative.nodes(A(i)).isInstanceOf[Evidence]) update(A(i)))
    activations
  }

  def computeDelta(old : DenseVector[Double]) : Unit = {
    delta = Math.abs(activations(0)-old(0))
    for(i <- 1 until activations.length) {
      delta = Math.max(delta,Math.abs(activations(i)-old(i)))
    }
  }

  override def simulate(): Unit = {
    var iteration : Int = 0
    var old = DenseVector.tabulate[Double](N) {i => activations(i)}
    while ((iteration < maxIterations) && (delta > deltaThreshold)) {
      computeActivations()
      computeDelta(old)
      old = DenseVector.tabulate[Double](N) {i => activations(i)}
      iteration = iteration + 1
    }
  }

}