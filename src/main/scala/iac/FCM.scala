package iac

import breeze.linalg._

trait Concept {
  var name: String
  var activation: Double
}

trait Edge {
  var src: Concept
  var tgt: Concept
  var weight: Double
}

trait FCM {
  type InputVector = DenseVector[Double]
  type ActivationVector = DenseVector[Double]
  type ConstraintMatrix = DenseMatrix[Double]
  val DEFAULT = 0.01
  val maxIterations : Int = 200
  var N : Int
  var delta : Double = 0.02
  val deltaThreshold = 0.01
  var externalInput: InputVector
  var activations: ActivationVector
  var initialState: ActivationVector
  var updatedActivations : ActivationVector
  var constraintMatrix : ConstraintMatrix
  def reset() : Unit
  def thresholdFunction : Double => Double
  def update : Int => ActivationVector
  def computeActivations() :  ActivationVector
  def simulate() : Unit
}

abstract class FCMBase extends FCM {

  def reset(): Unit = {
    (0 until activations.length)
      .foreach(i => activations(i) = initialState(i))
  }

  def report: Unit = {
    println(activations)
  }

  def thresholdFunction: Double => Double = {a => if (a <= 0.0) 0.0 else 1.0}

  def update : Int => ActivationVector = {j => {
    var net = (constraintMatrix(::, j)  *:* activations) reduce (_ + _)
    net = net + externalInput(j)
    updatedActivations(j) = thresholdFunction(net)
    updatedActivations
  }}

  def computeActivations() : ActivationVector = {
    val A = shuffle(DenseVector.tabulate[Int](N){i => i})
    (0 until activations.length)
      .foreach(i => update(A(i)))
    activations
  }

  def computeDelta() : Unit = {
    delta = Math.abs(updatedActivations(0)-activations(0))
    for(i <- 1 until activations.length) {
      delta = Math.max(delta,Math.abs(updatedActivations(i)-activations(i)))
    }
  }

  def synchronousUpdate: Unit = {
    for(i <- 0 until activations.length)
      activations(i) = updatedActivations(i)
  }

  def simulate(): Unit = {
    var iteration : Int = 0
    while ((iteration < maxIterations) && (delta > deltaThreshold)) {
      computeActivations()
      computeDelta()
      synchronousUpdate
      iteration = iteration + 1
      report
    }
  }
}


object FCMapTest {
  def apply(): Unit = {
    var fcm = new FCMBase {
      var N = 5
      var externalInput : InputVector = DenseVector(0,0,0,10,0)
      var initialState : ActivationVector = DenseVector(0,0,0,1,0)
      var activations : ActivationVector = DenseVector(0,0,0,1,0)
      var updatedActivations : ActivationVector = DenseVector(0,0,0,1,0)
      var constraintMatrix : ConstraintMatrix =
        DenseMatrix(
          (0.0,1.0,0.0,-1.0,0.0),
          (0.0,0.0,1.0,0.0,-1.0),
          (0.0,-1.0,0.0,1.0,-1.0),
          (1.0,0.0,-1.0,0.0,1.0),
          (-1.0,1.0,0.0,-1.0,0.0))
    }

    fcm.report
    println("Simulating.....")
    fcm.simulate()
    println("Results:")
    fcm.report

  }
}