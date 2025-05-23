#!/usr/bin/env amm

import scala.util.Random

object ProbSim {
  // Model a single trial
  case class Trial(backet: List[Int] = List(1, 1, 1, 0, 0, 0)) {
    def drawBalls: Boolean = 
      Random.shuffle(backet)
        .take(2)
        .exists(_ == 1)
  }

  def runSimulation(numTrials: Int = 10000): Unit = {
    
    // Create and run all trials
    val results = (1 to numTrials).map(_ => Trial().drawBalls)
    
    // Calculate probability
    val successCount = results.foldLeft(0) {
      case (count, true) => count + 1
      case (count, false) => count
    }
    
    // Alternative count using filter+size
    // val successCount = results.filter(identity).size
    
    val probability = successCount.toDouble / numTrials
    
    // Analytical solution
    val analytical = 0.8
    
    // Format output
    List(
      s"Experimental: $probability",
      s"Analytical:  $analytical",
      s"Difference:  ${math.abs(probability - analytical)}"
    ).foreach(println)
  }
}

ProbSim.runSimulation()
