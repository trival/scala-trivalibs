package trivalibs.utils.random
import scala.scalajs.js

private inline def random: Double =
  js.Math.random()

def rand(): Double = random

def randInRange(min: Double, max: Double): Double =
  random * (max - min) + min
