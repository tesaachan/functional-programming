package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal {
      math.pow(b(), 2) - 4 * a() * c()
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      delta() match
        case d if d < 0 => Set()
        case _          => 
          val negativeValueOfB = Signal(-b())
          val doubledValueOfA = Signal(2 * a())
          val sqrtOfDelta = Signal(math.sqrt(delta()))
          Set ((negativeValueOfB() + sqrtOfDelta()) / doubledValueOfA(),
                (negativeValueOfB() - sqrtOfDelta()) / doubledValueOfA())
    }
