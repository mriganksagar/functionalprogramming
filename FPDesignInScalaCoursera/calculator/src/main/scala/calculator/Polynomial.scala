package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal{
        val _b = b()
        _b*_b - (4 * a()*c())
      }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
        val rootd = math.sqrt(delta())
        if rootd == Double.NaN 
          then Set()
        else 
          val _a = a()
          val _b = b()        
          val root1 = (-_b + rootd)/ (2 * _a)
          val root2 = (-_b - rootd)/ (2 * _a)
          Set(root1, root2)
      }
