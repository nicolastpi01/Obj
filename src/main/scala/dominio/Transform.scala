package dominio

object Transform {

  /// METODOS DE TRANSFORMACIÓN DE FIGURAS

  def trasladar[T <: Figura](x: Int, y: Int) (figura: T) :T = {
    val result = figura match {
      case c @ Circulo(xPos, yPos, _) => c.copy(xPos+x, yPos+y)
      case r @ Rectangulo(xPos, yPos, _, _) => r.copy(xPos+x, yPos+y)
      case l @ Linea(xPos, yPos, _, _) => l.copy(xPos+x, yPos+y)
    }
    result.asInstanceOf[T]
  }


  def mover[T <: Figura](x: Int, y: Int) (figura: T): T = {
    val result = figura match {
      case c : Circulo => c.copy(x,y)
      case r : Rectangulo => r.copy(x,y)
      case l : Linea => l.copy(x,y)
    }
    result.asInstanceOf[T]
  }


  def escalar[T <: Figura] (n: Double) (figura: T) : T = {
    val result = figura match {
      case c @ Circulo(_, _, radio) => c.copy(radio = n*radio)
      case r @ Rectangulo(_, _, ancho, alto) => r.copy(ancho = ancho*n, alto = alto*n)
      case l @ Linea(_, _, otroX, otroY) => l.copy(otroX = otroX*n, otroY = otroY*n)
    }
    result.asInstanceOf[T]
  }

                    /// TRANFORMACIONES ÚTILES

  def moverX[T <: Figura](x :Int) (figura: T) :T = mover(x, figura.y) (figura)

  def moverOrigen[T <: Figura](figura: T) :T = mover(0,0) (figura)

  def moverY[T <: Figura](y :Int) (figura: T) :T = mover(figura.x, y) (figura)

  def trasladarXeY[T <: Figura](n :Int) (figura: T) :T = trasladar(n,n) (figura)

  def duplicar[T <: Figura] (figura: T) :T = escalar(2) (figura)

  def cuadruplicar[T <: Figura] (figura: T) :T = duplicar(duplicar (figura))

  def doble[T <: Figura] (f: T => T): T => T = f compose f

}
