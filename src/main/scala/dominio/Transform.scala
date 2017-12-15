package dominio

object Transform {

  /// METODOS DE TRANSFORMACIÓN DE FIGURAS

  def trasladar[T <: Figura](x: Int, y: Int) (figura: T) :T = {
    val result = figura match {
      case Circulo(xPos, yPos, radio) => Circulo(xPos+x, yPos+y, radio)
      case Rectangulo(xPos, yPos, ancho, alto) => Rectangulo(xPos+x, yPos+y, ancho, alto)
      case Linea(xPos, yPos, otroX, otroY) => Linea(xPos+x, yPos+y, otroX, otroY)
    }
    result.asInstanceOf[T]
  }


  def mover[T <: Figura](x: Int, y: Int) (figura: T): T = {
    val result = figura match {
      case Circulo(xPos, yPos, radio) => Circulo(x, y, radio)
      case Rectangulo(xPos, yPos, ancho, alto) => Rectangulo(x, y, ancho, alto)
      case Linea(xPos, yPos, otroX, otroY) => Linea(x, y, otroX, otroY)
    }
    result.asInstanceOf[T]
  }

  def escalar[T <: Figura] (n: Double) (figura: T) : T = {
    val result = figura match {
      case Circulo(xPos, yPos, radio) => Circulo(xPos, yPos, n*radio)
      case Rectangulo(xPos, yPos, ancho, alto) => Rectangulo(xPos, yPos, ancho*n, alto*n)
      case Linea(xPos, yPos, otroX, otroY) => Linea(xPos, yPos, otroX*n, otroY*n)
    }
    result.asInstanceOf[T]
  }

  def moverX[T <: Figura](x :Int) (figura: T) :T = mover(x, figura.y) (figura)

  def moverOrigen[T <: Figura](figura: T) :T = mover(0,0) (figura)

  def moverY[T <: Figura](y :Int) (figura: T) :T = mover(figura.x, y) (figura)

  def trasladarXeY[T <: Figura](n :Int) (figura: T) :T = trasladar(n,n) (figura)

  def duplicar[T <: Figura] (figura: T) :T = escalar(2) (figura)

  def cuadruplicar[T <: Figura] (figura: T) :T = duplicar(duplicar (figura))

  def doble[T <: Figura] (f: T => T): T => T = f compose f

}
