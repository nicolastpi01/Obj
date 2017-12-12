package dominio

trait Figura {
  def x:Int
  def y:Int
}

case class Circulo(x:Int, y: Int, radio: Double)
  extends Figura

case class Rectangulo(x:Int, y: Int, ancho: Double, alto: Double)
  extends Figura

case class Linea(x:Int, y: Int, otroX: Double, otroY: Double)
  extends Figura

object Motor {

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
}






