package dominio

trait Figura {
  def x:Int
  def y:Int
}

case class Circulo(x:Int, y: Int, radio: Double)
  extends Figura

case class Rectangulo(x:Int, y: Int, ancho: Int, alto: Int)
  extends Figura

object Motor {

  def trasladar[T](x: Int, y: Int, figura: T) :Figura = figura match {
    case Circulo(xPos, yPos, radio) => Circulo(xPos+x, yPos+y, radio)
    case Rectangulo(xPos, yPos, ancho, alto) => Rectangulo(xPos+x, yPos+y, ancho, alto)
  }
/*
  def mover(x: Int, y: Int) = (figura: Figura) => figura match {
    case Circulo(xPos, yPos, radio) => Circulo(x, y, radio)
    case Rectangulo(xPos, yPos, ancho, alto) => Rectangulo(x, y, ancho, alto)
    //case Linea(xPos, yPos, ancho, alto) => Linea(x, y, ancho, alto)
  }

  def escalar(n: Double) = (figura: Figura) => figura match {
    case Circulo(xPos, yPos, radio) => Circulo(xPos, yPos, n*radio)
    case Rectangulo(xPos, yPos, ancho, alto) => Rectangulo(xPos, yPos, ancho*2, alto*2)
    //case Linea(xPos, yPos, ancho, alto) => Linea(x, y, ancho, alto)
  }

*/
}






