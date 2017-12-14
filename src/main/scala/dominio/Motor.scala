package dominio

import exceptions.NoRollbackException

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

case class Motor() {
  var motorAnterior :Option[Motor] = None

  var figuras: List[Figura] = List()

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



                        // METODOS DE MOTOR

  def create : Motor = {
    val motor = new Motor
    motor.motorAnterior = Some(this)
    motor
  }

  def agregarFigura(figura: Figura) : Motor = {
    val motor = create
    motor.figuras = figura :: figuras
    motor
  }

  def getFiguras: List[Figura] = figuras

  def getEstadoAnterior :List[Figura] = {
    motorAnterior match {
      case Some(motor) => motor.figuras
      case None => List()
    }
  }

  // No funciona
  def transformar(f : Figura => Figura) :Motor = {
    val figurasTransformadas : List[Figura] = transformarFiguras(f, this.figuras)
    val motor : Motor = create
    motor.figuras = figurasTransformadas
    motor
  }

  // temporal
  def transformarFiguras(f: Figura => Figura, figuras: List[Figura]) : List[Figura] = {
    figuras match {
      case Nil => List()
      case cabeza :: cola => f(cabeza) :: transformarFiguras(f, cola)
    }
  }

  def getEstados : List[List[Figura]] = {
    var figurasRet = List(figuras)
    var motorAntiguo : Option[Motor] = motorAnterior
    while (motorAntiguo.isDefined) {
      if (motorAntiguo.get.figuras.nonEmpty) {
        figurasRet = motorAntiguo.get.figuras :: figurasRet
      }
      motorAntiguo = motorAntiguo.get.motorAnterior
    }
    figurasRet
  }

  // Si n es negativo, 1 o 0 el efecto es el mismo
  def rollback(n : Int = 0) : Motor = {
    if(motorAnterior.isEmpty) throw NoRollbackException("no se puede rollbackear si no hay un motor previo")
    var motorAnt : Option[Motor] = motorAnterior
    for (i <- 2 to n) {
      if(motorAnt.get.motorAnterior.isDefined)
        motorAnt = motorAnt.get.motorAnterior
      else
        throw NoRollbackException("no se puede rollbackear si no hay un motor previo")
    }
    motorAnt.get
  }


}

object Motor extends Motor









