package dominio
import exceptions.{NoRollbackException, ThereAreNoPreviousTransformationsException}

object Motor extends Motor

case class Motor() {
  var motorAnterior :Option[Motor] = None

  var funcionTransformacionAnterior : Option[Figura=>Figura] = None

  var figuras: List[Figura] = List()

        /// MÉTODOS DE MOTOR

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

  def transformar(f : Figura => Figura) :Motor = {
    val figurasTransformadas : List[Figura] = transformarFiguras(f, this.figuras)
    val motor : Motor = create
    motor.figuras = figurasTransformadas
    motor.funcionTransformacionAnterior = Some(f)
    motor
  }

  private def transformarFiguras(f: Figura => Figura, figuras: List[Figura]) : List[Figura] = {
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

  // Si n es negativo, 1 v 0 el efecto es el mismo
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

  def repetir : Motor = {
    funcionTransformacionAnterior match {
      case Some(funcionTransformacion) => transformar(funcionTransformacion)
      case None => throw ThereAreNoPreviousTransformationsException("no hay funciones de transformacion previas")
    }
  }

}











