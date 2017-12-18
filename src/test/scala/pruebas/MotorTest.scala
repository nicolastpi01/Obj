
package dominio
package pruebas

import exceptions.{NoRollbackException, ThereAreNoPreviousTransformationsException}
import org.scalatest.FlatSpec

class MotorTest extends FlatSpec{

  "A Motor" should "add a figure" in {
    assert(Motor.motorAnterior === None)
    val motor : Motor = Motor.agregarFigura(Circulo(1,1,50))
    assert(Motor.figuras.size === 0)
    assert(motor.figuras.size === 1)
    assert(motor.motorAnterior === Some(Motor))
  }

  "A Motor" should "return their figures" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)

    assert(otroMotor.motorAnterior === Some(motor))
    assert(otroMotor.motorAnterior.get.motorAnterior === Some(Motor))
    assert(otroMotor.motorAnterior.get.motorAnterior.get.motorAnterior === None)
    assert(otroMotor.getFiguras.size === 2)
    assert(otroMotor.getFiguras.head === rectangulo)
    assert(otroMotor.getFiguras.tail.head === circulo)

  }


  "A Motor" should "apply the transformation of trasladar over your figures" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)
    val otroMotorMas : Motor = otroMotor.transformar(Transform.trasladar(2,2)(_:Figura))
    val rectanguloTrasladado :Rectangulo = otroMotorMas.figuras.head.asInstanceOf[Rectangulo]
    assert(rectanguloTrasladado.x === 7)
    assert(rectanguloTrasladado.y === 7)
    val circuloTrasladado = otroMotorMas.figuras.tail.head.asInstanceOf[Circulo]
    assert(circuloTrasladado.x === 3)
    assert(circuloTrasladado.y === 3)
    assert(otroMotorMas.getFiguras.size === 2)
  }

  "A Motor" should "repeat the last function performed, in this case trasladar" in {
    val circulo = Circulo(1,1,50)
    val motor : Motor = Motor.agregarFigura(circulo)
    val motorTrasladado : Motor = motor.transformar(Transform.trasladar(2,2))
    val circuloPos3x3 :Circulo = motorTrasladado.figuras.head.asInstanceOf[Circulo]
    assert(circuloPos3x3.x === 3)
    assert(circuloPos3x3.y === 3)
    val motorRepetirTraslado : Motor = motorTrasladado.repetir
    val circuloPos5x5 :Circulo = motorRepetirTraslado.figuras.head.asInstanceOf[Circulo]
    assert(circuloPos5x5.x === 5)
    assert(circuloPos5x5.y === 5)
  }

  "A Motor" should "thrown an error if it were to repeat a transformation that never took place" in {
    val circulo = Circulo(1,1,50)
    val motor : Motor = Motor.agregarFigura(circulo)
    assertThrows[ThereAreNoPreviousTransformationsException](motor.repetir) // No hay transformaciones previas
  }


  "A Motor" should "apply the transformation of moverOrigen to their figures" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)
    val motorMoveOrigen : Motor = otroMotor.transformar(Transform.moverOrigen)
    val rectanguloTrasladado :Figura = motorMoveOrigen.figuras.head
    assert(rectanguloTrasladado.x === 0)
    assert(rectanguloTrasladado.y === 0)
    val circuloTrasladado :Figura = motorMoveOrigen.figuras.tail.head
    assert(circuloTrasladado.x === 0)
    assert(circuloTrasladado.y === 0)
    assert(motorMoveOrigen.getFiguras.size === 2)
  }

  "A Motor" should "apply the transformation of escalar to their figures" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)
    val motorDuplicado : Motor = otroMotor.transformar(Transform.duplicar)
    val rectanguloDuplicado :Rectangulo = motorDuplicado.figuras.head.asInstanceOf[Rectangulo]
    assert(rectanguloDuplicado.alto === 4)
    assert(rectanguloDuplicado.ancho === 4)
    val circuloDuplicado :Circulo = motorDuplicado.figuras.tail.head.asInstanceOf[Circulo]
    assert(circuloDuplicado.radio === 100)
    assert(motorDuplicado.getFiguras.size === 2)
  }

  "A Motor" should "return the previous state" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val estadoAnterior : List[Figura] = motor.getEstadoAnterior
    assert(estadoAnterior.size === 0)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)
    val nuevoEstadoAnterior : List[Figura] = otroMotor.getEstadoAnterior
    assert(nuevoEstadoAnterior.size === 1)
    assert(nuevoEstadoAnterior.head === circulo)
  }


  "A Motor" should "return all the states" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)
    val estados = otroMotor.getEstados
    assert(estados.size === 2)
    assert(estados.head.head === circulo)
    assert(estados.tail.head.head === rectangulo)
  }

  "A Motor" should "be able to rollback" in {
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(rectangulo)
    val motorRollback : Motor = motor.rollback()
    assert(motorRollback.figuras.size === 0)
    assert(motorRollback.motorAnterior === None)
  }

  "A Motor" should "be able to rollback after a transformation that moves the figure to the origen" in {
    val rectangulo = Rectangulo(5,5,2,2)
    val circulo = Circulo(1,1,25)
    val motor : Motor = Motor.agregarFigura(rectangulo)
    val otroMotor : Motor = motor.agregarFigura(circulo)
    val motorMoveOrigen : Motor = otroMotor.transformar(Transform.moverOrigen)
    val rectanguloOrigen :Figura = motorMoveOrigen.figuras.head
    assert(rectanguloOrigen.x === 0)
    assert(rectanguloOrigen.y === 0)
    val circuloOrigen :Figura = motorMoveOrigen.figuras.tail.head
    assert(circuloOrigen.x === 0)
    assert(circuloOrigen.y === 0)
    assert(motorMoveOrigen.getFiguras.size === 2)
    val motorRollback : Motor = motorMoveOrigen.rollback()
    assert(motorRollback.getFiguras.size === 2)
    val circuloRollback :Figura = motorRollback.figuras.head
    assert(circuloRollback.x === 1)
    assert(circuloRollback.y === 1)
    val rectanguloRollback :Figura = motorRollback.figuras.tail.head
    assert(rectanguloRollback.x === 5)
    assert(rectanguloRollback.y === 5)
  }

  "A Motor" should "be able to rollback once" in {
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(rectangulo)
    val motorRollback : Motor = motor.rollback(1)
    assert(motorRollback.figuras.size === 0)
    assert(motorRollback.motorAnterior === None)
  }

  "A Motor" should "be able to rollback twice" in {
    val rectangulo = Rectangulo(5,5,2,2)
    val circulo = Circulo(1,1,50)
    val motor : Motor = Motor.agregarFigura(rectangulo)
    val otroMotor : Motor = motor.agregarFigura(circulo)
    val motorRollback : Motor = otroMotor.rollback(2)
    assert(motorRollback.figuras.size === 0)
    assert(motorRollback.motorAnterior === None)
  }

  "A Motor" should "be able to rollback and still have figures" in {
    val rectangulo = Rectangulo(5,5,2,2)
    val circulo = Circulo(1,1,50)
    val motor : Motor = Motor.agregarFigura(rectangulo)
    val otroMotor : Motor = motor.agregarFigura(circulo)
    val motorRollback : Motor = otroMotor.rollback(1)
    assert(motorRollback.figuras.size === 1)
    assert(motorRollback.motorAnterior === Some(Motor))
    assert(motorRollback.figuras.head === rectangulo)
  }

  "A Motor" should "not be able to rollback if it does not have a previous state" in {
    val rectangulo = Rectangulo(5,5,2,2)
    assertThrows[NoRollbackException](Motor.rollback(1))
  }

  "A Motor" should "not be able to rollback three times (there are not so many states)" in {
    val rectangulo = Rectangulo(5,5,2,2)
    assertThrows[NoRollbackException](Motor.rollback(3)) // No hay estados ni para un rollback
  }

}
