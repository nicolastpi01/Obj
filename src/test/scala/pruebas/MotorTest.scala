
package dominio
package pruebas

import exceptions.NoRollbackException
import org.scalatest.FlatSpec

class MotorTest extends FlatSpec{

  "Un Motor" should "agregar figuras" in {
    assert(Motor.motorAnterior === None)
    val motor : Motor = Motor.agregarFigura(Circulo(1,1,50))
    assert(Motor.figuras.size === 0)
    assert(motor.figuras.size === 1)
    assert(motor.motorAnterior === Some(Motor))
  }

  "Un Motor" should "retornar sus figuras" in {
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


  "Un Motor" should "aplicar la transformación de trasladar a sus figuras" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)
    val otroMotorMas : Motor = otroMotor.transformar(otroMotor.trasladar(2,2)(_:Figura))
    val rectanguloTrasladado :Rectangulo = otroMotorMas.figuras.head.asInstanceOf[Rectangulo]
    assert(rectanguloTrasladado.x === 7)
    assert(rectanguloTrasladado.y === 7)
    val circuloTrasladado = otroMotorMas.figuras.tail.head.asInstanceOf[Circulo]
    assert(circuloTrasladado.x === 3)
    assert(circuloTrasladado.y === 3)
    assert(otroMotorMas.getFiguras.size === 2)
  }

  "Un Motor" should "aplicar la transformación de mover origen a sus figuras" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)
    val motorMoveOrigen : Motor = otroMotor.transformar(otroMotor.moverOrigen)
    val rectanguloTrasladado :Figura = motorMoveOrigen.figuras.head
    assert(rectanguloTrasladado.x === 0)
    assert(rectanguloTrasladado.y === 0)
    val circuloTrasladado :Figura = motorMoveOrigen.figuras.tail.head
    assert(circuloTrasladado.x === 0)
    assert(circuloTrasladado.y === 0)
    assert(motorMoveOrigen.getFiguras.size === 2)
  }

  "Un Motor" should "aplicar la transformación de escalar a sus figuras" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)
    val motorDuplicado : Motor = otroMotor.transformar(otroMotor.duplicar)
    val rectanguloDuplicado :Rectangulo = motorDuplicado.figuras.head.asInstanceOf[Rectangulo]
    assert(rectanguloDuplicado.alto === 4)
    assert(rectanguloDuplicado.ancho === 4)
    val circuloDuplicado :Circulo = motorDuplicado.figuras.tail.head.asInstanceOf[Circulo]
    assert(circuloDuplicado.radio === 100)
    assert(motorDuplicado.getFiguras.size === 2)
  }


  "Un Motor" should "devolver el estado anterior" in {
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

  "Un Motor" should "devolver todos los estados" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)
    val estados = otroMotor.getEstados
    assert(estados.size === 2)
    assert(estados.head.head === circulo)
    assert(estados.tail.head.head === rectangulo)

  }

  "Un Motor" should "poderse rollbackear" in {
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(rectangulo)
    val motorRollback : Motor = motor.rollback()
    assert(motorRollback.figuras.size === 0)
    assert(motorRollback.motorAnterior === None)
  }

  "Un Motor" should "poderse rollbackear luego de una transformación que mueva la pos al origen" in {
    val rectangulo = Rectangulo(5,5,2,2)
    val circulo = Circulo(1,1,25)
    val motor : Motor = Motor.agregarFigura(rectangulo)
    val otroMotor : Motor = motor.agregarFigura(circulo)
    val motorMoveOrigen : Motor = otroMotor.transformar(otroMotor.moverOrigen)
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

  "Un Motor" should "poderse rollbackear una vez" in {
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(rectangulo)
    val motorRollback : Motor = motor.rollback(1)
    assert(motorRollback.figuras.size === 0)
    assert(motorRollback.motorAnterior === None)
  }

  "Un Motor" should "poderse rollbackear dos veces" in {
    val rectangulo = Rectangulo(5,5,2,2)
    val circulo = Circulo(1,1,50)
    val motor : Motor = Motor.agregarFigura(rectangulo)
    val otroMotor : Motor = motor.agregarFigura(circulo)
    val motorRollback : Motor = otroMotor.rollback(2)
    assert(motorRollback.figuras.size === 0)
    assert(motorRollback.motorAnterior === None)
  }

  "Un Motor" should "poderse rollbackear una vez y aún tener figuras" in {
    val rectangulo = Rectangulo(5,5,2,2)
    val circulo = Circulo(1,1,50)
    val motor : Motor = Motor.agregarFigura(rectangulo)
    val otroMotor : Motor = motor.agregarFigura(circulo)
    val motorRollback : Motor = otroMotor.rollback(1)
    assert(motorRollback.figuras.size === 1)
    assert(motorRollback.motorAnterior === Some(Motor))
    assert(motorRollback.figuras.head === rectangulo)
  }


  "Un Motor" should "no deberia poder rollbackearse si no tiene un estado anterior" in {
    val rectangulo = Rectangulo(5,5,2,2)
    assertThrows[NoRollbackException](Motor.rollback(1))
  }

  "Un Motor" should "no deberia poder rollbackearse tres veces,(no hay tantos estados)" in {
    val rectangulo = Rectangulo(5,5,2,2)
    assertThrows[NoRollbackException](Motor.rollback(3)) // No hay estados ni para un rollback
  }

}
