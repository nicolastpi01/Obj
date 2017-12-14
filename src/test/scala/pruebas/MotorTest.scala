
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

  /*
  // No funciona !!!!!!!!!!!!!
  "Un Motor" should "aplicar la transformación de trasladar a sus figuras" in {
    val circulo = Circulo(1,1,50)
    val rectangulo = Rectangulo(5,5,2,2)
    val motor : Motor = Motor.agregarFigura(circulo)
    val otroMotor : Motor = motor.agregarFigura(rectangulo)
    val otroMotorMas : Motor = otroMotor.transformar(otroMotor.mover(2,2))
    print(otroMotor.figuras)
    val rectanguloTrasladado :Rectangulo = otroMotor.figuras.head.asInstanceOf[Rectangulo]
    assert(rectanguloTrasladado.x === 2)
    assert(rectanguloTrasladado.y === 2)
    //val circuloTrasladado = otroMotor.getFiguras.tail.head.asInstanceOf[Circulo]
    //assert(circuloTrasladado.x === 3)
    //assert(circuloTrasladado.y === 3)
    //assert(motor.getFiguras.size === 1)
    //assert(otroMotor.getFiguras.size === 2)
    //assert(otroMotorMas.getFiguras.size === 2)

  }
  */

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
