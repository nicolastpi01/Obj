
package dominio
package pruebas

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
    val otroMotorMas : Motor = motor.transformar(otroMotor.trasladar(2,2))

    //assert(otroMotor.getFiguras.size === 2)
    //assert(otroMotor.getFiguras.head === rectangulo)
    //assert(otroMotor.getFiguras.tail.head === circulo)
    assert(motor.getFiguras.size === 1)
    assert(otroMotor.getFiguras.size === 2)
    assert(otroMotorMas.getFiguras.size === 2)
    //assert(otroMotorMas.figuras.size === 2)

  }

}
