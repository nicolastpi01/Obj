
package dominio
package pruebas

import org.scalatest.FlatSpec

class MotorTest extends FlatSpec{

  "Un Motor" should "agregar figuras" in {
    val motor : Motor.type = Motor.agregarFigura(Circulo(1,1,50))

    assert(motor.figuras.size === 1)
    assert(Motor.figuras.size === 1)
    //print(motor.figuras.size)
    //print(Motor.figuras.size)

  }

}
