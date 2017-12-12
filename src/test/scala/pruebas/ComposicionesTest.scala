package dominio
package pruebas

import org.scalatest.FlatSpec

class ComposicionesTest extends FlatSpec{

  "Un Motor" should "aplicar doble a una tranformación trasladar" in {
    val dobleTraslacion = Motor.doble[Circulo](Motor.trasladar(1,1))
    val circulo : Circulo = dobleTraslacion(Circulo(1,1,43))

    assert(circulo.x === 3)
    assert(circulo.y === 3)
    assert(circulo.radio === 43)
  }

  "Un Motor" should "aplicar doble a una tranformación escalar sobre un rectangulo" in {
    val dobleEscalar = Motor.doble[Rectangulo](Motor.escalar(2))
    val rectangulo : Rectangulo = dobleEscalar(Rectangulo(1,1,10,10))

    assert(rectangulo.ancho === 40)
    assert(rectangulo.alto === 40)
  }

  "Un Motor" should "aplicar doble a una tranformación escalar sobre una linea" in {
    val dobleEscalar = Motor.doble[Linea](Motor.escalar(2))
    val linea : Linea = dobleEscalar(Linea(1,1,0,5))

    assert(linea.otroX === 0)
    assert(linea.otroY === 20)
  }

}
