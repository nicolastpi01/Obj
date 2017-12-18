package dominio
package pruebas

import org.scalatest.FlatSpec

class ComposicionesTest extends FlatSpec{

  "A Transform object" should "apply a trasladar function twice upon a circle" in {
    val dobleTraslacion = Transform.doble[Circulo](Transform.trasladar(1,1))
    val circulo : Circulo = dobleTraslacion(Circulo(1,1,43))

    assert(circulo.x === 3)
    assert(circulo.y === 3)
    assert(circulo.radio === 43)
  }

  "A Transform object" should "apply a escalar function twice upon a rectangle" in {
    val dobleEscalar = Transform.doble[Rectangulo](Transform.escalar(2))
    val rectangulo : Rectangulo = dobleEscalar(Rectangulo(1,1,10,10))

    assert(rectangulo.ancho === 40)
    assert(rectangulo.alto === 40)
  }

  "A Transform object" should "apply a escalar function twice upon a line" in {
    val dobleEscalar = Transform.doble[Linea](Transform.escalar(2))
    val linea : Linea = dobleEscalar(Linea(1,1,0,5))

    assert(linea.otroX === 0)
    assert(linea.otroY === 20)
  }

}
