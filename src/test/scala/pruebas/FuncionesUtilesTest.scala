package dominio
package pruebas

import org.scalatest.FlatSpec

class FuncionesUtilesTest extends FlatSpec {

  "A Transform object" should "moverOrigen a circle" in {

    val moverOrigen = Transform.moverOrigen[Circulo]_
    val circulo :Circulo = moverOrigen(Circulo(1,1,50))

    assert(circulo.x === 0)
    assert(circulo.y === 0)
  }


  "A Transform object" should "moverX a circle (move an x number of times)" in {

    val moverXfunction = Transform.moverX[Circulo](5)_
    val circulo :Circulo = moverXfunction(Circulo(1,1,50))

    assert(circulo.x === 5)
    assert(circulo.y === 1)
  }

  "A Transform object" should "moverY a rectangle (move the y position of a figure)" in {

    val moverYfunction = Transform.moverY[Rectangulo](3)_
    val rectangulo :Rectangulo = moverYfunction(Rectangulo(1,10,4,6))

    assert(rectangulo.x === 1)
    assert(rectangulo.y === 3)
  }


  "A Transform object" should "trasladarXeY a rectangle (traslada to x&y position)" in {

    val function = Transform.trasladarXeY[Rectangulo](2)_
    val rectangulo :Rectangulo = function(Rectangulo(1,1,4,6))

    assert(rectangulo.x === 3)
    assert(rectangulo.y === 3)
  }

  "A Transform object" should "trasladarXeY a line" in {

    val function = Transform.trasladarXeY[Linea](2)_
    val linea :Linea = function(Linea(1,1,0,6))

    assert(linea.x === 3)
    assert(linea.y === 3)
  }

  "A Transform object" should "duplicate a circle" in {

    val function = Transform.duplicar[Circulo]_
    val circulo :Circulo = function(Circulo(0,0,25))

    assert(circulo.radio === 50)
  }

  "A Transform object" should "quadruple a rectangle" in {

    val function = Transform.cuadruplicar[Rectangulo]_
    val rectangulo :Rectangulo = function(Rectangulo(1,1,2,2))

    assert(rectangulo.alto === 8)
    assert(rectangulo.ancho === 8)
  }

}
