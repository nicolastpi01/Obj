package dominio
package pruebas

import org.scalatest.FlatSpec

class FuncionesUtilesTest extends FlatSpec{

  "Un Motor" should "moverX un circulo" in {

    val moverXfunction = Motor.moverX[Circulo](5)_
    val circulo :Circulo = moverXfunction(Circulo(1,1,50))

    assert(circulo.x === 5)
    assert(circulo.y === 1)
  }

  "Un Motor" should "moverY un rectangulo" in {

    val moverYfunction = Motor.moverY[Rectangulo](3)_
    val rectangulo :Rectangulo = moverYfunction(Rectangulo(1,10,4,6))

    assert(rectangulo.x === 1)
    assert(rectangulo.y === 3)
  }

  "Un Motor" should "trasladarXeY un rectangulo" in {

    val function = Motor.trasladarXeY[Rectangulo](2)_
    val rectangulo :Rectangulo = function(Rectangulo(1,1,4,6))

    assert(rectangulo.x === 3)
    assert(rectangulo.y === 3)
  }

  "Un Motor" should "trasladarXeY una linea" in {

    val function = Motor.trasladarXeY[Linea](2)_
    val linea :Linea = function(Linea(1,1,0,6))

    assert(linea.x === 3)
    assert(linea.y === 3)
  }

  "Un Motor" should "duplicar un circulo" in {

    val function = Motor.duplicar[Circulo]_
    val circulo :Circulo = function(Circulo(0,0,25))

    assert(circulo.radio === 50)
  }

  "Un Motor" should "cuadruplicar un rectangulo" in {

    val function = Motor.cuadruplicar[Rectangulo]_
    val rectangulo :Rectangulo = function(Rectangulo(1,1,2,2))

    assert(rectangulo.alto === 8)
    assert(rectangulo.ancho === 8)
  }

}
