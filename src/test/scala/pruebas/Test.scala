package dominio
package pruebas

import org.scalatest.FlatSpec

class Test extends FlatSpec{
  // TRASLADAR

  "Un Motor" should "trasladar un Circulo" in {
    val trasladar = Motor.trasladar[Circulo](2,-1)_
    val circulo: Circulo = trasladar(Circulo(1,1,43))

    assert(circulo.x === 3)
    assert(circulo.y === 0)
    assert(circulo.radio === 43)
  }

  "Un Motor" should "trasladar una linea" in {
    val trasladar = Motor.trasladar[Linea](2, 2)_
    val linea = trasladar(Linea(1,1,0,3))

    assert(linea.x === 3)
    assert(linea.y === 3)
    assert(linea.otroX === 0)
    assert(linea.otroY === 3)
  }

  "Un Motor" should "trasladar un Rectangulo" in {

    val trasladar = Motor.trasladar[Rectangulo](2,2)_
    val rectangulo :Rectangulo = trasladar(Rectangulo(1,3,5,7))

    assert(rectangulo.x === 3)
    assert(rectangulo.y === 5)
    assert(rectangulo.ancho === 5)
    assert(rectangulo.alto === 7)
  }

  // MOVER

  "Un Motor" should "mover un Rectangulo" in {

    val move = Motor.mover[Rectangulo](2, 2) _
    val rectangulo = move(Rectangulo(1, 3, 5, 7))

    assert(rectangulo.x === 2)
    assert(rectangulo.y === 2)
    assert(rectangulo.ancho === 5)
    assert(rectangulo.alto === 7)
  }

  "Un Motor" should "mover un Circulo" in {

    val move = Motor.mover[Circulo](1, 5)_
    val circulo = move(Circulo(1,0,58))

    assert(circulo.x === 1)
    assert(circulo.y === 5)
    assert(circulo.radio === 58)
  }

  "Un Motor" should "mover una linea" in {
    val mover = Motor.mover[Linea](2, 2)_
    val linea = mover(Linea(1,1,-5,0))

    assert(linea.x === 2)
    assert(linea.y === 2)
    assert(linea.otroX === -5)
    assert(linea.otroY === 0)
  }

  // Escalar n
  "Un Motor" should "escalar un Rectangulo" in {

    val escalar = Motor.escalar[Rectangulo](2)_
    val rectangulo : Rectangulo = escalar(Rectangulo(1,3,4,6))

    assert(rectangulo.alto === 12)
    assert(rectangulo.ancho === 8)
  }

  "Un Motor" should "escalar un Circulo" in {

    val escalar = Motor.escalar[Circulo](0.5)_
    val circulo :Circulo = escalar(Circulo(1,1,50))

    assert(circulo.radio === 25)
  }


  "Un Motor" should "escalar una Linea" in {

    val escalar = Motor.escalar[Linea](2)_
    val linea = escalar(Linea(1,1,5,0))

    assert(linea.otroX === 10)
    assert(linea.otroY === 0)
  }
}
