package dominio
package pruebas

import org.scalatest.FlatSpec

class TransformacionesTest extends FlatSpec{
  // TRASLADAR

  "A Transform object" should "trasladar a Circle" in {
    val trasladar = Transform.trasladar[Circulo](2,-1)_
    val circulo : Circulo = Circulo(1,1,43)
    val circuloNuevo: Circulo = trasladar(circulo)

    assert(circuloNuevo.x === 3)
    assert(circuloNuevo.y === 0)
    assert(circuloNuevo.radio === 43)
    assert(circulo !== circuloNuevo)
  }

  "A Transform object" should "trasladar a line" in {
    val trasladar = Transform.trasladar[Linea](2, 2)_
    val linea = trasladar(Linea(1,1,0,3))

    assert(linea.x === 3)
    assert(linea.y === 3)
    assert(linea.otroX === 0)
    assert(linea.otroY === 3)
  }


  "A Transform object" should "trasladar a Rectangle" in {

    val trasladar = Transform.trasladar[Rectangulo](2,2)_
    val rectangulo :Rectangulo = trasladar(Rectangulo(1,3,5,7))

    assert(rectangulo.x === 3)
    assert(rectangulo.y === 5)
    assert(rectangulo.ancho === 5)
    assert(rectangulo.alto === 7)
  }


  // MOVER

  "A Transform object" should "move a Rectangle" in {

    val move = Transform.mover[Rectangulo](2, 2) _
    val rectangulo = move(Rectangulo(1, 3, 5, 7))

    assert(rectangulo.x === 2)
    assert(rectangulo.y === 2)
    assert(rectangulo.ancho === 5)
    assert(rectangulo.alto === 7)
  }


  "A Transform object" should "move a Circle" in {

    val move = Transform.mover[Circulo](1, 5)_
    val circulo = move(Circulo(1,0,58))

    assert(circulo.x === 1)
    assert(circulo.y === 5)
    assert(circulo.radio === 58)
  }


  "A Transform object" should "move a line" in {
    val mover = Transform.mover[Linea](2, 2)_
    val linea = mover(Linea(1,1,-5,0))

    assert(linea.x === 2)
    assert(linea.y === 2)
    assert(linea.otroX === -5)
    assert(linea.otroY === 0)
  }


  "A Transform object" should "escalar a Rectangle" in {

    val escalar = Transform.escalar[Rectangulo](2)_
    val rectangulo : Rectangulo = escalar(Rectangulo(1,3,4,6))

    assert(rectangulo.alto === 12)
    assert(rectangulo.ancho === 8)
  }


  "A Transform object" should "escalar a Circle" in {

    val escalar = Transform.escalar[Circulo](0.5)_
    val circulo :Circulo = escalar(Circulo(1,1,50))

    assert(circulo.radio === 25)
  }


  "A Transform object" should "escalar a Line" in {

    val escalar = Transform.escalar[Linea](2)_
    val linea = escalar(Linea(1,1,5,0))

    assert(linea.otroX === 10)
    assert(linea.otroY === 0)
  }

}
