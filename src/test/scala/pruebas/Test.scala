package dominio
package pruebas

import org.scalatest.FlatSpec

class Test extends FlatSpec{
  // TRASLADAR

  "Un Motor" should "trasladar un Circulo" in {
    val trasladar: Figura = Motor.trasladar(2, -1, Circulo(1,1,43))
    val circulo : Circulo = trasladar.asInstanceOf[Circulo]
    println(circulo)

    assert(circulo.x === 3)
    assert(circulo.y === 0)
    assert(circulo.radio === 43)
  }
/*
  "Un Motor" should "trasladar una linea" in {
    val trasladarF = Motor.trasladar(2, 2)
    val lineaBase = Linea(1,1,5,0)
    val linea = trasladarF(lineaBase)

    assert(linea.x === 3)
    assert(linea.y === 3)
  }
*/
  "Un Motor" should "trasladar un Rectangulo" in {

    val trasladar = Motor.trasladar(2, 2, Rectangulo(1,3,5,7))
    //val rectangulo = trasladar(Rectangulo(1,3,5,7))

    assert(trasladar.x === 3)
    assert(trasladar.y === 5)
    //assert(rectangulo.ancho === 1)
    //assert(rectangulo.alto === 1)
  }

  // MOVER
  /*
    "Un Motor" should "mover un Rectangulo" in {

      val Fmover = Motor.mover(2, 2)
      val rectangulo = Fmover(Rectangulo(1,3,5,7))

      assert(rectangulo.x === 2)
      assert(rectangulo.y === 2)
      //assert(rectangulo.ancho === 1)
      //assert(rectangulo.alto === 1)
    }

    "Un Motor" should "mover un Circulo" in {

      val Fmover = Motor.mover(1, 5)
      val cuadrado = Fmover(Circulo(1,0,58))

      assert(cuadrado.x === 1)
      assert(cuadrado.y === 5)
      //assert(rectangulo.ancho === 1)
      //assert(rectangulo.alto === 1)
    }

    "Un Motor" should "mover una linea" in {
      val moverF = Motor.mover(2, 2)
      val lineaBase = Linea(1,1,5,0)
      val linea = moverF(lineaBase)

      assert(linea.x === 2)
      assert(linea.y === 2)
    }

    // Escalar n

    "Un Motor" should "escalar un Rectangulo" in {

      val Fescalar = Motor.escalar(2)
      val rectangulo = Fescalar(Rectangulo(1,3,4,6))

      //assert(rectangulo.alto === 12)
      //assert(rectangulo.ancho === 8)
      //assert(rectangulo.x === 1) # No se modif
    }

    "Un Motor" should "escalar un Circulo" in {

      val Fescalar = Motor.escalar(0.5)
      val circulo = Fescalar(Circulo(1,1,50))

      //assert(circulo.radio === 25)
    }

    "Un Motor" should "escalar una Linea" in {

      val Fmover = Motor.mover(1, 5)
      val linea = Fmover(Linea(1,1,5,0))

      //assert(linea.x === 1)
      //assert(linea.y === 5)
    }
  */
}
