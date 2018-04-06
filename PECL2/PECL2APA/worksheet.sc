import scala.io.StdIn
import util.Random

object worksheet
{

  def crearLista(tam: Int, numColores: Int): List[Int] = tam match
 	{
 		case 0 => Nil
 		case _ => (Random.nextInt(numColores) + 1)::crearLista(tam -1, numColores)
 	}                                         //> crearLista: (tam: Int, numColores: Int)List[Int]
 	
 	def poner(posicion: Int, color: Int, tablero: List[Int]): List[Int] =
 	{
 		if(tablero.isEmpty) Nil
 		else if(posicion == 1) color::tablero.tail
 		else tablero.head::poner(posicion - 1, color, tablero)
 	}                                         //> poner: (posicion: Int, color: Int, tablero: List[Int])List[Int]
	
	def pedirDificultad(): Int =
	{
		println("Introduzca la dificultad (1/2/3):")
    val dificultad = StdIn.readInt()
    
    if( (dificultad == 1) || (dificultad == 2) || (dificultad == 3))
    {
    	dificultad
    }
    else
    {
    	println("Error al elegir la dificultad")
    	pedirDificultad()
    }
    
	}                                         //> pedirDificultad: ()Int
	
	def asignarNumFilas(dificultad: Int): Int = dificultad match
	{
		case 1 => 7
		case 2 => 11
		case 3 => 15
	}                                         //> asignarNumFilas: (dificultad: Int)Int
	
	def asignarNumColumnas(dificultad: Int): Int = dificultad match
	{
		case 1 => 9
		case 2 => 17
		case 3 => 27
	}                                         //> asignarNumColumnas: (dificultad: Int)Int
	
	def asignarNumColores(dificultad: Int): Int = dificultad match
	{
		case 1 => 4
		case 2 => 5
		case 3 => 6
	}                                         //> asignarNumColores: (dificultad: Int)Int
	
	def auxDibujarTablero(num: Int, numColumnas: Int): Unit =
	{
		if((num < numColumnas) && (num < 10))
		{
			print(s"$num  ")
			auxDibujarTablero(num + 1, numColumnas)
		}
		else if(num < numColumnas)
		{
			print(s"$num ")
			auxDibujarTablero(num + 1, numColumnas)
		}
	}                                         //> auxDibujarTablero: (num: Int, numColumnas: Int)Unit
	
	def dibujarTablero(tablero: List[Int], numColumnas: Int, fila: Int): Unit =
	{
		if(tablero.nonEmpty)
		{
			print(tablero.head + "  ")
			if(tablero.tail.length % numColumnas == 0) println()
		}
		if(tablero.nonEmpty) dibujarTablero(tablero.tail, numColumnas, fila + 1)
	}                                         //> dibujarTablero: (tablero: List[Int], numColumnas: Int, fila: Int)Unit

	def juego(tablero: List[Int],numFilas: Int, numColumnas: Int, dificultad: Int, numVidas: Int): Unit =
	{
		println()
		print(s"Vidas: $numVidas Dificultad: $dificultad\n")
		
		auxDibujarTablero(0, numColumnas)
		print("\n\n")
		dibujarTablero(tablero, numColumnas, 0)
		
		val fila = pedirFila(numFilas)
		val columna = pedirColumna(numColumnas)
		
		juego(tablero, numFilas, numColumnas, dificultad, numVidas)
	}                                         //> juego: (tablero: List[Int], numFilas: Int, numColumnas: Int, dificultad: In
                                                  //| t, numVidas: Int)Unit
	
	def pedirFila(numFilas: Int): Int =
	{
		print("Introduzca fila: ")
		val fila = StdIn.readInt
		
		if((fila < 0) || (fila >= numFilas))
		{
			printf("Error al introducir la fila\n")
			pedirFila(numFilas)
		}
		else
		{
			fila
		}
	}                                         //> pedirFila: (numFilas: Int)Int
	
	def pedirColumna(numColumnas: Int): Int =
	{
		print("Introduzca columna: ")
		val columna = StdIn.readInt
		
		if((columna < 0) || (columna >= numColumnas))
		{
			println("Error al introducir la columna")
			pedirColumna(numColumnas)
		}
		else
		{
			columna
		}
	}                                         //> pedirColumna: (numColumnas: Int)Int
                                                  
}