import scala.io.StdIn
import util.Random

object worksheet
{;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(231); 

  def crearLista(tam: Int, numColores: Int): List[Int] = tam match
 	{
 		case 0 => Nil
 		case _ => (Random.nextInt(numColores) + 1)::crearLista(tam -1, numColores)
 	};System.out.println("""crearLista: (tam: Int, numColores: Int)List[Int]""");$skip(214); 
 	
 	def poner(posicion: Int, color: Int, tablero: List[Int]): List[Int] =
 	{
 		if(tablero.isEmpty) Nil
 		else if(posicion == 1) color::tablero.tail
 		else tablero.head::poner(posicion - 1, color, tablero)
 	};System.out.println("""poner: (posicion: Int, color: Int, tablero: List[Int])List[Int]""");$skip(319); 
	
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
    
	};System.out.println("""pedirDificultad: ()Int""");$skip(114); 
	
	def asignarNumFilas(dificultad: Int): Int = dificultad match
	{
		case 1 => 7
		case 2 => 11
		case 3 => 15
	};System.out.println("""asignarNumFilas: (dificultad: Int)Int""");$skip(117); 
	
	def asignarNumColumnas(dificultad: Int): Int = dificultad match
	{
		case 1 => 9
		case 2 => 17
		case 3 => 27
	};System.out.println("""asignarNumColumnas: (dificultad: Int)Int""");$skip(114); 
	
	def asignarNumColores(dificultad: Int): Int = dificultad match
	{
		case 1 => 4
		case 2 => 5
		case 3 => 6
	};System.out.println("""asignarNumColores: (dificultad: Int)Int""");$skip(277); 
	
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
	};System.out.println("""auxDibujarTablero: (num: Int, numColumnas: Int)Unit""");$skip(277); 
	
	def dibujarTablero(tablero: List[Int], numColumnas: Int, fila: Int): Unit =
	{
		if(tablero.nonEmpty)
		{
			print(tablero.head + "  ")
			if(tablero.tail.length % numColumnas == 0) println()
		}
		if(tablero.nonEmpty) dibujarTablero(tablero.tail, numColumnas, fila + 1)
	};System.out.println("""dibujarTablero: (tablero: List[Int], numColumnas: Int, fila: Int)Unit""");$skip(417); 

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
	};System.out.println("""juego: (tablero: List[Int], numFilas: Int, numColumnas: Int, dificultad: Int, numVidas: Int)Unit""");$skip(240); 
	
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
	};System.out.println("""pedirFila: (numFilas: Int)Int""");$skip(272); 
	
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
	};System.out.println("""pedirColumna: (numColumnas: Int)Int""")}
                                                  
}
