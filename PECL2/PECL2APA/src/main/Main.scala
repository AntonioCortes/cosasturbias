package main

import scala.io.StdIn
import util.Random

object Main 
{
  def main(args: Array [String]): Unit = 
  { 
    val dificultad = pedirDificultad()
    val numFilas = asignarNumFilas(dificultad)
    val numColumnas = asignarNumColumnas(dificultad)
    val numColores = asignarNumColores(dificultad)
    val numVidas = 5
    val tablero = crearLista(numFilas * numColumnas, numColores)
    
    juego(tablero, numFilas, numColumnas, dificultad, numVidas)
        
  }
  
  def crearLista(tam: Int, numColores: Int): List[Int] = tam match
 	{
 		case 0 => Nil
 		case _ => (Random.nextInt(numColores) + 1)::crearLista(tam -1, numColores)
 	}                                         
 	
 	def poner(posicion: Int, color: Int, tablero: List[Int]): List[Int] =
 	{
 		if(tablero.isEmpty) Nil
 		else if(posicion == 1) color::tablero.tail
 		else tablero.head::poner(posicion - 1, color, tablero)
 	}        

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
    	println("Error, al elegir la dificultad")
    	pedirDificultad()
    }   
	}
 	
 	def asignarNumFilas(dificultad: Int): Int = dificultad match 
	{
		case 1 => 7
		case 2 => 11
		case 3 => 15
	}
	
	def asignarNumColumnas(dificultad: Int): Int = dificultad match
	{
		case 1 => 9
		case 2 => 17
		case 3 => 27
	}
	
	def asignarNumColores(dificultad: Int): Int = dificultad match
	{
		case 1 => 4
		case 2 => 5
		case 3 => 6
	}
	
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
	}
	
	def dibujarTablero(tablero: List[Int], numColumnas: Int, fila: Int): Unit =
	{
		if(tablero.nonEmpty)
		{
			print(tablero.head + "  ")
			if(tablero.tail.length % numColumnas == 0) println()
		}
		if(tablero.nonEmpty) dibujarTablero(tablero.tail, numColumnas, fila + 1)
	}

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
	}
	
	def pedirFila(numFilas: Int): Int =
	{
		print("Introduzca fila: ")
		val fila = StdIn.readInt
		
		if((fila < 0) || (fila >= numFilas))
		{
			println("Error al introducir la fila")
			pedirFila(numFilas)
		}
		else
		{
			fila
		}
	}
	
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
	}
}