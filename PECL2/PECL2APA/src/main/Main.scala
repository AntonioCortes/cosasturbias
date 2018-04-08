package main
//Hola soy Flavius
//Hola Soy Antonio
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
  def poner(pos:Int,color:Int,tablero:List[Int]): List[Int] = {
  			if (tablero.isEmpty) Nil
  			else if (pos==0) color::tablero.tail //else if (pos==0) color::tablero.tail
  			else tablero.head::poner(pos-1,color,tablero.tail)
  }
  def crearLista(tam: Int, numColores: Int): List[Int] = tam match
 	{
 		case 0 => Nil
 		case _ => (Random.nextInt(numColores) + 1)::crearLista(tam -1, numColores)
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
	  if(num == 0) print(s"    ")
	  
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
	  
	  if(num == (numColumnas - 1)) print("\n\n0   ")
	}
	
	def auxFila(lista: List[Int], numColumnas: Int, fila: Int): Int = if(lista.length % numColumnas == 0) fila + 1 else fila
	
	def dibujarTablero(tablero: List[Int], numColumnas: Int, fila: Int): Unit =
	{		  
		if(tablero.nonEmpty)
		{
			print(tablero.head + "  ")
			
			if(tablero.tail.length % numColumnas == 0 && tablero.tail.nonEmpty) print(s"\n${fila + 1}   ")
			else if(tablero.tail.isEmpty) println()
		}
		if(tablero.nonEmpty) dibujarTablero(tablero.tail, numColumnas, auxFila(tablero.tail, numColumnas, fila))
	}

	def juego(tablero: List[Int],numFilas: Int, numColumnas: Int, dificultad: Int, numVidas: Int): Unit =
	{
	  println()
		print(s"Vidas: $numVidas Dificultad: $dificultad\n")
		
		auxDibujarTablero(0, numColumnas)		
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
  //Funcion que comprubeba si el elemento
 def comprobarElementoArriba(pos:Int,tablero:List[Int], width:Int): List[Int] ={
 		if ((pos-width>0) && (tablero(pos)==tablero(pos-width))){
 							comprobarElementoArriba(pos+width,poner(pos,0,tablero), width)}
 		else if(pos==0){ //Si he llegado hasta la pos 0 es que el de abajo era igual que yo, por lo que me pongo a 0
 								poner(pos,0,tablero)}
 		else tablero
 }                                                //> comprobarElementoArriba: (pos: Int, tablero: List[Int], width: Int)List[Int
                                                  //| ]
                                                  
                                                  

 
 def comprobarElementoIzquierda(pos:Int,tablero:List[Int],width:Int) : List[Int] = {
 		if ((pos-1>0) && (tablero(pos)==tablero(pos-1))){
 							comprobarElementoArriba(pos+1,poner(pos,0,tablero), width)}
 		else if(pos==0){ //Si he llegado hasta la pos 0 es que el de abajo era igual que yo, por lo que me pongo a 0
 								poner(pos,0,tablero)}
 		else tablero
 }                                                //> comprobarElementoIzquierda: (pos: Int, tablero: List[Int], width: Int)List[
                                                  //| Int]
 
 
 def comprobarElementoDerecha(pos:Int,tablero:List[Int],width:Int): List[Int] = {
 		if ((pos+1<tablero.length) && (tablero(pos)==tablero(pos+1))){
 							comprobarElementoArriba(pos+1,poner(pos,0,tablero), width)}
 		else if(pos==tablero.length){ //Si he llegado hasta la pos 0 es que el de abajo era igual que yo, por lo que me pongo a 0
 								poner(pos,0,tablero)}
 		else tablero
 }                                                //> comprobarElementoDerecha: (pos: Int, tablero: List[Int], width: Int)List[In
                                                  //| t]
 def comprobarElementoAbajo(pos:Int,tablero:List[Int], width:Int): List[Int] = {
 		if ((pos+width<tablero.length) && (tablero(pos)==tablero(pos+width))){
 							comprobarElementoArriba(pos+width,poner(pos,0,tablero), width)}
 		else if(pos==(tablero.length-1)){ //Si he llegado hasta la pos 0 es que el de abajo era igual que yo, por lo que me pongo a 0
 								poner(pos,0,tablero)}
 		else tablero
 } 
}