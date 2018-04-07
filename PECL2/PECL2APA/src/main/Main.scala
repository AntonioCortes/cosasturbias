package main
//Hola soy Flavius
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
		val pos = (fila*numColumnas) + columna
		
		val tablero1= comprobarElementoArriba(pos, tablero, numColumnas)
		val tablero2= comprobarElementoDerecha(pos, tablero1, numColumnas)
		val tablero3= comprobarElementoIzquierda(pos, tablero2, numColumnas)
		val tablero4= comprobarElementoAbajo(pos, tablero3, numColumnas)
		dibujarTablero(tablero4, numColumnas, 0)
		quitarCeros(tablero4, 0)
		juego(tablero4, numFilas, numColumnas, dificultad, numVidas)		
	}
	def quitarCeros(tablero:List[Int], pos:Int): List[Int] = {
  val r = scala.util.Random
  	if (tablero.length == pos) tablero
  	else if(tablero(pos)==0){
  		quitarCeros(poner(pos,(1+ r.nextInt((4-1)+1)),tablero), pos+1)
  		}
  else quitarCeros(tablero, pos+1)
  }
	
	
	
	def algoritmoEstrella(tablero:List[Int],width:Int,lpos:List[Int],pos:Int):List[Int] = {
	  val posArriba = comprobarIguales(lpos.head, lpos.head-width, tablero)
	  val posAbajo = comprobarIguales(lpos.head, lpos.head+width, tablero)
	  val posDerecha = comprobarIguales(lpos.head, lpos.head+1, tablero)
	  val posIzquierda = comprobarIguales(lpos.head, lpos.head-1, tablero)
	  val lposaux = posArriba::posAbajo::posDerecha::posIzquierda::lpos
	  val lposaux2 = lposaux.filter(_ > -1)
	  val lposaux3 = lposaux2.distinct
	  if (lposaux3==lpos) lposaux3
	  else algoritmoEstrella(tablero, width, lpos, pos+1)
	}
	
	
	// Función que devuelve la posición si esta es igual, o -1 si no es igual. El -1 lo eliminará antes de quitarRepetidos.
	def comprobarIguales(pos:Int,pos2:Int,tablero:List[Int]): Int = {
	  if(tablero(pos)==tablero(pos2)) pos2
	  else -1
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