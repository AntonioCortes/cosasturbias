package main


import scala.io.StdIn
import util.Random

object Main  extends App
{
  val dificultad = pedirDificultad()
  val numFilas = asignarNumFilas(dificultad)
  val numColumnas = asignarNumColumnas(dificultad)
  val numColores = asignarNumColores(dificultad)
  val numVidas = 5
  val tablero = crearLista(numFilas * numColumnas, numColores)
    
  juego(tablero, numFilas, numColumnas, dificultad, numVidas)
  
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
	
	def auxFila(lista: List[Int], numColumnas: Int, fila: Int): Int = if(lista.length % numColumnas == 0) fila + 1 else fila

	def dibujarTablero(tablero: List[Int], numColumnas: Int, fila: Int, columna: Int, dibujarColumnas: Boolean): Unit =
	{	
	  if (dibujarColumnas)
	  {
	    if(columna == 0) print(s"    ")
	  
	    if(columna == numColumnas)
	    {
	      print("\n\n0   ")
	      dibujarTablero(tablero, numColumnas, fila, columna, false)
	    }
	    else if((columna < numColumnas) && (columna < 10))
		  {
			  print(s"$columna  ")
			  
			  
			  dibujarTablero(tablero, numColumnas, fila, columna + 1, true)
		  }
		  else if(columna < numColumnas)
		  {
			  print(s"$columna ")
			  dibujarTablero(tablero, numColumnas, fila, columna + 1, true)
		  }
	  }
	  else if(tablero.nonEmpty)
		{
			print(tablero.head + "  ")
			
			if(tablero.tail.length % numColumnas == 0 && tablero.tail.nonEmpty) 
			{
			  if(fila + 1 < 10) print(s"\n${fila + 1}   ")
			  else print(s"\n${fila + 1}  ")
			}
			
			dibujarTablero(tablero.tail, numColumnas, auxFila(tablero.tail, numColumnas, fila), columna,false)
		}
		else
		{
		  println()
		}
	}

	def juego(tablero: List[Int],numFilas: Int, numColumnas: Int, dificultad: Int, numVidas: Int): Unit =
	{
	  println()
		print(s"Vidas: $numVidas Dificultad: $dificultad\n")
		dibujarTablero(tablero, numColumnas, 0, 0, true)
		val columa = pedirColumna(numColumnas)
		val fila = pedirFila(numFilas)
		val pos= fila*numColumnas+columa
		val listpos= List[Int]()
		val listaMuerte = algoritmoEstrella(tablero, numColumnas, listpos, pos)
		println(listaMuerte)
		val tableroFin=eliminarIguales(tablero, listaMuerte)
		dibujarTablero(tableroFin, numColumnas, 0, 0, true)
		juego(tableroFin, numFilas, numColumnas, dificultad, numVidas)
  }
	
	
	
	def algoritmoEstrella(tablero:List[Int],width:Int,lpos:List[Int],pos:Int):List[Int] = {
	       if(lpos.isEmpty){
	                val posArriba = comprobarIgualesArriba(pos,tablero,width)
	                val posAbajo = comprobarIgualesAbajo(pos,tablero,width)
              	   val posDerecha = comprobarIgualesDerecha(pos,tablero,width)
              	   val posIzquierda = comprobarIgualesIzquierda(pos,tablero,width)
              	   val lposaux = lpos:+pos
              	   val lposaux2 = lposaux:+posArriba:+posAbajo:+posDerecha:+posIzquierda
              	   val lposaux3 = lposaux2.filter(_ > -1)
              	   val lposaux4 = lposaux3.distinct
              	   //println(lposaux4)
              	   if (lposaux4==lpos) lposaux4
              	   else algoritmoEstrella(tablero, width, lposaux4, pos+1)
	      }
	       else{
	          val posArriba = comprobarIgualesArriba(lpos.head,tablero,width)
	          val posAbajo = comprobarIgualesAbajo(lpos.head,tablero,width)
            val posDerecha = comprobarIgualesDerecha(lpos.head,tablero,width)
        	    val posIzquierda = comprobarIgualesIzquierda(lpos.head,tablero,width)
        	    val lposaux = lpos:+pos
        	    val lposaux2 = lposaux:+posArriba:+posAbajo:+posDerecha:+posIzquierda
        	    val lposaux3 = lposaux2.filter(_ > -1)
        	    val lposaux4 = lposaux3.distinct
        	    //println(lposaux4)
        	    if (lposaux4==lpos) lposaux4
        	    else algoritmoEstrella(tablero, width, lposaux4.tail, pos)
	       }
	}
	// Función que devuelve la posición si esta es igual, o -1 si no es igual. El -1 lo eliminará antes de quitarRepetidos.
	def comprobarIgualesArriba(pos:Int,tablero:List[Int],width:Int): Int = {
	  if((pos-width>0) && (tablero(pos)==tablero(pos-width))) pos-width
	  else -1
	}
	def comprobarIgualesAbajo(pos:Int,tablero:List[Int],width:Int): Int = {
	  if((pos+width<tablero.length) && (tablero(pos)==tablero(pos+width))) pos+width
	  else -1
	}
	def comprobarIgualesDerecha(pos:Int,tablero:List[Int],width:Int): Int = {
	  if((pos+1<tablero.length) && (tablero(pos)==tablero(pos+1))) pos+1
	  else -1
	}
	def comprobarIgualesIzquierda(pos:Int,tablero:List[Int],width:Int): Int = {
	  if((pos-1>0) && (tablero(pos)==tablero(pos-1))) pos-1
	  else -1
	}
	
	def eliminarIguales(tablero:List[Int],lpos:List[Int]): List[Int] = {
	  if(lpos.length !=0 ){
	    val tableroaux= poner(lpos.head,0,tablero)
	    eliminarIguales(tableroaux, lpos.tail)
	  } 
	  else {
	    tablero
	  }
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