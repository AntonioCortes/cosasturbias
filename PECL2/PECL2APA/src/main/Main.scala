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
	  val mPunt= mejorJugada(tablero, 0, 0, numColumnas,0, 0)
	  println()
		print(s"Vidas: $numVidas Dificultad: $dificultad\n")
		dibujarTablero(tablero, numColumnas, 0, 0, true)
		print(s"La máquina recomienda como mejor posición: $mPunt\n")
		val columna = pedirColumna(numColumnas)
		val fila = pedirFila(numFilas)
		val pos= fila*numColumnas+columna
		val listpos= List[Int]()
		val listaMuerte =  buscarIguales(tablero, pos, columna, numColumnas,listpos)
		println(listaMuerte)
		val tableroFin=eliminarIguales(tablero, listaMuerte)
		dibujarTablero(tableroFin, numColumnas, 0, 0, true)
		juego(tableroFin, numFilas, numColumnas, dificultad, numVidas)
  }
	
	
	def mejorJugada(tablero:List[Int],pos:Int,columna:Int,width:Int,mPuntacion:Int,mPos:Int):Int={
	  if (pos+2>=tablero.length) mPos
	  else{
	    if(tablero(pos)==0) mejorJugada(tablero, pos+1, columna, width, mPuntacion, pos)
	    else{
	      val puntAux=buscarIguales(tablero, pos, columna, width, List[Int]())
	      if(puntAux.length<mPuntacion) mejorJugada(tablero, pos+1, columna, width, mPuntacion, mPos)
	      else mejorJugada(tablero, pos+1, columna, width, puntAux.length, pos)
	  }
	}
	}
	def buscarIguales(tablero:List[Int],pos:Int,columna:Int,width:Int,lpos:List[Int]):List[Int] = {
	  //busca en todas direcciones
	  val arriba=buscarIgualesArriba(tablero,pos-width,columna,width,lpos)
	  val abajo=buscarIgualesAbajo(tablero,pos+width,columna,width,lpos)
	  val derecha=buscarIgualesDerecha(tablero,pos+1,columna+1,width,lpos)
	  val izquierda=buscarIgualesIzquierda(tablero,pos-1,columna-1,width,lpos)
	  ////Une todas las listas con las posiciones que recibe
	  val lposaux=unirListas(lpos,unirListas(arriba, (unirListas(abajo, (unirListas(derecha, izquierda)))))) 
	  val lposaux1= pos::lposaux //Añade el primer elemento
	  return lposaux1
	}
	
	
	def buscarIgualesArriba(tablero:List[Int],pos:Int,columna:Int,width:Int,lpos:List[Int]):List[Int] = {
	  if((pos>0) && (tablero(pos)==tablero(pos+width))){
	    val arriba=buscarIgualesArriba(tablero,pos-width,columna,width,lpos)
	    val derecha=buscarIgualesDerecha(tablero,pos+1,columna+1,width,lpos)
	    val izquierda=buscarIgualesIzquierda(tablero,pos-1,columna-1,width,lpos)
	    val lposaux=unirListas( lpos,unirListas(arriba,(unirListas(derecha, izquierda))))
	    val lposaux1= pos::lposaux
	    return lposaux1
	  }
	  else{
	    return List[Int]()
	  }
	}
	
	def buscarIgualesAbajo(tablero:List[Int],pos:Int,columna:Int,width:Int,lpos:List[Int]):List[Int] = {
	  if((pos<tablero.length-width) && (tablero(pos)==tablero(pos-width))){
	    val abajo=buscarIgualesAbajo(tablero, pos+width, columna, width,  lpos)
	    val derecha=buscarIgualesDerecha(tablero,pos+1,columna+1,width,lpos)
	    val izquierda=buscarIgualesIzquierda(tablero,pos-1,columna-1,width,lpos)
	    val lposaux=unirListas( lpos,(unirListas(abajo, (unirListas(derecha, izquierda)))))
	    val lposaux1= pos::lposaux
	    return lposaux1
	  }
	  else{
	    return List[Int]()
	  }
	}
	
	def buscarIgualesDerecha(tablero:List[Int],pos:Int,columna:Int,width:Int,lpos:List[Int]):List[Int] = {
	  val n1= pos.toFloat/width
	  val n2= scala.math.round(n1)
	  if((n2!=width) && (tablero(pos)==tablero(pos-1))){//pos-width
	    val arriba=buscarIgualesArriba(tablero, pos-width, columna, width,lpos)
	    val abajo=buscarIgualesAbajo(tablero,pos+width,columna,width,lpos)
	    val derecha=buscarIgualesDerecha(tablero,pos+1,columna+1,width,lpos)
	    val lposaux=unirListas( lpos,unirListas(arriba, (unirListas(abajo,derecha))))
	    val lposaux1= pos::lposaux
	    return lposaux1
	  }
	  else if((n2==width) && (tablero(pos)==tablero(pos-1))) pos::lpos
	  else{
	    return List[Int]()
	  }
	}
	
	def buscarIgualesIzquierda(tablero:List[Int],pos:Int,columna:Int,width:Int,lpos:List[Int]):List[Int] = {
	  if (pos<0) List[Int]()
	  else{
	  val n1= pos%width
	  if((n1 != 0) && (tablero(pos)==tablero(pos+1))){
	    val arriba=buscarIgualesArriba(tablero,pos-width,columna,width,lpos)
	    val abajo=buscarIgualesAbajo(tablero,pos+width,columna,width,lpos)
	    val izquierda=buscarIgualesIzquierda(tablero,pos-1,columna-1,width,lpos)
	    val lposaux=unirListas( lpos,unirListas(arriba, (unirListas(abajo,izquierda))))
	    val lposaux1= pos::lposaux
	    return lposaux1
	  }
	  else if((pos%width==0) && (tablero(pos)==tablero(pos+1))) pos::lpos
	  else{
	    return List[Int]()
	  }
	}
	}
	
	
	
	
	/*def algoritmoEstrella(tablero:List[Int],width:Int,lpos:List[Int],pos:Int,posLista:Int,columna:Int):List[Int] = {
	       if(posLista==0){ //Caso inicial
	                val lposaux1=lpos:+pos
	                val lposaux=  unirListas(lpos,verPosiciones(pos, tablero, width,columna))
	                if(lposaux.length==1) lposaux //Fallo no hay iguales, no se eliminará.
	                else algoritmoEstrella(tablero, width, lposaux,pos,posLista+1,columna)
	      }
	      else if(posLista>=lpos.length){
	         return lpos
	       }
	      else{
	         val lposaux=  unirListas(lpos,verPosiciones(lpos(pos), tablero, width,columna))
	         algoritmoEstrella(tablero, width, lposaux,pos,posLista+1,columna)
	       }
	}*/
	
	def unirListas(l1:List[Int],l2:List[Int]):List[Int]={
	  if(l2.isEmpty) l1
	  else unirListas(l1:+l2.head,l2.tail)
	}
	
	/*def verPosiciones(pos:Int,tablero:List[Int],width:Int,columna:Int):List[Int]= {
	  val posArriba = comprobarIgualesArriba(pos,tablero,width)
	  val posAbajo = comprobarIgualesAbajo(pos,tablero,width)
    val posDerecha = comprobarIgualesDerecha(pos,tablero,width,columna)
    val posIzquierda = comprobarIgualesIzquierda(pos,tablero,width,columna)
    val lposaux = List(pos)
    val lposaux2 = lposaux:+posArriba:+posAbajo:+posDerecha:+posIzquierda
    val lposaux3 = lposaux2.filter(_ > -1)
    val lposaux4 = lposaux3.distinct
    lposaux4
	}*/
	// Función que devuelve la posición si esta es igual, o -1 si no es igual. El -1 lo eliminará antes de quitarRepetidos.
	def comprobarIgualesArriba(pos:Int,tablero:List[Int],width:Int): Boolean = {
	  if((pos-width>0) && (tablero(pos)==tablero(pos-width))) true
	  else false
	}
	def comprobarIgualesAbajo(pos:Int,tablero:List[Int],width:Int): Boolean = {
	  if((pos+width<tablero.length) && (tablero(pos)==tablero(pos+width))) true
	  else false
	}
	def comprobarIgualesDerecha(pos:Int,tablero:List[Int],width:Int,columna:Int): Boolean = {
	  if((columna+1<width) && (tablero(pos)==tablero(pos+1))) true
	  else false
	}
	def comprobarIgualesIzquierda(pos:Int,tablero:List[Int],width:Int,columna:Int): Boolean = {
	  if((columna-1>0) && (tablero(pos)==tablero(pos-1))) true
	  else false
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