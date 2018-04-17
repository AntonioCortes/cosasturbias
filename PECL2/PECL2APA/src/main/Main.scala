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
		val listaMuerte =  buscarIguales(tablero, List(pos),List[Int]() ,List[Int](),numColumnas)
		println(listaMuerte)
		val tableroFin=eliminarIguales(tablero, listaMuerte)
		dibujarTablero(tableroFin, numColumnas, 0, 0, true)
		juego(tableroFin, numFilas, numColumnas, dificultad, numVidas)
  }
	
	
	def mejorJugada(tablero:List[Int],pos:Int,columna:Int,width:Int,mPuntacion:Int,mPos:Int):Int={
	  if (pos+3>=tablero.length) mPos
	  else{
	    if(tablero(pos)==0) mejorJugada(tablero, pos+1, columna, width, mPuntacion, pos)
	    else{
	      val puntAux=buscarIguales(tablero, List(pos),List[Int](),List[Int](),width)
	      if(puntAux.length<mPuntacion) mejorJugada(tablero, pos+1, columna, width, mPuntacion, mPos)
	      else mejorJugada(tablero, pos+1, columna, width, puntAux.length, pos)
	  }
	}
	}
	
	
	
	def buscarIguales(tablero:List[Int],posExpandir:List[Int],posEliminar:List[Int],posVisitar:List[Int],width:Int): List[Int]={
	  if(posExpandir.isEmpty) posEliminar
	  else{
      	  val listaExpandida= algoritmoEstrella(tablero,posExpandir.head,width)
      	  //Aquí elimino iguales ordeno y tpda esa basura.
      	  //val listaExpandida1= unirListas(posExpandir, listaExpandida)
      	  val listaExpandida2= listaExpandida.distinct
      	  val posVisitadosAux= añadirElementos(posVisitar,List(listaExpandida2.head))
      	  val listaExpandida3= añadirElementos(posEliminar, listaExpandida2)
      	  val listaExpandida4= limpiarLista1000(listaExpandida3)
      	  if (listaExpandida4==posEliminar){
      	    val l1= ordenarLista(posEliminar)
      	    val l2= ordenarLista(posVisitadosAux)
      	    if (l1==l2) posEliminar
      	    else{ //Busco con el primer elemento no visitado
      	      val elemento= elementoNoIgual(posVisitadosAux,posEliminar)
      	      buscarIguales(tablero,List(elemento),posEliminar,posVisitadosAux,width)
      	    }
      	  }
      	 else buscarIguales(tablero,listaExpandida3.tail,listaExpandida3.distinct,posVisitadosAux,width)
	}
	}
	
	def algoritmoEstrella(tablero:List[Int],pos:Int,width:Int):List[Int]={
	  val arriba= comprobarIgualesArriba(pos, tablero, width)
	  val abajo = comprobarIgualesAbajo(pos, tablero, width)
	  val derecha = comprobarIgualesDerecha(pos, tablero, width)
	  val izquierda = comprobarIgualesIzquierda(pos, tablero, width)
	  val lposaux = List(pos)
    val lposaux2 = lposaux:+arriba:+abajo:+derecha:+izquierda
    val lposaux3 = limpiarLista(lposaux2)
    val lposaux4 = lposaux3.distinct
	  return lposaux4
	}
	
	def comprobarIgualesArriba(pos:Int,tablero:List[Int],width:Int): Int = {
   if((pos-width>0) && (tablero(pos)==tablero(pos-width))) pos-width
 	  else -1
 	}
 	def comprobarIgualesAbajo(pos:Int,tablero:List[Int],width:Int): Int = {
 	  if((pos+width<tablero.length) && (tablero(pos)==tablero(pos+width))) pos+width
 	  else -1
 	}
 	def comprobarIgualesDerecha(pos:Int,tablero:List[Int],width:Int): Int = {
 	  val n1= pos.toFloat/width
	  val n2= scala.math.round(n1)
 	  if((n2+1!=width) && (tablero(pos)==tablero(pos+1))) pos+1
 	  else -1
 	}
	def comprobarIgualesIzquierda(pos:Int,tablero:List[Int],width:Int): Int = {
	  val n1= pos%width
 	  if((n1 != 0) && (tablero(pos)==tablero(pos-1))) pos-1
  	  else -1
  	}
	

	
	def unirListas(l1:List[Int],l2:List[Int]):List[Int]={
	  if(l2.isEmpty) l1
	  else unirListas(l1:+l2.head,l2.tail)
	}
	
	def eliminarListasVacias(lista: List[Any]): List[Any] =
  {
    if(lista.nonEmpty)
    {
      if(lista.head == List()) eliminarListasVacias(lista.tail)
      else lista.head :: eliminarListasVacias(lista.tail)
    }
    else
    {
      lista
    }
  }
	
	def limpiarLista(lista: List[Int]): List[Int] =
  {
    if(lista.nonEmpty)
    {
      if(lista.head == -1) limpiarLista(lista.tail)
      else lista.head :: limpiarLista(lista.tail)
    }
    else
    {
      lista
     }
  }
	def limpiarLista1000(lista: List[Int]): List[Int] =
  {
    if(lista.nonEmpty)
    {
      if(lista.head == 1000) limpiarLista(lista.tail)
      else lista.head :: limpiarLista(lista.tail)
    }
    else
    {
      lista
     }
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
	def getElemento(tablero:List[Int],pos:Int,posActual:Int): Int = {
  			if(tablero.isEmpty) 0
  			else if(posActual==pos) tablero.head
  			else getElemento(tablero.tail, pos, posActual+1)
  	}
	
	
	def elementosDiferentes(lista: List[Int], elementosPorMeter: List[Int]): List[Int] =
  {
    if(elementosPorMeter.nonEmpty)
    {
      if(elementoEnLista(lista, elementosPorMeter.head)) elementosDiferentes(lista, elementosPorMeter.tail)
      else elementosPorMeter.head :: elementosDiferentes(lista, elementosPorMeter.tail)
    }
    else
    {
      Nil
    }
  }
	def añadirElementos(lista: List[Int], elementosPorMeter: List[Int]): List[Int] =
  {
    lista ::: elementosDiferentes(lista, elementosPorMeter)
  }
	
	def elementoNoIgual(lista1:List[Int],lista2:List[Int]):Int={
	  if(lista2.isEmpty) 1000
	  else{
	    if(elementoEnLista(lista1, lista2.head)==false) lista2.head
	  else elementoNoIgual(lista1, lista2.tail)
	  }
	}
	def elementoEnLista(lista: List[Int], elemento: Int): Boolean =
  {
    if(lista.nonEmpty)
    {
      if(lista.head == elemento) true
      else elementoEnLista(lista.tail, elemento)
    }
    else
    {
      false
    }
  }
	
	
	def menor(valor1: Int, valor2: Int): Int = if(valor1 < valor2) valor1 else valor2
                                                  
  def minimo(lista: List[Int]):Int =
  {
    if(lista.tail.nonEmpty)
    {
      menor(lista.head, minimo(lista.tail))
    }
    else
    {
      lista.head
    }
  }                                         
  
  def eliminarValor(lista: List[Int], valor: Int): List[Int] =
  {
    if(lista.nonEmpty)
    {
      if(lista.head == valor) eliminarValor(lista.tail, valor)
      else lista.head :: eliminarValor(lista.tail, valor)
    }
    else
    {
      lista
    }
  }                                         
  
  def ordenarLista(lista: List[Int]): List[Int] =
  {
    if(lista.nonEmpty)
    {
      minimo(lista) :: ordenarLista(eliminarValor(lista, minimo(lista)))
    }
    else
    {
      lista
    }
  }
}