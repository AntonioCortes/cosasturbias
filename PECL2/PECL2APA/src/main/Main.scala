package main


import scala.io.StdIn
import util.Random
import java.io._

/**
 * AZUL 1
 * ROJO 2
 * NARANJA 3
 * VERDE 4
 * PLATA 5
 * MORADO 6
 * BOMBA VERTICAL 7
 * BOMBA HORIZONTAL 8
 * TNT 9
 * BOMBAPUZLE AZUL 10
 * BOMBAPUZLE ROJO 11 
 * BOMBAPUZLE NARANJA 12 
 * BOMBAPUZLE VERDE 13 
 * BOMBAPUZLE PLATA 14
 * BOMBAPUZLE MORADO 15 
 */

object Main  extends App
{
  val cargaPartida = preguntarCargarPartida()
  val dificultad = asignarDificultad(cargaPartida)
  val numFilas = asignarNumFilas(dificultad)
  val numColumnas = asignarNumColumnas(dificultad)
  val numColores = asignarNumColores(dificultad)
  val numVidas = asignarVidas(cargaPartida)
  val tablero = asignarTablero(cargaPartida, numFilas, numColumnas, numColores)
  val puntos = asignarPuntuacion(cargaPartida)
  juego(tablero, numFilas, numColumnas, dificultad, numVidas, numColores, puntos)
  
  def preguntarCargarPartida(): Boolean = 
  {
    println("1 - Nueva Partida\n2 - Cargar Partida")
    
    val opcion = StdIn.readInt()
    
    if(opcion == 1) false
    else if(opcion == 2) 
    {
      val ficheroDatos = new File("Datos Partida.txt")
      val ficheroTablero = new File("tablero.dat")
      val ficheroPuntos = new File("puntos.dat")
      
      if(ficheroDatos.exists && ficheroTablero.exists && ficheroPuntos.exists)
      {
        true
      }
      else
      {
        println("ERROR: Aún no hay ninguna partida guardada, vuelva a elegir opción")
        
        preguntarCargarPartida()
      }
      
    }
    else 
    {
      println("Opción incorrecta, vuelva a elegir opción")
      
      preguntarCargarPartida()
    }
  }
  
  //Ofrecela opcion de elegir entre jugar, guardar la partida o salir del juego, y en caso de elegir guardar, la guarda
  def preguntarQueHacer(tablero: List[Int], dificultad: Int, puntos: List[Int], numVidas: Int): Boolean =
  {
    println("1 - Jugar\n2 - Guardar Partida\n3 - Salir")
    
    val opcion = StdIn.readInt()
    
    if((opcion != 1) && (opcion != 2) && (opcion != 3)) 
    {
      println("Opción incorrecta, vuelva a elegir opción")
      
      preguntarQueHacer(tablero, dificultad, puntos, numVidas)
    }
    else if(opcion == 1) false
    else if(opcion == 2) 
    {
      
      val ficheroDatos = new File("Datos Partida.txt")
      val writerDatos = new BufferedWriter(new FileWriter(ficheroDatos))
      try
      {
        writerDatos.write(s"$dificultad\n$numVidas")
      }
      finally
      {
        writerDatos.close()
      }
      
      val ficheroTablero = new File("tablero.dat")
      val salidaTablero = new ObjectOutputStream(new FileOutputStream(ficheroTablero))
      
      try
      {
        salidaTablero.writeObject(tablero)
      }
      finally
      {
        salidaTablero.close()
      }
      
      val ficheroPuntos = new File("puntos.dat")
      val salidaPuntos = new ObjectOutputStream(new FileOutputStream(ficheroPuntos))
      
      try
      {
        salidaPuntos.writeObject(puntos)
      }
      finally
      {
        salidaPuntos.close()
      }
      
      println("Partida guardada")
      preguntarQueHacer(tablero, dificultad, puntos, numVidas)
    }
    else true
  }
  
  //si se ha seleccionado cargar partida, devuelve la dificultad guardada en el fichero, si no, se pide al usuario que
  //introduzca la dificultad y devuelve dicha dificultad
  def asignarDificultad(cargarPartida: Boolean): Int = 
  {
    if(cargarPartida)
    {
      val fichero = new File("Datos Partida.txt")
      val entrada = new BufferedReader(new FileReader(fichero))
      
      try
      {
        entrada.readLine().toInt
      }
      finally
      {
        entrada.close()
      }
    }
    else pedirDificultad()
  }
  
  //si se ha elegido la opcion cargar devuelve el numero de vidas guardadas en el fichero, si no, devuelve el valor 5
  def asignarVidas(cargarPartida: Boolean): Int =
  {
    if(cargarPartida)
    {
      val fichero = new File("Datos Partida.txt")
      val entrada = new BufferedReader(new FileReader(fichero))
      
      try
      {
        entrada.readLine()
        entrada.readLine().toInt
      }
      finally
      {
        entrada.close()
      }
      
    }
    else 5
  }
  
  //si se ha elegido cargar partida devuelde el tablero guardado en el fichero, si no genera uno nuevo y lo devuelve
  def asignarTablero(cargarPartida: Boolean, numFilas: Int, numColumnas: Int, numColores: Int): List[Int] = 
  {
    if(cargarPartida)
    {
      val fichero = new File("tablero.dat")
      val entrada = new ObjectInputStream(new FileInputStream(fichero))
      
      try
      {
        entrada.readObject().asInstanceOf[List[Int]]
      }
      finally
      {
        entrada.close()
      }
    }
    else
    {
      crearLista(numFilas * numColumnas, numColores)
    }
  }
  
  //si se ha elegido la opcion cargar devuelve una lista con la puntuacion guardada en el fichero, si no devuelve
  //una lista de puntuaciones con todos los valores a 0
  def asignarPuntuacion(cargarPartida: Boolean): List[Int] = 
  {
    if(cargarPartida)
    {
      val fichero = new File("puntos.dat")
      val entrada = new ObjectInputStream(new FileInputStream(fichero))
      
      try
      {
        entrada.readObject().asInstanceOf[List[Int]]
      }
      finally
      {
        entrada.close()
      }
    }
    else
    {
      List(0,0,0,0,0,0)
    }   
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
  
  //pide al usuario el nivel de dificultad y devuelve el nivel de dificultad introducido
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
	
	//funcion auxiliar de dibujarTablero que ayuda a dicha funcion a saber si debe saltar a la siguiente fila o no
	def auxFila(lista: List[Int], numColumnas: Int, fila: Int): Int = if(lista.length % numColumnas == 0) fila + 1 else fila

	//dibuja el tablero por pantalla
	def dibujarTablero(tablero: List[Int], numColumnas: Int, fila: Int, columna: Int, dibujarColumnas: Boolean): Unit =
	{	
	  if (dibujarColumnas) //si hay que dibujar el numero que indica cada columna 0 , 1, 2, 3, ..., anchuraMatriz
	  {
	    if(columna == 0) print(s"    ")
	  
	    if(columna == numColumnas) //si ya se han dibujado todos los numero que representan cada columna
	    {
	      print("\n\n0   ")
	      dibujarTablero(tablero, numColumnas, fila, columna, false)
	    }
	    else if((columna < numColumnas) && (columna < 10))// si aun no se han representado todos los numeros que representan 	                                                       
		  {                                                  //a cada columna y el numero de columna solo tiene un digito 
			  print(s"$columna  ")
			  
			  
			  dibujarTablero(tablero, numColumnas, fila, columna + 1, true)
		  }
		  else if(columna < numColumnas)//si aun no se han representado todos los numeros que representan 	                                                       
		  {                                                  //a cada columna y el numero de columna tiene mas de un digito
			  print(s"$columna ") 
			  dibujarTablero(tablero, numColumnas, fila, columna + 1, true)
		  }
	  }
	  else if(tablero.nonEmpty)
		{
			print(devolverElemento(tablero.head)) //dibujar la letra equivalente al numero en la posicion del tablero correspondiente
			
			if(tablero.tail.length % numColumnas == 0 && tablero.tail.nonEmpty) //si estamos en la ultima columna y por lo tanto 
			{                                                                   //en la siguiente iteracion hay que cambiar de fila
			  if(fila + 1 < 10) print(s"\n${fila + 1}   ") //si el numero de fila tiene un digito, lo dibuja, y deja 3 espacios
			  else print(s"\n${fila + 1}  ")//si el numero de fila tiene dos digitos, lo dibuja y deja 2 espacios
			}
			
			dibujarTablero(tablero.tail, numColumnas, auxFila(tablero.tail, numColumnas, fila), columna,false)
		}
		else
		{
		  println()
		}
	}
	
	//devuelve la letra correspondiente a cada color o bomba
	def devolverElemento(n:Int): String =  n match {
	  case 1 => "A  "
	  case 2 => "R  "
	  case 3 => "N  "
	  case 4 => "V  "
	  case 5 => "P  "
	  case 6 => "M  "
	  case 7 => "Bv "
	  case 8 => "BH "
	  case 9 => "TNT"
	  case 10 => "PA "
	  case 11 => "PR "
	  case 12 => "PN "
	  case 13 => "PV "
	  case 14 => "PP "
	  case 15 => "PM "
	  case _ => "   "
	}

	//escribe por pantalla la lista de puntos acumulados
	def mostrarPuntos(puntos: List[Int], dificultad: Int): Unit = dificultad match
	{
	  case 1 => print(s"PUNTOS: Azul: ${getElemento(puntos, 0, 0)} Rojo: ${getElemento(puntos, 1, 0)} Naranja: ${getElemento(puntos, 2, 0)} Verde: ${getElemento(puntos, 3, 0)}\n\n")
	  case 2 => print(s"PUNTOS: Azul: ${getElemento(puntos, 0, 0)} Rojo: ${getElemento(puntos, 1, 0)} Naranja: ${getElemento(puntos, 2, 0)} Verde: ${getElemento(puntos, 3, 0)} Plata: ${getElemento(puntos, 4, 0)}\n\n")
	  case 3 => print(s"PUNTOS: Azul: ${getElemento(puntos, 0, 0)} Rojo: ${getElemento(puntos, 1, 0)} Naranja: ${getElemento(puntos, 2, 0)} Verde: ${getElemento(puntos, 3, 0)} Plata: ${getElemento(puntos, 4, 0)} Morado: ${getElemento(puntos, 5, 0)}\n\n")
	}
	
	//actualiza la lista de puntos acumulados
	def actualizarPuntos(listaPosiciones: List[Int], puntos: List[Int], color: Int, tablero: List[Int], esBomba: Boolean): List[Int] = 
	{
	  if(esBomba && listaPosiciones.nonEmpty)//si la posicion seleccionada es bomba y la lista de posiciones a explotar no es vacia
	  {
	    val col = getElemento(tablero, listaPosiciones.head, 0)//color del primer elemento de la lista de posiciones a explotar
	    
	    val puntuacionActualizada = poner((col - 1),(getElemento(puntos, (col - 1), 0) + 1) , puntos)// suma un punto al color del primer elemento de la lista de posiciones a explotar
	    
	    actualizarPuntos(listaPosiciones.tail, puntuacionActualizada, color, tablero, esBomba)
	  }
	  else if(esBomba && listaPosiciones.isEmpty) puntos //si la posicion seleccionada es bomba  y ya no quedan posiciones por explotar, se devuelve la lista de puntos
	  else
	  {
	    val puntuacion = listaPosiciones.length
	    if(puntuacion < 1) puntos
	    else
	    {
	      poner((color - 1),(getElemento(puntos, (color - 1), 0) + puntuacion) , puntos)   
	    }	    
	  }	  
	}
	
	//si el tablero antes de elegir una posicion es igual que el tablero despues de elegir la posicion significa que no se ha explotado
	//ningun bloque y por lo tanto se pierde una vida, en cuyo caso la funcion devuelve el numero de vidas - 1, en caso contrario devuelve el numero de vidas
	def actualizarVidas(tableroOriginal: List[Int], tableroFinal: List[Int], numVidas: Int): Int = 
	{
	  if(tableroOriginal == tableroFinal) 
	  {
	    println("\n Posicion equivocada, vuelva a elegir, posicion")
	    numVidas - 1 
	  }
	  else numVidas
	}
	
	//esta funcion determina si se cumplen las cndiciones necesarias para terminar el juego(ganar partida o quedarse sin vidas)
	//y devuelve true en caso de que se cumplan las condiciones o devuelve false en caso contrario
	def terminarJuego(numVidas: Int, dificultad: Int, puntos: List[Int]): Boolean =
	{
	  val ptsAzul = getElemento(puntos, 0, 0)
	  val ptsRojo = getElemento(puntos, 1, 0)
	  val ptsNaranja = getElemento(puntos, 2, 0)
	  val ptsVerde = getElemento(puntos, 3, 0)
	  val ptsPlata = getElemento(puntos, 4, 0)
	  val ptsMorado = getElemento(puntos, 5, 0)
	  
	  if(numVidas == 0)
	  {
	    println("\n\n*************")
	    println("* GAME OVER *")
	    println("*************")
	    
	    true
	  }
	  else if((dificultad == 1) && (ptsAzul >= 20) && (ptsRojo >= 20) && (ptsNaranja >= 20) && (ptsVerde >= 20))
	  {
	    println("\n\n**************")
	    println("* HAS GANADO *")
	    println("**************")
	    
	    true
	  }  
	  else if((dificultad == 2) && (ptsAzul >= 15) && (ptsRojo >= 15) && (ptsNaranja >= 15) && (ptsVerde >= 15) && (ptsPlata >= 15))
	  {
	    println("\n\n**************")
	    println("* HAS GANADO *")
	    println("**************")
	    
	    true
	  }    
	  else if((dificultad == 3) && (ptsAzul >= 10) && (ptsRojo >= 10) && (ptsNaranja >= 10) && (ptsVerde >= 10) && (ptsPlata >= 10) && (ptsMorado >= 10))
	  {
	    println("\n\n**************")
	    println("* HAS GANADO *")
	    println("**************")
	    
	    true
	  }
	  else
	  {
	    false
	  }
	  
	}
	
	//si la posicion elegida por el jugador es una bomba, devuleve una lista de posiciones a explotar por la bomba, si no lo es
	//devuelve la lista de posiciones a eliminar (eliminacion normal de bloques conriguos) recibida por parametro
	def comprobarListaPos(tablero: List[Int], listaPosiciones: List[Int], esBomba: Boolean, posicion: Int, numColumnas: Int): List[Int] = 
	{
	  if(esBomba)
	  {
	    explotarBomba(tablero, posicion, numColumnas)//devuelve la lista de posiciones a explotar por la bomba
	  }
	  else if(listaPosiciones.length == 1) List[Int]()
	  else listaPosiciones
	}
	
	//en funcion de la posicion elegida por el usuario, ejecuta las acciones correspondientes, explotar la bomba, en caso de 
	//tratarse de una bomba
  def ejecutar(tablero: List[Int], posicion: Int, numColumnas: Int, numColores: Int): List[Int] =
  {
    val columna = averiguarColumna(posicion, numColumnas)

    if (esBomba(tablero(posicion))) 
    {
      val listaPos = explotarBomba(tablero, posicion, numColumnas)//devuelve la lista de posiciones a eliminar por la bomba
      val tablero2 = eliminarIguales(tablero, listaPos, true)//elimina los elementos del tablero correspondientes a las posiciones de la lista de posiciones a eliminar

      animacion(listaPos, tablero2, numColumnas, numColores)
    } 
    else
    {
      val listpos = List[Int]()
      val listaPos = buscarIguales(tablero, List(posicion),List[Int]() ,List[Int](),numColumnas)
      val listaPosFin = comprobarListaPos(tablero, listaPos, esBomba(getElemento(tablero, posicion, 0)), posicion, numColumnas)
      val tablero2 = eliminarIguales(tablero, listaPosFin, true)
      val tablero3 = ponerBomba(tablero2, posicion, listaPosFin, numColumnas, numColores)      

      animacion(listaPos, tablero3, numColumnas, numColores)
    }
  }
  
	def juego(tablero: List[Int],numFilas: Int, numColumnas: Int, dificultad: Int, numVidas: Int, numColores: Int, puntos: List[Int]): Unit =
	{
	  val mPunt= mejorJugada(tablero, 0, 0, numColumnas,0, 0)
	  
	  println()
		print(s"Vidas: $numVidas Dificultad: $dificultad ")	
		mostrarPuntos(puntos, dificultad)		
		dibujarTablero(tablero, numColumnas, 0, 0, true)
		
		println(s"\nLa máquina recomienda como mejor posición: fila: ${averiguarFila(mPunt, numColumnas)} columna: ${averiguarColumna(mPunt, numColumnas)}\n")

		val salir = preguntarQueHacer(tablero, dificultad, puntos, numVidas)
		
		if(salir) 
		{
		  println("Saliendo del juego...")
		  System.exit(0)
		}
		
		val fila = pedirFila(numFilas)
		val columna = pedirColumna(numColumnas)		
		val pos= fila*numColumnas+columna
		val color = getElemento(tablero, pos, 0)
		val tableroFin = ejecutar(tablero, pos, numColumnas, numColores)
		val listaPos = buscarIguales(tablero, List(pos),List[Int]() ,List[Int](),numColumnas)		
		val puntosActualizados = actualizarPuntos(comprobarListaPos(tablero, listaPos, esBomba(color), pos, numColumnas), puntos, color, tablero, esBomba(color))
		val vidasActualizadas = actualizarVidas(tablero, tableroFin, numVidas)

		if(!terminarJuego(vidasActualizadas, dificultad, puntosActualizados)) 
		{
		  juego(tableroFin, numFilas, numColumnas, dificultad, vidasActualizadas, numColores, puntosActualizados)
		}
	}
	
	//Esta funcion calculará la mejor jugada cogiendo todas las posibles combinaciones del tablero y sacando la mejor.
	def mejorJugada(tablero:List[Int],pos:Int,columna:Int,width:Int,mPuntacion:Int,mPos:Int):Int={
	  if (pos+3>=tablero.length) mPos//Se sale de rango
	  else{
	    if(getElemento(tablero, pos, 0)==0) mejorJugada(tablero, pos+1, columna, width, mPuntacion, pos) //Si la posicion es 0 no me sirve
	    else{
	      val puntAux=buscarIguales(tablero, List(pos),List[Int](),List[Int](),width)//Puntuación sacada de esa posición
	      if(puntAux.length<mPuntacion) mejorJugada(tablero, pos+1, columna, width, mPuntacion, mPos) //Si la puntuación es menor, no lo tengo en cuenta y sigo avanzando
	      else mejorJugada(tablero, pos+1, columna, width, puntAux.length, pos) //Si no, es que es mayor o igual, entonces guardo esa puntuación y sigo avanzando.
	  }
	}
	}
	
	
	//Función que buscará las combinaciones posibles
	def buscarIguales(tablero:List[Int],posExpandir:List[Int],posEliminar:List[Int],posVisitar:List[Int],width:Int): List[Int]={
	  if(posExpandir.isEmpty) posEliminar
	  else{
      	  val listaExpandida= algoritmoEstrella(tablero,posExpandir.head,width)//Ejecuta el algoritmo de expansión en estrlla.
      	  val listaExpandida2= listaExpandida.distinct //Quito iguales
      	  val posVisitadosAux= añadirElementos(posVisitar,List(listaExpandida2.head))//Añado el nodo a la lista de posciones visitadas
      	  val listaExpandida3= añadirElementos(posEliminar, listaExpandida2) //Añado la lista a las posiciones a eliminar puesto que son iguales.
      	  val listaExpandida4= limpiarLista1000(listaExpandida3) //Elimino residuos, marcados como 1000
      	  if (listaExpandida4==posEliminar){ //Si la lista de eliminados es igual que la que ya tenía es que he llegado a punto muerto
      	    val l1= ordenarLista(posEliminar)
      	    val l2= ordenarLista(posVisitadosAux)
      	    if (l1==l2) posEliminar //Si las dos listas son iguales, es que he llegado a punto muerto y visitado todos los nodos, entonces
      	    //tengo ya todas las combinaciones
      	    else{ //Busco con el primer elemento no visitado que me falta
      	      val elemento= elementoNoIgual(posVisitadosAux,posEliminar)
      	      buscarIguales(tablero,List(elemento),posEliminar,posVisitadosAux,width)
      	    }
      	  }
      	 else buscarIguales(tablero,listaExpandida3.tail,listaExpandida3.distinct,posVisitadosAux,width)
	}
	}
	
	def algoritmoEstrella(tablero:List[Int],pos:Int,width:Int):List[Int]={
	  //algoritmo que me saca todas las posiciones del algoritmo estrella tal que así:
	  /*              |
	   *          -- Pos --
	   *              | 
	   */
	  val arriba= comprobarIgualesArriba(pos, tablero, width)
	  val abajo = comprobarIgualesAbajo(pos, tablero, width)
	  val derecha = comprobarIgualesDerecha(pos, tablero, width)
	  val izquierda = comprobarIgualesIzquierda(pos, tablero, width)
	  val lposaux = List(pos)//Añado la posicion
    val lposaux2 = lposaux:+arriba:+abajo:+derecha:+izquierda //Añado las posiciones
    val lposaux3 = limpiarLista(lposaux2) //Quito los menos uno
    val lposaux4 = lposaux3.distinct //Quito iguales y devuelvo
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
 	  if((n2+1!=width) && (pos+1<tablero.length) && (tablero(pos)==tablero(pos+1))) pos+1
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
	def eliminarIguales(tablero:List[Int],lpos:List[Int], primeraI:Boolean): List[Int] = {
	  if(lpos.length>1){
	    val tableroaux= poner(lpos.head,0,tablero)
	    eliminarIguales(tableroaux, lpos.tail,false)
	  }
	  else if(lpos.length==1 && primeraI==false){
	    val tableroaux= poner(lpos.head,0,tablero)
	    return tableroaux
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

  def bajarColumna(numColores: Int, fila: Int, columna: Int, numColumnas: Int, tablero: List[Int]): List[Int] =
  {
    val posicion = fila * numColumnas + columna
    val arriba = (fila - 1) * numColumnas + columna

    if ((fila == 0) && (tablero(posicion) == 0))
    {
      val color = Random.nextInt(numColores) + 1

      poner(posicion, color, tablero)
    } 
    else if (arriba >= 0) 
    {
      if (tablero(arriba) == 0) {
        bajarColumna(numColores, fila - 1, columna, numColumnas, tablero)
      } 
      else if (tablero(posicion) == 0) 
      {
        val nuevoTablero = poner(posicion, tablero(arriba), tablero)

        if (fila != 0) bajarColumna(numColores, fila - 1, columna, numColumnas, poner(arriba, 0, nuevoTablero))
        else bajarColumna(numColores, fila, columna, numColumnas, nuevoTablero)
      } 
      else 
      {
        tablero
      }

    } 
    else 
    {
      tablero
    }
  }

  def averiguarColumna(posicion: Int, numColumnas: Int): Int =
  {
    if ((posicion >= 0) && (posicion < numColumnas)) 
    {
       posicion
    } 
    else 
    {
      averiguarColumna(posicion - numColumnas, numColumnas)
    }
  }

  def averiguarFila(posicion: Int, numColumnas: Int): Int =
  {
    (posicion / numColumnas).toInt
  }

  def mismaColumna(pos1: Int, pos2: Int, numColumnas: Int): Boolean =
  {
    if (averiguarColumna(pos1, numColumnas) == averiguarColumna(pos2, numColumnas)) true
    else false
  }

  //devuelve una lista con los elementos pertenecientes a la columna especificada
  def listaColumna(lista: List[Int], columna: Int, numColumnas: Int): List[Int] =
  {
    if (lista.nonEmpty) 
    {
      if (mismaColumna(lista.head, columna, numColumnas)) lista.head :: listaColumna(lista.tail, columna, numColumnas)
      else listaColumna(lista.tail, columna, numColumnas)
    } 
    else lista
  }

  //devuelve una lista con las posiciones mas bajas pertenecientes a cada columna de la lista recibe por parametro
  def separarEnColumnas(lista: List[Int], numColumnas: Int, columna: Int): List[Int] =
  {
    if (columna < numColumnas)
    {
      val listaCol = listaColumna(lista, columna, numColumnas)

      if (listaCol.nonEmpty) listaCol.max :: separarEnColumnas(lista, numColumnas, columna + 1)
      else separarEnColumnas(lista, numColumnas, columna + 1)
    } 
    else
    {
      Nil
    }
  }

  def bajarCeros(lista: List[Int], tablero: List[Int], numColumnas: Int, numColores: Int): List[Int] =
  {
    val lista2 = separarEnColumnas(lista, numColumnas, 0)

    if (lista2.nonEmpty)
    {
      val fila = averiguarFila(lista.head, numColumnas)
      val columna = averiguarColumna(lista.head, numColumnas)
      val tablero2 = bajarColumna(numColores, fila, columna, numColumnas, tablero)

      bajarCeros(lista2.tail, tablero2, numColumnas, numColores)
    } 
    else 
    {
      tablero
    }
  }

  def listaCeros(tablero: List[Int], posicion: Int): List[Int] =
  {
    if (tablero.nonEmpty) 
    {
      if (tablero.head == 0) posicion :: listaCeros(tablero.tail, posicion + 1)
      else listaCeros(tablero.tail, posicion + 1)
    } 
    else 
    {
      tablero
    }
  }

  def hayCeros(lista: List[Int]): Boolean =
  {
    if (lista.isEmpty) false
    else if (lista.head == 0) true
    else hayCeros(lista.tail)
  }

  def animacion(listaPosiciones: List[Int], tablero: List[Int], numColumnas: Int, numColores: Int): List[Int] =
  {
    dibujarTablero(tablero, numColumnas, 0, 0, true)
    println()

    if (hayCeros(tablero)) {
      val tablero2 = bajarCeros(listaPosiciones, tablero, numColumnas, numColores)
      val listCeros = separarEnColumnas(listaCeros(tablero2, 0), numColumnas, 0)

      Thread.sleep(150)
      
      animacion(listCeros, tablero2, numColumnas, numColores)
    } 
    else 
    {
      tablero
    }
  }

  def elegirBomba(listaPosiciones: List[Int], numColores: Int, tablero: List[Int], posicion: Int): Int =
  {
    if (listaPosiciones.length < 2) getElemento(tablero, posicion, 0)
    else if ((listaPosiciones.length < 5) && (listaPosiciones.length > 1)) 0
    else if (listaPosiciones.length == 5) Random.nextInt(2) + 7
    else if (listaPosiciones.length == 6) 9
    else Random.nextInt(numColores) + 10
  }
  
  def bajarElemento(tablero: List[Int], posicion: Int, numColumnas: Int): List[Int] =
  {
    val abajo = posicion + numColumnas
    
    if ((abajo < tablero.length) && (tablero(posicion) != 0))
    {
      if (tablero(abajo) == 0) 
      {
        val tablero2 = poner(abajo, tablero(posicion), tablero)
        val tablero3 = poner(posicion, 0, tablero2)

        bajarElemento(tablero3, abajo, numColumnas)
      } 
      else
      {
        tablero
      }
    } 
    else
    {
      tablero
    }
  }

  def esBomba(valor: Int): Boolean = if (valor > 6) true else false

  def ponerBomba(tablero: List[Int], posicion: Int, listaPosiciones: List[Int], numColumnas: Int, numColores: Int): List[Int] =
  {
    val bomba = elegirBomba(listaPosiciones, numColores, tablero, posicion)
    val tablero2 = poner(posicion, bomba, tablero)

    bajarElemento(tablero2, posicion, numColumnas)
  }

  def explosionHorizontal(tablero: List[Int], fila: Int, tamTablero: Int, numColumnas: Int): List[Int] =
  {
    val posicion = tamTablero - tablero.length
    val filaActual = averiguarFila(posicion, numColumnas)

    if (tablero.nonEmpty)
    {
      if (filaActual == fila) posicion :: explosionHorizontal(tablero.tail, fila, tamTablero, numColumnas)
      else explosionHorizontal(tablero.tail, fila, tamTablero, numColumnas)
    } 
    else 
    {
      Nil
    }
  }

  def explosionVertical(tablero: List[Int], columna: Int, tamTablero: Int, numColumnas: Int): List[Int] =
  {
    val posicion = tamTablero - tablero.length
    val columnaActual = averiguarColumna(posicion, numColumnas)
    if (tablero.nonEmpty) 
    {
      if (columnaActual == columna) posicion :: explosionVertical(tablero.tail, columna, tamTablero, numColumnas)
      else explosionVertical(tablero.tail, columna, tamTablero, numColumnas)
    } 
    else 
    {
      Nil    
    }
  }

  def explosionTNT(tablero: List[Int], posSeleccionada: Int, tamTablero: Int, numColumnas: Int): List[Int] =
  {
    val posicion = tamTablero - tablero.length
    val filaPosSel = averiguarFila(posSeleccionada, numColumnas)
    val colPosSel = averiguarColumna(posSeleccionada, numColumnas)
    val arribaPosSel = (filaPosSel - 1) * numColumnas + colPosSel
    val abajoPosSel = (filaPosSel + 1) * numColumnas + colPosSel
    val izPosSel = filaPosSel * numColumnas + (colPosSel - 1)
    val derPosSel = filaPosSel * numColumnas + (colPosSel + 1)
    val arrDerPosSel = (filaPosSel - 1) * numColumnas + (colPosSel + 1)
    val arrIzPosSel = (filaPosSel - 1) * numColumnas + (colPosSel - 1)
    val abDerPosSel = (filaPosSel + 1) * numColumnas + (colPosSel + 1)
    val abIzPosSel = (filaPosSel + 1) * numColumnas + (colPosSel - 1)

    if (tablero.nonEmpty) 
    {
      if ((posicion == posSeleccionada) || (posicion == arribaPosSel) || (posicion == abajoPosSel) || (posicion == izPosSel) || (posicion == derPosSel)) 
      {
        posicion :: explosionTNT(tablero.tail, posSeleccionada, tamTablero, numColumnas)
      } 
      else if ((posicion == arrDerPosSel) || (posicion == arrIzPosSel) || (posicion == abDerPosSel) || (posicion == abIzPosSel)) 
      {
        posicion :: explosionTNT(tablero.tail, posSeleccionada, tamTablero, numColumnas)
      } 
      else 
      {
        explosionTNT(tablero.tail, posSeleccionada, tamTablero, numColumnas)
      }
    } 
    else
    {
      Nil
    }
  }
  
  def explotarPuzle(tablero: List[Int], tipoPuzle: Int): List[Int] = tipoPuzle match
  {
    case 10 => explosionPuzle(tablero, 1, 0)
    case 11 => explosionPuzle(tablero, 2, 0)
    case 12 => explosionPuzle(tablero, 3, 0)
    case 13 => explosionPuzle(tablero, 4, 0)
    case 14 => explosionPuzle(tablero, 5, 0)
    case _ => explosionPuzle(tablero, 6, 0)
  }
  
  def explosionPuzle(tablero: List[Int], color: Int, posActual: Int): List[Int] =
  {
    if (tablero.nonEmpty) {
      if ((tablero.head == color) || (tablero.head > 9)) posActual :: explosionPuzle(tablero.tail, color, posActual + 1)
      else explosionPuzle(tablero.tail, color, posActual + 1)
    } 
    else 
    {
      Nil
    }
  }

  def explotarBomba(tablero: List[Int], posicion: Int, numColumnas: Int): List[Int] =
  {
    val tipoBomba = tablero(posicion)
    val fila = averiguarFila(posicion, numColumnas)
    val columna = averiguarColumna(posicion, numColumnas)
      
    tipoBomba match 
    {
      case 7 => explosionVertical(tablero, columna, tablero.length, numColumnas)
      case 8 => explosionHorizontal(tablero, fila, tablero.length, numColumnas)
      case 9 => explosionTNT(tablero, posicion, tablero.length, numColumnas)
      case _ => explotarPuzle(tablero, tipoBomba)
    }
  }
  
}