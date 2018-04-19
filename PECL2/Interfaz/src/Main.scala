import javax.swing._
import javax.swing.border.LineBorder;
import java.awt.BorderLayout
import java.awt.Dimension
import javax.swing.border.Border
import util.Random
import java.awt.GridLayout
import java.awt.Color
import scala.io.StdIn
import java.io._

object Main extends App {
  UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName())
  val frame = new JFrame("TOYBlast")
  val boton1 = new JButton("Nueva Partida")
  boton1.addActionListener(ActionListener => {
    frame.setVisible(false); //you can't see me!
    frame.dispose(); //Elimina el frame
    ventanaDificultad()
  })
  boton1.setBounds(25, 75, 150, 30)
  val boton2 = new JButton("Cargar Partida")
  boton2.setBounds(200, 75, 150, 30)
  boton2.addActionListener(ActionListener => { ////////Aqui es Cargar partida todo esto Cuadno lo tengas me lo pasas

    frame.setVisible(false); //you can't see me!
    frame.dispose(); //Elimina el frame
    ventanaDificultad()
  })
  frame.getContentPane.add(boton1)
  frame.getContentPane.add(boton2)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.getContentPane().setLayout(null)
  frame.setSize(new Dimension(400, 200))
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)

  def ventanaDificultad() {
    val frame = new JFrame("Seleccione Dificultad")
    val boton1 = new JButton("Nivel 1")
    val boton2 = new JButton("Nivel 2")
    val boton3 = new JButton("Nivel 3")
    frame.setSize(new Dimension(600, 200))
    frame.setLocationRelativeTo(null)
    boton1.addActionListener(ActionListener => {
      frame.setVisible(false)
      frame.dispose(); //Elimina el frame
      ventanaTablero(1)
    })
    boton2.addActionListener(ActionListener => {
      frame.setVisible(false)
      frame.dispose(); //Elimina el frame
      ventanaTablero(2)
    })
    boton3.addActionListener(ActionListener => {
      frame.setVisible(false)
      frame.dispose(); //Elimina el frame
      ventanaTablero(3)
    })
    boton1.setBackground(new java.awt.Color(21, 255, 0))
    //boton1.setForeground(new java.awt.Color(21,255,0))
    boton2.setBackground(new java.awt.Color(242, 68, 0))
    //boton2.setForeground(new java.awt.Color(242,68,0))
    boton3.setBackground(new java.awt.Color(255, 29, 0))
    //boton3.setForeground(new java.awt.Color(255,29,0))
    boton1.setBounds(25, 75, 150, 30)
    boton2.setBounds(200, 75, 150, 30)
    boton3.setBounds(375, 75, 150, 30)

    frame.getContentPane.add(boton1)
    frame.getContentPane.add(boton2)
    frame.getContentPane.add(boton3)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.getContentPane().setLayout(null)
    frame.setVisible(true)

  }

  def ventanaTablero(dificultad: Int) = dificultad match {
    case 1 => creacionTablero(9, 7, 4, 1)
    case 2 => creacionTablero(17, 11, 5, 2)
    case 3 => creacionTablero(27, 15, 6, 3)
  }

  def creacionTablero(columnas: Int, filas: Int, numColores: Int, dificultad: Int) {
    val frame = new JFrame
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize((columnas + 3) * 50, (filas + 3) * 50) //Lo hago más grande para que me quede bien
    frame.setVisible(true)
    frame.setLayout(null);
    frame.setLocationRelativeTo(null)
    frame.setTitle("Toy_Blast Nivel De Dificultad " + dificultad.toString())
    val botonGuardar = new JButton("Guardar y salir")
    val listaACrear = crearLista(filas * columnas, numColores)
    val mPunt = mejorJugada(listaACrear, 0, 0, columnas, 0, 0)
    val listaBotones = crearBotones(listaACrear, 0)
    botonGuardar.setBounds(300*dificultad, 0, 100, 50)
    botonGuardar.setBorderPainted(false)
    botonGuardar.addActionListener(ActionListener => {
      val ficheroDatos = new File("Datos Partida.txt")
      val writerDatos = new BufferedWriter(new FileWriter(ficheroDatos))
      try {
        writerDatos.write(s"$dificultad\n5")
      } finally {
        writerDatos.close()
      }

      val ficheroTablero = new File("tablero.dat")
      val salidaTablero = new ObjectOutputStream(new FileOutputStream(ficheroTablero))

      try {
        salidaTablero.writeObject(listaACrear)
      } finally {
        salidaTablero.close()
      }

      val ficheroPuntos = new File("puntos.dat")
      val salidaPuntos = new ObjectOutputStream(new FileOutputStream(ficheroPuntos))

      try {
        val puntos = 0
        salidaPuntos.writeObject(puntos)
      } finally {
        salidaPuntos.close()
        frame.dispose()
        System.exit(1)
      }
    })
    frame.getContentPane().add(botonGuardar)
    crearTableroColores(listaBotones, frame, columnas, filas, 0, 30, 60, numColores, listaACrear, dificultad, mPunt, List[Int](0, 0, 0, 0, 0, 0))
  }

  def continuacionTablero(tablero: List[Int], numColumnas: Int, numFilas: Int, numColores: Int, dificultad: Int, listaPuntos: List[Int]) {
    val frame = new JFrame
    frame.setLayout(null);
    val botonGuardar = new JButton("Guardar y salir")
    botonGuardar.setBounds(300*dificultad, 0, 100, 50)
    botonGuardar.setBorderPainted(false)
    botonGuardar.addActionListener(ActionListener => {
      val ficheroDatos = new File("Datos Partida.txt")
      val writerDatos = new BufferedWriter(new FileWriter(ficheroDatos))
      try {
        writerDatos.write(s"$dificultad\n5")
      } finally {
        writerDatos.close()
      }

      val ficheroTablero = new File("tablero.dat")
      val salidaTablero = new ObjectOutputStream(new FileOutputStream(ficheroTablero))

      try {
        salidaTablero.writeObject(tablero)
      } finally {
        salidaTablero.close()
      }

      val ficheroPuntos = new File("puntos.dat")
      val salidaPuntos = new ObjectOutputStream(new FileOutputStream(ficheroPuntos))

      try {
        salidaPuntos.writeObject(listaPuntos)
      } finally {
        salidaPuntos.close()
        frame.dispose()
        System.exit(1)
      }
    })
    frame.getContentPane().add(botonGuardar)
    dificultad match {
      case 1 => {
        val puntos = new JLabel("Azules: " + getElemento(listaPuntos, 0, 0).toString() + " Rojos: " + getElemento(listaPuntos, 1, 0).toString() + " Naranjas: " + getElemento(listaPuntos, 2, 0).toString() + " Verdes: " + getElemento(listaPuntos, 3, 0).toString() + " ")
        puntos.setBounds(10, 10, 400, 20)
        frame.getContentPane().add(puntos)
        if (getElemento(listaPuntos, 0, 0) == 20 && getElemento(listaPuntos, 1, 0) == 20 && getElemento(listaPuntos, 2, 0) == 20 && getElemento(listaPuntos, 3, 0) == 20) {
          frame.setVisible(false)
          frame.dispose()
          panelVictoria()
        }
      }
      case 2 => {
        val puntos = new JLabel("Azules: " + getElemento(listaPuntos, 0, 0).toString() + " Rojos: " + getElemento(listaPuntos, 1, 0).toString() + " Naranjas: " + getElemento(listaPuntos, 2, 0).toString() + " Verdes: " + getElemento(listaPuntos, 3, 0).toString() + " Platas: " + getElemento(listaPuntos, 4, 0).toString() + " ")
        puntos.setBounds(10, 10, 500, 20)
        frame.getContentPane().add(puntos)
        if (getElemento(listaPuntos, 0, 0) == 15 && getElemento(listaPuntos, 1, 0) == 15 && getElemento(listaPuntos, 2, 0) == 15 && getElemento(listaPuntos, 3, 0) == 15 && getElemento(listaPuntos, 4, 0) == 15) {
          frame.setVisible(false)
          frame.dispose()
          panelVictoria()
        }
      }
      case 3 => {
        val puntos = new JLabel("Azules: " + getElemento(listaPuntos, 0, 0).toString() + " Rojos: " + getElemento(listaPuntos, 1, 0).toString() + " Naranjas: " + getElemento(listaPuntos, 2, 0).toString() + " Verdes: " + getElemento(listaPuntos, 3, 0).toString() + " Platas: " + getElemento(listaPuntos, 4, 0).toString() + " Morado: " + getElemento(listaPuntos, 5, 0).toString() + " ")
        puntos.setBounds(10, 10, 600, 20)
        frame.getContentPane().add(puntos)
        if (getElemento(listaPuntos, 0, 0) == 10 && getElemento(listaPuntos, 1, 0) == 10 && getElemento(listaPuntos, 2, 0) == 10 && getElemento(listaPuntos, 3, 0) == 10 && getElemento(listaPuntos, 4, 0) == 10) {
          frame.setVisible(false)
          frame.dispose()
          panelVictoria()
        }
      }
    }
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize((numColumnas + 3) * 50, (numFilas + 3) * 50) //Lo hago más grande para que me quede bien
    frame.setVisible(true)
    //frame.setLayout(null);
    frame.setLocationRelativeTo(null)
    frame.setTitle("Toy_Blast Nivel De Dificultad " + dificultad.toString())
    val mPunt = mejorJugada(tablero, 0, 0, numColumnas, 0, 0)
    val listaBotones = crearBotones(tablero, 0)
    crearTableroColores(listaBotones, frame, numColumnas, numFilas, 0, 30, 60, numColores, tablero, dificultad, mPunt, listaPuntos)
  }
  def panelVictoria() {
    val frame = new JFrame()
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(200, 200) //Lo hago más grande para que me quede bien
    frame.setVisible(true)
    frame.setLayout(null);
    frame.setLocationRelativeTo(null)
    val label = new JLabel("Has Ganado!!!!")
    frame.getContentPane().add(label, BorderLayout.CENTER)
  }
  def crearTableroColores(tablero: List[JButton], frame: JFrame, numColumnas: Int, numFilas: Int, pos: Int, columna: Int, fila: Int, numColores: Int, tableroN: List[Int], dificultad: Int, mPunt: Int, listaPuntos: List[Int]) {
    val numColumnasAux = numColumnas - 1
    if (tablero.isEmpty) Nil
    else if (pos < numColumnasAux) {
      if (mPunt == 0) {
        val boton = tablero.head
        boton.setBounds(columna, fila, 50, 50)
        boton.setBorder(new LineBorder(Color.YELLOW, 12))
        boton.addActionListener(ActionListener => {
          val color = getElemento(tableroN, boton.getText().toInt, 0)
          val tableroFin = ejecutar(tableroN, boton.getText().toInt, numColumnas, numColores)
          val listaPos = buscarIguales(tableroN, List(boton.getText().toInt), List[Int](), List[Int](), numColumnas)
          val puntosActualizados = actualizarPuntos(comprobarListaPos(tableroN, listaPos, esBomba(color), boton.getText().toInt, numColumnas), listaPuntos, color, tableroN, esBomba(color))
          println(listaPos)
          println(tableroN)
          frame.setVisible(false)
          frame.dispose(); //Elimina el frame
          if (dificultad == 1) {
            if (getElemento(puntosActualizados, 0, 0) == 20 && getElemento(puntosActualizados, 1, 0) == 20 && getElemento(puntosActualizados, 2, 0) == 20 && getElemento(puntosActualizados, 3, 0) == 20) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          } else if (dificultad == 2) {
            if (getElemento(puntosActualizados, 0, 0) == 15 && getElemento(puntosActualizados, 1, 0) == 15 && getElemento(puntosActualizados, 2, 0) == 15 && getElemento(puntosActualizados, 3, 0) == 15 && getElemento(puntosActualizados, 4, 0) == 15) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          } else {
            if (getElemento(puntosActualizados, 0, 0) == 10 && getElemento(puntosActualizados, 1, 0) == 10 && getElemento(puntosActualizados, 2, 0) == 10 && getElemento(puntosActualizados, 3, 0) == 10 && getElemento(puntosActualizados, 4, 0) == 10) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          }
        })
        frame.getContentPane().add(boton)
        crearTableroColores(tablero.tail, frame, numColumnas, numFilas, pos + 1, columna + 50, fila, numColores, tableroN, dificultad, 1000, listaPuntos)
      } else {
        val boton = tablero.head
        boton.setBounds(columna, fila, 50, 50)
        //boton.setBorder(new LineBorder(Color.YELLOW,12))
        boton.addActionListener(ActionListener => {
          val tableroFin = ejecutar(tableroN, boton.getText().toInt, numColumnas, numColores)
          val color = getElemento(tableroN, boton.getText().toInt, 0)
          val listaPos = buscarIguales(tableroN, List(boton.getText().toInt), List[Int](), List[Int](), numColumnas)
          println(listaPos)
          println(tableroN)
          val puntosActualizados = actualizarPuntos(comprobarListaPos(tableroN, listaPos, esBomba(color), boton.getText().toInt, numColumnas), listaPuntos, color, tableroN, esBomba(color))
          frame.setVisible(false)
          frame.dispose(); //Elimina el frame
          if (dificultad == 1) {
            if (getElemento(puntosActualizados, 0, 0) == 20 && getElemento(puntosActualizados, 1, 0) == 20 && getElemento(puntosActualizados, 2, 0) == 20 && getElemento(puntosActualizados, 3, 0) == 20) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          } else if (dificultad == 2) {
            if (getElemento(puntosActualizados, 0, 0) == 15 && getElemento(puntosActualizados, 1, 0) == 15 && getElemento(puntosActualizados, 2, 0) == 15 && getElemento(puntosActualizados, 3, 0) == 15 && getElemento(puntosActualizados, 4, 0) == 15) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          } else {
            if (getElemento(puntosActualizados, 0, 0) == 10 && getElemento(puntosActualizados, 1, 0) == 10 && getElemento(puntosActualizados, 2, 0) == 10 && getElemento(puntosActualizados, 3, 0) == 10 && getElemento(puntosActualizados, 4, 0) == 10) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          }
        })
        frame.getContentPane().add(boton)
        crearTableroColores(tablero.tail, frame, numColumnas, numFilas, pos + 1, columna + 50, fila, numColores, tableroN, dificultad, mPunt - 1, listaPuntos)
      }
    } else if (pos == numColumnasAux) {
      if (mPunt == 0) {
        val boton = tablero.head
        boton.setBounds(columna, fila, 50, 50)
        boton.setBorder(new LineBorder(Color.YELLOW, 12))
        boton.addActionListener(ActionListener => {
          val tableroFin = ejecutar(tableroN, boton.getText().toInt, numColumnas, numColores)
          val color = getElemento(tableroN, boton.getText().toInt, 0)
          val listaPos = buscarIguales(tableroN, List(boton.getText().toInt), List[Int](), List[Int](), numColumnas)
          println(listaPos)
          println(tableroN)
          val puntosActualizados = actualizarPuntos(comprobarListaPos(tableroN, listaPos, esBomba(color), boton.getText().toInt, numColumnas), listaPuntos, color, tableroN, esBomba(color))
          frame.setVisible(false)
          frame.dispose(); //Elimina el frame
          if (dificultad == 1) {
            if (getElemento(puntosActualizados, 0, 0) == 20 && getElemento(puntosActualizados, 1, 0) == 20 && getElemento(puntosActualizados, 2, 0) == 20 && getElemento(puntosActualizados, 3, 0) == 20) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          } else if (dificultad == 2) {
            if (getElemento(puntosActualizados, 0, 0) == 15 && getElemento(puntosActualizados, 1, 0) == 15 && getElemento(puntosActualizados, 2, 0) == 15 && getElemento(puntosActualizados, 3, 0) == 15 && getElemento(puntosActualizados, 4, 0) == 15) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          } else {
            if (getElemento(puntosActualizados, 0, 0) == 10 && getElemento(puntosActualizados, 1, 0) == 10 && getElemento(puntosActualizados, 2, 0) == 10 && getElemento(puntosActualizados, 3, 0) == 10 && getElemento(puntosActualizados, 4, 0) == 10) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          }
        })
        frame.getContentPane().add(boton)
        crearTableroColores(tablero.tail, frame, numColumnas, numFilas, 0, 30, fila + 50, numColores, tableroN, dificultad, 1000, listaPuntos)
      } else {
        val boton = tablero.head
        boton.setBounds(columna, fila, 50, 50)
        //boton.setBorder(new LineBorder(Color.YELLOW,12))
        boton.addActionListener(ActionListener => {
          val tableroFin = ejecutar(tableroN, boton.getText().toInt, numColumnas, numColores)
          val color = getElemento(tableroN, boton.getText().toInt, 0)
          val listaPos = buscarIguales(tableroN, List(boton.getText().toInt), List[Int](), List[Int](), numColumnas)
          println(tableroN)
          println(listaPos)
          val puntosActualizados = actualizarPuntos(comprobarListaPos(tableroN, listaPos, esBomba(color), boton.getText().toInt, numColumnas), listaPuntos, color, tableroN, esBomba(color))
          frame.setVisible(false)
          frame.dispose(); //Elimina el frame
          if (dificultad == 1) {
            if (getElemento(puntosActualizados, 0, 0) == 20 && getElemento(puntosActualizados, 1, 0) == 20 && getElemento(puntosActualizados, 2, 0) == 20 && getElemento(puntosActualizados, 3, 0) == 20) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          } else if (dificultad == 2) {
            if (getElemento(puntosActualizados, 0, 0) == 15 && getElemento(puntosActualizados, 1, 0) == 15 && getElemento(puntosActualizados, 2, 0) == 15 && getElemento(puntosActualizados, 3, 0) == 15 && getElemento(puntosActualizados, 4, 0) == 15) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          } else {
            if (getElemento(puntosActualizados, 0, 0) == 10 && getElemento(puntosActualizados, 1, 0) == 10 && getElemento(puntosActualizados, 2, 0) == 10 && getElemento(puntosActualizados, 3, 0) == 10 && getElemento(puntosActualizados, 4, 0) == 10) {
              frame.setVisible(false)
              frame.dispose()
              panelVictoria()
            } else {
              continuacionTablero(tableroFin, numColumnas, numFilas, numColores, dificultad, puntosActualizados)
            }
          }
        })
        frame.getContentPane().add(boton)
        crearTableroColores(tablero.tail, frame, numColumnas, numFilas, 0, 30, fila + 50, numColores, tableroN, dificultad, mPunt - 1, listaPuntos)
      }
    }
  }

  //Función que devuelve una lista de botones correspondientes al tablero.
  def crearBotones(tablero: List[Int], pos: Int): List[JButton] = {
    if (tablero.isEmpty) Nil
    else {
      val boton = new JButton(pos.toString())
      tablero.head match {
        case 1 => {
          boton.setBackground(new java.awt.Color(0, 78, 255)) //Color azul
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          ////boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1) //Añado el boton a la lista
        }
        case 2 => {
          boton.setBackground(new java.awt.Color(255, 17, 0)) //Color Rojo
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          ////boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 3 => {
          boton.setBackground(new java.awt.Color(255, 82, 0)) //Color Naranja
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          ////boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 4 => {
          boton.setBackground(new java.awt.Color(0, 255, 0)) //Color Verde
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 5 => {
          boton.setBackground(new java.awt.Color(118, 118, 118)) //Color Plata
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 6 => {
          boton.setBackground(new java.awt.Color(204, 0, 153)) //Color Morado
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 7 => {
          boton.setBackground(new java.awt.Color(0, 0, 0)) //Bomba Vertical
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 8 => {
          boton.setBackground(new java.awt.Color(255, 255, 255)) //Bomba Horizontal
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 9 => {
          boton.setBackground(new java.awt.Color(255, 255, 0)) //Bomba TNT
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 10 => {
          boton.setBackground(new java.awt.Color(0, 255, 255)) //Bomba Puzzle Azul
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 11 => {
          boton.setBackground(new java.awt.Color(255, 129, 255)) //Bomba Puzzle Rojo
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 12 => {
          boton.setBackground(new java.awt.Color(255, 129, 116)) //Bomba Puzzle Naranja
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 13 => {
          boton.setBackground(new java.awt.Color(130, 255, 116)) //Bomba Puzzle Verde
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 14 => {
          boton.setBackground(new java.awt.Color(148, 148, 148)) //Bomba Puzzle Plata
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
        case 15 => {
          boton.setBackground(new java.awt.Color(255, 51, 204)) //Bomba Puzzle Morado
          boton.setForeground(new java.awt.Color(255, 255, 255, 0)) //Sin texto
          boton.setOpaque(true)
          //boton.setBorderPainted(false)
          boton :: crearBotones(tablero.tail, pos + 1)
        }
      }
    }
  }

  /* *******************************************************************************************************************
*
*
*
*
* 																									Parte Lógica
*
*
*
*
*/

  def ejecutar(tablero: List[Int], posicion: Int, numColumnas: Int, numColores: Int): List[Int] =
    {
      val columna = averiguarColumna(posicion, numColumnas)

      if (esBomba(tablero(posicion))) {
        val tablero2 = explotarBomba(tablero, posicion, numColumnas)
        val listCeros = listaCeros(tablero2, 0)

        animacion(listCeros, tablero2, numColumnas, numColores)
      } else {
        val listpos = List[Int]()
        val listaMuerte = buscarIguales(tablero, List(posicion), List[Int](), List[Int](), numColumnas)
        val listaPosFin = comprobarListaPos(tablero, listaMuerte)
        val tablero2 = eliminarIguales(tablero, listaPosFin, true)
        val tablero3 = ponerBomba(tablero2, posicion, listaPosFin, numColumnas, numColores)
        val listCeros = listaCeros(tablero3, 0)

        animacion(listCeros, tablero3, numColumnas, numColores)
      }
    }

  def auxFila(lista: List[Int], numColumnas: Int, fila: Int): Int = if (lista.length % numColumnas == 0) fila + 1 else fila

  def actualizarPuntos(listaPosiciones: List[Int], puntos: List[Int], color: Int, tablero: List[Int], esBomba: Boolean): List[Int] =
    {
      if (esBomba && listaPosiciones.nonEmpty) {
        val col = getElemento(tablero, listaPosiciones.head, 0) //color

        val puntuacionActualizada = poner((col - 1), (getElemento(puntos, (col - 1), 0) + 1), puntos)

        actualizarPuntos(listaPosiciones.tail, puntuacionActualizada, color, tablero, esBomba)
      } else if (esBomba && listaPosiciones.isEmpty) puntos
      else {
        val puntuacion = listaPosiciones.length
        if (puntuacion < 1) puntos
        else {
          poner((color - 1), (getElemento(puntos, (color - 1), 0) + puntuacion), puntos)
        }
      }
    }
  def dibujarTablero(tablero: List[Int], numColumnas: Int, fila: Int, columna: Int, dibujarColumnas: Boolean): Unit =
    {
      if (dibujarColumnas) {
        if (columna == 0) print(s"    ")

        if (columna == numColumnas) {
          print("\n\n0   ")
          dibujarTablero(tablero, numColumnas, fila, columna, false)
        } else if ((columna < numColumnas) && (columna < 10)) {
          print(s"$columna  ")

          dibujarTablero(tablero, numColumnas, fila, columna + 1, true)
        } else if (columna < numColumnas) {
          print(s"$columna ")
          dibujarTablero(tablero, numColumnas, fila, columna + 1, true)
        }
      } else if (tablero.nonEmpty) {
        print(devolverElemento(tablero.head) + "  ")

        if (tablero.tail.length % numColumnas == 0 && tablero.tail.nonEmpty) {
          if (fila + 1 < 10) print(s"\n${fila + 1}   ")
          else print(s"\n${fila + 1}  ")
        }

        dibujarTablero(tablero.tail, numColumnas, auxFila(tablero.tail, numColumnas, fila), columna, false)
      } else {
        println()
      }
    }
  def devolverElemento(n: Int): String = n match {
    case 1  => "A"
    case 2  => "R"
    case 3  => "N"
    case 4  => "V"
    case 5  => "P"
    case 6  => "M"
    case 7  => "Bv"
    case 8  => "Bh"
    case 9  => "TNT"
    case 10 => "BA"
    case 11 => "BR"
    case 12 => "BN"
    case 13 => "BV"
    case 14 => "BP"
    case 15 => "BM"
    case _  => " "
  }
  def animacion(listaPosiciones: List[Int], tablero: List[Int], numColumnas: Int, numColores: Int): List[Int] =
    {
      dibujarTablero(tablero, numColumnas, 0, 0, true)
      println()

      if (hayCeros(tablero)) {
        val tablero2 = bajarCeros(listaPosiciones, tablero, numColumnas, numColores)
        val listCeros = separarEnColumnas(listaCeros(tablero2, 0), numColumnas, 0)

        animacion(listCeros, tablero2, numColumnas, numColores)
      } else {
        tablero
      }
    }

  def poner(pos: Int, color: Int, tablero: List[Int]): List[Int] = {
    if (tablero.isEmpty) Nil
    else if (pos == 0) color :: tablero.tail //else if (pos==0) color::tablero.tail
    else tablero.head :: poner(pos - 1, color, tablero.tail)
  }

  def comprobarListaPos(tablero: List[Int], listaPosiciones: List[Int]): List[Int] =
    {
      if (listaPosiciones.length == 1) List[Int]()
      else listaPosiciones
    }
  def comprobarListaPos(tablero: List[Int], listaPosiciones: List[Int], esBomba: Boolean, posicion: Int, numColumnas: Int): List[Int] =
    {
      if (esBomba) {
        explotarBomba(tablero, posicion, numColumnas) //devuelve la lista de posiciones a explotar por la bomba
      } else if (listaPosiciones.length == 1) List[Int]()
      else listaPosiciones
    }

  def crearLista(tam: Int, numColores: Int): List[Int] = tam match {
    case 0 => Nil
    case _ => (Random.nextInt(numColores) + 1) :: crearLista(tam - 1, numColores)
  }
  //Esta funcion calculará la mejor jugada cogiendo todas las posibles combinaciones del tablero y sacando la mejor.
  def mejorJugada(tablero: List[Int], pos: Int, columna: Int, width: Int, mPuntacion: Int, mPos: Int): Int = {
    if (pos + 3 >= tablero.length) mPos //Se sale de rango
    else {
      if (getElemento(tablero, pos, 0) == 0) mejorJugada(tablero, pos + 1, columna, width, mPuntacion, pos) //Si la posicion es 0 no me sirve
      else {
        if (getElemento(tablero, pos, 0) >= 10) mejorJugada(tablero, pos + 1, columna, width, 1000, pos) //Selecciono Bomba Puzzle
        else if (getElemento(tablero, pos, 0) == 9) {
          if (mPuntacion == 1000) mejorJugada(tablero, pos + 1, columna, width, mPuntacion, mPos) //No hago nada ya hay bombra Puzzle seleccionada
          else mejorJugada(tablero, pos + 1, columna, width, 500, pos) //Selecciono TNT
        } else if (getElemento(tablero, pos, 0) == 8 || getElemento(tablero, pos, 0) == 9) {
          if (mPuntacion == 1000) mejorJugada(tablero, pos + 1, columna, width, mPuntacion, mPos) //No hago nada ya hay bombra Puzzle seleccionada
          else if (mPuntacion == 500) mejorJugada(tablero, pos + 1, columna, width, mPuntacion, mPos) //No hago nada ya hay bomba TNT seleccionada
          else mejorJugada(tablero, pos + 1, columna, width, 250, pos) //Selecciono TNT
        } else {
          val puntAux = buscarIguales(tablero, List(pos), List[Int](), List[Int](), width) //Puntuación sacada de esa posición
          if (puntAux.length < mPuntacion) mejorJugada(tablero, pos + 1, columna, width, mPuntacion, mPos) //Si la puntuación es menor, no lo tengo en cuenta y sigo avanzando
          else mejorJugada(tablero, pos + 1, columna, width, puntAux.length, pos) //Si no, es que es mayor o igual, entonces guardo esa puntuación y sigo avanzando.
        }
      }
    }
  }

  //Función que buscará las combinaciones posibles
  def buscarIguales(tablero: List[Int], posExpandir: List[Int], posEliminar: List[Int], posVisitar: List[Int], width: Int): List[Int] = {
    if (posExpandir.isEmpty) posEliminar
    else {
      val listaExpandida = algoritmoEstrella(tablero, posExpandir.head, width) //Ejecuta el algoritmo de expansión en estrlla.
      val listaExpandida2 = listaExpandida.distinct //Quito iguales
      val posVisitadosAux = añadirElementos(posVisitar, List(listaExpandida2.head)) //Añado el nodo a la lista de posciones visitadas
      val listaExpandida3 = añadirElementos(posEliminar, listaExpandida2) //Añado la lista a las posiciones a eliminar puesto que son iguales.
      val listaExpandida4 = limpiarLista1000(listaExpandida3) //Elimino residuos, marcados como 1000
      if (listaExpandida4 == posEliminar) { //Si la lista de eliminados es igual que la que ya tenía es que he llegado a punto muerto
        val l1 = ordenarLista(posEliminar)
        val l2 = ordenarLista(posVisitadosAux)
        if (l1 == l2) posEliminar //Si las dos listas son iguales, es que he llegado a punto muerto y visitado todos los nodos, entonces
        //tengo ya todas las combinaciones
        else { //Busco con el primer elemento no visitado que me falta
          val elemento = elementoNoIgual(posVisitadosAux, posEliminar)
          buscarIguales(tablero, List(elemento), posEliminar, posVisitadosAux, width)
        }
      } else buscarIguales(tablero, listaExpandida3.tail, listaExpandida3.distinct, posVisitadosAux, width)
    }
  }

  def algoritmoEstrella(tablero: List[Int], pos: Int, width: Int): List[Int] = {
    //algoritmo que me saca todas las posiciones del algoritmo estrella tal que así:
    /*              |
	   *          -- Pos --
	   *              |
	   */
    val arriba = comprobarIgualesArriba(pos, tablero, width)
    val abajo = comprobarIgualesAbajo(pos, tablero, width)
    val derecha = comprobarIgualesDerecha(pos, tablero, width)
    val izquierda = comprobarIgualesIzquierda(pos, tablero, width)
    val lposaux = List(pos) //Añado la posicion
    val lposaux2 = lposaux :+ arriba :+ abajo :+ derecha :+ izquierda //Añado las posiciones
    val lposaux3 = limpiarLista(lposaux2) //Quito los menos uno
    val lposaux4 = lposaux3.distinct //Quito iguales y devuelvo
    return lposaux4
  }

  def comprobarIgualesArriba(pos: Int, tablero: List[Int], width: Int): Int = {
    if ((pos - width > 0) && (tablero(pos) == tablero(pos - width))) pos - width
    else -1
  }
  def comprobarIgualesAbajo(pos: Int, tablero: List[Int], width: Int): Int = {
    if ((pos + width < tablero.length) && (tablero(pos) == tablero(pos + width))) pos + width
    else -1
  }
  def comprobarIgualesDerecha(pos: Int, tablero: List[Int], width: Int): Int = {
    val n1 = pos.toFloat / width
    val n2 = scala.math.round(n1)
    if ((n2 + 1 != width) && (pos + 1 < tablero.length) && (tablero(pos) == tablero(pos + 1))) pos + 1
    else -1
  }
  def comprobarIgualesIzquierda(pos: Int, tablero: List[Int], width: Int): Int = {
    val n1 = pos % width
    if ((n1 != 0) && (tablero(pos) == tablero(pos - 1))) pos - 1
    else -1
  }

  def unirListas(l1: List[Int], l2: List[Int]): List[Int] = {
    if (l2.isEmpty) l1
    else unirListas(l1 :+ l2.head, l2.tail)
  }

  def eliminarListasVacias(lista: List[Any]): List[Any] =
    {
      if (lista.nonEmpty) {
        if (lista.head == List()) eliminarListasVacias(lista.tail)
        else lista.head :: eliminarListasVacias(lista.tail)
      } else {
        lista
      }
    }

  def limpiarLista(lista: List[Int]): List[Int] =
    {
      if (lista.nonEmpty) {
        if (lista.head == -1) limpiarLista(lista.tail)
        else lista.head :: limpiarLista(lista.tail)
      } else {
        lista
      }
    }
  def limpiarLista1000(lista: List[Int]): List[Int] =
    {
      if (lista.nonEmpty) {
        if (lista.head == 1000) limpiarLista(lista.tail)
        else lista.head :: limpiarLista(lista.tail)
      } else {
        lista
      }
    }
  def eliminarIguales(tablero: List[Int], lpos: List[Int], primeraI: Boolean): List[Int] = {
    if (lpos.length > 1) {
      val tableroaux = poner(lpos.head, 0, tablero)
      eliminarIguales(tableroaux, lpos.tail, false)
    } else if (lpos.length == 1 && primeraI == false) {
      val tableroaux = poner(lpos.head, 0, tablero)
      return tableroaux
    } else {
      tablero
    }
  }

  def getElemento(tablero: List[Int], pos: Int, posActual: Int): Int = {
    if (tablero.isEmpty) 0
    else if (posActual == pos) tablero.head
    else getElemento(tablero.tail, pos, posActual + 1)
  }

  def elementosDiferentes(lista: List[Int], elementosPorMeter: List[Int]): List[Int] =
    {
      if (elementosPorMeter.nonEmpty) {
        if (elementoEnLista(lista, elementosPorMeter.head)) elementosDiferentes(lista, elementosPorMeter.tail)
        else elementosPorMeter.head :: elementosDiferentes(lista, elementosPorMeter.tail)
      } else {
        Nil
      }
    }
  def añadirElementos(lista: List[Int], elementosPorMeter: List[Int]): List[Int] =
    {
      lista ::: elementosDiferentes(lista, elementosPorMeter)
    }

  def elementoNoIgual(lista1: List[Int], lista2: List[Int]): Int = {
    if (lista2.isEmpty) 1000
    else {
      if (elementoEnLista(lista1, lista2.head) == false) lista2.head
      else elementoNoIgual(lista1, lista2.tail)
    }
  }
  def elementoEnLista(lista: List[Int], elemento: Int): Boolean =
    {
      if (lista.nonEmpty) {
        if (lista.head == elemento) true
        else elementoEnLista(lista.tail, elemento)
      } else {
        false
      }
    }

  def menor(valor1: Int, valor2: Int): Int = if (valor1 < valor2) valor1 else valor2

  def minimo(lista: List[Int]): Int =
    {
      if (lista.tail.nonEmpty) {
        menor(lista.head, minimo(lista.tail))
      } else {
        lista.head
      }
    }

  def eliminarValor(lista: List[Int], valor: Int): List[Int] =
    {
      if (lista.nonEmpty) {
        if (lista.head == valor) eliminarValor(lista.tail, valor)
        else lista.head :: eliminarValor(lista.tail, valor)
      } else {
        lista
      }
    }

  def ordenarLista(lista: List[Int]): List[Int] =
    {
      if (lista.nonEmpty) {
        minimo(lista) :: ordenarLista(eliminarValor(lista, minimo(lista)))
      } else {
        lista
      }
    }

  def bajarColumna(numColores: Int, fila: Int, columna: Int, numColumnas: Int, tablero: List[Int]): List[Int] =
    {
      val posicion = fila * numColumnas + columna
      val arriba = (fila - 1) * numColumnas + columna

      if ((fila == 0) && (tablero(posicion) == 0)) {
        val color = Random.nextInt(numColores) + 1

        poner(posicion, color, tablero)
      } else if (arriba >= 0) {
        if (tablero(arriba) == 0) {
          bajarColumna(numColores, fila - 1, columna, numColumnas, tablero)
        } else if (tablero(posicion) == 0) {
          val nuevoTablero = poner(posicion, tablero(arriba), tablero)

          if (fila != 0) bajarColumna(numColores, fila - 1, columna, numColumnas, poner(arriba, 0, nuevoTablero))
          else bajarColumna(numColores, fila, columna, numColumnas, nuevoTablero)
        } else {
          tablero
        }

      } else {
        tablero
      }
    }

  def averiguarColumna(posicion: Int, numColumnas: Int): Int =
    {
      if ((posicion >= 0) && (posicion < numColumnas)) {
        posicion
      } else {
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
      if (lista.nonEmpty) {
        if (mismaColumna(lista.head, columna, numColumnas)) lista.head :: listaColumna(lista.tail, columna, numColumnas)
        else listaColumna(lista.tail, columna, numColumnas)
      } else lista
    }

  //devuelve una lista con las posiciones mas bajas pertenecientes a cada columna de la lista recibe por parametro
  def separarEnColumnas(lista: List[Int], numColumnas: Int, columna: Int): List[Int] =
    {
      if (columna < numColumnas) {
        val listaCol = listaColumna(lista, columna, numColumnas)

        if (listaCol.nonEmpty) listaCol.max :: separarEnColumnas(lista, numColumnas, columna + 1)
        else separarEnColumnas(lista, numColumnas, columna + 1)
      } else {
        Nil
      }
    }

  def bajarCeros(lista: List[Int], tablero: List[Int], numColumnas: Int, numColores: Int): List[Int] =
    {
      val lista2 = separarEnColumnas(lista, numColumnas, 0)

      if (lista2.nonEmpty) {
        val fila = averiguarFila(lista.head, numColumnas)
        val columna = averiguarColumna(lista.head, numColumnas)
        val tablero2 = bajarColumna(numColores, fila, columna, numColumnas, tablero)

        bajarCeros(lista2.tail, tablero2, numColumnas, numColores)
      } else {
        tablero
      }
    }

  def listaCeros(tablero: List[Int], posicion: Int): List[Int] =
    {
      if (tablero.nonEmpty) {
        if (tablero.head == 0) posicion :: listaCeros(tablero.tail, posicion + 1)
        else listaCeros(tablero.tail, posicion + 1)
      } else {
        tablero
      }
    }

  def hayCeros(lista: List[Int]): Boolean =
    {
      if (lista.isEmpty) false
      else if (lista.head == 0) true
      else hayCeros(lista.tail)
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

      if ((abajo < tablero.length) && (tablero(posicion) != 0)) {
        if (tablero(abajo) == 0) {
          val tablero2 = poner(abajo, tablero(posicion), tablero)
          val tablero3 = poner(posicion, 0, tablero2)

          bajarElemento(tablero3, abajo, numColumnas)
        } else {
          tablero
        }
      } else {
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
      println(s"hago explosion horizontal en la fila $fila")
      val posicion = tamTablero - tablero.length
      val filaActual = averiguarFila(posicion, numColumnas)

      if (tablero.nonEmpty) {
        if (filaActual == fila) 0 :: explosionHorizontal(tablero.tail, fila, tamTablero, numColumnas)
        else tablero.head :: explosionHorizontal(tablero.tail, fila, tamTablero, numColumnas)
      } else {
        tablero
      }
    }

  def explosionVertical(tablero: List[Int], columna: Int, tamTablero: Int, numColumnas: Int): List[Int] =
    {
      val posicion = tamTablero - tablero.length
      val columnaActual = averiguarColumna(posicion, numColumnas)
      if (tablero.nonEmpty) {
        if (columnaActual == columna) 0 :: explosionVertical(tablero.tail, columna, tamTablero, numColumnas)
        else tablero.head :: explosionVertical(tablero.tail, columna, tamTablero, numColumnas)
      } else {
        tablero
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

      if (tablero.nonEmpty) {
        if ((posicion == posSeleccionada) || (posicion == arribaPosSel) || (posicion == abajoPosSel) || (posicion == izPosSel) || (posicion == derPosSel)) {
          0 :: explosionTNT(tablero.tail, posSeleccionada, tamTablero, numColumnas)
        } else if ((posicion == arrDerPosSel) || (posicion == arrIzPosSel) || (posicion == abDerPosSel) || (posicion == abIzPosSel)) {
          0 :: explosionTNT(tablero.tail, posSeleccionada, tamTablero, numColumnas)
        } else {
          tablero.head :: explosionTNT(tablero.tail, posSeleccionada, tamTablero, numColumnas)
        }
      } else {
        tablero
      }
    }

  def explotarPuzle(tablero: List[Int], tipoPuzle: Int): List[Int] = tipoPuzle match {
    case 10 => explosionPuzle(tablero, 1)
    case 11 => explosionPuzle(tablero, 2)
    case 12 => explosionPuzle(tablero, 3)
    case 13 => explosionPuzle(tablero, 4)
    case 14 => explosionPuzle(tablero, 5)
    case _  => explosionPuzle(tablero, 6)
  }

  def explosionPuzle(tablero: List[Int], color: Int): List[Int] =
    {
      if (tablero.nonEmpty) {
        if ((tablero.head == color) || (tablero.head > 9)) 0 :: explosionPuzle(tablero.tail, color)
        else tablero.head :: explosionPuzle(tablero.tail, color)
      } else {
        tablero
      }
    }

  def explotarBomba(tablero: List[Int], posicion: Int, numColumnas: Int): List[Int] =
    {
      val tipoBomba = tablero(posicion)
      val fila = averiguarFila(posicion, numColumnas)
      val columna = averiguarColumna(posicion, numColumnas)

      tipoBomba match {
        case 7 => explosionVertical(tablero, columna, tablero.length, numColumnas)
        case 8 => explosionHorizontal(tablero, fila, tablero.length, numColumnas)
        case 9 => explosionTNT(tablero, posicion, tablero.length, numColumnas)
        case _ => explotarPuzle(tablero, tipoBomba)
      }
    }
}