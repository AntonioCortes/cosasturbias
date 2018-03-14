
#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <stdio.h>
#include <iostream>
#include <time.h>
#include <stdlib.h>
#include <windows.h>
#include "book.h"

#define AZUL 1
#define ROJO 2
#define NARANJA 3
#define VERDE 4
#define MARRON 5
#define AMARILLO 6

void juego(int filas, int columnas, int num_colores, bool cargar_partida, FILE *& datos_partida, FILE *& archivo_matriz);
void generar_matriz(int *& matriz, long tam_matriz, int num_colores);
void dibujar_matriz(int * matriz, int filas, int columnas);
void cargar_datos(int &dificultad, int &filas, int &columnas, FILE *& datos_partida); //carga los datos de la partida (dificultad, numero filas y columnas, pero no la matriz)
void cargar_matriz(int *& matriz, long tam_matriz, FILE *& archivo_matriz);
void guardar_partida(int * matriz, int dificultad, int filas, int columnas,FILE *& archivo_matriz, FILE *& datos_partida);//guarda los datos de la partida (dificultad, numero de filas y columnas) en un archivo .txt y la matriz en un archivo .data
void comprobar_dimensiones(int filas, int columnas, bool & dimensiones_adecuadas);//comprueba si las dimensiones del tableropermiten correr en un bloque SM

int main(int argc, char ** argv)
{
	int dificultad = 0;
	int filas = 0;
	int columnas = 0;
	int opcion = 0;
	FILE * datos_partida;
	FILE  * archivo_matriz;
	bool dimensiones_adecuadas = false;

	//SetConsoleDisplayMode(GetStdHandle(STD_OUTPUT_HANDLE), CONSOLE_FULLSCREEN_MODE, 0);
	srand(time(NULL));

	printf("Seleccione 1 para empezar una nueva partida o 2 para cargar partida\n");
	scanf("%i", &opcion);

	if (opcion == 1)
	{
		do
		{
			printf("Introduzca nivel de dificultad (1 o 2): ");
			scanf("%i", &dificultad);

			if ((dificultad != 1) && (dificultad != 2))
			{
				printf("Error al elegir nivel de dificultad\n");
				system("cls");
			}

		} while ((dificultad != 1) && (dificultad != 2));

		do
		{
			printf("Introduzca el numero de filas y columnas del tablero\n");
			printf("Filas: ");
			scanf("%i", &filas);
			printf("Columnas: ");
			scanf("%i", &columnas);

			comprobar_dimensiones(filas, columnas, dimensiones_adecuadas);

			if (!dimensiones_adecuadas)
			{
				printf("Error, el tablero es demasiado grande para correr en un bloque SM\n");
				system("pause");
				system("cls");
			}
		} while (!dimensiones_adecuadas);

		juego(filas, columnas, dificultad, false, datos_partida, archivo_matriz);
	}
	else if (opcion == 2)
	{		
		cargar_datos(dificultad, filas, columnas, datos_partida);
		juego(filas, columnas, dificultad, true, datos_partida, archivo_matriz);
	}

	system("pause");
	return 0;
}

void juego(int filas,int columnas, int dificultad, bool cargar_partida, FILE *& datos_partida, FILE *& archivo_matriz)
{

	bool salir = false;
	int opcion = 0;
	int num_colores = (dificultad == 1) ? 5 : 6;
	long tam_matriz = filas * columnas;
	int  * matriz = (int *) malloc(tam_matriz * sizeof(int));
	int pos_fila = 0;
	int pos_columna = 0;
	
	if (cargar_partida)
	{
		cargar_matriz(matriz, tam_matriz, archivo_matriz);
	}
	else
	{
		generar_matriz(matriz, tam_matriz, num_colores);
	}

	while (!salir)
	{
		system("cls");
		printf("dificuldad = %i\tfilas = %i\tcolumnas = %i\n", dificultad, filas, columnas);
		printf(" ----------------------\n"
			   "| 1 = seguir jugando   |\n"
			   "| 2 = guardar partida  |\n"
			   "| 3 = salir del juego  |\n"
			   " ---------------------- \n\n");
		dibujar_matriz(matriz, filas, columnas);

		printf("Elija opcion: ");
		scanf("%i", &opcion);

		switch (opcion)
		{
			case 1:
			{
				printf("Introduzca posicion de la casilla (fila/columna):\n");
				printf("fila: ");
				scanf("%i", &pos_fila);
				printf("columna: ");
				scanf("%i", &pos_columna);

				/*
				do
				{
					//jugar()
				} while (hay_ceros());
				*/

				
				break;
			}
			case 2:
			{				
				guardar_partida(matriz, dificultad, filas, columnas, archivo_matriz, datos_partida);
				break;
			}
			case 3:
			{
				salir = true;
				break;
			}
			default:
				break;
		}
	}
}

void comprobar_dimensiones(int filas, int columnas, bool & dimensiones_adecuadas)
{
	cudaDeviceProp propiedades_gpu;
	cudaGetDeviceProperties(&propiedades_gpu, 0);

	long capacidad_sm = propiedades_gpu.maxThreadsDim[0] * propiedades_gpu.maxThreadsDim[1] * propiedades_gpu.maxThreadsDim[2];
	long tam_matriz =  filas * columnas;

	dimensiones_adecuadas = (tam_matriz > capacidad_sm) ? false : true;
}

void generar_matriz(int *& matriz, long tam_matriz, int num_colores)
{
	for (int i = 0; i < tam_matriz; i++)
	{
		matriz[i] = rand() % num_colores + 1;
	}
}

void dibujar_matriz(int * matriz, int filas, int columnas)
{
	int valor = 0;

	printf(" \t");

	for (int i = 0; i < columnas; i++)
	{
		printf("%i   ", i);
	}
	printf("\n\n\n");

	for (int i = 0; i < filas; i++)
	{
		printf("%i\t", i);
		for (int n = 0; n < columnas; n++)
		{
			valor = matriz[i * columnas + n];

			switch (valor) 
			{
			case 0:
				SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 0);
				break;
			case 1:
				SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 1);
				break;
			case 2:
				SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 4);
				break;
			case 3:
				SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 13);
				break;
			case 4:
				SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 2);
				break;
			case 5:
				SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 6);
				break;
			case 6:
				SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 14);
				break;
			}

			printf("%i   ", valor);
		}
		printf("\n");
		SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 15);
	}

}

void guardar_partida(int * matriz, int dificultad, int filas, int columnas, FILE *& archivo_matriz, FILE *& datos_partida)
{
	long tam_matriz = filas * columnas; 

	if ((datos_partida = fopen("datos_partida.txt", "w")) == NULL)
	{
		printf("error abriendo el archivo \"datos_partida.txt\" \n");
		system("pause");
		exit(1);
	}
	else
	{
		fprintf(datos_partida, "%i\n", dificultad);
		fprintf(datos_partida, "%i\n", filas);
		fprintf(datos_partida, "%i", columnas);
		fclose(datos_partida);
	}

	if ((archivo_matriz = fopen("matriz.data", "wb")) == NULL)
	{
		printf("error abriendo el archivo \"archivo_matriz.txt\" \n");
		system("pause");
		exit(1);
	}
	else
	{
		fwrite(matriz, sizeof(int), tam_matriz, archivo_matriz);
		fclose(archivo_matriz);
	}
}



void cargar_datos(int &dificultad, int &filas, int &columnas, FILE *& datos_partida)
{
	if ((datos_partida = fopen("datos_partida.txt", "r")))
	{
		int linea_actual = 0;
		while (!feof(datos_partida))
		{
			linea_actual++;

			switch (linea_actual)
			{
			case 1:
			{
					  fscanf(datos_partida, "%d", &dificultad);
					  break;
			}
			case 2:
			{
					  fscanf(datos_partida, "%d", &filas);
					  break;
			}
			case 3:
			{
					  fscanf(datos_partida, "%d", &columnas);
					  break;
			}
			default:
				break;
			}
		}

		fclose(datos_partida);
	}
	else
	{
		printf("error abriendo el archivo datos_partida.txt\n");
		return;
	}
}

void cargar_matriz(int *& matriz, long tam_matriz, FILE *& archivo_matriz)
{
	if ((archivo_matriz = fopen("matriz.data", "rb")))
	{
		fread(matriz, sizeof(int), tam_matriz, archivo_matriz);
	}
	else
	{
		printf("error abriendo el archivo \"archivo_matriz\"");
		return;
	}
	fclose(archivo_matriz);
}

