
#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <stdio.h>
#include <iostream>
#include <time.h>
#include <stdlib.h>

#define AZUL 1
#define ROJO 2
#define NARANJA 3
#define VERDE 4
#define MARRON 5
#define AMARILLO 6

void juego(const int filas, const int columnas, int num_colores, bool cargar_partida);
void generar_matriz(int *& matriz, int tam_matriz, int num_colores);

int main(int argc, char ** argv)
{
	int dificultad = 0;
	int filas = 0;
	int columnas = 0;
	int opcion = 0;

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

		printf("Introduzca el numero de filas y columnas del tablero\n");
		printf("Filas: ");
		scanf("%i", &filas);
		printf("Columnas: ");
		scanf("%i", &columnas);

		juego(filas, columnas, dificultad, false);
	}
	else if (opcion == 2)
	{
		//TODO: recoger los datos del txt e inicializar los valores de filas columnas y dificultad
		filas = 0;
		columnas = 0;
		dificultad = 0;

		printf("cargar partida\n");
		juego(filas, columnas, dificultad, true);
	}


	system("pause");
	return 0;
}

void juego(const int filas,const int columnas, int dificultad, bool cargar_partida)
{

	bool salir = false;
	int opcion = 0;
	int num_colores = (dificultad == 1) ? 5 : 6;
	const int tam_matriz = filas * columnas;
	int  * matriz = (int *) malloc(tam_matriz * sizeof(int));

	if (cargar_partida)
	{
		//TODO:recoger la matriz serializada deserializarla y guardarla en la variable matriz
		//matriz = 
	}
	else
	{
		generar_matriz(matriz, tam_matriz, num_colores);

		//comprobar los valores de la matriz
		for (int i = 0; i < tam_matriz; i++)
		{
			printf("%i\n",matriz[i]);
		}
		scanf("%i", opcion);
	}

	while (!salir)
	{
		system("cls");
		printf(" ----------------------\n"
			   "| 1 = seguir jugando   |\n"
			   "| 2 = guardar partida  |\n"
			   "| 3 = salir del juego  |\n"
			   " ---------------------- \n");

		scanf("%i", &opcion);

		switch (opcion)
		{
			case 1:
				break;
			case 2:
				break;
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

void generar_matriz(int *& matriz, int tam_matriz, int num_colores)
{
	for (int i = 0; i < tam_matriz; i++)
	{
		matriz[i] = rand() % num_colores + 1;
	}
}