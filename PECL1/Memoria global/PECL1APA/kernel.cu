
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
#define BOMBAHOR 7
#define BOMBAVER 8
#define BOMBATNT 9
#define BOMBAPUZZLE 10

void juego(int filas, int columnas, int num_colores, bool cargar_partida, FILE *& datos_partida, FILE *& archivo_matriz);
void generar_matriz(int *& matriz, long tam_matriz, int num_colores);
void dibujar_matriz(int * matriz, int filas, int columnas);
void cargar_datos(int &dificultad, int &filas, int &columnas, FILE *& datos_partida); //carga los datos de la partida (dificultad, numero filas y columnas, pero no la matriz)
void cargar_matriz(int *& matriz, long tam_matriz, FILE *& archivo_matriz);
void guardar_partida(int * matriz, int dificultad, int filas, int columnas,FILE *& archivo_matriz, FILE *& datos_partida);//guarda los datos de la partida (dificultad, numero de filas y columnas) en un archivo .txt y la matriz en un archivo .data
void comprobar_dimensiones(int filas, int columnas, bool & dimensiones_adecuadas);//comprueba si las dimensiones del tableropermiten correr en un bloque SM
void jugar(int *tablero, int fil, int col, int size, int fila, int columna, int num_colores, int * num_vidas);
void generarAleatorios(int *& matriz, int tam_matriz, int num_colores);
bool es_bomba(int * matriz, int fila, int columna, int num_columnas, int &tipo_bomba);//comprueba si la posici�n elegida por el jugador corresponde a una bomba
void explotar_vertical(int *& tablero, long tam_tablero, int filas, int columnas, int columna);//helper que ejecuta el kernel de la explosion de la bomba vertical
void explotar_horizontal(int *& tablero, long tam_tablero, int filas, int columnas, int fila);//helper que ejecuta el kernel de la explosion de la bomba horizontal
void explotar_tnt(int *& tablero, long tam_tablero, int filas, int columnas, int fila, int columna);
void explotar_puzle(int *& tablero, long tam_tablero, int filas, int columnas, int fila, int columna, int num_colores);

__global__ void KernelJugar(int *tablero, int fila, int columna, int i, int j, int bomba, int color, int * num_vidas);
__global__ void explosion_vertical(int * tablero, int anchura_tablero, int columna);
__global__ void explosion_horizontal(int * tablero, int anchura_tablero, int fila);
__global__ void explosion_tnt(int * tablero, long tam_tablero, int filas, int columnas, int fila, int columna);
__global__ void explosion_puzle(int * tablero, int columnas, int fila, int columna, int color);
__device__ void comprobarBloques(int *tablero, int x, int y, int fila, int columna, int * num_vidas);
__device__ void comprobarBloquesArriba(int *tablero, int x, int y, int fila, int columna);
__device__ void comprobarBloquesDerecha(int *tablero, int x, int y, int fila, int columna);
__device__ void comprobarBloquesIzquierda(int *tablero, int x, int y, int fila, int columna);
__device__ void comprobarBloquesAbajo(int *tablero, int x, int y, int fila, int columna);

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
				printf("Error, el tablero es demasiado grande para correr en un bloque\n");
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
	int tipo_bomba = 0;
	int num_vidas = 5;
	int * ptr_numVidas = &num_vidas;
	
	if (cargar_partida)
	{
		cargar_matriz(matriz, tam_matriz, archivo_matriz);
	}
	else
	{
		generar_matriz(matriz, tam_matriz, num_colores);
	}

	while (!salir && (num_vidas > 0))
	{
		//system("cls");
		generarAleatorios(matriz, tam_matriz, num_colores);
		printf("dificuldad = %i\tfilas = %i\tcolumnas = %i\tnumero de vidas = %i\n", dificultad, filas, columnas, num_vidas);
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

				if (es_bomba(matriz, pos_fila, pos_columna, columnas, tipo_bomba))
				{
					switch (tipo_bomba)
					{
						case BOMBAVER:
						{
							explotar_vertical(matriz, tam_matriz, filas, columnas, pos_columna);
							break;
						}
						case BOMBAHOR:
						{
							explotar_horizontal(matriz, tam_matriz, filas, columnas, pos_fila);
							break;
						}
						case BOMBATNT:
						{
							explotar_tnt(matriz, tam_matriz, filas, columnas, pos_fila, pos_columna);
							break;
						}
						case BOMBAPUZZLE:
						{
							explotar_puzle(matriz,tam_matriz, filas, columnas, pos_fila, pos_columna, num_colores);
							break;
						}
						default:
							break;
					}
				}
				else
				{
					jugar(matriz, filas, columnas, filas*columnas*sizeof(int), pos_fila, pos_columna, num_colores, ptr_numVidas);
					if (num_vidas == 0)
					{
						printf("GAME OVER\n");
					}
				}

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

//comprueba si las dimensiones de la matriz introducida por el usuario caben en un bloque
void comprobar_dimensiones(int filas, int columnas, bool & dimensiones_adecuadas)
{
	cudaDeviceProp propiedades_gpu;
	cudaGetDeviceProperties(&propiedades_gpu, 0);

	long capacidad_bloque = propiedades_gpu.maxThreadsPerBlock;
	long tam_matriz =  filas * columnas;

	dimensiones_adecuadas = (tam_matriz > capacidad_bloque) ? false : true;
}

void generar_matriz(int *& matriz, long tam_matriz, int num_colores)
{
	for (int i = 0; i < tam_matriz; i++)
	{
		matriz[i] = rand() % num_colores + 1;
	}
}

//genera numeros aleatorios y rellena el tablero con ellos, en los huecos vac�os que quedan, despues de haber explotado alg�n bloque
void generarAleatorios(int *& matriz, int tam_matriz, int num_colores){
	for (int i = 0; i < tam_matriz; i++)
	{
		if (matriz[i] == 0){
			matriz[i] = rand() % num_colores + 1;
		}
	}
}

//dibuja y muestra la matriz por pantalla
void dibujar_matriz(int * matriz, int filas, int columnas)
{
	int valor = 0;

	printf(" \t");

	for (int i = 0; i < columnas; i++)
	{
		if (i < 10)
		{
			printf("%i   ", i);
		}
		else if (i < 100)
		{
			printf("%i  ", i);
		}
		
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
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 0);
					  printf("%i   ", valor);
					  break;
			}
			case 1:
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 1);
					  printf("%i   ", valor);
					  break;
			}
			case 2:
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 4);
					  printf("%i   ", valor);
					  break;
			}
			case 3:
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 13);
					  printf("%i   ", valor);
					  break;
			}
			case 4:
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 2);
					  printf("%i   ", valor);
					  break;
			}
			case 5:
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 6);
					  printf("%i   ", valor);
					  break;
			}
			case 6:
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 14);
					  printf("%i   ", valor);
					  break;
			}
			case BOMBAHOR:
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 15);
					  printf("H   ");
					  break;
			}
			case BOMBAVER:
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 15);
					  printf("V   ");
					  break;
			}
			case BOMBATNT:
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 15);
					  printf("T   ");
					  break;
			}
			case BOMBAPUZZLE:
			{
					  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 15);
					  printf("P   ");
					  break;
			}
			default:
			{
				SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 15);
				break;
			}
			}
			
			
		}
		printf("\n");
		SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 15);
	}

}

//guarda los datos de dificultad, numero de filas y de columnas en un fichero .txt y guarda la matriz que representa al tablero en un fichero .data
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


//carga los datos de dificultad, numero de columnas y numero de filas de la partida
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

//carga la matriz que representa al tablero guardada en el fichero .data
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

//devuelve true si la casilla indicada es una bomba y guarda el tipo de bomba en la variable tipo_bomba
bool es_bomba(int * matriz, int fila, int columna, int num_columnas, int &tipo_bomba)
{
	bool es_bomba = false;
	int valor = matriz[fila * num_columnas + columna];

	if ((valor == BOMBAHOR) || (valor == BOMBAVER) || (valor == BOMBATNT) || (valor == BOMBAPUZZLE))
	{
		es_bomba = true;
		tipo_bomba = valor;
	}

	return es_bomba;
}

//helper para la ejecuci�n del KernelJugar
void jugar(int *tablero, int fil, int col, int size, int fila, int columna, int num_colores, int * num_vidas){
	//Este m�todo lanzar� el kernel de juego.
	//Primero creamos la variable que va al device:
	int* tableroD;
	int * num_vidasD;

	//Reservamos memoria 
	cudaMalloc(&tableroD, size);
	cudaMalloc(&num_vidasD, sizeof(int));

	//Copiamos variables al device.
	cudaMemcpy(tableroD, tablero, size, cudaMemcpyHostToDevice);
	cudaMemcpy(num_vidasD, num_vidas, sizeof(int), cudaMemcpyHostToDevice);

	//definimos las dimensionew del grid y del bloque
	dim3 DimGrid(1, 1);
	dim3 DimBlock(col, fil);
	KernelJugar << <DimGrid, DimBlock >> >(tableroD, fil, col, fila, columna, (rand() % 2 + 1), (rand() % num_colores + 1),num_vidasD);

	//copiamos variables al host
	cudaMemcpy(tablero, tableroD, size, cudaMemcpyDeviceToHost);
	cudaMemcpy(num_vidas, num_vidasD, sizeof(int), cudaMemcpyDeviceToHost);

	//liberamos memoria
	cudaFree(tableroD);
	cudaFree(num_vidasD);

	//Falta mostrarlo
	dibujar_matriz(tablero, fil, col);
	cudaDeviceReset();
}

//helper para la ejecuci�n del kernel explosion_vertical
void explotar_vertical(int *& tablero, long tam_tablero ,int filas, int columnas, int columna)
{
	int * tablero_d;

	//reservar memoria
	cudaMalloc(&tablero_d, tam_tablero * sizeof(int));
	//copiar tablero al device
	cudaMemcpy(tablero_d, tablero, tam_tablero * sizeof(int), cudaMemcpyHostToDevice);
	//definir tama�o de grid y de bloque
	dim3 DimGrid(1, 1);
	dim3 DimBlock(columnas, filas);

	explosion_vertical<<<DimGrid,DimBlock>>>(tablero_d, columnas, columna);

	//copiar tablero al host
	cudaMemcpy(tablero, tablero_d, tam_tablero * sizeof(int), cudaMemcpyDeviceToHost);

	dibujar_matriz(tablero, filas, columnas);

	//liberar memoria
	cudaFree(tablero_d);
	cudaDeviceReset();

}

//helper para la ejecucion del kernel explotar_horizontal
void explotar_horizontal(int *& tablero, long tam_tablero, int filas, int columnas, int fila)
{
	int * tablero_d;

	//reservar memoria
	cudaMalloc(&tablero_d, tam_tablero * sizeof(int));
	//copiar tablero al device
	cudaMemcpy(tablero_d, tablero, tam_tablero * sizeof(int), cudaMemcpyHostToDevice);
	//definir tama�o de grid y de bloque
	dim3 DimGrid(1, 1);
	dim3 DimBlock(columnas, filas);

	explosion_horizontal << <DimGrid, DimBlock >> >(tablero_d, columnas, fila);

	//copiar tablero al host
	cudaMemcpy(tablero, tablero_d, tam_tablero * sizeof(int), cudaMemcpyDeviceToHost);

	dibujar_matriz(tablero, filas, columnas);

	//liberar memoria
	cudaFree(tablero_d);
	cudaDeviceReset();

}

//helper para la ejecucion del kernel explosion_tnt
void explotar_tnt(int *& tablero, long tam_tablero, int filas, int columnas, int fila, int columna)
{
	int * tablero_d;

	//reservar memoria
	cudaMalloc(&tablero_d, tam_tablero * sizeof(int));
	//copiar tablero al device
	cudaMemcpy(tablero_d, tablero, tam_tablero * sizeof(int), cudaMemcpyHostToDevice);
	//definir tama�o de grid y de bloque
	dim3 DimGrid(1, 1);
	dim3 DimBlock(columnas, filas);

	explosion_tnt << <DimGrid, DimBlock >> >(tablero_d,tam_tablero, filas, columnas, fila, columna);

	//copiar tablero al host
	cudaMemcpy(tablero, tablero_d, tam_tablero * sizeof(int), cudaMemcpyDeviceToHost);

	dibujar_matriz(tablero, filas, columnas);

	//liberar memoria
	cudaFree(tablero_d);
	cudaDeviceReset();

}

//helper para la ejecucion del kernel explosion_puzle
void explotar_puzle(int *& tablero, long tam_tablero, int filas, int columnas, int fila, int columna, int num_colores)
{
	int * tablero_d;
	int color = rand() % num_colores + 1;

	//reservar memoria
	cudaMalloc(&tablero_d, tam_tablero * sizeof(int));
	//copiar tablero al device
	cudaMemcpy(tablero_d, tablero, tam_tablero * sizeof(int), cudaMemcpyHostToDevice);
	//definir tama�o de grid y de bloque
	dim3 DimGrid(1, 1);
	dim3 DimBlock(columnas, filas);

	explosion_puzle << <DimGrid, DimBlock >> >(tablero_d, columnas, fila, columna, color);

	//copiar tablero al host
	cudaMemcpy(tablero, tablero_d, tam_tablero * sizeof(int), cudaMemcpyDeviceToHost);

	dibujar_matriz(tablero, filas, columnas);

	//liberar memoria
	cudaFree(tablero_d);
	cudaDeviceReset();

}

__global__ void KernelJugar(int *tablero, int fila, int columna, int i, int j, int bomba, int color, int * num_vidas){ //fila y columna indican el m�ximo n�mero en el tablero de juego, i y j las cordenadas del a eliminar.
	//Si el hilo es el que ha seleccionado el jugador:
	int x = threadIdx.x;
	int y = threadIdx.y;
	int numCeros = 0;

	if (x == i && y == j){ //si el hilo se corresponde con la casilla elegida por el jugador

		comprobarBloques(tablero, x, y, fila, columna, num_vidas); //comprueba si los bloques adyacentes contienen el mismo valor, para eliminarlos

		for (int l = 0; l < fila*columna; l++)//contar cuantos bloques vac�os hay en el tablero despu�s de eliminar bloques
		{
			if (tablero[l] == 0){
				numCeros++;
			}
		}

		//en funci�n de cuantos bloques haya vac�os, se determina que bomba aparece
		if (numCeros >= 7){
			tablero[x*fila + y] = BOMBAPUZZLE;
		}
		else if (numCeros == 6){
			tablero[x*fila + y] = BOMBATNT;
		}
		else if (numCeros >= 4){
			if (bomba == 1){
				tablero[x*columna + y] = BOMBAHOR;
			}
			else{
				tablero[x*columna + y] = BOMBAVER;
			}
		}
	}
	
	__syncthreads();
	for (int i = 0; i <= fila; i++){
		if (x > 0){
			if (tablero[x*columna + y] == 0 && !tablero[(x - 1)*columna + y] == 0){
				tablero[x*columna + y] = tablero[(x - 1)*columna + y];
				tablero[(x - 1)*columna + y] = 0;
			}
		}
		__syncthreads();
	}
}

//comprueba si los bloques adyacentes contienen el mismo valor, para eliminarlos
__device__ void comprobarBloques(int *tablero, int x, int y, int fila, int columna , int * num_vidas){ //X indica la fila, Y la columna
	//Primero compruebo si en alg�n lateral del tablero En el juego solo puede estar arriba, abajo, derecha o izquierda.Sin diagonales.
	bool fallo = true;
	if (x != 0 && tablero[(x*columna) + y] == tablero[((x*columna) + y) - columna]){//Compruebo arriba
		//La primera comprobaci�n comprueba que no sea la ficha de m�s arriba, por que si lo es no puede comprobar.
		//Si se cumple es que hay una ficha igual arriba.
		fallo = false;
		comprobarBloquesArriba(tablero, x - 1, y, fila, columna);
	}
	if (y != columna - 1 && (y + 1) && tablero[(x*columna) + y] == tablero[((x*columna) + y) + 1]){//Compruebo a la derecha.
		//La primera comprobacion mira si el elemento no es el �ltimo de la matriz a la derecha, porque si lo fuera no puede comprobar a la derecha, pues 
		//Ya no habr�a m�s derecha.
		//Si se cumple.
		fallo = false;
		//Llamo a eliminar derecha.
		comprobarBloquesDerecha(tablero, x, y + 1, fila, columna);
	}
	if (y != 0 && tablero[(x*columna) + y] == tablero[((x*columna) + y) - 1]){ //Compruebo a la izquierda.
		//Si la columna es 0 es que es el elemento de m�s a la izquierda.
		//Si se cumple llamo a eliminar izquierda.
		fallo = false;
		comprobarBloquesIzquierda(tablero, x, y - 1, fila, columna);
	}
	if (x != fila - 1 && tablero[(x*columna) + y] == tablero[((x*columna) + y) + columna]){//Compruebo abajo.
		//La primera comprobaci�n comprubea si no es el elemento de la �ltima fila, en caso afirmativo, no busca m�s abajo pues no hay.
		//Si se cumple llamo a eliminar abajo.
		fallo = false;
		comprobarBloquesAbajo(tablero, x + 1, y, fila, columna);
	}
	if (fallo == true){
		printf("Ninguna combinacion posible,vuelve a intentarlo:");
		atomicSub(num_vidas, 1);
	}
	else{
		tablero[(x*columna) + y] = 0;//Lo pasamos a cero para despu�s eliminarlo.
	}
}

__device__ void comprobarBloquesArriba(int *tablero, int x, int y, int fila, int columna){
	//Funci�n que comprueba arriba del bloque inicial si hay m�s bloques a eliminar.
	//Misma comprobaci�n que en comprobar bloques normales solo que ya no mira abajo.
	if (x != 0 && tablero[(x*columna) + y] == tablero[((x*columna) + y) - columna]){//Compruebo arriba
		//Si se cumple es que hay una ficha igual arriba.
		comprobarBloquesArriba(tablero, x - 1, y, fila, columna);
	}
	if (y != columna - 1 && (y + 1) && tablero[(x*columna) + y] == tablero[((x*columna) + y) + 1]){//Compruebo a la derecha.
		//La primera comprobacion mira si el elemento no es el �ltimo de la matriz a la derecha, porque si lo fuera no puede comprobar a la derecha, pues 
		//Ya no habr�a m�s derecha.
		//Si se cumple.
		//Llamo a eliminar derecha.
		comprobarBloquesDerecha(tablero, x, y + 1, fila, columna);
	}
	if (y != 0 && tablero[(x*columna) + y] == tablero[((x*columna) + y) - 1]){ //Compruebo a la izquierda.
		//Si la columna es 0 es que es el elemento de m�s a la izquierda.
		//Si se cumple llamo a eliminar izquierda.
		comprobarBloquesIzquierda(tablero, x, y - 1, fila, columna);
	}
	tablero[(x*columna) + y] = 0;
}

__device__ void comprobarBloquesDerecha(int *tablero, int x, int y, int fila, int columna){
	//Funci�n que comprueba a la derecha del bloque inicial si hay m�s bloques a eliminar.

	//Misma comprobaci�n que en comprobar bloques normales solo que ya no mira a la izquierda.
	if (x != 0 && tablero[(x*columna) + y] == tablero[((x*columna) + y) - columna]){//Compruebo arriba
		//Si se cumple es que hay una ficha igual arriba.
		comprobarBloquesArriba(tablero, x - 1, y, fila, columna);
	}
	if (y != columna - 1 && (y + 1) && tablero[(x*columna) + y] == tablero[((x*columna) + y) + 1]){//Compruebo a la derecha.
		//La primera comprobacion mira si el elemento no es el �ltimo de la matriz a la derecha, porque si lo fuera no puede comprobar a la derecha, pues 
		//Ya no habr�a m�s derecha.
		//Si se cumple.
		comprobarBloquesDerecha(tablero, x, y + 1, fila, columna);
		//Llamo a eliminar derecha.
	}
	if (x != fila - 1 && tablero[(x*columna) + y] == tablero[((x*columna) + y) + columna]){//Compruebo abajo.
		//La primera comprobaci�n comprubea si no es el elemento de la �ltima fila, en caso afirmativo, no busca m�s abajo pues no hay.
		//Si se cumple llamo a eliminar abajo.
		comprobarBloquesAbajo(tablero, x + 1, y, fila, columna);
	}
	tablero[(x*columna) + y] = 0;//Si se llama a esta funci�n, es que el elemento actual tambi�n debemos eliminarlo.
}

__device__ void comprobarBloquesIzquierda(int *tablero, int x, int y, int fila, int columna){
	//Funci�n que comprueba a la izquierda del bloque inicial si hay m�s bloques a eliminar.
	//Misma comprobaci�n que en comprobar bloques normales solo que ya no mira a la derecha.
	if (x != 0 && tablero[(x*columna) + y] == tablero[((x*columna) + y) - columna]){//Compruebo arriba
		//Si se cumple es que hay una ficha igual arriba.
		comprobarBloquesArriba(tablero, x - 1, y, fila, columna);

	}
	if (y != 0 && tablero[(x*columna) + y] == tablero[((x*columna) + y) - 1]){ //Compruebo a la izquierda.
		//Si la columna es 0 es que es el elemento de m�s a la izquierda.
		//Si se cumple llamo a eliminar izquierda.
		comprobarBloquesIzquierda(tablero, x, y - 1, fila, columna);
	}
	if (x != fila - 1 && tablero[(x*columna) + y] == tablero[((x*columna) + y) + columna]){//Compruebo abajo.
		//La primera comprobaci�n comprubea si no es el elemento de la �ltima fila, en caso afirmativo, no busca m�s abajo pues no hay.
		//Si se cumple llamo a eliminar abajo.
		comprobarBloquesAbajo(tablero, x + 1, y, fila, columna);
	}
	tablero[(x*columna) + y] = 0;//Si se llama a esta funci�n, es que el elemento actual tambi�n debemos eliminarlo.
}

__device__ void comprobarBloquesAbajo(int *tablero, int x, int y, int fila, int columna){
	//Funci�n que comprueba abajo del bloque inicial si hay m�s bloques a eliminar.
	//Misma comprobaci�n que en comprobar bloques normales solo que ya no mira arriba
	if (y != columna - 1 && (y + 1) && tablero[(x*columna) + y] == tablero[((x*columna) + y) + 1]){//Compruebo a la derecha.
		//La primera comprobacion mira si el elemento no es el �ltimo de la matriz a la derecha, porque si lo fuera no puede comprobar a la derecha, pues 
		//Ya no habr�a m�s derecha.
		//Si se cumple.
		comprobarBloquesDerecha(tablero, x, y + 1, fila, columna);
		//Llamo a eliminar derecha.
	}
	if (y != 0 && tablero[(x*columna) + y] == tablero[((x*columna) + y) - 1]){ //Compruebo a la izquierda.
		//Si la columna es 0 es que es el elemento de m�s a la izquierda.
		//Si se cumple llamo a eliminar izquierda.
		comprobarBloquesIzquierda(tablero, x, y - 1, fila, columna);
	}
	if (x != fila - 1 && tablero[(x*columna) + y] == tablero[((x*columna) + y) + columna]){//Compruebo abajo.
		//La primera comprobaci�n comprubea si no es el elemento de la �ltima fila, en caso afirmativo, no busca m�s abajo pues no hay.
		comprobarBloquesAbajo(tablero, x + 1, y, fila, columna);
		//Si se cumple llamo a eliminar abajo.
	}
	tablero[(x*columna) + y] = 0;//Si se llama a esta funci�n, es que el elemento actual tambi�n debemos eliminarlo.
}

//ejecuta la explosi�n de una bomba vertical
__global__ void explosion_vertical(int * tablero, int anchura_tablero,int columna)
{
	int fila_hilo = threadIdx.y;
	int columna_hilo = threadIdx.x;

	if (columna_hilo == columna)//si la columna a la que pertenece el hilo, es la columna donde se da la explosi�n, el hilo elimina el valor que haya en su posici�n
	{
		tablero[fila_hilo * anchura_tablero + columna_hilo] = 0;
	}

	__syncthreads();
}

//ejecuta la explosi�n de una bomba horizontal
__global__ void explosion_horizontal(int * tablero, int anchura_tablero, int fila)
{
	int fila_hilo = threadIdx.y;
	int columna_hilo = threadIdx.x;

	if (fila_hilo == fila)//si la fila a la que pertenece el hilo, es la fila donde se da la explosi�n, el hilo elimina el valor que haya en su posici�n
	{
		tablero[fila_hilo * anchura_tablero + columna_hilo] = 0;
	}

	__syncthreads();
}

//ejecuta la explosi�n de una bomba tnt
__global__ void explosion_tnt(int * tablero, long tam_tablero, int filas, int columnas, int fila, int columna)
{
	int fila_hilo = threadIdx.y;
	int columna_hilo = threadIdx.x;
	int pos_hilo = fila_hilo * columnas + columna_hilo;//posici�n del hilo

	//posicion elegida por el usuario
	int pos_elegida = fila * columnas + columna;

	//posiciones contiguas
	int arriba = (fila - 1) * columnas + columna;
	int abajo = (fila + 1) * columnas + columna;
	int derecha = fila * columnas + columna + 1;
	int izquierda = fila * columnas + columna - 1;
	int arriba_izq = (fila - 1) * columnas + columna - 1;
	int arriba_der = (fila - 1) * columnas + columna + 1;
	int abajo_izq = (fila + 1) * columnas + columna - 1;
	int abajo_der = (fila + 1) * columnas + columna + 1;

	//si la posici�n del hilo, es adyacente a la posici�n elegida por el usuario, el hilo elimina el valor correspondiente a su posici�n
	if ((pos_hilo == pos_elegida) || (pos_hilo == arriba) || (pos_hilo == abajo) || (pos_hilo == derecha) || (pos_hilo == izquierda))
	{
		tablero[pos_hilo] = 0;
	}
	else if ((pos_hilo == arriba_izq) || (pos_hilo == abajo_izq) || (pos_hilo == arriba_der) || (pos_hilo == abajo_der))
	{
		tablero[pos_hilo] = 0;
	}
}

//ejecuta la explosi�n de una bomba puzle
__global__ void explosion_puzle(int * tablero, int columnas, int fila, int columna, int color)
{
	int fila_hilo = threadIdx.y;
	int columna_hilo = threadIdx.x;

	if (tablero[fila * columnas + columna] == tablero[fila_hilo * columnas + columna_hilo])//si el valor de la posicion del hilo es la bomba puzle, esta se borra
	{
		tablero[fila_hilo * columnas + columna_hilo] = 0;
	}

	if (tablero[fila_hilo * columnas + columna_hilo] == color)//si en la posicion del hilo hay un valor del color elegido, se borra
	{
		tablero[fila_hilo * columnas + columna_hilo] = 0;
	}
}

