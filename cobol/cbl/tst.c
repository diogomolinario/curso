#include <stdio.h>
#include <ctype.h>
void main (){
char *origem = "1234";
unsigned char *destino;
int tamanho_da_origem;
 char caracter, c, d;
// *origem = "123";
 tamanho_da_origem=4;
 int i;

 for ( i = 0, tamanho_da_origem /= 2; i < tamanho_da_origem; i++ )
   {
	   ////printf("### I =>> %i\n\n\n\n",i);
    c = toupper( origem[i*2] );
    d = toupper( origem[i*2+1] );
    if (isdigit(c))
      caracter = ( c & 0x0f ) << 4;
    else
      caracter = ( c - 'A' + 0x0A ) << 4;
    if (isdigit(d))
      destino[i] = ( d & 0x0f ) | caracter;
    else
      destino[i] = ( d - 'A' + 0x0A ) | caracter;
   }
   printf(destino," =>> %s\n\n\n\n");
}