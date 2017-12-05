#include <stdio.h>
#include <stdlib.h>
#include <minix/syslib.h>
 
int main(int argc, char **argv)
{
    sef_startup();
    
    printf("Hello, ECE-565!\n");
    return EXIT_SUCCESS;
}
