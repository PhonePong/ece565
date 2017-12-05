# Makefile for the hello driver.
PROG=   hello565
SRCS=   hello565.c
 
FILES=${PROG}.conf
FILESNAME=${PROG}
FILESDIR= /etc/system.conf.d
 
DPADD+= ${LIBCHARDRIVER} ${LIBSYS}
LDADD+= -lchardriver -lsys
 
MAN=
 
.include <minix.service.mk>
