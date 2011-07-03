# $Id: Makefile,v 1.1 2009/06/09 18:37:57 gdsjaar Exp $
#

all:    ex1

ex1:	ex1.o queries1.o 
	@echo "Building $@"
	@$(CC) $(CFLAGS) -o $@ $(INCLUDE) $(LIBDIR) $^ $(LIBS)


%.o : %.c  matrix.h
	@echo "Building $@"
	@$(CC) $(CFLAGS) $(INCLUDE) -c $<

clean:
	rm -f *.o ex1



