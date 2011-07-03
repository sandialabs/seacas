# $Id: Makefile,v 1.1 2006/08/10 22:26:59 gdsjaar Exp $
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



