# $Id: Makefile,v 1.1 2006/08/10 22:26:59 gdsjaar Exp $
#

EXAMPLES = zCPPExample1 zCPPExample2

all:    $(EXAMPLES)

% : %.cpp ../lib/libexzoltan.a 
	@echo "Building $@"
	@$(CC) $(CFLAGS) -o $@ $(INCLUDE) $(LIBDIR) $< $(LIBS)

../lib/libexzoltan.a:
	@cd ..; $(MAKE) exampleLibrary

clean:
	rm -f *.o $(EXAMPLES)



