# $XConsortium: Makefile /main/36 1996/11/13 14:42:50 lehors $

# Luna users will need to either run make as "make MAKE=make"
# or add "MAKE = make" to this file.

DIST_NAME = "itools distribution"
RELEASE = "Release 6.3"
SHELL = /bin/sh
RM = rm -f
MV = mv
WORLDOPTS = -k
WIN32WORLDOPTS = -i
TOP = .
CURRENT_DIR = .
CONFIGSRC = $(TOP)/config
IMAKESRC = $(CONFIGSRC)/imake
DEPENDSRC = $(CONFIGSRC)/makedepend
DEPENDTOP = ../..
IMAKETOP = ../..
IRULESRC = $(CONFIGSRC)/cf
IMAKE = $(IMAKESRC)/imake
IMAKE_CMD = $(IMAKE) -I$(IRULESRC) $(IMAKE_DEFINES)
MAKE_OPTS = -f xmakefile
MAKE_CMD = $(MAKE) $(MAKE_OPTS)
FLAGS = $(MFLAGS) -f Makefile.ini BOOTSTRAPCFLAGS="$(BOOTSTRAPCFLAGS)" $(EXTRA_BOOTSTRAPCFLAGS)

all:
	@$(MAKE_CMD) xmakefile-exists || $(MAKE) all-initial
	@$(MAKE_CMD) $@

all-initial:
	@echo Please use make World, or on NT use nmake World.Win32.
	@echo Check the configuration parameters in the $(IRULESRC) directory,
	@echo and pass BOOTSTRAPCFLAGS if necessary.
	@echo Read the release notes carefully before proceeding.
	@echo Do not name your log file make.log or it will be deleted.

World:
	@echo ""
	@echo Building $(RELEASE) of the $(DIST_NAME).
	@echo ""
	@case "x$(BOOTSTRAPCFLAGS)" in x) \
	echo I hope you checked the configuration parameters in $(IRULESRC) ; \
	echo to see if you need to pass BOOTSTRAPCFLAGS. ; \
	echo "" ; \
	;; esac;
	@echo I hope you checked config/extras/extras.tmpl to see if there
	@echo are any parameters there you wish to change.
	@echo ""
	@date
	@echo ""
	cd $(IMAKESRC) && $(MAKE) $(FLAGS) clean
	$(MAKE) $(MFLAGS) Makefile.boot
	$(MAKE_CMD) $(MFLAGS) VerifyOS
	$(MAKE_CMD) $(MFLAGS) Makefiles
	$(MAKE_CMD) $(MFLAGS) clean BOOTSTRAPSUBDIRS=
	$(MAKE_CMD) $(MFLAGS) includes
	$(MAKE_CMD) $(MFLAGS) depend
	$(MAKE_CMD) $(MFLAGS) $(WORLDOPTS)
	@echo ""
	@date
	@echo ""
	@echo Full build of $(RELEASE) of the $(DIST_NAME) complete.
	@echo ""

.PRECIOUS: Makefile

# This is just a sequence of bootstrapping steps we have to do.
# The steps are listed as separate targets so clearmake can wink in
# the Makefile.proto files.
Makefile.boot: imake.proto $(DEPENDSRC)/Makefile.proto depend.bootstrap $(IMAKESRC)/Makefile.proto imake.bootstrap

imake.proto:
	cd $(IMAKESRC) && $(MAKE) $(FLAGS)
	$(RM) $(DEPENDSRC)/Makefile.proto

$(DEPENDSRC)/Makefile.proto:
	$(IMAKE_CMD) -s $(DEPENDSRC)/Makefile.proto -f $(DEPENDSRC)/Imakefile -DTOPDIR=$(DEPENDTOP) -DCURDIR=$(DEPENDSRC)

depend.bootstrap:
	cd $(DEPENDSRC) && $(RM) -r Makefile Makefile.dep makedepend *.o bootstrap
	cd $(DEPENDSRC) && $(MAKE) -f Makefile.proto bootstrap

$(IMAKESRC)/Makefile.proto:
	$(IMAKE_CMD) -s $(IMAKESRC)/Makefile.proto -f $(IMAKESRC)/Imakefile -DTOPDIR=$(IMAKETOP) -DCURDIR=$(IMAKESRC)

imake.bootstrap:
	cd $(IMAKESRC) && $(MAKE) -f Makefile.proto bootstrapdepend
	cd $(IMAKESRC) && $(MAKE) $(FLAGS) bootstrap
	cd $(IMAKESRC) && $(MAKE) -f Makefile.proto all 
	-@if [ -f xmakefile ]; then set -x; \
	  $(RM) xmakefile.bak; $(MV) xmakefile xmakefile.bak; \
	  else exit 0; fi
	$(MAKE) $(MFLAGS) xmakefile

Makefile::
	$(MAKE) $(MFLAGS) xmakefile

xmakefile: Imakefile
	$(IMAKE_CMD) -s xmakefile -DTOPDIR=$(TOP) -DCURDIR=$(CURRENT_DIR)

World.Win32:
	@echo :
	@echo Building $(RELEASE) of the $(DIST_NAME).
	@echo :
	@echo I hope you checked config/extras/extras.tmpl to see if there
	@echo are any parameters there you wish to change.
	@echo :
	cd $(IMAKESRC)
	$(MAKE) -f Makefile.ini clean.Win32
	$(MAKE) -f Makefile.ini imake.exe
	cd ..\..
	-if exist xmakefile.bak del xmakefile.bak
	-if exist xmakefile ren xmakefile xmakefile.bak
	$(IMAKE:/=\) -s xmakefile -I$(IRULESRC) $(IMAKE_DEFINES) -DTOPDIR=$(TOP) -DCURDIR=$(CURRENT_DIR)
	$(MAKE_CMD) $(MFLAGS) VerifyOS
	$(MAKE_CMD) $(MFLAGS) Makefiles
	$(MAKE_CMD) $(MFLAGS) clean
	cd $(CONFIGSRC)\util
	$(MAKE) mkdirhier.exe
	cd ..\..
	$(MAKE_CMD) $(MFLAGS) includes
	$(MAKE_CMD) $(MFLAGS) depend
	$(MAKE_CMD) $(MFLAGS) $(WIN32WORLDOPTS)
	@echo :
	@echo :
	@echo Full build of $(RELEASE) of the $(DIST_NAME) complete.
	@echo :


# don't allow any default rules in this Makefile
.SUFFIXES:
# quiet "make" programs that display a message if suffix list empty
.SUFFIXES: .Dummy

# a copy of every rule that might be invoked at top level

clean:
	    $(MAKE_CMD) $@
dangerous_strip_clean:
	    $(MAKE_CMD) $@
depend:
	    $(MAKE_CMD) $@
Everything:
	    $(MAKE_CMD) $@
external.ln:
	    $(MAKE_CMD) $@
includes:
	    $(MAKE_CMD) $@
install.ln:
	    $(MAKE_CMD) $@
install.man:
	    $(MAKE_CMD) $@
install:
	    $(MAKE_CMD) $@
Makefiles:
	    $(MAKE_CMD) $@
man_keywords:
	    $(MAKE_CMD) $@
tags:
	    $(MAKE_CMD) $@
VerifyOS:
	    $(MAKE_CMD) $@

README:
	groff -mgs tmac.wrprc README.ms > README.ps
	troff2html -ms tmac.wrprc -a tc.wrprc-html README.ms > README.html
	unroff -ms tmac.wrprc README.ms > README.txt
