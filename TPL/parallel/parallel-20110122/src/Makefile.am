bin_SCRIPTS = parallel sem sql niceload
man_MANS = parallel.1 sem.1 sql.1 niceload.1
doc_DATA = parallel.html sem.html sql.html niceload.html

parallel.1: parallel.pod Makefile
	pod2man --release='$(PACKAGE_VERSION)' --center='$(PACKAGE_NAME)' \
		--section=1 $(srcdir)/parallel.pod > $(srcdir)/parallel.1

sem.1: sem.pod Makefile
	pod2man --release='$(PACKAGE_VERSION)' --center='$(PACKAGE_NAME)' \
		--section=1 $(srcdir)/sem.pod > $(srcdir)/sem.1

sql.1: sql Makefile
	pod2man --release='$(PACKAGE_VERSION)' --center='$(PACKAGE_NAME)' \
		--section=1 $(srcdir)/sql > $(srcdir)/sql.1

niceload.1: niceload Makefile
	pod2man --release='$(PACKAGE_VERSION)' --center='$(PACKAGE_NAME)' \
		--section=1 $(srcdir)/niceload > $(srcdir)/niceload.1

parallel.html: parallel Makefile sem
	./sem --fg --id pod2html pod2html $(srcdir)/parallel.pod > $(srcdir)/parallel.html
	./sem --fg --id pod2html rm -f $(srcdir)/pod2htm*

sem.html: sem.pod Makefile sem
	./sem --fg --id pod2html pod2html $(srcdir)/sem.pod > $(srcdir)/sem.html
	./sem --fg --id pod2html rm -f $(srcdir)/pod2htm*

sql.html: sql Makefile sem
	./sem --fg --id pod2html pod2html $(srcdir)/sql > $(srcdir)/sql.html
	./sem --fg --id pod2html rm -f $(srcdir)/pod2htm*

niceload.html: niceload Makefile sem
	./sem --fg --id pod2html pod2html $(srcdir)/niceload > $(srcdir)/niceload.html
	./sem --fg --id pod2html rm -f $(srcdir)/pod2htm*

sem: parallel
	ln -fs parallel sem

DISTCLEANFILES = parallel.html sem.html sql.html niceload.html
EXTRA_DIST = parallel sem sql niceload \
	parallel.1 sem.1 sql.1 niceload.1 \
	parallel.html sem.html sql.html niceload.html \
	sem.pod parallel.pod
