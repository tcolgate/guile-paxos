SCHEMESOURCES=mcast.scm datagram-stream.scm

if HAVE_GUILE_COMPILE
GOBJECTS = $(SCHEMESOURCES:%.scm=%.go)
else
GOBJECTS =
endif

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go
.scm.go:
	$(top_srcdir)/build-environ $(GUILE_COMPILE) -o "$@" "$<"

EXTRA_DIST = $(SCHEMESOURCES)
guilesite = @GUILE_SITE@
guileprefix = @GUILE_PREFIX@
guilesitesuffix = `echo $(guilesite) | sed -e 's!^$(guileprefix)!!'`

installdir =  $(exec_prefix)/$(guilesitesuffix)/paxos/net
install_DATA = $(SCHEMESOURCES) $(GOBJECTS)

