TESTS=test-stataemachine.scm \
			test-coroutines.scm \
			test-mcast.scm

TESTS_ENVIRONMENT=\
  guile --no-auto-compile -L $(top_srcdir) -L $(top_srcdir)/src --use-srfi=64

%.check: %
	echo $(TESTS_ENVIRONMENT)
	$(TESTS_ENVIRONMENT) $*

EXTRA_DIST=$(TESTS)
CLEANFILES=*.log

