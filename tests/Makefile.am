TESTS=test-stataemachine.scm \
			test-coroutines.scm \
			test-mcast.scm \
			test-utils.scm

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = guile
AM_SCM_LOG_FLAGS = --no-auto-compile -L $(top_srcdir) -L $(top_srcdir)/src --use-srfi=64

#%.check: %
#	echo $(TESTS_ENVIRONMENT)
#	$(TESTS_ENVIRONMENT) $*

EXTRA_DIST=$(TESTS)
CLEANFILES=*.log

