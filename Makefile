NAME=utilada

-include Makefile.conf

MAKE_ARGS = -XHARDWARE_PLATFORM=$(HARDWARE_PLATFORM)
STATIC_MAKE_ARGS = $(MAKE_ARGS) -XUTIL_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XLZMA_LIBRARY_TYPE=relocatable -XLZMA_BUILD=relocatable
ifeq ($(HAVE_AWS),yes)
SHARED_MAKE_ARGS += -XAWS_BUILD=relocatable
endif

# The timeout execution time in second for a test case.
# The concurrent fifo test takes arround 120 seconds on some ARM but only
# 4 seconds on an i7.  Make this a make configuration variable so that it
# can be given when launching make.
TEST_TIMEOUT=30

ifeq ($(HAVE_UTILGEN),yes)
UTIL_GEN_FILES=src/sys/os-generated/util-systems-constants.ads
UTIL_GEN_FILES+=src/sys/os-generated/util-systems-types.ads
ifeq ($(HAVE_CURL),yes)
UTIL_GEN_FILES+=src/sys/http/curl/util-http-clients-curl-constants.ads
endif
endif

include Makefile.defaults

setup:: $(UTIL_GEN_FILES)

$(eval $(call ada_library,utilada_core))
$(eval $(call ada_library,utilada_base))
$(eval $(call ada_library,utilada_sys))

ifeq ($(HAVE_XML_ADA),yes)
$(eval $(call ada_library,utilada_xml))
endif

ifeq ($(HAVE_CURL),yes)
$(eval $(call ada_library,utilada_curl))
endif

ifeq ($(HAVE_AWS),yes)
$(eval $(call ada_library,utilada_aws))
endif

ifeq ($(HAVE_LZMA),yes)
$(eval $(call ada_library,utilada_lzma))
endif

$(eval $(call ada_library,utilada_unit))

install::
	mkdir -p $(DESTDIR)${prefix}/${ADA_PRJ_BASE}
	cp utilada_http.gpr $(DESTDIR)${prefix}/${ADA_PRJ_BASE}

$(eval $(call ada_library,utilada_http))

build-test:: regtests/util-testsuite.adb
	$(GNATMAKE) $(GPRFLAGS) -p -Ptests_proc $(MAKE_ARGS)
	$(GNATMAKE) $(GPRFLAGS) -p -Putilada_tests $(MAKE_ARGS)

# Build and run the unit tests
test:	build
	-bin/util_harness -l $(NAME): -xml util-aunit.xml -timeout ${TEST_TIMEOUT}

regtests/util-testsuite.adb: regtests/util-testsuite.gpb Makefile.conf
	gnatprep -DHAVE_XML=$(HAVE_XML_ADA) -DHAVE_CURL=$(HAVE_CURL) \
                 -DHAVE_AWS=$(HAVE_AWS) \
				 -DHAVE_VECTOR_MAPPERS=$(HAVE_VECTOR_MAPPERS) \
                 -DHAVE_LZMA=$(HAVE_LZMA) \
		 -DOS_VERSION='"$(OS_VERSION)"' \
		 regtests/util-testsuite.gpb $@

CLEAN_FILES=$(UTIL_GEN_FILES) bin/util_harness
CLEAN_FILES+= bin/util_test_process bin/utilgen

# Clean the root project of all build products.
clean::
	-rm -f test?.log test.log test-stream.txt test-write.txt util-tests.xml

ifeq (${HAVE_PANDOC},yes)
ifeq (${HAVE_DYNAMO},yes)
doc::  docs/util-book.pdf docs/util-book.html
	$(DYNAMO) build-doc -markdown wiki

UTIL_DOC= \
  docs/title.md \
  docs/pagebreak.tex \
  docs/index.md \
  docs/pagebreak.tex \
  docs/Installation.md \
  docs/pagebreak.tex \
  docs/Util_Log.md \
  docs/pagebreak.tex \
  docs/Util_Properties.md \
  docs/pagebreak.tex \
  docs/Util_Dates.md \
  docs/pagebreak.tex \
  docs/Util_Beans.md \
  docs/pagebreak.tex \
  docs/Util_Commands.md \
  docs/pagebreak.tex \
  docs/Serialization.md \
  docs/pagebreak.tex \
  docs/Util_Http.md \
  docs/pagebreak.tex \
  docs/Util_Streams.md \
  docs/pagebreak.tex \
  docs/Util_Encoders.md \
  docs/pagebreak.tex \
  docs/Util_Events_Timers.md \
  docs/pagebreak.tex \
  docs/Util_Measures.md

DOC_OPTIONS=-f markdown -o docs/util-book.pdf
DOC_OPTIONS+= --listings --number-sections --toc
HTML_OPTIONS=-f markdown -o docs/util-book.html
HTML_OPTIONS+= --listings --number-sections --toc --css docs/pandoc.css

docs/util-book.pdf:  force
	$(DYNAMO) build-doc -pandoc docs
	pandoc $(DOC_OPTIONS) --template=./docs/eisvogel.tex $(UTIL_DOC)

docs/util-book.html: docs/util-book.pdf force
	pandoc $(HTML_OPTIONS) $(UTIL_DOC)
endif
endif

install-support:
	$(MKDIR) -p ${bindir}
	${CP} support/*.sh ${bindir}
	${CP} support/*.xsl ${bindir}

src/sys/os-generated/util-systems-constants.ads:	bin/utilgen
	mkdir -p src/sys/os-generated
	bin/utilgen > $@

src/sys/os-generated/util-systems-types.ads:	bin/utilgen
	mkdir -p src/sys/os-generated
	bin/utilgen types > $@

src/sys/http/curl/util-http-clients-curl-constants.ads:	bin/utilgen
	bin/utilgen curl > $@

# Utility for the generation of util-systems-constants.ads
bin/utilgen:    support/utilgen.c Makefile.conf
	mkdir -p bin
	$(CC) -o $@ $(CFLAGS) -g support/utilgen.c

