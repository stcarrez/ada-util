NAME=utilada
VERSION=2.8.0

DIST_DIR=ada-util-$(VERSION)
DIST_FILE=ada-util-$(VERSION).tar.gz

MAKE_ARGS += -XUTIL_BUILD=$(BUILD)

-include Makefile.conf

HAVE_XML_ADA?=yes
HAVE_CURL?=yes
HAVE_AWS?=yes
HAVE_LZMA?=yes

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
UTIL_GEN_FILES=src/base/os-generated/util-systems-constants.ads
UTIL_GEN_FILES+=src/base/os-generated/util-systems-types.ads
ifeq ($(HAVE_CURL),yes)
UTIL_GEN_FILES+=src/sys/http/curl/util-http-clients-curl-constants.ads
endif
endif

include Makefile.defaults

setup:: $(UTIL_GEN_FILES)

$(eval $(call ada_library,utilada_core,.))
$(eval $(call ada_library,utilada_base,.))
$(eval $(call ada_library,utilada_sys,.))

ifeq ($(HAVE_XML_ADA),yes)
$(eval $(call ada_library,utilada_xml,xml))
endif

ifeq ($(HAVE_CURL),yes)
$(eval $(call ada_library,utilada_curl,curl))
endif

ifeq ($(HAVE_AWS),yes)
$(eval $(call ada_library,utilada_aws,aws))
endif

ifeq ($(HAVE_LZMA),yes)
$(eval $(call ada_library,utilada_lzma,lzma))
endif

$(eval $(call ada_library,utilada_unit,unit))

# install::
#	mkdir -p $(DESTDIR)${prefix}/${ADA_PRJ_BASE}
#	cp utilada_http.gpr $(DESTDIR)${prefix}/${ADA_PRJ_BASE}
#
# $(eval $(call ada_library,utilada_http))

build-test:: regtests/src/util-testsuite.adb
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

# Build and run the unit tests
test:	build samples
	-bin/util_harness -v -l $(NAME): -xml util-aunit.xml -timeout ${TEST_TIMEOUT}

regtests/src/util-testsuite.adb: regtests/src/util-testsuite.gpb
	$(ALR) exec -- gnatprep -DHAVE_XML=$(HAVE_XML_ADA) -DHAVE_CURL=$(HAVE_CURL) \
                 -DHAVE_AWS=$(HAVE_AWS) \
                 -DHAVE_LZMA=$(HAVE_LZMA) \
		 -DOS_VERSION='"$(OS_VERSION)"' \
		 regtests/src/util-testsuite.gpb $@

samples:
	cd samples && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

CLEAN_FILES=$(UTIL_GEN_FILES) bin/util_harness
CLEAN_FILES+= bin/util_test_process bin/utilgen

# Clean the root project of all build products.
clean::
	-rm -f test?.log test.log test-stream.txt test-write.txt util-tests.xml

UTIL_DOC= \
  title.md \
  pagebreak.tex \
  index.md \
  pagebreak.tex \
  Installation.md \
  pagebreak.tex \
  Util_Files.md \
  pagebreak.tex \
  Util_Log.md \
  pagebreak.tex \
  Util_Properties.md \
  pagebreak.tex \
  Util_Dates.md \
  pagebreak.tex \
  Util_Beans.md \
  pagebreak.tex \
  Util_Commands.md \
  pagebreak.tex \
  Serialization.md \
  pagebreak.tex \
  Util_Http.md \
  pagebreak.tex \
  Util_Streams.md \
  pagebreak.tex \
  Util_Encoders.md \
  pagebreak.tex \
  Util_Misc.md \
  pagebreak.tex \
  Util_Measures.md

DOC_OPTIONS=-f markdown
DOC_OPTIONS+= --listings --number-sections --toc
HTML_OPTIONS=-f markdown
HTML_OPTIONS+= --listings --number-sections --toc --css docs/pandoc.css

$(eval $(call pandoc_build,utilada-book,$(UTIL_DOC),\
	rm -f docs/user-list.md docs/alloc-sequence.md docs/user_hbm.md; \
	cat docs/Misc.md docs/Util_Nullables.md docs/Util_Texts_Builders.md docs/Util_Listeners.md docs/Util_Events_Timers.md docs/Util_Executors.md > docs/Util_Misc.md; \
	rm -f docs/Util_Nullables.md docs/Util_Texts_Builders.md docs/Util_Listeners.md docs/Util_Events_Timers.md docs/Util_Executors.md))

install-support:
	$(MKDIR) -p ${bindir}
	${CP} support/*.sh ${bindir}
	${CP} support/*.xsl ${bindir}

src/base/os-generated/util-systems-constants.ads:	bin/utilgen
	mkdir -p src/base/os-generated
	bin/utilgen > $@

src/base/os-generated/util-systems-types.ads:	bin/utilgen
	mkdir -p src/base/os-generated
	bin/utilgen types > $@

src/sys/http/curl/util-http-clients-curl-constants.ads:	bin/utilgen
	bin/utilgen curl > $@

# Utility for the generation of util-systems-constants.ads
bin/utilgen:    support/utilgen.c
	mkdir -p bin
	$(CC) -o $@ $(CFLAGS) -g support/utilgen.c

$(eval $(call alire_publish,.,ut/utilada,utilada-$(VERSION).toml))
$(eval $(call alire_publish,unit,ut/utilada_unit,utilada_unit-$(VERSION).toml))
$(eval $(call alire_publish,xml,ut/utilada_xml,utilada_xml-$(VERSION).toml))
$(eval $(call alire_publish,curl,ut/utilada_curl,utilada_curl-$(VERSION).toml))
$(eval $(call alire_publish,aws,ut/utilada_aws,utilada_aws-$(VERSION).toml))
$(eval $(call alire_publish,lzma,ut/utilada_lzma,utilada_lzma-$(VERSION).toml))

.PHONY: samples
