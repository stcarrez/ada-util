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
HAVE_UTILGEN?=no
UTIL_OS?=linux64

ifneq ($(HAVE_ALIRE),yes)
MAKE_ARGS += -XUTIL_OS=$(UTIL_OS)
endif

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
UTIL_GEN_FILES+=curl/src/util-http-clients-curl-constants.ads
endif
endif

include Makefile.defaults

DEFAULT_ADA_PROJECT_PATH=$(SRC_ROOT)

ifeq ($(HAVE_XML_ADA),yes)
DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/xml
endif

ifeq ($(HAVE_AWS),yes)
DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/aws
endif

ifeq ($(HAVE_CURL),yes)
DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/curl
endif

ifeq ($(HAVE_LZMA),yes)
DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/lzma
endif

DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/unit:$(ADA_PROJECT_PATH)

setup:: $(UTIL_GEN_FILES)
	echo "HAVE_XML_ADA=$(HAVE_XML_ADA)" >> Makefile.conf
	echo "HAVE_CURL=$(HAVE_CURL)" >> Makefile.conf
	echo "HAVE_AWS=$(HAVE_AWS)" >> Makefile.conf
	echo "HAVE_LZMA=$(HAVE_LZMA)" >> Makefile.conf
	echo "UTIL_OS=$(UTIL_OS)" >> Makefile.conf

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

ifeq ($(HAVE_ALIRE),yes)
build-test:: regtests/src/util-testsuite.adb
	cd regtests && $(BUILD_COMMAND) $(MAKE_ARGS)
else
build-test:: regtests/src/util-testsuite.adb regtests/utilada_tests_custom.gpr
	cd regtests && $(BUILD_COMMAND) $(MAKE_ARGS) -Ptests_proc.gpr
	cd regtests && $(BUILD_COMMAND) $(MAKE_ARGS) -Putilada_tests_custom.gpr
endif

# Build and run the unit tests
test:	build samples
	-bin/util_harness -v -l $(NAME): -xml util-aunit.xml -timeout ${TEST_TIMEOUT}

regtests/src/util-testsuite.adb: regtests/src/util-testsuite.gpb
	$(GNATPREP) -DHAVE_XML=$(HAVE_XML_ADA) -DHAVE_CURL=$(HAVE_CURL) \
		 -DHAVE_AWS=$(HAVE_AWS) \
		 -DHAVE_LZMA=$(HAVE_LZMA) \
		 -DOS_VERSION='"$(UTIL_OS)"' \
		 regtests/src/util-testsuite.gpb $@

regtests/utilada_tests_custom.gpr: regtests/utilada_tests.gpg
	$(GNATPREP) -DHAVE_XML=$(HAVE_XML_ADA) -DHAVE_CURL=$(HAVE_CURL) \
		 -DHAVE_AWS=$(HAVE_AWS) \
		 -DHAVE_LZMA=$(HAVE_LZMA) \
		 -DOS_VERSION='"$(UTIL_OS)"' \
		 regtests/utilada_tests.gpg $@

setup::
	rm -f regtests/src/util-testsuite.adb regtests/utilada_tests_custom.gpr

samples:
ifeq ($(HAVE_ALIRE),yes)
	cd samples && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)
else
	cd samples && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS) -Psamples.gpr
ifeq ($(HAVE_XML_ADA),yes)
	cd samples && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS) -Psamples_xml.gpr
endif
ifeq ($(HAVE_CURL),yes)
	cd samples && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS) -Psamples_curl.gpr
endif
ifeq ($(HAVE_LZMA),yes)
	cd samples && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS) -Psamples_lzma.gpr
endif
endif

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

curl/src/util-http-clients-curl-constants.ads:	bin/utilgen
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
