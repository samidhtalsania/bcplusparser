#libtool
ACLOCAL_AMFLAGS = -I $(PROJECT_ROOT)/build-scripts

AM_CPPFLAGS = -I $(PROJECT_ROOT)/include
AM_CFLAGS = -I $(PROJECT_ROOT)/include
AM_CXXFLAGS = -I $(PROJECT_ROOT)/include

# Debugging Flags
if DEBUG
AM_CPPFLAGS += -DDEBUG -g3 -O0
AM_CFLAGS += -g3 -O0 
AM_CXXFLAGS += -DDEBUG -g3 -O0
else
AM_CPPFLAGS += -DNDEBUG -O3
AM_CFLAGS += -O3 
AM_CXXFLAGS += -DNDEBUG -O3
endif

# RE2C rules
if MAINTAINER_MODE 
if HAVE_RE2C
.re2c.cpp:
	$(RE2C) -c $< > $@
.r2c.cpp:
	$(RE2C) $< > $@
else 
.re2c.cpp:
	@- echo "ERROR: Configured to build in maintainer mode but re2c is not installed on the computer."
	@- echo "ERROR: Modified re2c files cannot be compiled into the corresponding .cpp file."
	@- echo "ERROR: Please install re2c or add the '--disable-maintainer-mode' option during configuration."
	@- exit 1
.r2c.cpp:
	@- echo "ERROR: Configured to build in maintainer mode but re2c is not installed on the computer."
	@- echo "ERROR: Modified re2c files cannot be compiled into the corresponding .cpp file."
	@- echo "ERROR: Please install re2c or add the '--disable-maintainer-mode' option during configuration."
	@- exit 1
endif
else
.re2c.cpp:
	@- echo "WARNING: The system must be configured to build in maintainer mode in order to rebuild re2c files."
	@- echo "WARNING: These files will not be rebuilt unless you rerun the configure script with the '--enable-maintainer-mode' flag."
.r2c.cpp:
	@- echo "WARNING: The system must be configured to build in maintainer mode in order to rebuild re2c files."
	@- echo "WARNING: These files will not be rebuilt unless you rerun the configure script with the '--enable-maintainer-mode' flag."
endif

# lemon rules
if MAINTAINER_MODE
if HAVE_LEMON
.y.h:
	$(LEMON) $<
.yy.h:
	$(LEMON) $<
.ypp.h:
	$(LEMON) $<

.y.c:
	$(LEMON) $<
.yy.c:
	$(LEMON) $<
.ypp.c:
	$(LEMON) $<
else

.y.h:
	@- echo "ERROR: Configured to build in maintainer mode but lemon is not installed on the computer."
	@- echo "ERROR: Modified lemon files cannot be compiled into the corresponding .cpp file."
	@- echo "ERROR: Please install lemon or add the '--disable-maintainer-mode' option during configuration."
	@- exit 1
.yy.h:
	@- echo "ERROR: Configured to build in maintainer mode but lemon is not installed on the computer."
	@- echo "ERROR: Modified lemon files cannot be compiled into the corresponding .cpp file."
	@- echo "ERROR: Please install lemon or add the '--disable-maintainer-mode' option during configuration."
	@- exit 1
.ypp.h:
	@- echo "ERROR: Configured to build in maintainer mode but lemon is not installed on the computer."
	@- echo "ERROR: Modified lemon files cannot be compiled into the corresponding .cpp file."
	@- echo "ERROR: Please install lemon or add the '--disable-maintainer-mode' option during configuration."
	@- exit 1

.y.c:
	@- echo "ERROR: Configured to build in maintainer mode but lemon is not installed on the computer."
	@- echo "ERROR: Modified lemon files cannot be compiled into the corresponding .cpp file."
	@- echo "ERROR: Please install lemon or add the '--disable-maintainer-mode' option during configuration."
	@- exit 1
.yy.c:
	@- echo "ERROR: Configured to build in maintainer mode but lemon is not installed on the computer."
	@- echo "ERROR: Modified lemon files cannot be compiled into the corresponding .cpp file."
	@- echo "ERROR: Please install lemon or add the '--disable-maintainer-mode' option during configuration."
	@- exit 1
.ypp.cpp:
	@- echo "ERROR: Configured to build in maintainer mode but lemon is not installed on the computer."
	@- echo "ERROR: Modified lemon files cannot be compiled into the corresponding .cpp file."
	@- echo "ERROR: Please install lemon or add the '--disable-maintainer-mode' option during configuration."
	@- exit 1

endif
else

.y.h:
	@- echo "WARNING: The system must be configured to build in maintainer mode in order to rebuild lemon files."
	@- echo "WARNING: These files will not be rebuilt unless you rerun the configure script with the '--enable-maintainer-mode' flag."
.yy.h:
	@- echo "WARNING: The system must be configured to build in maintainer mode in order to rebuild lemon files."
	@- echo "WARNING: These files will not be rebuilt unless you rerun the configure script with the '--enable-maintainer-mode' flag."
.ypp.h:
	@- echo "WARNING: The system must be configured to build in maintainer mode in order to rebuild lemon files."
	@- echo "WARNING: These files will not be rebuilt unless you rerun the configure script with the '--enable-maintainer-mode' flag."

.y.c:
	@- echo "WARNING: The system must be configured to build in maintainer mode in order to rebuild lemon files."
	@- echo "WARNING: These files will not be rebuilt unless you rerun the configure script with the '--enable-maintainer-mode' flag."
.yy.c:
	@- echo "WARNING: The system must be configured to build in maintainer mode in order to rebuild lemon files."
	@- echo "WARNING: These files will not be rebuilt unless you rerun the configure script with the '--enable-maintainer-mode' flag."
.ypp.c:
	@- echo "WARNING: The system must be configured to build in maintainer mode in order to rebuild lemon files."
	@- echo "WARNING: These files will not be rebuilt unless you rerun the configure script with the '--enable-maintainer-mode' flag."
endif


bin_PROGRAMS = bcplusparser

bcplusparser_CPPFLAGS = $(BOOST_CPPFLAGS)
bcplusparser_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
bcplusparser_LDADD = $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)

BUILT_SOURCES = \
	Configuration.cpp \
	parser/symbols/Symbol.cpp \
	parser/detail/Scanner.cpp \
	parser/detail/atom_parser.c

#cplus2asp_SOURCES = \
#	Configuration.cpp \
#	main.cpp \
#	Parser.cpp \
#	Referenced.cpp \
#	parser/SymbolTable.cpp \
#	parser/elements/AtomicFormula.cpp \
#	parser/elements/Element.cpp \
#	parser/elements/QuantifierFormula.cpp \
#	parser/symbols/ConstantSymbol.cpp \
#	parser/symbols/ObjectSymbol.cpp \
#	parser/symbols/SortSymbol.cpp \
#	parser/symbols/Symbol.cpp \
#	parser/symbols/VariableSymbol.cpp \
#	parser/symbols/detail/BaseSymbol.cpp \
#	parser/detail/atom_parser.c \
#	parser/detail/ScannerBufferSource.cpp \
#	parser/detail/ScannerFileSource.cpp \
#	parser/detail/Scanner.cpp \
#	parser/detail/ScannerSource.cpp 


bcplusparser_SOURCES = \
	Configuration.cpp \
	main.cpp \
	Referenced.cpp \
	parser/SymbolTable.cpp \
	parser/elements/AtomicFormula.cpp \
	parser/elements/Element.cpp \
	parser/elements/QuantifierFormula.cpp \
	parser/symbols/ConstantSymbol.cpp \
	parser/symbols/ObjectSymbol.cpp \
	parser/symbols/SortSymbol.cpp \
	parser/symbols/Symbol.cpp \
	parser/symbols/VariableSymbol.cpp \
	parser/symbols/detail/BaseSymbol.cpp 
#	parser/detail/atom_parser.c \
#	parser/detail/ScannerBufferSource.cpp \
#	parser/detail/ScannerFileSource.cpp \
#	parser/detail/Scanner.cpp \
#	parser/detail/ScannerSource.cpp 

