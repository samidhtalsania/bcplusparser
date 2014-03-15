include $(top_srcdir)/src/Makefile.rls

bin_PROGRAMS = bcplusparser

bcplusparser_CPPFLAGS = $(BOOST_CPPFLAGS)
bcplusparser_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
bcplusparser_LDADD = $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)

BUILT_SOURCES = \
	bcplus/Configuration.cpp \
	bcplus/symbols/Symbol.cpp \
	bcplus/parser/detail/Scanner.cpp \
	bcplus/parser/detail/lemon_parser.cpp

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
	main.cpp \
	bcplus/Configuration.cpp \
	bcplus/Location.cpp \
	bcplus/elements/AtomicFormula.cpp \
	bcplus/elements/Element.cpp \
	bcplus/elements/QuantifierFormula.cpp \
	bcplus/elements/Macro.cpp \
	bcplus/languages/Language.cpp \
	bcplus/languages/BCPlus.cpp \
	bcplus/symbols/ConstantSymbol.cpp \
	bcplus/symbols/ObjectSymbol.cpp \
	bcplus/symbols/SortSymbol.cpp \
	bcplus/symbols/Symbol.cpp \
	bcplus/symbols/VariableSymbol.cpp \
	bcplus/symbols/MacroSymbol.cpp \
	bcplus/symbols/detail/BaseSymbol.cpp  \
	bcplus/symbols/SymbolTable.cpp \
	bcplus/statements/Statement.cpp \
	bcplus/statements/IncludeStatement.cpp \
	bcplus/parser/Token.cpp \
	bcplus/parser/BCParser.cpp \
	bcplus/parser/detail/lemon_parser.cpp \
	bcplus/parser/detail/Scanner.cpp \
	bcplus/parser/detail/ScannerSource.cpp \
	bcplus/parser/detail/ScannerTokenSource.cpp \
	bcplus/parser/detail/ScannerRawSource.cpp \
	bcplus/parser/detail/ScannerBufferSource.cpp \
	bcplus/parser/detail/ScannerFileSource.cpp 

