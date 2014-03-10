
#include <iostream>

#include "pointers.h"
#include "Configuration.h"

#include "parser/SymbolTable.h"

#define VERSION "0.1 ALPHA"

using namespace bcplus;

/// Main driver program
int main(int argc, char const** argv) {

	// load configuration information
	ref_ptr<Configuration> conf = new Configuration(argv[0], VERSION);
	
	switch (conf->load(argc, argv)) {
	case Option::STAT_OK:					
		break;
	case Option::STAT_BAD_ARG:
		std::cerr << "ERROR: An error occurred parsing command line options. Exiting." << std::endl;
		return 1;
	case Option::STAT_HELP:
		conf->outputHelp(std::cout);
		return 0;
	case Option::STAT_VERSION:
		conf->outputVersion(std::cout);
		return 0;
	}

	if (!conf->good()) {
		std::cerr << "ERROR: An error occurred verifying consistency of command line options. Exiting." << std::endl;
		return 1;
	}

	// Set up a symbol table
	conf->ostream(Verb::OP) << "Loading symbol table..." << std::endl;
	ref_ptr<parser::SymbolTable> symtab = new parser::SymbolTable(conf);

	if (!symtab->good()) {
		conf->ostream(Verb::ERROR) << "ERROR: An error occurred loading the symbol table. Exiting." << std::endl;
		return 2;
	}

	conf->ostream(Verb::OP) << "Saving symbol table..." << std::endl;
	symtab = NULL;

	return 0;
}
