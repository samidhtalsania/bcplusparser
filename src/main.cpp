
#include <iostream>

#include <boost/filesystem/path.hpp>

#include "babb/utils/memory.h"
#include "memwrappers.h"

#include "bcplus/Configuration.h"
#include "bcplus/Location.h"
#include "bcplus/languages/BCPlus.h"
#include "bcplus/symbols/SymbolTable.h"
#include "bcplus/parser/BCParser.h"


#define VERSION "0.1 ALPHA"

namespace fs = boost::filesystem;
namespace u = babb::utils;
using namespace bcplus;


/// Main driver program
int main(int argc, char const** argv) {

	// load configuration information
	u::ref_ptr<Configuration> conf = new Configuration(argv[0], VERSION);
	
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

	// setup the parser
	conf->ostream(Verb::OP) << "Initializing parser..." << std::endl;
	u::ref_ptr<parser::BCParser> p = new parser::BCParser(conf, new languages::BCPlus());
	conf->ostream(Verb::OP) << "Adding test input..." << std::endl;
	p->push_back(fs::path("tmp.cp"), false);
	
	conf->ostream(Verb::OP) << "Parsing..." << std::endl;
	parser::BCParser::ParseType result;
	do {
		result = p->parse();
	} while (result.first != parser::BCParser::Status::HARD_EOF);


	conf->ostream(Verb::OP) << "Done parsing..." << std::endl;

	// Set up a symbol table
	u::ref_ptr<symbols::SymbolTable> symtab = new symbols::SymbolTable(conf);

	if (!symtab->good()) {
		conf->ostream(Verb::ERROR) << "ERROR: An error occurred loading the symbol table. Exiting." << std::endl;
		return 2;
	}

	conf->ostream(Verb::OP) << "Saving symbol table..." << std::endl;
	symtab = NULL;

	return 0;
}
