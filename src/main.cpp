
#include <iostream>

#include "pointers.h"
#include "Configuration.h"
#include "ModuleGraph.h"
#include "AtomParser.h"
#include "Status.h"

#define VERSION "0.1 ALPHA"

/// Main driver program
int main(int argc, char const** argv) {
	ref_ptr<Configuration> config = new Configuration(VERSION);

	switch (config->load(argc, argv)) {
	case Option::STAT_OK:
		break;
	case Option::STAT_HELP:
		return Status::OK;
	case Option::STAT_VERSION:
		return Status::OK;
	case Option::STAT_BAD_ARG:
		return Status::ERR_CMD_LINE_SYNTAX;
	}



	config->ostream(Verb::DETAIL) << "Configuration: " << std::endl;
	config->output(config->ostream(Verb::DETAIL)) << std::endl;	
	config->ostream(Verb::OP) << "Verifying command-line... " << std::endl;

	if (!config->verify()) {
		config->outputHelp(config->ostream(Verb::ERROR));
		return Status::ERR_CMD_LINE;
	}

	// TODO


}
