
#include <boost/foreach.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/fstream.hpp>

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/exceptions.hpp>
#include <boost/property_tree/xml_parser.hpp>

#include "babb/utils/memory.h"

#include "bcplus/Configuration.h"
#include "bcplus/symbols/Symbol.h"
#include "bcplus/symbols/ConstantSymbol.h"
#include "bcplus/symbols/ObjectSymbol.h"
#include "bcplus/symbols/VariableSymbol.h"
#include "bcplus/symbols/SortSymbol.h"
#include "bcplus/symbols/MacroSymbol.h"
#include "bcplus/symbols/SymbolTable.h"

namespace u = babb::utils;
namespace fs = boost::filesystem;
namespace pt = boost::property_tree;

namespace bcplus {
namespace symbols{

SymbolTable::SymbolTable(Configuration* config)
	: _config(config) {

	// See if we should load anything from a file
	if (config->symtabInput()) _good = load(*(config->symtabInput()));
	else _good = true;

}

SymbolTable::~SymbolTable() {
	// See if we should write anything to a file
	if (_config->symtabOutput() && good()) save(*(_config->symtabOutput()));
}


Symbol const* SymbolTable::resolve(size_t typemask, std::string const& name, size_t arity) const {
	std::string s = Symbol::genName(name, arity);

	SymbolMap::const_iterator it;
	size_t type = 0x0001;
	while (type <= Symbol::Type::_LARGEST_) {
		SymbolMap const& m = _symbols.find((Symbol::Type::Value)type)->second;
		it = m.find(s);
		if (it != m.end()) {
			return it->second;
		}
	
		type = (type << 1);
	}
	return NULL;
}

Symbol* SymbolTable::resolve(size_t typemask, std::string const& name, size_t arity)  {
	std::string s = Symbol::genName(name, arity);

	SymbolMap::iterator it;
	size_t type = 0x0001;
	while (type <= Symbol::Type::_LARGEST_) {
		SymbolMap& m = _symbols[(Symbol::Type::Value)type];
		it = m.find(s);
		if (it != m.end()) {
			return it->second;
		}
	
		type = (type << 1);
	}
	return NULL;
}

bool SymbolTable::create(Symbol* symbol) {

	// check for uniqueness
	// sorts and other symbols are distinguished from each other
	size_t mask = Symbol::Type::SORT;
	if (symbol->type() != Symbol::Type::SORT) mask = ~mask;

	if (resolve(mask, *(symbol->base()), symbol->arity())) {
		return false;
	} 

	// Add the symbol
	_symbols[symbol->type()][*(symbol->name())] = symbol;
	return true;
}

Symbol* SymbolTable::resolveOrCreate(Symbol* symbol) {
	
	// sorts and other symbols are distinguished from each other
	size_t mask = Symbol::Type::SORT;
	if (symbol->type() != Symbol::Type::SORT) mask = ~mask;

	// check if the symbol exists
	Symbol* s;

	if ((s = resolve(mask, *(symbol->base()), symbol->arity()))) {
		// the symbol exists, make sure the definition is compatible
		if (*s == *symbol) return s;
		else return NULL;
	} 


	// Add the symbol
	_symbols[symbol->type()][*(symbol->name())] = symbol;
	return symbol;
}

bool SymbolTable::load(boost::filesystem::path const& path) {
	fs::ifstream in;
	bool good = true;


	try {
		// make sure the file exists
		if (!fs::exists(path)) {
			_config->ostream(Verb::ERROR) << "ERROR: Could not open file \"" << path.native() << "\". File does not exist." << std::endl;
			return false;
		}
	
		// try to open it
		in.open(path);

		// check if we opened it
		if (in.good()) {
			pt::ptree xml;

			// see about reading the file
			read_xml(in, xml, pt::xml_parser::no_comments | pt::xml_parser::trim_whitespace);

			// scan the file for sorts first then rescan for everything else
			for (int pass2 = 0; pass2 < 2; pass2++) {
				BOOST_FOREACH(pt::ptree::value_type& symbols, xml) {
					if (!boost::iequals(symbols.first, "symbols")) {
						_config->ostream(Verb::ERROR) << "ERROR: Encountered unexpected top-level key \"" << symbols.first << "\" in symbol table file \"" << path.native() << "\". Expected \"symbols\"." << std::endl;
						good = false;
					}

					BOOST_FOREACH(pt::ptree::value_type& s, symbols.second) {
						bool add_sym = false;
						u::ref_ptr<Symbol> sym;
						u::ref_ptr<Symbol> tmp;

						switch (Symbol::Type::val(s.first.c_str())) {
						case Symbol::Type::SORT:
							if (!pass2) {
								// First pass, just create the symbol
								sym = new symbols::SortSymbol(s.second, &(_config->ostream(Verb::ERROR)));
								add_sym = true;
							} else {
								// second pass, load the symbol definition
								tmp = new symbols::SortSymbol(s.second, &(_config->ostream(Verb::ERROR)));
								sym = resolveOrCreate(tmp);

								if (!sym) {
									// we weren't able to resolve the sort
									_config->ostream(Verb::ERROR) << "ERROR: Encountered conflicting definitions of sort \"" << *(tmp->base()) << "\" while loading the symbol table from file \"" << path.native() << "\"." << std::endl;
									good = false;
								} else {
									// resolved the sort, try loading the definition
									if (!((symbols::SortSymbol*)sym.get())->loadDefinition(s.second, this, &(_config->ostream(Verb::ERROR)))) {
										_config->ostream(Verb::ERROR) << "ERROR: Encountered conflicting definitions of sort \"" << *(tmp->base()) << "\" while loading the symbol table from file \"" << path.native() << "\"." << std::endl;
										good = false;
									}
								}
							}
							break;

						case Symbol::Type::CONSTANT:
							if (!pass2) {
								// first pass: ignore
							} else {
								// second pass, create the symbol
								sym = new symbols::ConstantSymbol(s.second, this, &(_config->ostream(Verb::ERROR)));
								add_sym = true;
							}
							break;
	
						case Symbol::Type::VARIABLE:
							if (!pass2) {
								// first pass: ignore
							} else {
								// second pass, create the symbol
								sym = new symbols::VariableSymbol(s.second, this, &(_config->ostream(Verb::ERROR)));
								add_sym = true;
							}
							break;

						case Symbol::Type::OBJECT:
							if (!pass2) {
								// first pass: ignore
							} else {
								// second pass, create the symbol
								sym = new symbols::ObjectSymbol(s.second, this, &(_config->ostream(Verb::ERROR)));
								add_sym = true;
							}
							break;

						case Symbol::Type::MACRO:
							if (!pass2) {
							} else {
								// TODO
								_config->ostream(Verb::WARN) << "WARN: Macros are currently not support. Ignoring macro definition." << std::endl;
								add_sym = true;
							}
							break;

						case Symbol::Type::ERR_INVALID_SYMBOL:
							_config->ostream(Verb::ERROR) << "ERROR: Encountered unexpected symbol key \"" << s.first << "\" in symbol table file \"" << path.native() << "\". Expected one of \"sort\", \"constant\", \"variable\", \"object\", or \"macro\"." << std::endl;
							good = false;
						};

						// finish up by adding the symbol if we need to.
						if (add_sym) {
							if (!sym->good()) {
								_config->ostream(Verb::ERROR) << "ERROR: An error occurred while trying to load a symbol of type \"" << s.first << "\" in symbol table file \"" << path.native() << "\"." << std::endl;
								good = false;
							} else {
								// try to create the symbol
								tmp = resolveOrCreate(sym);
								if (!tmp) {
									_config->ostream(Verb::ERROR) << "ERROR: Detected a conflicting definition of symbol \"" << *(sym->name()) << "\" while loading symbol table from file \"" << path.native() << "\"." << std::endl;
									good = false;
								}
							}
						}
					}
				}
			}

		} else {
			_config->ostream(Verb::ERROR) << "ERROR: An error occurred while opening file \"" << path.native() << "\"." << std::endl;
			good = false;
		}


	} catch (fs::filesystem_error& e) {
		_config->ostream(Verb::ERROR) << "ERROR: An error occurred while reading from \"" << path.native() << "\"." << std::endl;
		good = false;
	} catch (pt::xml_parser::xml_parser_error& e) {
		_config->ostream(Verb::ERROR) << "ERROR: An error occurred parsing the symbol table input file \"" << path.native() << "\"." << std::endl;
		good = false;
	}

	if (in.is_open()) in.close();	
	if (!good) {
		_config->ostream(Verb::ERROR) << "ERROR: An error occurred while loading the symbol table from file \"" << path.native() << "\"." << std::endl;
	}

	return good;

}

bool SymbolTable::save(boost::filesystem::path const& path) const {

	// create the XML property tree
	pt::ptree xml;
	pt::ptree& symbols = xml.put("symbols", "");

	size_t type = 0x0001;
	while (type <= Symbol::Type::_LARGEST_) {
		char const* typestr = Symbol::Type::cstr((Symbol::Type::Value)type);
		for (const_iterator it = begin((Symbol::Type::Value)type); it != end((Symbol::Type::Value)type); it++) {
			pt::ptree& node = symbols.add(typestr, "");
			(*it)->save(node);
		}
		type = type << 1;
	}

	// open the file
	fs::ofstream out;
	bool good = true;


	try {

		// try to open it
		out.open(path);

		// check if we opened it
		if (!out.good()) {
			_config->ostream(Verb::ERROR) << "ERROR: Unable to open symbol table output file \"" << path.native() << "\"." << std::endl;
			good = false;
		}


		// write the file
		pt::xml_parser::write_xml(out, xml, pt::xml_parser::xml_writer_settings<char>('\t', 1));

	} catch (fs::filesystem_error& e) {
		_config->ostream(Verb::ERROR) << "ERROR: An error occurred while writing the symbol table to \"" << path.native() << "\"." << std::endl;
		good = false;
	} catch (pt::xml_parser::xml_parser_error& e) {
		_config->ostream(Verb::ERROR) << "ERROR: An error occurred while writing the symbol table to \"" << path.native() << "\"." << std::endl;
		good = false;	
	}

	if (out.is_open()) out.close();
	return good;
}






}}
