#include <string>
#include <iostream>

#include <boost/filesystem.hpp>

#include "AtomParser.h"
#include "parser/atom_parser.h"

namespace fs = boost::filesystem;

// parser functions
void* AtomParser_Alloc(void* (*mallocProc)(size_t));
void* AtomParser_Free(void* yyp, void (*freeProc)(void*));
void  AtomParser_(void* yyp, int tokentype, ref_ptr<Token>* token, AtomParser* parser);


AtomParser::AtomParser(Configuration* config)
	: _config(config) {
	_scanner = new Scanner(config);
	_parser = AtomParser_Alloc(malloc);
}

AtomParser::~AtomParser() {
	AtomParser_Free(_parser, free);
	// intentionally left blank
}

AtomParser::SolutionList* AtomParser::parse(bool reset, fs::path const& input) {
	if (reset) this->reset();

	if (!_scanner->init(input)) return NULL;
	_file = input;

	_list = new SolutionList();

	bool good = true;

	Token* token;
	do {

		token = _scanner->readToken();

		switch (token->type()) {

		case T_ERR_UNTERMINATED_STRING:
			_config->ostream(Verb::ERROR) << "ERROR: Unexpected end of input file \"" << input.native() << "\". Unterminated string literal." << std::endl;
			good = false;
			break;

		case T_ERR_UNKNOWN_SYMBOL:
			_config->ostream(Verb::ERROR) << "ERROR: [" << token->loc().file()->native() << ":" << token->loc().line() << ":" << token->loc().col() << "]: \"" << "Encountered an unknown symbol \"" << *token->str() << "\"." << std::endl;
			good = false;
			break;
		
		default:

			AtomParser_(_parser, token->type(), new ref_ptr<Token>(token), this);

			break;
		}

	} while (token && token->type() != T_EOF);

	return _list;
}


void AtomParser::reset() {
	_preds.clear();
	_funcs.clear();
}


BaseType const* AtomParser::_resolveType(BaseType::Type type, ReferencedString const* name, size_t arity) {

	ref_ptr<const BaseType> t = new BaseType(type, name, arity);

	// check if it already exists and use the existing copy if it does.
	// Otherwise add it.
	predIterator it;
	switch (type) {
	case BaseType::FUNCTION:
		it = _funcs.find(t);
		if (it != _funcs.end()) t = *it;
		else {
			_funcs.insert(t);
		}
		break;
	case BaseType::PREDICATE:
		it = _preds.find(t);
		if (it != _preds.end()) t = *it;
		else {
			_preds.insert(t);
		}
		break;
	} 

	return t;
}

void AtomParser::_parse_error(std::string const& error) {
	Token const * t = _scanner->lastToken();
	_config->ostream(Verb::ERROR) << "ERROR: [" << t->loc().file()->native() << ":" << t->loc().line() << ":" << t->loc().col() << "]: \"" << error << "\" occurred parsing token \"" << *(t->str()) << "\"." << std::endl;
}
