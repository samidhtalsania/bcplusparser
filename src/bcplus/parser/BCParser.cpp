#include <string>
#include <iostream>

#include <boost/foreach.hpp>
#include <boost/filesystem.hpp>

#include "babb/utils/memory.h"
#include "memwrappers.h"

#include "bcplus/Configuration.h"
#include "bcplus/statements/Statement.h"
#include "bcplus/statements/IncludeStatement.h"
#include "bcplus/parser/detail/lemon_parser.h"
#include "bcplus/parser/BCParser.h"
#include "bcplus/parser/Token.h"
#include "bcplus/parser/detail/Scanner.h"
#include "bcplus/symbols/SymbolTable.h"

namespace u = babb::utils;
namespace fs = boost::filesystem;

// parser functions
void*       lemon_parserAlloc(void* (*mallocProc)(size_t));
void*       lemon_parserFree(void* yyp, void (*freeProc)(void*));
void        lemon_parser(void* yyp, int tokentype, bcplus::parser::Token const* token, bcplus::parser::BCParser* parser);
int         lemon_parserPreInject(void* yyp, int pop, bcplus::parser::Token const** token);
void		lemon_parserAttemptReduce(void* yyp);
namespace bcplus {
namespace parser {



BCParser::BCParser(Configuration* config, languages::Language const* lang, symbols::SymbolTable* symtab)
	: _config(config), _lang(lang) {
	if (symtab) _symtab = symtab;
	else _symtab = new symbols::SymbolTable(config);
	_scanner = new detail::Scanner(config);
	_parser = lemon_parserAlloc(malloc);
	_stat = Status::OK;
	_soft_eof = false;
}

BCParser::~BCParser() {
	lemon_parserFree(_parser, free);
}


BCParser::ParseType BCParser::parse() {

	u::ref_ptr<const statements::Statement> stmt;

	// pick up any statement that was generated simultaneously with an error (just in case)
	if (_stmt) {
		stmt = _stmt;
		_stmt = NULL;
		return ParseType(Status::OK, stmt);
	}

	_stat = Status::OK;

	u::ref_ptr<const Token> token;
	do {

		token = _scanner->readToken();

		int type = token->type();

		// track the soft end of file flag
		if (type != T_EOF) _soft_eof = false;

		switch (type) {

		case T_ERR_IO:
			// an IO error occurred
			_config->ostream(Verb::ERROR) << "ERROR: An IO error occurred while reading from input \"" << token->begin().filename() << "\"." << std::endl;
			_stat = Status::IO_ERR;
			break;

		case T_ERR_UNTERMINATED_STRING:

			_config->ostream(Verb::ERROR) << "ERROR: ";
			token->begin().output(_config->ostream(Verb::ERROR));
			_config->ostream(Verb::ERROR) << ": Unexpected end of input: unterminated string literal." << std::endl;
			_stat = Status::SYNTAX_ERR;
			break;
		
		case T_ERR_UNTERMINATED_ASP:

			_config->ostream(Verb::ERROR) << "ERROR: ";
			token->begin().output(_config->ostream(Verb::ERROR));
			_config->ostream(Verb::ERROR) << ": Unexpected end of input: unterminated asp code block." << std::endl;
			_stat = Status::SYNTAX_ERR;
			break;
		
		case T_ERR_UNTERMINATED_LUA:

			_config->ostream(Verb::ERROR) << "ERROR: ";
			token->begin().output(_config->ostream(Verb::ERROR));
			_config->ostream(Verb::ERROR) << ": Unexpected end of input: unterminated lua code block." << std::endl;
			_stat = Status::SYNTAX_ERR;
			break;
		
		case T_ERR_UNTERMINATED_F2LP:

			_config->ostream(Verb::ERROR) << "ERROR: ";
			token->begin().output(_config->ostream(Verb::ERROR));
			_config->ostream(Verb::ERROR) << ": Unexpected end of input: unterminated f2lp code block." << std::endl;
			_stat = Status::SYNTAX_ERR;
			break;
		
		case T_ERR_UNTERMINATED_BLK_COMMENT:

			_config->ostream(Verb::ERROR) << "ERROR: ";
			token->begin().output(_config->ostream(Verb::ERROR));
			_config->ostream(Verb::ERROR) << ": Unexpected end of input: unterminated block comment." << std::endl;
			_stat = Status::SYNTAX_ERR;
			break;

		case T_ERR_UNKNOWN_SYMBOL:
			_config->ostream(Verb::ERROR) << "ERROR: ";
			token->begin().output(_config->ostream(Verb::ERROR));
			_config->ostream(Verb::ERROR) << ": Encountered an unknown symbol \"" << *token->str() << "\"." << std::endl;
			_stat = Status::SYNTAX_ERR;
			break;

		case T_EOF:
			// only fed it to the parser if it's a hard EOF (i.e. we already encoutnered a soft EOF)
			if (_soft_eof) {
				_config->ostream(Verb::TRACE) << "TRACE: Hard EOF." << std::endl;
				_last_token = token;
				lemon_parser(_parser, type, token.release(), this);
				_stat = Status::HARD_EOF;
			} else {
				_config->ostream(Verb::TRACE) << "TRACE: Soft EOF." << std::endl;
				_soft_eof = true;
				_stat = Status::SOFT_EOF;
				lemon_parserAttemptReduce(_parser);
			}
			break;

		default:
			_last_token = token;
			lemon_parser(_parser, type, token.release(), this);
			break;
		} 
	} while (!_stmt && _stat == Status::OK);
	


	// Figure out exactly why we stopped
	if (_stat != Status::OK && _stat != Status::SOFT_EOF) {
		return ParseType(_stat, NULL);
	} else {
		stmt = _stmt;
		_stmt = NULL;
		return ParseType(_stat, stmt);
	}
}


void BCParser::reset() {
	_scanner = new detail::Scanner(_config);
	lemon_parserFree(_parser, free);
	_parser = lemon_parserAlloc(malloc);
	_stat = Status::OK;
	_stmt = NULL;	
	_soft_eof = false;
}


bool BCParser::push_front(fs::path const& file, bool squelch) {
	preInjectPrep();
	return _scanner->push_front(file, squelch);
} 

bool BCParser::push_back(fs::path const& file, bool squelch) {
	if (!_scanner->size()) preInjectPrep();
	return _scanner->push_back(file, squelch);
}

void BCParser::push_front(char const* buffer, Location const& loc) {
	preInjectPrep();
	_scanner->push_front(buffer, loc);
}

void BCParser::push_back(char const* buffer, Location const& loc) {
	if (!_scanner->size()) preInjectPrep();
	_scanner->push_back(buffer, loc);
}

void BCParser::_feature_error(languages::Language::Feature::Value feature) {
	Token const* t = _last_token;
	std::ostream& out = _config->ostream(Verb::ERROR);
	out << "ERROR: ";
	if (t) {
		out << t->begin() << ": \"" << *t << "\": ";
	}
	out << lang()->featureDescription(feature) << " are not supported by language " << lang()->name() << "." << std::endl;
	_stat = Status::SYNTAX_ERR;
}

void BCParser::_parse_error(std::string const& error) {
	Token const* t = _last_token;
	std::ostream& out = _config->ostream(Verb::ERROR);
	out << "ERROR: ";
	if (t) {
		out << t->begin() << ": \"" << *t << "\": ";
	}
	out << error << std::endl;
	_stat = Status::SYNTAX_ERR;
}

elements::Element const* BCParser::_resolve(ReferencedString const* name, size_t arity) {
	// TODO
	return NULL;
}

void BCParser::_handle_stmt(statements::Statement const* stmt) {
	_config->ostream(Verb::DETAIL) << "TRACE: Got statement \"";
	stmt->output(_config->ostream(Verb::DETAIL));
	_config->ostream(Verb::DETAIL) << "\"." << std::endl;
	

	// Handle include statements internally
	if ( stmt->type() == statements::Statement::Type::INCLUDE ) {
		Location l = stmt->beginLoc();
	
		BOOST_FOREACH(ReferencedString const* name, *((statements::IncludeStatement*)stmt)) {
			fs::path p = fs::path(*name);
			try {
				// Try to resolve the name
				fs::path fullpath;

				// try to resolve the path of the file

				if (p.is_absolute() || !l.file()) {
					fullpath = p;
				} else if (!l.file()) {
					fullpath = fs::current_path() / p;
				} else {
					// Try a path relative to the current file we're in first
					// Then try resolving with our default path.
					fullpath = l.file()->parent_path() / p;
					if (!fs::exists(fullpath) || fs::is_directory(fullpath)) {
						fullpath = fs::current_path() / p;
					}
				}
	
				if (!fs::exists(fullpath)) {
					// We were unable to resolve the file..
					std::ostream& out = _config->ostream(Verb::ERROR);
					out << "ERROR: ";
					l.output(out);
					out << ": Could not open file \"" << p.native() << "\". File does not exist." << std::endl;

					_stat = Status::SYNTAX_ERR;
					break;
				}

				if (fs::is_directory(fullpath)) {
					// We were unable to resolve the file..
					std::ostream& out = _config->ostream(Verb::ERROR);
					out << "ERROR: ";
					l.output(out);
					out << ": Could not open file \"" << p.native() << "\". The file is a directory." << std::endl;

					_stat = Status::SYNTAX_ERR;
					break;
					// We can't open a directory.
				}

				// The file appears to be good.
				if (!push_front(fullpath, false)) {
					std::ostream& out = _config->ostream(Verb::ERROR);
					out << "ERROR: ";
					l.output(out);
					out << ": An error occurred openning file \"" << p.native() << "\"." << std::endl;

					_stat = Status::IO_ERR;
					break;
				}

			} catch (fs::filesystem_error& err) {
					std::ostream& out = _config->ostream(Verb::ERROR);
					out << "ERROR: ";
					l.output(out);
					out << ": An error occurred openning file \"" << p.native() << "\"." << std::endl;

					_stat = Status::IO_ERR;
					break;
			}
		}
	} else {
		_stmt = stmt;
	}
}

void BCParser::preInjectPrep(bool pop_stack) {
	Token const* tok = NULL;
	int t = lemon_parserPreInject(_parser, pop_stack, &tok);
	if (t) {
		_config->ostream(Verb::TRACE) << "TRACE: " << tok->begin() << ": Retracted (" << tok->typeString() << ") \"" << *tok << "\"." << std::endl;
		_scanner->push_front(tok);
	} else {
		_config->ostream(Verb::TRACE) << "TRACE: No token to retract from the parser." << std::endl;
	}
}

}}
