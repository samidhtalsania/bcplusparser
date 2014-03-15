
#include "bcplus/parser/Token.h"

char const* lemon_parserTokenName(int tok);

namespace bcplus {
namespace parser {

Token::Token(int type, ReferencedString const* string, Location const& begin, Location const& end)
	: _type(type), _string(string), _begin(begin), _end(end) {
	// Inentionally left blank
}

Token::~Token() {
	// Intentionally left blank
}

char const* Token::typeString(int token) {
	return lemon_parserTokenName(token);
}




}}





