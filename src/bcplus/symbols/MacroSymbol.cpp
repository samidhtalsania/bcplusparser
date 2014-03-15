
#include <string>
#include <sstream>

#include <boost/foreach.hpp>
#include <boost/algorithm/string.hpp>

#include "babb/utils/memory.h"
#include "memwrappers.h"

#include "bcplus/elements/Element.h"
#include "bcplus/symbols/MacroSymbol.h"


namespace bcplus {
namespace symbols{


MacroSymbol::MacroSymbol(ReferencedString const* base, ReferencedString const* text, ArgList const* args)
	: Symbol(Symbol::Type::MACRO, base, (args ? args->size() : 0)), _text(text) {
	if (args) _args = args;
	else _args = new ArgList();
}

MacroSymbol::~MacroSymbol() {
	/* Intentionally left blank */
}

std::string MacroSymbol::expand(ArgList const* args) const {
	if (!args && !arity()) return *_text;
	else if (!args) return "<MACROERROR>";	
	if (args->size() != arity()) return "<MACROERROR>";

	// start with macro definition
	std::string result = *_text;


	// expand each formal argument
	const_iterator it = args->begin();
	BOOST_FOREACH(ReferencedString const* formalArg, *this) {
		boost::replace_all(result, *formalArg, **it);
		it++;
	}
	return result;
}

std::string MacroSymbol::expand(ElementList const* args) const {

	if (!args && !arity()) return *_text;
	else if (!args) return "<MACROERROR>";
	if (args->size() != arity()) return "<MACROERROR>";
	
	std::stringstream tmp;
	std::string result = *_text;

	ElementList::const_iterator it = args->begin();
	BOOST_FOREACH(ReferencedString const* formalArg, *this) {
		tmp.str("");
		(*it)->output(tmp);
		boost::replace_all(result, *formalArg, tmp.str());
		it++;
	}
	return result;
}

bool MacroSymbol::integral() const {
	return false;
}


}}


