
#include "utils.h"

#include "parser/symbols/Symbol.h"
#include "parser/symbols/detail/BaseSymbol.h"
#include "parser/symbols/ObjectSymbol.h"



namespace bcplus {
namespace parser {
namespace symbols {

ObjectSymbol::ObjectSymbol(ReferencedString const* base, SortList const* args) 
	: BaseSymbol(Symbol::Type::OBJECT, base, args) {
	// figure out if we're integral
	if (arity()) _integral = false;
	else _integral = utils::fromString(base()->cstr(), _int);
}

ObjectSymbol::ObjectSymbol(boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err)
	: BaseSymbol(Symbol::Type::OBJECT, node, resolver, err) {
	// figure out if we're integral
	if (arity()) _integral = false;
	else _integral = utils::fromString(base()->cstr(), _int);
}

ObjectSymbol::~ObjectSymbol() {
	// Intentionally left blank
}

int const* ObjectSymbol::integer() const {
	return (integral() ? &_int : NULL);
}

}}}

