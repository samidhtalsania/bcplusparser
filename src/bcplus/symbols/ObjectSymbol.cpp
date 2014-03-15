
#include "babb/utils/utils.h"

#include "bcplus/symbols/Symbol.h"
#include "bcplus/symbols/detail/BaseSymbol.h"
#include "bcplus/symbols/ObjectSymbol.h"


namespace u = babb::utils;


namespace bcplus {
namespace symbols {

ObjectSymbol::ObjectSymbol(ReferencedString const* b, SortList const* args) 
	: BaseSymbol(Symbol::Type::OBJECT, b, args) {
	// figure out if we're integral
	if (arity()) _integral = false;
	else _integral = u::fromString(base()->c_str(), _int);
}

ObjectSymbol::ObjectSymbol(boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err)
	: BaseSymbol(Symbol::Type::OBJECT, node, resolver, err) {
	// figure out if we're integral
	if (arity()) _integral = false;
	else _integral = u::fromString(base()->c_str(), _int);
}

ObjectSymbol::~ObjectSymbol() {
	// Intentionally left blank
}

bool ObjectSymbol::integral() const {
	return _integral;
}

int const* ObjectSymbol::integer() const {
	return (integral() ? &_int : NULL);
}

}}

