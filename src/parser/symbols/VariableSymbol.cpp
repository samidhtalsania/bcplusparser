
#include "parser/symbols/Resolver.h"
#include "parser/symbols/Symbol.h"
#include "parser/symbols/VariableSymbol.h"


namespace bcplus {
namespace parser {
namespace symbols{


VariableSymbol::VariableSymbol(ReferencedString const* base, SortSymbol const* sort)
	: Symbol(Symbol::Type::VARIABLE, base, 0), _sort(sort) {
	// Intentionally left blank
}

VariableSymbol::VariableSymbol(boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err)
	: Symbol(node, err) {

	if (good() && arity()) {
		good(false);
		if (err) *err << "ERROR: Variable \"" << *base() << "\" cannot be declared with non-zero arity." << std::endl;
	}
	
	std::string sortname = node.get("<xmlattr>.sort", "");
	if (sortname  == "") {
		good(false);
		if (err) *err << "ERROR: Expected a 'sort' attribute in the declaration of variable \"" << *base() << "\"." << std::endl;
	} else {

		VariableSymbol const* s = (VariableSymbol const*)resolver->resolve(Symbol::SORT, sortname);
		if (!s) {
			good(false);

			if (err) {
				// see if the symbol is declared elsewhere
				if (resolver->resolve(~Symbol::SORT, sortname)) {
					*err << "ERROR: Variable \"" << *base() << "\" has invalid 'sort' attribute. \"" << sortname << "\" is not a sort." << std::endl;
				} else {
					*err << "ERROR: Variable \"" << *base() << "\" has invlaid 'sort' attribute. \"" << sortname << "\" has not been declared." << std::endl;
				}
			}
		}

		_sort = s;
	}
}

VariableSymbol::~VariableSymbol() {
	// Intentionally left blank
}

void VariableSymbol::save(boost::property_tree::ptree& node) const {
	Symbol::save(node);
	node.put("<xmlattr>.sort", *(sort()->base()));
}


}}}

