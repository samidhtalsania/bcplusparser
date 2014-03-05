
#include "utils.h"

#include "parser/symbols/Resolver.h"
#include "parser/symbols/Symbol.h"
#include "parser/symbols/detail/BaseSymbol.h"
#include "parser/symbols/ConstantSymbol.h"



namespace bcplus {
namespace parser {
namespace symbols {

ConstantSymbol::ConstantSymbol(ReferencedString const* base, SortSymbol sort, SortList const* args) 
	: BaseSymbol(Symbol::Type::CONSTANT, base, args) , _sort(sort){
	// intentionally left blank
}

ConstantSymbol::ConstantSymbol(boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err)
	: BaseSymbol(Symbol::Type::OBJECT, node, resolver, err) {

	try {
		std::string sort_name = node.get<std::string>("<xmlattr>.sort");
		SortSymbol const* sort = resolver->resolve(Symbol::Type::SORT, sort_name);
		if (!sort) {
			good(false);
			if (err) *err << "ERROR: An error occurred while scanning the definition of \"" << *name() << "\". \"" << sort_name << "\" is not a valid sort." << std::endl;
		} else {
			_sort = sort;
		}
	} catch (boost::property_tree::ptree_error& err) {
		good(false);
		if (err) *err << "ERROR: An error occurred while scanning the definition of \"" << *name() << "\". Expected a 'sort' attribute declaring the constant's sort." << std::endl;
	}
}

ConstantSymbol::~ConstantSymbol() {
	// Intentionally left blank
}


bool ConstantSymbol::integral() const {
	return _sort->integral();
}

bool ConstantSymbol::save(boost::property_tree::ptree& node) const {
	BaseSymbol::save(node);
	node.put("<xmlattr>.sort", *_sort->base());
}

}}}

