
#include <ostream>

#include "bcplus/symbols/Resolver.h"
#include "bcplus/symbols/Symbol.h"
#include "bcplus/symbols/detail/BaseSymbol.h"
#include "bcplus/symbols/ConstantSymbol.h"



namespace bcplus {
namespace symbols {

ConstantSymbol::ConstantSymbol(ReferencedString const* base, SortSymbol const* sort, SortList const* args) 
	: BaseSymbol(Symbol::Type::CONSTANT, base, args) , _sort(sort){
	// intentionally left blank
}

ConstantSymbol::ConstantSymbol(boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err)
	: BaseSymbol(Symbol::Type::OBJECT, node, resolver, err) {

	try {
		std::string sort_name = node.get<std::string>("<xmlattr>.sort");
		SortSymbol const* sort = (SortSymbol const*)resolver->resolve(Symbol::Type::SORT, sort_name);
		if (!sort) {
			good(false);
			if (err) *err << "ERROR: An error occurred while scanning the definition of \"" << *name() << "\". \"" << sort_name << "\" is not a valid sort." << std::endl;
		} else {
			_sort = sort;
		}
	} catch (boost::property_tree::ptree_error& e) {
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

bool ConstantSymbol::operator==(Symbol const& other) const {
	if (!BaseSymbol::operator==(other)) return false;
	ConstantSymbol const& o = (ConstantSymbol const&)other;

	// ensure the sort is the same
	if (sort() != o.sort()) return false;
	return true;
}

void ConstantSymbol::save(boost::property_tree::ptree& node) const {
	BaseSymbol::save(node);
	node.put("<xmlattr>.sort", *_sort->base());
}

}}

