
#include "boost/lexical_cast.hpp"
#include "boost/property_tree/ptree.hpp"
#include "boost/property_tree/exceptions.hpp"

#include "parser/symbols/Resolver.h"
#include "parser/symbols/Symbol.h"
#include "parser/symbols/SortSymbol.h"
#include "parser/symbols/detail/BaseSymbol.h"


namespace bcplus {
namespace parser {
namespace symbols{
namespace detail {

BaseSymbol::BaseSymbol(Symbol::Type::Value type, ReferencedString const* base, SortList const* args)
	: Symbol(type, base, (args ? args->size() : 0)) {
	if (args) _args = args;
	else _args = new SortList();

}

BaseSymbol::BaseSymbol(Symbol::Type::Value type, boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err)
	: Symbol(type, node, err) {

	ref_ptr<SortList> l = new SortList();

	// Get all of the sorts for each of the arguments
	for (size_t i = 0; i < arity(); i++) {
		try {
			std::string sort_name = node.get("<xmlattr>.arg" + boost::lexical_cast(i));
			SortSymbol const* sort = resolver->resolve(Symbol::Type::SORT, sort_name);
			if (!sort) {
				good(false);
				if (err) *err << "ERROR: An error occurred scanning the definition of \"" << *name() << "\". \"" << sort_name << "\" is not a declared sort." << std::endl;
			} else {
				l->push_back(sort);
			}
			
		} catch (boost::property_tree::ptree_error& err) {
			good(false);
			if (err) *err << "ERROR: An error occurred scanning the definition of \"" << *name() << "\". The sort for argument " << i << " is not properly declared. Please ensure there is an 'arg" << i << "' attribute attached to the declaration." << std::endl;
		}
	}

	_args = l;
}

BaseSymbol::~BaseSymbol() {
	// Intentionally left blank
}

void BaseSymbol::save(boost::property_tree::ptree& node) const {
	Symbol::save(node);
	node.put("<xmlattr>.sort", *(sort()->base()));
	size_t arg = 0;
	for(SortSymbol const* sort : _args) {
		node.put("<xmlattr>.arg" + boost::lexical_cast(arg++), *(sort->base()));
	}
}


}}}}
