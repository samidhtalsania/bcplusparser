#pragma once

#include <string>
#include <ostream>

#include <boost/property_tree/ptree.h>

#include "Referenced.h"
#include "pointers.h"
#include "referencedwrappers.h"

#include "parser/symbols/Symbol.h"
#include "parser/symbols/SortSymbol.h"

namespace bcplus {
namespace parser {
namespace symbols {

class Resolver;

class VariableSymbol : public Symbol {

private:
	/*************************************************************************************/
	/* Private Members */
	/*************************************************************************************/
	/// The variable's sort
	ref_ptr<const SortSymbol> _sort;

public:
	/// Basic constructor
	/// @param base The name of the variable
	/// @param sort The sort the variable ranges over
	VariableSymbol(ReferencedString const* base, SortSymbol const* sort);

	/// Loads the variable from the property tree node
	/// @param node The node to load the symbol from
	/// @param err An error strem to write to (or NULL)
	/// Sets the symbol's good flag if it was successful
	VariableSymbol(boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err = NULL);

	/// Destructor stub
	virtual ~VariableSymbol();

	/// Get the variable's sort
	SortSymbol const* sort() const									{ return _sort; }

	/// Outputs the symbol to the provided property_tree node
	/// @param node The node to write to
	virtual void save(boost::property_tree::ptree& node) const;


};

}}}
