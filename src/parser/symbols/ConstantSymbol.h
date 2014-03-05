#pragma once

#include <string>
#include <ostream>

#include <boost/property_tree/ptree.h>

#include "Referenced.h"
#include "pointers.h"
#include "referencedwrappers.h"

#include "parser/symbols/Symbol.h"
#include "parser/symbols/SortSymbol.h"
#include "parser/symbols/detail/BaseSymbol.h"

namespace bcplus {
namespace parser {
namespace symbols {

class Resolver;

class ConstantSymbol : public detail::BaseSymbol {

private:
	/*************************************************************************************/
	/* Private Members */
	/*************************************************************************************/
	/// The sort this constant ranges over
	ref_ptr<const SortSymbol> _sort;

public:
	/// Basic constructor
	/// @param base The name of this object
	/// @param sort The sort that this constant ranges over
	/// @param args The sorts for each of the arguments for this object.
	ConstantSymbol(ReferencedString const* base, SortSymbol const* sort, SortList const* args = NULL);

	/// Loads the object from the property tree node
	/// @param node The node to load the symbol from
	/// @param err An error strem to write to (or NULL)
	/// Sets the symbol's good flag if it was successful
	ConstantSymbol(boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err = NULL);

	/// Destructor stub
	virtual ~ConstantSymbol();

	/// Get the sort this symbol ranges over
	SortSymbol const* sort() const						{ return _sort; }


	virtual bool integral() const;
	virtual bool save(boost::property_tree::ptree& node) const;

};

}}}
