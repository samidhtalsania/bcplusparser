#pragma once

#include <string>
#include <ostream>

#include <boost/property_tree/ptree.hpp>

#include "Referenced.h"
#include "pointers.h"
#include "referencedwrappers.h"

#include "parser/symbols/Symbol.h"
#include "parser/symbols/detail/BaseSymbol.h"

namespace bcplus {
namespace parser {
namespace symbols {

class Resolver;

class ObjectSymbol : public detail::BaseSymbol {

private:
	/*************************************************************************************/
	/* Private Members */
	/*************************************************************************************/

	bool _integral;

	/// The integer represented by this symbol, if any.
	int _int;

public:
	/// Basic constructor
	/// @param base The name of this object
	/// @param args The sorts for each of the arguments for this object.
	ObjectSymbol(ReferencedString const* base, SortList const* args = NULL);

	/// Loads the object from the property tree node
	/// @param node The node to load the symbol from
	/// @param err An error strem to write to (or NULL)
	/// Sets the symbol's good flag if it was successful
	ObjectSymbol(boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err = NULL);

	/// Destructor stub
	virtual ~ObjectSymbol();

	virtual bool integral() const;

	/// Gets the integer represented by this object, if any.
	int const* integer() const;

};

}}}
