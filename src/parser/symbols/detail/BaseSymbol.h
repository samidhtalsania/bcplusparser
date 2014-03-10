#pragma once

#include <string>
#include <ostream>
#include <list>

#include <boost/property_tree/ptree.hpp>

#include "Referenced.h"
#include "pointers.h"
#include "referencedwrappers.h"

#include "parser/symbols/Symbol.h"

namespace bcplus {
namespace parser {
namespace symbols {

class SortSymbol;
class Resolver;

namespace detail {


/// Base symbol class for objects and constants
class BaseSymbol : public Symbol {

public:
	/*************************************************************************************/
	/* Public Types */
	/*************************************************************************************/

	typedef ReferencedWrapper<std::list<ref_ptr<const SortSymbol> > > SortList;
	typedef SortList::iterator iterator;
	typedef SortList::const_iterator const_iterator;

private:
	/*************************************************************************************/
	/* Private Members */
	/*************************************************************************************/
	/// A list of argument sorts
	ref_ptr<const SortList> _args;	


public:
	/*************************************************************************************/
	/* Constructors / Destructor */
	/*************************************************************************************/

	/// Basic constructor
	/// @param type The type of symbol this is.
	/// @param name The name of the symbol
	/// @param args A list of argument sorts or NULL to indicate there are no sorts.
	BaseSymbol(Symbol::Type::Value type, ReferencedString const* name, SortList const* args = NULL);

	/// Loads the sort from the property tree node
	/// @param type The type of symbol this is.
	/// @param node The node to load the symbol from
	/// @param err An error stream to write to (or NULL)
	/// Sets the symbol's good flag if it was successful
	BaseSymbol(Symbol::Type::Value type, boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err = NULL);

	/// Destructor stub
	virtual ~BaseSymbol();
	
	/*************************************************************************************/
	/* Public Functions */
	/*************************************************************************************/

	/// Gets an iterator for the beginning of the parameters
	inline const_iterator begin() const							{ return _args->begin(); }

	/// Gets an iterator for the end of the parameters
	inline const_iterator end() const							{ return _args->end(); }

	virtual bool operator==(Symbol const& other) const;
	virtual void save(boost::property_tree::ptree& node) const;
	virtual bool integral() const = 0;

};

}}}}
