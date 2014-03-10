#pragma once

#include <ostream>
#include <string>
#include <list>

#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_same.hpp>

#include "Referenced.h"
#include "pointers.h"
#include "referencedwrappers.h"

#include "parser/Location.h"
#include "parser/elements/Element.h"


namespace bcplus {
namespace parser {
namespace elements {
namespace detail {

/// Version of IdentifierElement with no arguments
/// @param BaseType The base class for this template
/// @param type The type flag to indicate the specialization of the type
/// @param SymbolType The type that the SymbolTable is of.
template <typename BaseType, int type, typename SymbolType>
class IdentifierElement_bare : public BaseType
{


private:

	ref_ptr<const SymbolType> _sym;

public:
    /****************************************************************************/
    /* Constructor / Destructor */
    /****************************************************************************/

    /// Full constructor
	/// @param symbol The symbol entry in the symbol table.
    /// @param begin The beginning location of this element
    /// @param end The ending location of this element
    /// @param parens Whether the element is surrounded by parentheses
    IdentifierElement_bare(SymbolType const* symbol, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0), bool parens = false);

    /// Destructor stub
    virtual ~IdentifierElement_bare();


    /****************************************************************************/
    /* Public Members */
    /****************************************************************************/

	/// The symbol entry for this identifier
	inline SymbolType const* symbol() const					{ return _sym; }

	// Inherited from element
	virtual Element* copy() const;
	virtual void output(std::ostream& out) const;

};

/// Version of IdentifierElement with arguments
/// @param BaseType The base class for this template
/// @param type The type flag to indicate the specialization of the type
/// @param SymbolType The type that the SymbolTable is of.
/// @param ArgType The type that the arguments are allowed to take.
template <typename BaseType, int type, typename SymbolType, typename ArgType>
class IdentifierElement : public IdentifierElement_bare<BaseType, type, SymbolType>
{
public:
    /****************************************************************************/
    /* Public Types */
    /****************************************************************************/

	/// Argument type aliases
	typedef ArgType Argument;
	typedef ReferencedWrapper<std::list<ref_ptr<Argument > > > ArgumentList;
	    

	/// Argument iterators
	typedef typename ArgumentList::iterator iterator;
	typedef typename ArgumentList::const_iterator const_iterator;

private:
	/****************************************************************************/
    /* Private Members */
    /****************************************************************************/

	ref_ptr<ArgumentList> _args;

public:

	/****************************************************************************/
    /* Constructor / Destructor */
    /****************************************************************************/

    /// Full constructor
	/// @param symbol The symbol entry in the symbol table.
	/// @param args The argument list for this identifier.
    /// @param begin The beginning location of this element
    /// @param end The ending location of this element
    /// @param parens Whether the element is surrounded by parentheses
    IdentifierElement(SymbolType const* symbol, ArgumentList* args, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0), bool parens = false);

    /// Destructor stub
    virtual ~IdentifierElement();

    /****************************************************************************/
    /* Public Members */
    /****************************************************************************/

	/// Get the number of arguments
	inline size_t arity() const							{ return _args->size(); }

	/// Argument list iterators
	inline iterator begin() 							{ return _args->begin(); }
	inline const_iterator begin() const					{ return _args->begin(); }
	inline iterator end()								{ return _args->end(); }
	inline const_iterator end() const					{ return _args->end(); }


	// Inherited from element
	virtual Element* copy() const;
	virtual void output(std::ostream& out) const;

};

}}}}


#include "parser/elements/detail/IdentifierElement.impl.h"

