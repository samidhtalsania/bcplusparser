#pragma once

#include <ostream>
#include <string>

#include "babb/utils/memory.h"
#include "memwrappers.h"

#include "bcplus/Location.h"
#include "bcplus/DomainType.h"

#include "bcplus/symbols/SortSymbol.h"
#include "bcplus/elements/formulas.h"
#include "bcplus/elements/terms.h"

namespace bcplus {
namespace elements {

/**
 * @brief A nested quantifier formula of the form '[@1 V1 @2 V2 @3 V3 ...| F]' Where @i are big operators (/\ or \/), each Vi is a variable, and F is a formula.
 */
class CardinalityFormula : public Formula
{

public:
    /****************************************************************************/
    /* Public Types */
    /****************************************************************************/

	typedef std::pair<babb::utils::ref_ptr<const symbols::SortSymbol>, babb::utils::ref_ptr<LocalVariable> > Binding;
	typedef ReferencedList<Binding>::type BindingList;

	typedef BindingList::iterator iterator;
	typedef BindingList::const_iterator const_iterator;


private:
    /****************************************************************************/
    /* Private Members */
    /****************************************************************************/

	/// min/max limits. (if they exist)
	babb::utils::ref_ptr<Term> _min;
	babb::utils::ref_ptr<Term> _max;

    /// The sort bindings for local variables
	babb::utils::ref_ptr<BindingList> _binds;
	

	/// The atomic formula we're testing
	babb::utils::ref_ptr<AtomicFormula> _af;


public:
    /****************************************************************************/
    /* Constructor / Destructor */
    /****************************************************************************/
    /// Full constructor
	/// @param af The atomic formula being operated on.
	/// @param binds The sort-local variable bindings (or NULL to create a new list)
	/// @param min The minimum limit (or NULL).
	/// @param max The maximum limit (or NULL).
    /// @param begin The beginning location of this element
    /// @param end The ending location of this element
    /// @param parens Whether the element is surrounded by parentheses
    CardinalityFormula(AtomicFormula* af, BindingList* binds = NULL, Term* min = NULL, Term* max = NULL, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0), bool parens = false);

    /// Destructor stub
    virtual ~CardinalityFormula();

    /****************************************************************************/
    /* Public Methods */
    /****************************************************************************/

	/// Iterate over the local bindings
	inline iterator begin()					{ return _binds->begin(); }
	inline const_iterator begin() const		{ return _binds->begin(); }
	inline iterator end()					{ return _binds->end(); }
	inline const_iterator end() const		{ return _binds->end(); }

	/// Get the atomic formula.
	inline AtomicFormula* af()				{ return _af; }
	inline AtomicFormula const* af() const	{ return _af; }

	// Get the minimum limit (or NULL)
	inline Term* min()						{ return _min; }
	inline Term const* min() const			{ return _min; }

	// Get the maximum limit (or NULL)
	inline Term* max() 						{ return _max; }
	inline Term const* max() const			{ return _max; }


	// inherited
    virtual Element* copy() const;
    virtual void output(std::ostream& out) const;
	virtual DomainType::type domainType() const;

};

}}

