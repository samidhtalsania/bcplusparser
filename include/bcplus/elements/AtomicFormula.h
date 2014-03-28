#pragma once

#include <ostream>
#include <string>

#include "babb/utils/memory.h"

#include "bcplus/DomainType.h"
#include "bcplus/elements/formulas.h"
#include "bcplus/Location.h"
#include "bcplus/elements/terms.h"

namespace bcplus {
namespace elements {

/**
 * @brief A simple formula of the form c=v where c is a constant and v is a term
 */
class AtomicFormula : public Formula
{
private:
    /****************************************************************************/
    /* Private Members */
    /****************************************************************************/

    /// The constant
    babb::utils::ref_ptr<Constant> _c;

	/// The value
	babb::utils::ref_ptr<Term> _v;

public:
    /****************************************************************************/
    /* Constructor / Destructor */
    /****************************************************************************/
    /// Full constructor
    /// @param c The constant on the lhs of the atomic formula.
	/// @param v The value term on the rhs of the atomic formula.
    /// @param begin The beginning location of this element
    /// @param end The ending location of this element
    /// @param parens Whether the element is surrounded by parentheses
    AtomicFormula(Constant* c, Term* v, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0), bool parens = false);

    /// Destructor stub
    virtual ~AtomicFormula();

    /****************************************************************************/
    /* Public Methods */
    /****************************************************************************/

	/// The constant
	inline Constant* c()							{ return _c; }
	inline Constant const* c() const				{ return _c; }

	/// The value
	inline Term* v()								{ return _v; }
	inline Term const* v() const					{ return _v; }

	// inherited
    virtual Element* copy() const;
    virtual void output(std::ostream& out) const;
	virtual DomainType::type domainType() const;
private:

};

}}

