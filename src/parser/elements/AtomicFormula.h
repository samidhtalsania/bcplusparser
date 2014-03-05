#pragma once

#include <ostream>
#include <string>

#include "Referenced.h"
#include "pointers.h"
#include "referencedwrappers.h"

#include "parser/Location.h"
#include "parser/elements/terms.h"

namespace bcplus {
namespace parser {
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
    ref_ptr<Constant> _c;

	/// The value
	ref_ptr<Term> _v;

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
	inline Term* v()					{ return _v; }
	inline Term const* v() const		{ return _v; }

    virtual Element* copy() const;
    virtual void output(std::ostream& out) const;

private:

};

}}}

