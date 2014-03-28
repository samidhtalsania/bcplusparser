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
 * @brief A temporal binding for a formula of the form i:F
 */
class BindingFormula : public Formula
{
private:
    /****************************************************************************/
    /* Private Members */
    /****************************************************************************/

    /// The constant
    babb::utils::ref_ptr<Term> _step;

	/// The value
	babb::utils::ref_ptr<Formula> _formula;

public:
    /****************************************************************************/
    /* Constructor / Destructor */
    /****************************************************************************/
    /// Full constructor
    /// @param step The step to bind the formula to
	/// @param formula The formula to bind
	/// @param begin The beginning location of this element
    /// @param end The ending location of this element
    /// @param parens Whether the element is surrounded by parentheses
    BindingFormula(Term* step, Formula* formula, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0), bool parens = false);

    /// Destructor stub
    virtual ~BindingFormula();

    /****************************************************************************/
    /* Public Methods */
    /****************************************************************************/

	/// The step
	inline Term* step()							{ return _step; }
	inline Term const* step() const				{ return _step; }

	/// The value
	inline Formula* formula()					{ return _formula; }
	inline Formula const* formula() const		{ return _formula; }

	// inherited
    virtual Element* copy() const;
    virtual void output(std::ostream& out) const;
	virtual DomainType::type domainType() const;
private:

};

}}

