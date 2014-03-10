#pragma once

#include <ostream>
#include <string>

#include "Referenced.h"
#include "pointers.h"
#include "referencedwrappers.h"

#include "parser/Location.h"

#include "parser/elements/formulas.h"
#include "parser/elements/terms.h"

namespace bcplus {
namespace parser {
namespace elements {

/**
 * @brief A nested quantifier formula of the form '[@1 V1 @2 V2 @3 V3 ...| F]' Where @i are big operators (/\ or \/), each Vi is a variable, and F is a formula.
 */
class QuantifierFormula : public Formula
{

public:
    /****************************************************************************/
    /* Public Types */
    /****************************************************************************/

    /// Container of enumeration of possible operators
    struct Operator {
        enum Value {
			CONJ,
			DISJ
		};
    };


	typedef std::pair<Operator::Value, ref_ptr<Variable> > Quantifier;
	typedef ReferencedWrapper<std::list<Quantifier> > QuantifierList;

	typedef QuantifierList::iterator iterator;
	typedef QuantifierList::const_iterator const_iterator;


private:
    /****************************************************************************/
    /* Private Members */
    /****************************************************************************/

    /// The quantifiers
	ref_ptr<QuantifierList> _quants;
	

	/// The subformula
	ref_ptr<Formula> _sub;


public:
    /****************************************************************************/
    /* Constructor / Destructor */
    /****************************************************************************/
    /// Full constructor
    /// @param quants The list of quantifiers being applied to the subformula
	/// @param subformula The subformula being operated on.
    /// @param begin The beginning location of this element
    /// @param end The ending location of this element
    /// @param parens Whether the element is surrounded by parentheses
    QuantifierFormula(QuantifierList* quants, Formula* subformula, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0), bool parens = false);

    /// Destructor stub
    virtual ~QuantifierFormula();

    /****************************************************************************/
    /* Public Methods */
    /****************************************************************************/

	/// The operators for the formula
	inline iterator begin()					{ return _quants->begin(); }
	inline const_iterator begin() const		{ return _quants->begin(); }
	inline iterator end()					{ return _quants->end(); }
	inline const_iterator end() const		{ return _quants->end(); }

	/// The subformula
	inline Formula* subformula()					{ return _sub; }
	inline Formula const* subformula() const		{ return _sub; }

    virtual Element* copy() const;
    virtual void output(std::ostream& out) const;

private:

    /****************************************************************************/
    /* Private Methods */
    /****************************************************************************/
	/// Write the provided quantifier to the output stream
	/// @param q The quantifier to write out
	/// @param out The output stream to write to.
	void outputQuantifier(Quantifier const& q, std::ostream& out) const;



};

}}}

