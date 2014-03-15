#pragma once

#include <ostream>
#include <string>

#include "babb/utils/memory.h"


#include "bcplus/Location.h"

namespace bcplus {
namespace elements {
namespace detail {

/// template class for binary elements such as binary formulas and terms
/// @param BaseType The base class for this template
/// @param type The type flag to indicate the specialization of the type
/// @param Op The type for the operators that are allowed
/// @param preOpString A functor to call to get the pre-subelement string corresponding to the operation.
/// @param postOpString A functor to call to get the post-subelement string corresponding to the operation
template <typename BaseType, int type, typename Op, typename preOpString, typename postOpString>
class UnaryElement : public BaseType
{

public:
public:
    /****************************************************************************/
    /* Public Types */
    /****************************************************************************/
	/// Operator type alias
	typedef Op Operator;


private:
    /****************************************************************************/
    /* Private Members */
    /****************************************************************************/

    /// The operator
    Op _op;

	/// The subformula
	babb::utils::ref_ptr<BaseType> _sub;


public:
    /****************************************************************************/
    /* Constructor / Destructor */
    /****************************************************************************/
    /// Full constructor
    /// @param op The operator being applied to the subformulas
	/// @param sub The subformula
    /// @param begin The beginning location of this element
    /// @param end The ending location of this element
    /// @param parens Whether the element is surrounded by parentheses
    UnaryElement(Op const& op, BaseType* sub, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0), bool parens = false);

    /// Destructor stub
    virtual ~UnaryElement();

    /****************************************************************************/
    /* Public Methods */
    /****************************************************************************/

	/// The operator for the formula
	inline Op const& op() const				{ return _op; }

	/// The subformula
	inline BaseType* sub()						{ return _sub; }
	inline BaseType const* sub() const			{ return _sub; }

	// inherited from Element
    virtual Element* copy() const;
    virtual void output(std::ostream& out) const;

};

}}}


#include "bcplus/elements/detail/UnaryElement.impl.h"

