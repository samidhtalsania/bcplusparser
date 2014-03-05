#pragma once

#include <ostream>
#include <string>

#include "Referenced.h"
#include "pointers.h"
#include "referencedwrappers.h"

#include "parser/Location.h"
#include "parser/elements/Element.h"

namespace bcplus {
namespace parser {
namespace elements {
namespace detail {

/// Base class for classes of elements
/// @param type The element type flag for this class
/// @param Subtypes A list of possible Subtypes
template<int type, typename Subtypes>
class ElementClass : public Element
{

public:
	/****************************************************************************/
	/* Public Subtypes */
	/****************************************************************************/

	/// Alias for the possible subtype
	typedef Subtypes Subtype;

private:
	/****************************************************************************/
	/* Private Members */
	/****************************************************************************/

	/// The subtype we're looking at
	Subtype _type;

public:
	/****************************************************************************/
	/* Constructor / Destructor */
	/****************************************************************************/
	/// Full constructor
	/// @param type The type of element this is
	/// @param begin The beginning location of this element
	/// @param end The ending location of this element
	/// @param parens Whether the element is surrounded by parentheses
	ElementClass(Subtype type, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0), bool parens = false);

	/// Destructor stub
	virtual ~ElementClass();

	/****************************************************************************/
	/* Public Methods */
	/****************************************************************************/

	/// Get the formula type
	inline Subtype subType() const				{ return _type; }
	
	virtual Element* copy() const = 0;
	virtual void output(std::ostream& out) const = 0;

};

}}}}

#include "parser/elements/detail/ElementClass.impl.h"



