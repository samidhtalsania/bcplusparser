#pragma once

#include <ostream>
#include <string>
#include <sstream>
#include <set>

#include "babb/utils/memory.h"

#include "bcplus/DomainType.h"
#include "bcplus/Location.h"

#include "bcplus/symbols/Symbol.h"
#include "bcplus/symbols/ConstantSymbol.h"

namespace bcplus {
namespace elements {

/// Base class for a parser element
class Element : public babb::utils::Referenced
{

public:
	/****************************************************************************/
	/* Public Types */
	/****************************************************************************/

	/// Container of enumeration of possible types
	struct Type {
		enum type {
			FORMULA,
			TERM,
			MACRO			///< NOTE: Only present prior to macro expansion
		};


	};


	/// A set oc constant symbols
	typedef ReferencedSet<babb::utils::ref_ptr<const symbols::ConstantSymbol> >::type ConstantSet;

private:
	/****************************************************************************/
	/* Private Members */
	/****************************************************************************/

	/// Whether the element is surrounded by parentheses
	bool _parens;

	/// The type of element we're looking at
	Type::type _type;

	/// The start/end of this element in the original file
	Location _begin;
	Location _end;


	/// A set of constant symbols occuring within the element
	babb::utils::ref_ptr<const ConstantSet> _constants;

	/// A type mask of constant symbol types occuring within the element
	int _cmask;

public:
	/****************************************************************************/
	/* Constructor / Destructor */
	/****************************************************************************/
	/// Full constructor
	/// @param type The type of element this is
	/// @param The set of constants ocurring within the element.
	/// @param A mask of constant types occurring within the element.
	/// @param begin The beginning location of this element
	/// @param end The ending location of this element
	/// @param parens Whether the element is surrounded by parentheses
	Element(Type::type type, ConstantSet const* constants, int cmask, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0), bool parens = false);

	/// Destructor stub
	virtual ~Element();

	/****************************************************************************/
	/* Public Methods */
	/****************************************************************************/
	
	/// Getter/setter for whether or not this element has parentheses
	inline bool parens() const							{ return _parens; }
	inline void parens(bool p)							{ _parens = p; }

	/// Determines the type of the element
	inline Type::type type() const						{ return _type; }

	/// Get/set the starting location of this element
	inline Location const& beginLoc() const				{ return _begin; }
    inline void beginLoc(Location const& loc)           { _begin = loc; }

	/// Get the ending location of this element
	inline Location const& endLoc() const				{ return _end; }
    inline void endLoc(Location const& loc)             { _end = loc; }

	/// Helper function to output a string representing this element.
	inline std::string str() const						{ std::stringstream out; output(out); return out.str(); }
	
	/// Get the mask of constant types occurring within the element
	inline int cmask() const							{ return _cmask; }

	/// Iterate through the list of constants occurring within the element
	inline ConstantSet const* constants() const							{ return _constants; }
	inline ConstantSet::const_iterator beginConstants() const			{ return _constants->begin(); }
	inline ConstantSet::const_iterator endConstants() const				{ return _constants->end(); }

	/// Does a deep copy of the element
	virtual Element* copy() const = 0;

	/// Outputs the element to the given stream
	/// @param out The output stream to write to
	virtual void output(std::ostream& out) const = 0;
	
	/// Get a description of the domain this element ranges over.
	virtual DomainType::type domainType() const = 0;


protected:

	/// Initializes a new constant set as the union of the two constant sets provided
	static ConstantSet* newConstSet(Element const* a = NULL, Element const* b = NULL);

	/// Initializes a new constant set and the provided symbol if it's a constant symbol.
	static ConstantSet* newConstSet(symbols::Symbol const* sym);

	/// Adds the constants from an element to the provided set
	static ConstantSet* insertConstants(ConstantSet* dest, Element const* src = NULL);



};

}}

/// Element output operator
inline std::ostream& operator<<(std::ostream& out, bcplus::elements::Element const& elem) {
	elem.output(out); return out;
}


