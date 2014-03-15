#pragma once

#include <string>
#include <ostream>
#include <sstream>

#include "babb/utils/memory.h"
#include "bcplus/Location.h"

namespace bcplus {
namespace statements {

/// Abstract base class for a statement.
class Statement : public babb::utils::Referenced {
public:
	/****************************************************************************/
	/* Public Types */
	/****************************************************************************/
	/// Container for the list of statement types
	struct Type {
		enum Value {
			INCLUDE
		};
	};

private:

	/****************************************************************************/
	/* Private Members */
	/****************************************************************************/
	/// beginning location of the statement
	Location _begin;

	/// ending location of the statement
	Location _end;

	/// Statement type
	Type::Value _type;

public:

	/****************************************************************************/
	/* Constructors / Destructors */
	/****************************************************************************/

	/// Basic Contructor
	/// @param type The statement type
	/// @param begin The beginning location of the statmenet
	/// @param end The ending location of the statement
	Statement(Type::Value type, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0));

	/// Destructor Stub
	~Statement();


	/****************************************************************************/
	/* Public Functions */
	/****************************************************************************/
	
	/// Get the type of this statement
	inline Type::Value type() const									{ return _type; }

	/// Get the beginning location of this statement
	inline Location const& beginLoc() const							{ return _begin; }

	/// Get the ending location of this statement
	inline Location const& endLoc() const							{ return _end; }

	/// Helper function to output a string representation of this statement
	inline std::string str() const									{ std::stringstream out; output(out); return out.str(); }

	/// Perform a dep copy of this statement
	virtual Statement* copy() const = 0;

	/// Outputs this statement to the given stream
	/// @param out The output stream to write to
	virtual void output(std::ostream& out) const = 0;


};
}}

