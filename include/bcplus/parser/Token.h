#pragma once

#include <ostream>

#include "babb/utils/memory.h"
#include "memwrappers.h"

#include "bcplus/Location.h"

namespace bcplus {
namespace parser {


/// A simple token structure
class Token : public babb::utils::Referenced {
private:
	int _type;
	babb::utils::ref_ptr<const ReferencedString> _string;
	Location _begin;
	Location _end;

public:

	/// Basic Constructor
	Token(int type, ReferencedString const* string, Location const& begin, Location const& end);

	/// Destructor
	virtual ~Token();

	/// Static functions
	/// Get the name of the provided token type.
	static char const* typeString(int token);


	/// Accessors
	inline int type() const												{ return _type; }
	inline ReferencedString const* str() const							{ return _string; }
	inline Location const& begin() const								{ return _begin; }
	inline Location const& end() const									{ return _end; }
	inline char const* typeString() const								{ return typeString(type()); }
};



}}

inline std::ostream& operator<<(std::ostream& out, bcplus::parser::Token const& tok) {
	return (out << *(tok.str()));
}
