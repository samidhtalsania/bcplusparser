#pragma once

#include "Referenced.h"
#include "pointers.h"

#include "referencedwrappers.h"
#include "parser/Location.h"

namespace bcplus {
namespace parser {


/// A simple token structure
class Token : public Referenced {
private:
	int _type;
	ref_ptr<const ReferencedString> _string;
	Location _begin;
	Location _end

public:

	/// Basic Constructor
	inline Token(int type, ReferencedString const* string, Location const& begin, Location const& end)
		: _type(type), _string(string), _begin(begin), _end(end)
	{/* Intentionally left blank */ }

	/// Destructor
	inline virtual ~Token()												{ /* Intentionally left blank */ }

	/// Accessors
	inline int type() const												{ return _type; }
	inline ReferencedString const* str() const							{ return _string; }
	inline Location const& begin() const								{ return _begin; }
	inline Location const& end() const									{ return _end; }
};

}}
