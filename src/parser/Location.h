#pragma once

#include <boost/filesystem.hpp>

#include "pointers.h"
#include "referencedwrappers.h"

namespace bcplus {
namespace parser {

/// A simple location structure
class Location {
private:
	ref_ptr<const ReferencedPath > _file;
	size_t _line;
	size_t _col;

public:

	/// Default constructor
	inline Location()
		: _line(0), _col(0)
	{ /* Intentionally left blank */ }

	/// Full constructor.
	inline Location(ReferencedPath const* file, size_t line, size_t col)
		: _file(file), _line(line), _col(col)
	{ /* Intentionally left blank */ }

	/// Destructor
	inline virtual ~Location()												{ /* Intentionally left blank */ }

	/// Accessors
	inline ReferencedPath const* file() const								{ return _file; }
	inline size_t line() const												{ return _line; }
	inline size_t col() const												{ return _col; }

	/// Mutators
	inline void file(ReferencedPath const* f)								{ _file = f; }
	inline void line(size_t l)												{ _line = l; }
	inline void col(size_t c)												{ _col = c; }

};

}}

