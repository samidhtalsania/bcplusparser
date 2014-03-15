
#include "bcplus/Location.h"
#include "bcplus/statements/Statement.h"

namespace bcplus {
namespace statements {

Statement::Statement(Type::Value type, Location const& begin, Location const& end) 
	: _begin(begin), _end(end), _type(type) {
	/* Intentionally left blank */
}

Statement::~Statement() {
	/* Intentionally left blank */
}


}}


