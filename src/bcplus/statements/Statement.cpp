
#include "bcplus/Location.h"
#include "bcplus/statements/Statement.h"

namespace bcplus {
namespace statements {

char const* Statement::Type::typeString(type v) {
	switch (v) {
	case INCLUDE:			return "include";
	case MACROS:			return "macros";
	case CONSTANTS:			return "constants";
	case OBJECTS:			return "objects";
	case SORTS:				return "sorts";
	case VARIABLES:			return "variables";
	case COMMENTS:			return "comments";
	default:				return "<BAD_STMT_TYPE>";
	}

}

Statement::Statement(Type::type type, Location const& begin, Location const& end) 
	: _begin(begin), _end(end), _type(type) {
	/* Intentionally left blank */
}

Statement::~Statement() {
	/* Intentionally left blank */
}


}}


