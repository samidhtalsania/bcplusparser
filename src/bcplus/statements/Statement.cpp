
#include "bcplus/Location.h"
#include "bcplus/statements/Statement.h"

namespace bcplus {
namespace statements {

char const* Statement::Type::typeString(type v) {
	switch (v) {
	case INCLUDE:				return "include";
	case MACROS:				return "macros";
	case CONSTANTS:				return "constants";
	case OBJECTS:				return "objects";
	case SORTS:					return "sorts";
	case VARIABLES:				return "variables";
	case COMMENTS:				return "comments";


	F2LP:						return "F2LP block";
	LUA:						return "LUA block";
	ASP:						return "ASP block";
	SHOW:						return "show";
	SHOW_ALL:					return "show (all)";
	HIDE:						return "hide";
	HIDE_ALL:					return "hide (all)";
	NOCONCURRENCY:				return "noconcurrency";
	STRONG_NOCONCURRENCY:		return "strong noconcurrency";
	MAXAFVALUE:					return "maxAFValue";
	MAXADDITIVE:				return "maxAdditive";
	QUERY:						return "query";
	
	LAW_BASIC:					return "basic";
	LAW_CAUSED:					return "caused";
	LAW_PCAUSED:				return "possibly caused";
	LAW_IMPL:					return "implication";
	LAW_CAUSES:					return "causes";
	LAW_INCREMENTS:				return "increments";
	LAW_MCAUSE:					return "may cause";
	LAW_ALWAYS:					return "always";
	LAW_CONSTRAINT:				return "constraint";
	LAW_IMPOSSIBLE:				return "impossible";
	LAW_NEVER:					return "never";
	LAW_DEFAULT:				return "default";
	LAW_EXOGENOUS:				return "exogenous";
	LAW_INERTIAL:				return "inertial";
	LAW_NONEXECUTABLE:			return "nonexecutable";
	LAW_RIGID:					return "rigid";
	LAW_OBSERVED:				return "observed";

	default:					return "<BAD_STMT_TYPE>";
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


