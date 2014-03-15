#include <boost/foreach.hpp>

#include "bcplus/Location.h"
#include "bcplus/statements/IncludeStatement.h"

namespace bcplus {
namespace statements {

IncludeStatement::IncludeStatement(FilenameList* files, Location const& begin, Location const& end) 
	: Statement(Statement::Type::INCLUDE, begin, end) {
	if (files) _files = files;
	else _files = new FilenameList();
}

IncludeStatement::~IncludeStatement() {
	/* Intentionally left blank */
}

Statement* IncludeStatement::copy() const {
	FilenameList* l = new FilenameList();
	BOOST_FOREACH(ReferencedString const* t, *this) {
		l->push_back(t);
	}

	return new IncludeStatement(l, beginLoc(), endLoc());
}

void IncludeStatement::output(std::ostream& out) const {
	out << ":- include ";

	bool first = true;
	BOOST_FOREACH(ReferencedString const* t, *this) {
		if (!first) out << "; ";
		out << "\"" << *t << "\"";
		first = false;
	}
	out << ".";
}



}}
