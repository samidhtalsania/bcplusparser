	// Intentionally left blank
#pragma once
#include <list>
#include <set>
#include <map>
#include <iostream>

#include <boost/filesystem.hpp>

#include "Referenced.h"
#include "referencedwrappers.h"
#include "pointers.h"
#include "Solution.h"
#include "parser/BaseType.h"
#include "parser/BaseElement.h"
#include "parser/Scanner.h"
#include "Configuration.h"

/// A parser which allows us to scan answer sets and/or inputs for the atoms they contain
class AtomParser : public Referenced {
public:
	/***********************************************************************************/
	/* Types */
	/***********************************************************************************/

	typedef std::set<ref_ptr<const BaseType>, 
		ptr_less<ref_ptr<const BaseType> > > PredList;
	typedef PredList::iterator predIterator;
	typedef PredList::const_iterator const_predIterator;

	typedef ReferencedWrapper<std::list<ref_ptr<const Solution> > > SolutionList;

private:
	/***********************************************************************************/
	/* Members */
	/***********************************************************************************/

	/// The file we're currently reading.
	boost::filesystem::path _file;

	/// Set of predicates we've encountered.
	PredList _preds;

	/// Set of functions we've encountered.
	PredList _funcs;

	/// The scanner used for lexical analysis.
	ref_ptr<Scanner> _scanner;

	/// Whether oir not an error has occurred.
	bool _errflag;

	/// A temporary list for collecting parsed solutions
	ref_ptr<SolutionList> _list;

	/// The lemon parser
	void* _parser;

	/// The system-wide configuration information
	ref_ptr<Configuration> _config;

public:
	/***********************************************************************************/
	/* Constructors */
	/***********************************************************************************/
	/// @param config The system-wide configuration information 
	AtomParser(Configuration* config);

	/// Destructor
	virtual ~AtomParser();


	/***********************************************************************************/
	/* Public Functions */
	/***********************************************************************************/

	/// Attempts to parse the atoms from the provided input stream.
	/// @param reset Whether we should forget about previously parsed input.
	/// @param input The input file to read from.
	/// @return True if successful, false otherwise.
	SolutionList* parse(bool reset, boost::filesystem::path const& input);

	/// Resets the parser clearing all previous input
	void reset();

	/// Gets the predicates that have been parsed so far
	inline const_predIterator beginPredicates() const							{ return _preds.begin(); }
	inline const_predIterator endPredicates() const								{ return _preds.end(); }

	/***********************************************************************************/
	/* Stuff Thou Shall not Call */
	/***********************************************************************************/

	/// INTERNAL FUNCTION
	/// Finds (or creates) the matching base type
	BaseType const* _resolveType(BaseType::Type type, ReferencedString const* name, size_t arity);

	/// INTERNAL FUNCTION
	/// Registers a parse error.
	void _parse_error(std::string const& err);


	inline void _addSolution(Solution const* solution)							{ _list->push_back(solution); }

};





