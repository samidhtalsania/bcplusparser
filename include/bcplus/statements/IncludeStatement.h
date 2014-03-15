#pragma once

#include "babb/utils/memory.h"
#include "memwrappers.h"

#include "bcplus/Location.h"
#include "bcplus/statements/Statement.h"

namespace bcplus {
namespace statements {

class IncludeStatement : public Statement 
{
public:
	/************************************************************************************/
	/* Public Types */
	/************************************************************************************/
	typedef ReferencedList<babb::utils::ref_ptr<const ReferencedString> >::type FilenameList;

	typedef FilenameList::iterator iterator;
	typedef FilenameList::const_iterator const_iterator;

private:
	/************************************************************************************/
	/* Private Members */
	/************************************************************************************/
	/// The list of file names to load.
	babb::utils::ref_ptr<FilenameList> _files;


public:
	/************************************************************************************/
	/* Constructors / Destructors */
	/************************************************************************************/
	/// Basic constructor
	/// @param files The list of files that should be included (or NULL to create an empty include)
	/// @param begin The beginning location for this statement
	/// @param end The ending location for this statement
	IncludeStatement(FilenameList* files = NULL, Location const& begin = Location(NULL, 0, 0), Location const& end = Location(NULL, 0, 0));

	/// Destructor stub
	virtual ~IncludeStatement();


	/// Gets the number of files that should be included
	inline size_t size() const												{ return _files->size(); }

	/// Iterate over the file list
	inline iterator begin()													{ return _files->begin(); }
	inline const_iterator begin() const										{ return _files->begin(); }
	inline iterator end()													{ return _files->end(); }
	inline const_iterator end() const										{ return _files->end(); }

	/// Adds a filename to the beginning include statement
	inline void push_back(ReferencedString const* filename) 				{ _files->push_back(filename); }


	/// Perform a dep copy of this statement
	virtual Statement* copy() const;

	/// Outputs this statement to the given stream
	/// @param out The output stream to write to
	virtual void output(std::ostream& out) const;


};


}}


