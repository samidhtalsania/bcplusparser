#pragma once

#include <string>
#include <iostream>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include "Referenced.h"
#include "referencedwrappers.h"
#include "Configuration.h"

#include "parser/Location.h"
#include "parser/detail/ScannerSource.h"


namespace bcplus {
namespace parser {
namespace detail {


/// A simple class to track files being worked on by the scanner
class ScannerFileSource : public ScannerSource {

private:
	/***********************************************************/
	/* Private Members */
	/***********************************************************/

	/// Input file/stream
	ref_ptr<ReferencedPath> _file;
	boost::filesystem::fstream _input;

	/// Buffer information
	char* _buffer;
	size_t _buffer_sz;

	/// File position
	size_t _line;
	char* _newline;

public:
	/***********************************************************/
	/* Constructors */
	/***********************************************************/
	/// @param config The system wide configuration information
	/// @param file The file to read from
	/// @param squelch Whether to silence all errors resulting from this call.
	ScannerFileSource(Configuration* config, boost::filesystem::path const& file, bool squelch = false);

	/// Destructor
	virtual ~ScannerFileSource();

	/***********************************************************/
	/* Public Functions */
	/***********************************************************/

	/// Gets the current file being read from.
	inline ReferencedPath const* file() const					{ return _file; }

	/// Gets the current line number
	inline size_t line() const									{ return _line; }
	
	/// Gets the current column number
	inline size_t col() const									{ return (size_t)(_token - _newline + 1); }


	// inherited stuffs
	virtual void newline();
	virtual inline Location loc() const							{ return Location(_file, _line, _col); }
	virtual Status status() const;
	virtual void close();
	virtual void fill(size_t n);

};

}}}

