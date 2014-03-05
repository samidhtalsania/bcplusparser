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


/// A scanner source which draws directly from a pre-defined in-memory buffer
class ScannerBufferSource : public ScannerSource {

private:
	/***********************************************************/
	/* Private Members */
	/***********************************************************/

	/// Input file/stream
	ref_ptr<ReferencedPath> _file;
	boost::filesystem::fstream _input;

	/// Buffer information
	char const* _buffer;
	size_t buffer_sz;

	/// psuedo position
	Location _loc;

public:
	/***********************************************************/
	/* Constructors */
	/***********************************************************/
	/// @param config The system wide configuration information
	/// @param buffer The null-terminated buffer to read from.
	/// @param loc The location that should be displayed in output messages partaining to this source
	ScannerBufferSource(Configuration* config, char const* buffer, Location const& loc = Location(NULL, 0, 0));

	/// Destructor
	virtual ~ScannerBufferSource();

	/***********************************************************/
	/* Public Functions */
	/***********************************************************/

	// inherited stuffs
	virtual void newline();
	virtual Location loc() const;
	virtual Status status() const;
	virtual void close();
	virtual void fill(size_t n);

};

}}}

