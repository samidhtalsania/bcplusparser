#include <cstring>

#include "parser/detail/ScannerSource.h"
#include "parser/detail/ScannerBufferSource.h"

namespace fs = boost::filesystem;

ScannerBufferSource::ScannerBufferSource(Configuration* config, char const* buffer, Location const& loc)
	: ScannerSource(config), _buffer(buffer), _loc(loc) {


	// figure out the size of the buffer
	_buffer_sz = strlen(buffer) + 1;

}

ScannerBufferSource::~ScannerBufferSource() {
	// intentionally left blank
}

void ScannerBufferSource::newline() {
	// location is not tracked internally...
	// intentionally left blank
}

Location ScannerBufferSource::loc() const {
	return _loc;
}

Status ScannerBufferSource::status() {
	return STAT_OK;
}



void ScannerBufferSource::close() {
	// intentionaly left blankd
}


void ScannerBufferSource::fill(size_t n) {

	// basically this is a dud that will just initialize all of the
	// scanner state members to use the current buffer

	char* first = (token() < marker()) ? token() : marker();

	size_t marker_offset = (size_t)(marker() - first);
	size_t token_offset = (size_t)(token() - first);
	size_t cursor_offset = (size_t)(cursor() - first);
	size_t newline_offset = (size_t)(_newline - first);

	cursor() = _buffer + cursor_offset;
	marker() = _buffer + marker_offset;
	token() = _buffer + token_offset;
	limit() = _buffer + _buffer_sz;
}

