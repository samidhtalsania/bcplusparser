
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include "parser/detail/ScannerSource.h"
#include "parser/detail/ScannerFileSource.h"

namespace fs = boost::filesystem;

ScannerFileSource::ScannerFileSource(Configuration* config, fs::path const& file, bool squelch)
	: ScannerSource(config), _buffer(NULL), _buffer_sz(0), _line(1), _newline(NULL) {


	// Ensure the file exists
	if (!squelch && (!fs::exists(file) || !fs::is_regular_file(file))) {
		config()->ostream(Verb::ERROR) << "ERROR: Cannot load input from file \"" << file.native() << "\". The file does not exist." << std::endl;
	}

	_file = new ReferencedPath();

	// try to open the new file
	_input.open(file);
	if (!squelch && !_input.good()) {
		config()->ostream(Verb::ERROR) << "ERROR: An error occurred while opening \"" << file.native() <<"\"." << std::endl;
		_input.close();
		_input.clear();
	}	
	
	// save the details
	*_file += file;

	fill(10);
}

ScannerFileSource::~ScannerFileSource() {
	if (_buffer) delete _buffer;
	if (_input.is_open()) _input.close();
}


void ScannerFileSource::newline() {
	_line++;
	_newline = cursor();
}

Location ScannerFileSource::loc() const {
	return Location(file(), line(), col());
}


void ScannerFileSource::close() {
	if (_input.is_open()) _input.close();
}


void ScannerFileSource::fill(size_t n) {
	char* first = (token() < marker()) ? token() : marker();

	size_t remaining = _limit - first;
	size_t req_sz = remaining + n;

	if (req_sz > _buffer_sz) {
		// oh noes. need more buffer
		size_t new_sz = req_sz*2;
		char* new_buf = new char[new_sz];
		memcpy(new_buf, first, remaining);
		delete[] _buffer;
		_buffer = new_buf;
		_buffer_sz = new_sz;
		_limit = new_buf + new_sz;
	} else {
		// we can keep the buffer and shift the contents
		memmove(_buffer, first, remaining);
	}	

	size_t marker_offset = (size_t)(marker() - first);
	size_t token_offset = (size_t)(token() - first);
	size_t cursor_offset = (size_t)(cursor() - first);
	size_t newline_offset = (size_t)(_newline - first);


	cursor() = _buffer + cursor_offset;
	marker() = _buffer + marker_offset;
	token() = _buffer + token_offset;
	_newline = _buffer + newline_offset;

	// Fill the remainder of the buffer
	char* readpos = _buffer + remaining;
	size_t read_amount = _buffer_sz - remaining;

	_input.read(readpos, read_amount);

	// if we hit the end fill with EOF
	if (_input.gcount() < read_amount) {
		readpos += _input.gcount();
		memset(readpos, '\0', read_amount - _input.gcount());
	}

	if (_input.fail() && !_input.eof()) {
		config()->ostream(Verb::ERROR) << "ERROR: An error occurred while reading from \"" << _file->native() << "\"." << std::endl;
	}
}

