#ifndef UTILS_IO_INITIALIZER_H_
#define UTILS_IO_INITIALIZER_H_

#include <iostream>
#include <regex>
#include <string>
#include <map>
#include <fstream>
#include <vector>
#include <sstream>
#include <stdexcept>
#include <unordered_map>
#include <list>
#include <algorithm>

/** \ingroup utils */

/**
	\brief A simple initializer that reads a parameter file and stores the values in a named std::map.
	
	The parameter file must have three sections - STRINGS, SCALARS, ARRAYS. Sections start with '>'. 
	Each section has name-value pairs separated by whitespace. Arrays have a name followed by values, ending in '-1'.
	Comments are allowed. Comments start with "# " (note the space) and can come either on a new line or on the same line after
	the name-value(s) pair. 
	
	Here is an example parameter file:
	
	~~~{.ini}
	> STRINGS
	sim_name      mySimution
	output_file   ~/output/test.txt
	
	> SCALARS
	graphics      1           # Do we want graphics to be on? 
	timesteps     1000        # For how many timesteps do we run the simulation?
	dt            0.1
	# This is also a comment
	
	> ARRAYS
	parameter1    1 2 3 4 5 6 -1
	~~~
*/
namespace io{



//struct section{
//	std::string name;
//	std::unordered_map<std::string, std::string> keyvalues;
//};

class Initializer{
	using section = std::unordered_map<std::string, std::string>;

	private:
	//std::string init_fname;
	std::unordered_map<std::string, section> sections;
	//pieces[2].str();std::ifstream fin;

	public:
	inline void parse(std::istream& in) {
//		static const std::regex comment_regex{R"x(\s*[;#])x"};
		static const std::regex section_regex{R"(\s*\[([^\]]+)\])"};
		static const std::regex value_regex{R"(\s*(\S[^ \t=]*)\s*=\s*((\s*\S+)+)\s*$)"};
		static const std::regex comment_regex{"([^;#]*)([;#])"};
		std::string current_section;
		std::smatch pieces;
		std::string line;
		while (std::getline(in, line)){
			// trim text following comment characters
			std::regex_search(line, pieces, comment_regex);
			if (pieces.size() == 3){
				std::cout << "Trimming comment line [" << line << "] to [" << pieces[1].str() << "]\n";
				line = pieces[1].str();
			} 
			
			// parse line
			if (line.empty()) {
				// skip comment lines and blank lines					
			}
			else if (std::regex_match(line, pieces, section_regex)) {
				if (pieces.size() == 2) { // exactly one match
					current_section = pieces[1].str();
					std::cout << "Section = " << current_section << "\n--------------------------\n";
				}
			}
			else if (std::regex_match(line, pieces, value_regex)) {
				if (pieces.size() == 4) { // exactly enough matches
					sections[current_section][pieces[1].str()] = pieces[2].str();
					std::cout << pieces[1].str() << " = " << pieces[2].str() << "\n";
				}
			}
			else {
				std::cout << "skipping line [" << line << "]\n";
				//throw std::runtime_error("Cannot parse line "+line);
			}
		}
	}

	public:
//	inline Initializer(){}

//	inline Initializer(std::string fname){
//		init_fname = fname;
//	}

//	inline std::list<section>& config::get_sections() const {
//		return sections;
//	}

//	inline section* get_section(const std::string& sectionname) const {
//		auto found = std::find_if(sections.begin(), sections.end(), [sectionname](const section& sect){ 
//		                                                                 return sect.name.compare(sectionname) == 0; 
//		                                                                 });

//		return found != sections.end() ? &*found : nullptr;
//	}
//	
//	std::string config::get_value(const std::string& sectionname, const std::string&keyname) {
//		section* sect = get_section(sectionname);
//		if (sect != nullptr) {
//			std::unordered_map<std::string, std::string>::const_iterator it = sect->keyvalues.find(keyname);
//			if (it != sect->keyvalues.end()) return it->second;
//		}
//		return "";
//	}

};




//class Initializer{
//	private:
//	std::string init_fname;
//	std::ifstream fin;
//	
//	std::map <std::string, std::string> strings;
//	std::map <std::string, double>  scalars;
//	std::map <std::string, std::vector <double>> arrays;
//	
//	public:
//	
//	inline Initializer(){}

//	inline Initializer(std::string fname){
//		init_fname = fname;
//	}

//	inline void setInitFile(std::string fname){
//		init_fname = fname;
//	}

//	inline void readFile(){
//		// Reset maps here
//		strings.clear(); 
//		scalars.clear();
//		arrays.clear();

//		fin.open(init_fname.c_str());
//		if (!fin) {
//			throw std::runtime_error("Cannot open initializer file " + init_fname); 
//		}
//		
//		std::string attr_begin = ">";
//		std::string init_format = "    > STRINGS\n    ... \n\n    > SCALARS\n    ...\n\n    > ARRAYS \n    ...\n";

//		std::string s, v;
//		double f;
//		std::vector <double> vf;
//		
//		while (fin >> s && s != attr_begin);	// read until 1st > is reached

//		fin >> s; 
//		if (s != "STRINGS") {
//			throw std::runtime_error("STRINGS section missing in Initializer file " + init_fname + ". Format must be:\n" + init_format); 
//		}
//		while (fin >> s && s != attr_begin){
//			if (s == "") continue;	// skip empty lines
//			if (s == "#") {getline(fin,s,'\n'); continue;}	// skip #-followed lines (comments)
//			fin >> v;
//			strings[s] = v;
//		}

//		fin >> s;
//		if (s != "SCALARS") {
//			throw std::runtime_error("SCALARS section missing in Initializer file " + init_fname + ". Format must be:\n" + init_format); 
//		}
//		while (fin >> s && s != attr_begin){
//			if (s == "") continue;	// skip empty lines
//			if (s == "#") {getline(fin,s,'\n'); continue;}	// skip #-followed lines (comments)
//			fin >> f;
//			scalars[s] = f;
//		}

//		fin >> s;
//		if (s != "ARRAYS") {
//			throw std::runtime_error("ARRAYS section missing in Initializer file " + init_fname + ". Format must be:\n" + init_format);
//		}
//		while (fin >> s && s != attr_begin){
//			if (s == "") continue;	// skip empty lines
//			if (s == "#") {getline(fin,s,'\n'); continue;}	// skip #-followed lines (comments)
//			while (fin >> f && f != -1) vf.push_back(f);	// TODO: Replace -1 with a symbol
//			arrays[s] = vf;
//			vf.resize(0);
//		}

//		fin.close();
//	}

//	// Currently, this searches only in strings
//	// FIXME: in generic initializer class, sections can be arbitrary (except arrays) 
//	template<class T>
//	T get(std::string s){
//		std::map <std::string, std::string>::iterator it = strings.find(s);
//		if (it != strings.end()){
//			std::string val_s = it->second;
//			std::stringstream sin(val_s);
//			T val;
//			sin >> val;
//			return val;	
//		}
//		else {
//			throw std::runtime_error("Could not find required variable " + s + " in initializer file " + init_fname);
//		}
//		
//	}
//	
//	
//	inline std::string getString(std::string s){
//		std::map <std::string, std::string>::iterator it = strings.find(s);
//		if (it != strings.end()) return it->second;
//		else {
//			throw std::runtime_error("Could not find required string " + s + " in initializer file " + init_fname + "");
//		}
//	}

//	inline double getScalar(std::string s){
//		std::map <std::string, double>::iterator it = scalars.find(s);
//		if (it != scalars.end()) return it->second;
//		else {
//			throw std::runtime_error("Could not find required scalar " + s + " in initializer file " + init_fname + "");
//		}
//	}

//	inline std::vector <double> getArray(std::string s, int size = -1){
//		std::map <std::string, std::vector<double> >::iterator it = arrays.find(s);
//		if (it == arrays.end()) {	// array not found
//			throw std::runtime_error("Could not find required array " + s + " in initializer file " + init_fname + "");
//		}
//		if (it->second.size() == 0){
//			throw std::runtime_error("Required array " + s + " is empty.");
//		}
//		
//		if (size == -1 || size == it->second.size()) return it->second;
//		else {
//			std::stringstream sout;
//			sout << "FATAL ERROR: Incorrect size of array " << s << ". Required " << size << ", found " << it->second.size() << '\n'; 
//			throw std::runtime_error(sout.str());
//		}
//	}

//	inline void print(){
//		std::cout << "-------:\n";
//		std::cout << "STRINGS:\n";
//		std::cout << "-------:\n";
//		for (std::map<std::string, std::string>::iterator it = strings.begin(); it != strings.end(); ++it){
//			std::cout << it->first << ": " << it->second << '\n';
//		}
//		std::cout << "-------:\n";
//		std::cout << "SCALARS:\n";
//		std::cout << "-------:\n";
//		for (std::map<std::string, double>::iterator it = scalars.begin(); it != scalars.end(); ++it){
//			std::cout << it->first << ": " << it->second << '\n';
//		}
//		std::cout << "-------:\n";
//		std::cout << "ARRAYS:\n";
//		std::cout << "------:\n";
//		for (std::map<std::string, std::vector<double> >::iterator it = arrays.begin(); it != arrays.end(); ++it){
//			std::cout << it->first << ": ";
//			for (int i=0; i< it->second.size(); ++i) std::cout << it->second[i] << " ";
//			std::cout << "\n";
//		}
//		std::cout << "\n";
//	}
//	
//	
//};

} // namespace io

#endif

