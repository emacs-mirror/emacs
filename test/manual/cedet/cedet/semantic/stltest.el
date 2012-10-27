;;; semantic-stltest --- Test Semantic's C++ parser on the STL

;;; Copyright (C) 2012 David Engster

;; Author: David Engster <deng@randomsample.de>

;; This file is not part of GNU Emacs.

;; Semantic is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Test completions on STL classes and namespaces.
;; This test is not run by default since it requires a full STL installation.
;; It can also be used very well to benchmark the C++ parser.

(defvar semantic-stltest-vector-buf
  "#include <vector>\nint main(void) { std::vector<int> foo; foo.TESTHERE  \n}")
(defvar semantic-stltest-vector-members
  '("begin" "end" "rbegin" "rend" "size" "max_size" "resize" "capacity"
    "empty" "reserve" "at" "front" "back" "assign" "push_back" "pop_back"
    "insert" "erase" "swap" "clear" "iterator" "const_iterator" "reverse_iterator"
    "const_reverse_iterator" "reference" "const_reference" "size_type"
    "value_type" "difference_type" "allocator_type" "pointer" "const_pointer"))

(defvar semantic-stltest-string-buf
  "#include <string>\nint main(void) { std::string foo; foo.TESTHERE  \n}")
(defvar semantic-stltest-string-members
  '("allocator_type" "append" "assign" "at" "begin" "c_str" "capacity" "clear"
   "compare" "const_iterator" "const_pointer" "const_reference" "const_reverse_iterator"
   "copy" "data" "difference_type" "empty" "end" "erase" "find" "find_first_not_of"
   "find_first_of" "find_last_not_of" "find_last_of" "get_allocator" "insert"
   "iterator" "length" "max_size" "npos" "pointer" "push_back" "rbegin" "reference"
   "rend" "replace" "reserve" "resize" "reverse_iterator" "size" "size_type" "substr"
   "swap" "traits_type" "value_type"))

(defvar semantic-stltest-map-buf
  "#include <map>\n#include <string>\nint main(void) { std::map<int,std::string> foo; foo.TESTHERE  \n}")
(defvar semantic-stltest-map-members
  '("allocator_type" "at" "begin" "clear" "const_iterator" "const_pointer"
    "const_reference" "const_reverse_iterator" "count" "difference_type" "empty"
    "end" "equal_range" "erase" "find" "get_allocator" "insert" "iterator" "key_comp"
    "key_compare" "key_type" "lower_bound" "mapped_type" "max_size" "pointer" "rbegin"
    "reference" "rend" "reverse_iterator" "size" "size_type" "swap" "upper_bound" "value_comp"
    "value_compare" "value_type"))

(defvar semantic-stltest-map-buf
  "#include <map>\n#include <string>\nint main(void) { std::map<int,std::string> foo; foo.TESTHERE  \n}")
(defvar semantic-stltest-map-members
  '("allocator_type" "at" "begin" "clear" "const_iterator" "const_pointer"
    "const_reference" "const_reverse_iterator" "count" "difference_type" "empty"
    "end" "equal_range" "erase" "find" "get_allocator" "insert" "iterator" "key_comp"
    "key_compare" "key_type" "lower_bound" "mapped_type" "max_size" "pointer" "rbegin"
    "reference" "rend" "reverse_iterator" "size" "size_type" "swap" "upper_bound" "value_comp"
    "value_compare" "value_type"))

(defvar semantic-stltest-iostream-buf
  "#include <iostream>\nint main(void) { std::TESTHERE  \n}")
(defvar semantic-stltest-iostream-members
  '("ios_base" "ios" "istream" "ostream" "iostream" "ifstream" "ofstream" "fstream" "istringstream"
   "ostringstream" "stringstream" "streambuf" "filebuf" "stringbuf" "cin" "cout" "cerr" "clog" "fpos"
   "streamoff" "streampos" "streamsize" "boolalpha" "dec" "endl" "ends" "fixed" "flush" "hex" "internal"
   "left" "noboolalpha" "noshowbase" "noshowpoint" "noshowpos" "noskipws" "nounitbuf" "nouppercase" "oct"
   "right" "scientific" "showbase" "showpoint" "showpos" "skipws" "unitbuf" "uppercase" "ws"))


(defun semantic-stltest (class)
  "Test completions on CLASS."
  (semantic-mode 1)
  (with-current-buffer
      (find-file-noselect "/tmp/stltest.cpp")
    (erase-buffer)
    (insert (symbol-value (intern
			   (concat "semantic-stltest-" class "-buf"))))
    (goto-char (point-min))
    (search-forward "TESTHERE")
    (delete-region (match-beginning 0) (match-end 0))
    (let* ((out (or (semantic-analyze-possible-completions (point))
		    (error "%s: Did not get any tags." class)))
	   (names (mapcar 'semantic-tag-name out)))
      (dolist (cur (symbol-value (intern
			   (concat "semantic-stltest-" class "-members"))))
	(unless (member cur names)
	  (error "Did not find %s in %s class." cur class)))))
  (message "STL %s PASSED." class))

(setq-default semanticdb-new-database-class 'semanticdb-project-database)
(semantic-stltest "vector")
(semantic-stltest "string")
(semantic-stltest "map")
;; I know, not a class...
(semantic-stltest "iostream")
