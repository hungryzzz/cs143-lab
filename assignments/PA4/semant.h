#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <map>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

using std::map;
using std::vector;

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  // customer
  map<Symbol, Class_> class_map;
  map<Symbol, Symbol> inherits_class_graph;

  void add_class_table(Class_ c);

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  void tag_circle(Symbol tag_class, map<Symbol, int>& class_valid, map<Symbol, bool>& class_visit);
  bool check_class_table();
  Class_ get_class_by_name(Symbol name);
  Feature get_ancestor_method(Symbol c_name, Symbol m_name);
  bool is_ancestor(Symbol child_c, Symbol ancestor_c);
  void find_to_root_path(Symbol c, vector<Symbol>& seq);
  Symbol get_lowest_common_ancestor(Symbol c1, Symbol c2);
  Feature get_dispatch_method(Symbol c_name, Symbol m_name);
};


#endif

