#include <assert.h>
#include <stdio.h>
#include <vector>
#include <map>
#include <utility>
#include <algorithm>
#include <string>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

using std::vector;
using std::map;
using std::pair;
using std::sort;
using std::make_pair;
using std::to_string;

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

typedef struct Disp_method {
   Symbol disp_class_name;
   Feature disp_method;
} disp_method;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   map<Symbol, int> classtag_map;
   vector<CgenNodeP> classtag_vec;
   map<Symbol, int> smallest_classtag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   void code_class_name_table();
   void code_class_object_table();
   void visit_object(CgenNodeP nd, vector<CgenNodeP>& prefix_nd, int& visit_num);
   void code_object_layout_table();
   void code_object_init();
   void code_object_methods();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   int find_classtag(Symbol s) {
      if (classtag_map.find(s) != classtag_map.end()) {
         return classtag_map[s];
      }
      return -1;
   }
   int find_smallest_classtag(Symbol s) {
      if (smallest_classtag.find(s) != smallest_classtag.end()) {
         return smallest_classtag[s];
      }
      return -1;
   }
   CgenNodeP get_class_node(Symbol s) {
      if (classtag_vec.size() != 0) {
         return classtag_vec[classtag_map[s]];
      }
      return NULL;
   }
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   void code_disp_table(ostream& str);
   void code_proto_table(ostream& str);
   void code_features(ostream& str, bool is_attr, CgenClassTable* ct);

   void add_features_to_child(CgenNodeP child);

   vector<Feature> attrs_including_inherits;
   void add_attr(Feature ac) { attrs_including_inherits.push_back(ac); }
   int find_attr_by_name(Symbol attr_name) {
      for (size_t i = 0; i < attrs_including_inherits.size(); ++i) {
         if (attrs_including_inherits[i]->get_name() == attr_name) {
            return i;
         }
      }
      return -1;
   }
   int parent_attrs_num;
   void set_parent_attrs_num(int i) { parent_attrs_num = i; }

   vector<disp_method*> methods_including_inherits;
   void add_method(Symbol class_name, Feature mc) {
      disp_method* new_parent_m = new disp_method();
      new_parent_m->disp_class_name = class_name;
      new_parent_m->disp_method = mc;
      methods_including_inherits.push_back(new_parent_m);
   }
   void update_method(int i, Symbol class_name, Feature mc) {
      methods_including_inherits[i]->disp_class_name = class_name;
      methods_including_inherits[i]->disp_method = mc;
   }
   int find_method_by_name(Symbol mc_name) {
      for (size_t i = 0; i < methods_including_inherits.size(); ++i) {
         if (methods_including_inherits[i]->disp_method->get_name() == mc_name) {
            return i;
         }
      }
      return -1;
   }

   int class_tag;
   void set_class_tag(int i) { class_tag = i; }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

