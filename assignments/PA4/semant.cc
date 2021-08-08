

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <iostream>
#include <map>
#include <vector>

using std::cout;
using std::map;
using std::vector;

extern int semant_debug;
extern char *curr_filename;

map<Symbol, Class_>::iterator class_map_it;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */
    install_basic_classes();

    // create class graph
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        add_class_table(classes->nth(i));
    }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    add_class_table(Object_class);
    add_class_table(IO_class);
    add_class_table(Int_class);
    add_class_table(Bool_class);
    add_class_table(Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */

/*
    Add class to class map & graph.
    The following class will be rejected:
    1) redefined
    2) _no_class name
    3) inherits from Int, Bool or Str class

*/
void ClassTable::add_class_table(Class_ c) {
    Symbol name = c->get_name();
    Symbol parent = c->get_parent();

    if (name == No_class) {
        semant_error(c) << "The class name is not allow to be _no_class!\n";
    }
    if (name == SELF_TYPE) {
        semant_error(c) << "Redefinition of basic class SELF_TYPE.\n";
    }
    if (parent == Int || parent == Bool || parent == Str) {
        semant_error(c) << "The class is not allow to inherit from Int or Bool or Str!\n";
    }
    if (class_map.find(name) == class_map.end()) {
        class_map[name] = c;
        inherits_class_graph[name] = parent;
    } else {
        semant_error(c) << "Class " << name << " was previously defined.\n";
    }
}


/*
    Check whether the class graph is valid, including
    1) cycle?
    2) inherit class existence
    3) Main class existence
*/

void ClassTable::tag_circle(Symbol tag_class, map<Symbol, int>& class_valid, map<Symbol, bool>& class_visit) {
    Symbol parent = inherits_class_graph[tag_class];
    class_visit[tag_class] = true;

    if (class_map.find(tag_class) == class_map.end()) {
        class_valid[tag_class] = 1;
        return;
    }
    if (class_valid[parent] != 0) {
        class_valid[tag_class] = class_valid[parent];
        return;
    }
    if (class_visit[parent]) {
        class_valid[tag_class] = 2;
        return;
    }
    tag_circle(parent, class_valid, class_visit);
    class_valid[tag_class] = class_valid[parent];
}

bool ClassTable::check_class_table() {
    bool validation = true;

    map<Symbol, bool> class_visit;
    map<Symbol, int> class_valid;
    for (class_map_it = class_map.begin(); class_map_it != class_map.end(); ++class_map_it) {
        Symbol checking_class = class_map_it->first;
        Symbol parent = inherits_class_graph[checking_class];
        if (parent == No_class) continue;
        // check parent class
        if (class_map.find(parent) == class_map.end()) {
            semant_error(class_map_it->second) << "Class " << checking_class << " inherits from an undefined class " << parent << ".\n";
            validation = false;
            continue;
        }
        if (class_valid[checking_class] == 0) {
            tag_circle(checking_class, class_valid, class_visit);
        } 
        if (class_valid[checking_class] == 2) {
            semant_error(class_map_it->second) << "Class " << checking_class << ", or an ancestor of " << checking_class << ", is involved in an inheritance cycle.\n";
            validation = false;
        }
    }

    if (class_map.find(Main) == class_map.end()) {
        semant_error() << "Class Main is not defined.\n";
    }
    return validation;
}

/* class table utitls function */
Class_ ClassTable::get_class_by_name(Symbol name) {
    return class_map.find(name) == class_map.end() ? NULL : class_map[name];
}

Feature ClassTable::get_ancestor_method(Symbol c_name, Symbol m_name) {
    if (inherits_class_graph.find(c_name) == inherits_class_graph.end()) {
        return NULL;
    }
    Symbol child = c_name, parent;
    while (child != Object) {
        parent = inherits_class_graph[child];
        Feature ancestor_method = class_map[parent]->find_method(m_name);
        if (ancestor_method != NULL) {
            return ancestor_method;
        }
        child = parent;
    }
    return NULL;
}

Feature ClassTable::get_dispatch_method(Symbol c_name, Symbol m_name) {
    Feature method = class_map[c_name]->find_method(m_name);
    if (method == NULL) {
        method = this->get_ancestor_method(c_name, m_name);
    }
    return method;
}

bool ClassTable::is_ancestor(Symbol child_c, Symbol ancestor_c) {
    if (inherits_class_graph.find(child_c) == inherits_class_graph.end()) {
        return false;
    }
    Symbol child = child_c, parent;
    while (child != Object) {
        parent = inherits_class_graph[child];
        if (parent == ancestor_c) {
            return true;
        }
        child = parent;
    }
    return false;
}

void ClassTable::find_to_root_path(Symbol c, vector<Symbol>& seq) {
    Symbol child = c, parent;
    while (child != Object) {
        parent = inherits_class_graph[child];
        seq.push_back(parent);
        child = parent;
    }
}

Symbol ClassTable::get_lowest_common_ancestor(Symbol c1, Symbol c2) {
    if (c1 == c2) return c1;
    vector<Symbol> seq1, seq2;
    seq1.push_back(c1);
    seq2.push_back(c2);
    this->find_to_root_path(c1, seq1);
    this->find_to_root_path(c2, seq2);

    for (int i = seq1.size()-1, j = seq2.size()-1; i >= 0 && j >= 0; --i, --j) {
        if (i-1 >= 0 && j-1 >= 0 && seq1[i-1] != seq2[j-1]) return seq1[i];
        else if (i == 0) return seq1[i];
        else if (j == 0) return seq2[j];
    }

    return Object;
}

void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);
    type_env env;
    env.obj_env = new SymbolTable<Symbol, Symbol>();
    env.curr_class = NULL;
    env.ct = classtable;

    /* some semantic analysis code may go here */
    if (!classtable->errors() && classtable->check_class_table()) {
        for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
            env.obj_env->enterscope();
            env.curr_class = classes->nth(i);

            // build class env
            classes->nth(i)->init_class_env(env);
            // type check
            classes->nth(i)->type_check(env);

            env.obj_env->exitscope();
        }
    }

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

/*
    class methods
*/
void class__class::init_class_env(type_env& env) {
    if (name != Object) {
        env.ct->get_class_by_name(parent)->init_class_env(env);
    }

    for (int i = features->first(); features->more(i); i = features->next(i)) {
        features->nth(i)->add_env(env);
    }
}

void class__class::type_check(type_env& env) {
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        features->nth(i)->type_check(env);
    }
}

Feature class__class::find_method(Symbol m_name) {
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        if (f->is_method() && f->get_name() == m_name) {
            return f;
        }
    }
    return NULL;
}

/*
    feature methods
*/

bool method_class::is_method() { return true; }
bool attr_class::is_method() { return false; }

Symbol method_class::get_type() { return return_type; }
Symbol attr_class::get_type() { return type_decl; }

Formals method_class::get_formals() { return formals; }
Formals attr_class::get_formals() { return NULL; }

void method_class::add_env(type_env& env) { }

void attr_class::add_env(type_env& env) {
    if (env.obj_env->probe(name) == NULL) {
        env.obj_env->addid(name, &type_decl);
    } else if (name == self) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "'self' cannot be the name of an attribute.\n";
    } else {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Duplicate attribute " << name << ".\n";
    }
}

/*
    check inherit method:
    1) return type,
    2) formal number,
    3) formal type
*/
void method_class::check_inherit_method(Feature ancestor_method, type_env& env) {
    if (return_type != ancestor_method->get_type()) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "In redefined method " << name << ", return type " << return_type << " is different from original return type " << ancestor_method->get_type() << ".\n";
        return;
    }
    Formals ancestor_method_formals = ancestor_method->get_formals();
    if (formals->len() != ancestor_method_formals->len()) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Incompatible number of formal parameters in redefined method " << name << ".\n";
        return;
    }
    for (int i = formals->first(), j = ancestor_method_formals->first(); 
         formals->more(i) && ancestor_method_formals->more(j);
         i = formals->next(i), j = ancestor_method_formals->next(j)) {
        Symbol type1 = formals->nth(i)->get_type();
        Symbol type2 = ancestor_method_formals->nth(j)->get_type();
        if (type1 != type2) {
            env.ct->semant_error(env.curr_class->get_filename(), this) << "In redefined method " << name << ", parameter type " << type1 << " is different from original type " << type2 << ".\n";
            return;
        }
    }
}

void method_class::check_dispatch_method_signature(Expressions actual, type_env& env) {
    if (actual->len() != formals->len()) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Method " << name << " called with wrong number of arguments.\n";
    } else {
        for (int i = actual->first(), j = formals->first();
            actual->more(i) && formals->more(j);
            i = actual->next(i), j = formals->next(j)) {
            Symbol expr_type = actual->nth(i)->type_check(env);
            Symbol defined_type = formals->nth(j)->get_type();
            if (expr_type == SELF_TYPE) expr_type = *env.obj_env->lookup(self);
            if (expr_type != defined_type && !env.ct->is_ancestor(expr_type, defined_type)) {
                env.ct->semant_error(env.curr_class->get_filename(), this) << "In call of method " << name << ", type " << expr_type << " of parameter " << formals->nth(i)->get_name() << " does not conform to declared type " << defined_type << ".\n";
            }
        }
    }
}

/* Shouldn't be dispatched. */
void attr_class::check_dispatch_method_signature(Expressions actual, type_env& env) {}


/*
    type check invalid when:
    1) return type is undefined,
    2) invalid inherits method,
    3) return type is the subtype of defined return type,
    4) SELF_TYPE return type
*/
void method_class::type_check(type_env& env) {
    env.obj_env->enterscope();
    Symbol curr_class_name = env.curr_class->get_name();
    env.obj_env->addid(self, &curr_class_name);

    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {        
        formals->nth(i)->type_check(env);
    }

    if (return_type != SELF_TYPE && env.ct->get_class_by_name(return_type) == NULL) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Undefined return type " << return_type << " in method " << name << ".\n";
    }

    Feature ancestor_method = env.ct->get_ancestor_method(env.curr_class->get_name(), name);
    if (ancestor_method != NULL) {
        this->check_inherit_method(ancestor_method, env);
    }

    Symbol expr_type = expr->type_check(env);
    if (return_type != SELF_TYPE && expr_type == SELF_TYPE) {
        expr_type = *env.obj_env->lookup(self);
    }
    if (expr_type != return_type && !env.ct->is_ancestor(expr_type, return_type)) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Inferred return type " << expr_type << " of method get does not conform to declared return type " << return_type << ".\n";
    }
    

    env.obj_env->exitscope();
}

/*
    type check invalide when:
    1) undefined attr type,
    2) attr name is self,
    3) expression type is not the subtype of the defined type,

*/
void attr_class::type_check(type_env& env) {
    env.obj_env->enterscope();
    Symbol curr_class_name = env.curr_class->get_name();
    env.obj_env->addid(self, &curr_class_name);

    if (env.ct->get_class_by_name(type_decl) == NULL) {
       env.ct->semant_error(env.curr_class->get_filename(), this) << "Class " << type_decl << " of attribute " << name << " is undefined.\n";
    } else if (name == self) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "'self' cannot be the name of an attribute.\n";
    }

    Symbol expr_type = init->type_check(env);
    if (expr_type == SELF_TYPE) {
        expr_type = *env.obj_env->lookup(self);
    }
    if (expr_type != NULL && expr_type != type_decl && !env.ct->is_ancestor(expr_type, type_decl)) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Inferred type " << expr_type << " of initialization of attribute a does not conform to declared type " << type_decl << ".\n";
    }

    env.obj_env->exitscope();
}

/*
    formal methods
    type check invalid when:
    1) formal redefined,
    2) formal type is SELF_TYPE,
    3) formal name is self,
    4) formal type is undefined
*/
void formal_class::type_check(type_env& env) {
    if (env.obj_env->probe(name) != NULL) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Formal parameter " << name << " is multiply defined.\n";
    } else if (type_decl == SELF_TYPE) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Formal parameter " << name << " cannot have type SELF_TYPE\n";
    } else if (name == self) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "'self' cannot be the name of a formal parameter.\n";
    } else if (env.ct->get_class_by_name(type_decl) == NULL) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Class " << type_decl << " of formal parameter " << name << " is undefined.\n";
    } else {
        env.obj_env->addid(name, &type_decl);
    }
}


/*
    branch methods
*/
Symbol branch_class::type_check(type_env& env) {
    if (env.obj_env->probe(name) != NULL) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Case parameter" << name << " is multiply defined.\n";
    }
    if (type_decl == SELF_TYPE) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Identifier " << name << " declared with type SELF_TYPE in case branch.\n";
        env.obj_env->addid(name, &Object);
    } else {
        env.obj_env->addid(name, &type_decl);
    }
    return expr->type_check(env);
}


/*
    expression methods
*/
Symbol assign_class::type_check(type_env& env) {
    Symbol* type_p = env.obj_env->lookup(name);
    Symbol type_;
    if (type_p == NULL) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Assignment to undeclared variable " << name <<".\n";
        type_ = Object;
    } else if (name == self) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Cannot assign to 'self'.\n";
        type_ = Object;
    } else {
        type_ = *type_p;
    }

    Symbol expr_type = expr->type_check(env);
    if (expr_type == SELF_TYPE) {
        expr_type = *env.obj_env->lookup(self);
    }
    if (type_p != NULL && expr_type != *type_p && !env.ct->is_ancestor(expr_type, *type_p)) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Type " << expr_type << " of assigned expression does not conform to declared type " << *type_p << " of identifier " << name << ".\n";
    }
    this->set_type(type_);
    return type_;
}

Symbol static_dispatch_class::type_check(type_env& env) {
    Symbol e0_type = expr->type_check(env), ret_type = Object;
    if (e0_type == SELF_TYPE) e0_type = *env.obj_env->lookup(self);
    if (e0_type == NULL) {
        // bug
        env.ct->semant_error(env.curr_class->get_filename(), this) << "static dispatch must have expression!\n";
    } else if (env.ct->get_class_by_name(type_name) == NULL) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Static dispatch to undefined class " << type_name << ".\n";
    } else if (e0_type != type_name && !env.ct->is_ancestor(e0_type, type_name)) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Expression type " << e0_type << " does not conform to declared static dispatch type " << type_name << ".\n";
    } else {
        Feature defined_method = env.ct->get_dispatch_method(type_name, name);
        if (defined_method != NULL) {
            defined_method->check_dispatch_method_signature(actual, env);
            ret_type = defined_method->get_type();
            if (ret_type == SELF_TYPE && e0_type != env.curr_class->get_name()) {
                ret_type = e0_type;
            }
        } else {
            env.ct->semant_error(env.curr_class->get_filename(), this) << "Static dispatch to undefined method " << name << ".\n";
        }
    }

    this->set_type(ret_type);
    return ret_type;
}

/*
    1. check if class e0 contain method f
    2. if method f is defined, check signature
    
*/
Symbol dispatch_class::type_check(type_env& env) {
    Symbol e0_type = expr->type_check(env), ret_type = Object;
    if (e0_type == NULL || e0_type == SELF_TYPE) e0_type = *env.obj_env->lookup(self);

    // check 1
    Feature defined_method = env.ct->get_dispatch_method(e0_type, name);
    if (defined_method != NULL) {
        // check 2
        defined_method->check_dispatch_method_signature(actual, env);
        ret_type = defined_method->get_type();
        if (ret_type == SELF_TYPE && e0_type != env.curr_class->get_name()) {
            ret_type = e0_type;
        }
    } else {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Dispatch to undefined method " << name << ".\n";
    }

    this->set_type(ret_type);
     
    return ret_type;
}

Symbol cond_class::type_check(type_env& env) {
    if (pred->type_check(env) != Bool) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Predicate of 'if' does not have type Bool.\n";
    }
    Symbol type1 = then_exp->type_check(env);
    Symbol type2 = else_exp->type_check(env);
    if (type1 == SELF_TYPE) type1 = *env.obj_env->lookup(self);
    if (type2 == SELF_TYPE) type2 = *env.obj_env->lookup(self);
    Symbol common_ancestor = env.ct->get_lowest_common_ancestor(type1, type2);
    this->set_type(common_ancestor);

    return common_ancestor;
}

Symbol loop_class::type_check(type_env& env) {
    Symbol pred_type = pred->type_check(env);
    body->type_check(env);

    if (pred_type != Bool) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Loop condition does not have type Bool.\n";
    }
    this->set_type(Object);
    return Object;
}

Symbol typcase_class::type_check(type_env& env) {
    expr->type_check(env);

    map<Symbol, bool> case_type_exist;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        Symbol case_type = cases->nth(i)->get_type();
        if (case_type_exist[case_type]) {
            env.ct->semant_error(env.curr_class->get_filename(), this) << "Duplicate branch " << case_type << " in case statement.\n";
        } else {
            case_type_exist[case_type] = true;
        }
    }

    Symbol cases_type;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        env.obj_env->enterscope();
        Symbol case_type = cases->nth(i)->type_check(env);
        cases_type = i == cases->first() ? case_type : env.ct->get_lowest_common_ancestor(cases_type, case_type);
        env.obj_env->exitscope();
    }

    this->set_type(cases_type);

    return cases_type;
}

Symbol block_class::type_check(type_env& env) {
    Symbol type;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        type = body->nth(i)->type_check(env);
    }
    this->set_type(type);
    return type;
}

Symbol let_class::type_check(type_env& env) {
    Symbol type_decl_ = type_decl == SELF_TYPE ? *env.obj_env->lookup(self) : type_decl;
    Symbol init_type = init->type_check(env);
    if (init_type == SELF_TYPE) {
        init_type = *env.obj_env->lookup(self);
    }
    if (init_type != NULL && init_type != type_decl_ && !env.ct->is_ancestor(init_type, type_decl)) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Inferred type " << init_type << " of initialization of " << identifier << " does not conform to identifier's declared type " << type_decl << ".\n";
    }

    env.obj_env->enterscope();
    if (identifier == self) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "'self' cannot be bound in a 'let' expression.\n";
    } else {
        env.obj_env->addid(identifier, &type_decl);
    }
    Symbol body_type = body->type_check(env);
    env.obj_env->exitscope();
    this->set_type(body_type);
    return body_type;
}

Symbol plus_class::type_check(type_env& env) {
    Symbol type1 = this->e1->type_check(env);
    Symbol type2 = this->e2->type_check(env);

    if (type1 != Int || type2 != Int) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "non-Int arguments: " << type1 << " + " << type2 << '\n';
    }
    this->set_type(Int);
    return Int;
}

Symbol sub_class::type_check(type_env& env) {
    Symbol type1 = this->e1->type_check(env);
    Symbol type2 = this->e2->type_check(env);

    if (type1 != Int || type2 != Int) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "non-Int arguments: " << type1 << " - " << type2 << '\n';
    }
    this->set_type(Int);
    return Int;
}

Symbol mul_class::type_check(type_env& env) {
    Symbol type1 = this->e1->type_check(env);
    Symbol type2 = this->e2->type_check(env);

    if (type1 != Int || type2 != Int) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "non-Int arguments: " << type1 << " * " << type2 << '\n';
    }
    this->set_type(Int);
    return Int;
}

Symbol divide_class::type_check(type_env& env) {
    Symbol type1 = this->e1->type_check(env);
    Symbol type2 = this->e2->type_check(env);
    if (type1 != Int || type2 != Int) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "non-Int arguments: " << type1 << " / " << type2 << '\n';
        // return Object;
    }
    this->set_type(Int);
    return Int;
}

Symbol neg_class::type_check(type_env& env) {
    Symbol type = this->e1->type_check(env);
    if (type != Int) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Argument of '~' has type " << type << " instead of Int.\n";
        // return Object;
    }
    this->set_type(Int);
    return Int;
}

Symbol lt_class::type_check(type_env& env) {
    Symbol type1 = this->e1->type_check(env);
    Symbol type2 = this->e2->type_check(env);
    if (type1 != Int || type2 != Int) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "non-Int arguments: " << type1 << " < " << type2 << '\n';
        // return Object;
    }
    this->set_type(Bool);
    return Bool;
}

Symbol eq_class::type_check(type_env& env) {
    Symbol type1 = this->e1->type_check(env);
    Symbol type2 = this->e2->type_check(env);

    if ((type1 == Int || type2 == Int || type1 == Bool || type2 == Bool || type1 == Str || type2 == Str) 
        && type1 != type2) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Illegal comparison with a basic type.\n";
    }
    this->set_type(Bool);
    return Bool;
}

Symbol leq_class::type_check(type_env& env) {
    Symbol type1 = this->e1->type_check(env);
    Symbol type2 = this->e2->type_check(env);
    if (type1 != Int || type2 != Int) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "non-Int arguments: " << type1 << " <= " << type2 << '\n';
        // return Object;
    }
    this->set_type(Bool);
    return Bool;
}

Symbol comp_class::type_check(type_env& env) {
    Symbol type = this->e1->type_check(env);
    if (type != Bool) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "Argument of 'not' has type " << type << " instead of Bool.\n";
        // return Object;
    }
    this->set_type(Bool);
    return Bool;
}

Symbol int_const_class::type_check(type_env& env) {
    this->set_type(Int);
    return Int;
}

Symbol bool_const_class::type_check(type_env& env) {
    this->set_type(Bool);
    return Bool;
}

Symbol string_const_class::type_check(type_env& env) {
    this->set_type(Str);
    return Str;
}

Symbol new__class::type_check(type_env& env) {
    Symbol type_ = type_name;
    if (type_name != SELF_TYPE && env.ct->get_class_by_name(type_name) == NULL) {
        env.ct->semant_error(env.curr_class->get_filename(), this) << "'new' used with undefined class " << type_name << ".\n";
        type_ = Object;
    }
    this->set_type(type_);
    return type_;
}

Symbol isvoid_class::type_check(type_env& env) {
    e1->type_check(env);
    this->set_type(Bool);
    return Bool;
}

Symbol no_expr_class::type_check(type_env& env) {
    return NULL;
}

Symbol object_class::type_check(type_env& env) {
    Symbol ret_type;
    Symbol* type_p = env.obj_env->lookup(name);
    if (name == self) {
        ret_type = SELF_TYPE;
    } else if (type_p == NULL) {
       env.ct->semant_error(env.curr_class->get_filename(), this) << "Undeclared identifier " << name <<".\n";
       ret_type = Object;
    } else {
        ret_type = *type_p;
    }
    this->set_type(ret_type);
    return ret_type;
}