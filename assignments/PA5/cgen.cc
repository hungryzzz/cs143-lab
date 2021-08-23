
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

int label_num = 0;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_fetch_bool(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/
      s << STRINGNAME << DISPTAB_SUFFIX;
      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
      s << INTNAME << DISPTAB_SUFFIX;
      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
      s << BOOLNAME << DISPTAB_SUFFIX;
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_class_name_table() {
  //
  // Add class name to string constant tag, should be ordered by class tag 
  //
  str << CLASSNAMETAB << LABEL;
  for (size_t i = 0; i < classtag_vec.size(); ++i) {
    str << WORD;
    stringtable.lookup_string(classtag_vec[i]->get_name()->get_string())->code_ref(str);
    str << endl;
  }
}

void CgenClassTable::code_class_object_table() {
  //
  // Add class object: prototype object & initialization method
  //
  str << CLASSOBJTAB << LABEL;
  for (size_t i = 0; i < classtag_vec.size(); ++i) {
    Symbol class_name = classtag_vec[i]->get_name();
    str << WORD; emit_protobj_ref(class_name, str); str << endl;
    str << WORD; emit_init_ref(class_name, str); str << endl;
  }
}

void CgenClassTable::code_object_layout_table() {
  //
  // Add every object layout & dispatch method table
  //
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    // code dispatch table
    l->hd()->code_disp_table(str);
    //  code proto table
    l->hd()->code_proto_table(str);
  }
}

void CgenClassTable::code_object_init() {
  // 
  // Add every object init code
  // 
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    CgenNode *curr_node = l->hd();

    emit_init_ref(curr_node->get_name(), str); str << LABEL;
    
     // Save context
    emit_addiu(SP, SP, -12, str);
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    // update frame point
    emit_addiu(FP, SP, 4, str);
    // save current object instance
    emit_move(SELF, ACC, str);
    // init parent
    Symbol parent = curr_node->get_parent();
    if (parent != No_class) {
      str << JAL; emit_init_ref(parent, str); str << endl;
    }
    // attributes init
    curr_node->code_features(str, true, this);

    // recover context
    emit_move(ACC, SELF, str);
    emit_load(FP, 3, SP, str);
    emit_load(SELF, 2, SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, 12, str);
    
    // return
    emit_return(str);
  }
}

void CgenClassTable::code_object_methods() {
  // 
  // Add every object methods
  // 
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    l->hd()->code_features(str, false, this);
  }
}

void CgenClassTable::visit_object(CgenNodeP nd, vector<CgenNodeP>& prefix_nd, int& visit_num) {
  // 
  // DFS class tree
  // 
  ++visit_num;
  Symbol class_name = nd->get_name();

  // collect parent features info
  for (auto item: prefix_nd) {
    item->add_features_to_child(nd);
  }
  nd->set_parent_attrs_num((int)nd->attrs_including_inherits.size());
  nd->add_features_to_child(nd);

  classtag_map[class_name] = visit_num;
  classtag_vec.push_back(nd);
  nd->set_class_tag(visit_num);

  if (nd->get_children() != NULL) {
    prefix_nd.push_back(nd);
    for (List<CgenNode> *l = nd->get_children(); l; l = l->tl()) {
      visit_object(l->hd(), prefix_nd, visit_num);
    }
    prefix_nd.pop_back();
  }
  smallest_classtag[class_name] = visit_num;
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s) {

  enterscope();
  if (cgen_debug) cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();


  vector<CgenNodeP> prefix_nd;
  int visit_num = -1;
  visit_object(root(), prefix_nd, visit_num);
  // for (map<Symbol, int>::iterator it = classtag_map.begin(); it != classtag_map.end(); ++it) {
  //   cout << it->first << ' ' << it->second << ' ' << smallest_classtag[it->first] << endl;
  // }

  stringclasstag = classtag_map[Str];
  intclasstag = classtag_map[Int];
  boolclasstag = classtag_map[Bool];

  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenNode::add_features_to_child(CgenNodeP child) {
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature f = features->nth(i);
    if (f->is_method()) {
      int exist_index = child->find_method_by_name(f->get_name());
      if (exist_index != -1) {
        // if method is exist, update
        child->update_method(exist_index, name, f);
      } else {
        // add method
        child->add_method(name, f);
      }
    } else {
      // add attribute
      child->add_attr(f);
    }
  }
}

void CgenNode::code_disp_table(ostream& str) {
  emit_disptable_ref(name, str); str << LABEL;
  for (disp_method* pm: methods_including_inherits) {
    str << WORD; emit_method_ref(pm->disp_class_name, pm->disp_method->get_name(), str); str << endl;
  }
}

void CgenNode::code_proto_table(ostream& str) {
  str << WORD << "-1" << endl;
  emit_protobj_ref(name, str); str << LABEL;
  str << WORD << class_tag << endl;  // object unique index
  str << WORD << attrs_including_inherits.size() + 3 << endl;  //  object size
  str << WORD;  emit_disptable_ref(name, str); str << endl; //  object dispatch method
  // attributes default value
  for (Feature ac: attrs_including_inherits) {
    str << WORD;
    Symbol attr_type = ac->get_type();
    if (attr_type == Int) {
      inttable.lookup_string("0")->code_ref(str);
    } else if (attr_type == Bool) {
      falsebool.code_ref(str);
    } else if (attr_type == Str) {
      stringtable.lookup_string("")->code_ref(str);
    } else {
      str << "0";
    }
    str << endl;
  }
}

void CgenNode::code_features(ostream& str, bool is_attr, CgenClassTable* ct) {
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature f = features->nth(i);
    env e;
    e.curr_node = this;
    e.ct = ct;
    e.stack_num = 0;
    if (f->is_method() && !is_attr && !basic()) {
      // code method
      f->code_init(str, e);
    } else if (!f->is_method() && is_attr) {
      // code attribute
      f->code_init(str, e);
    }
  }
}


bool method_class::is_method() { return true; }
bool attr_class::is_method() { return false; }

Symbol method_class::get_type() { return return_type; }
Symbol attr_class::get_type() { return type_decl; }

void method_class::code_init(ostream& str, env& e) {
  emit_method_ref(e.curr_node->get_name(), name, str); str << LABEL;
  // save context
  emit_addiu(SP, SP, -12, str);
  emit_store(FP, 3, SP, str);
  emit_store(SELF, 2, SP, str);
  emit_store(RA, 1, SP, str);
  // update frame point
  emit_addiu(FP, SP, 4, str);
  // save current object instance
  emit_move(SELF, ACC, str);

  // add formals
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formals->nth(i)->code(str, e);
  }

  // code expr
  expr->code(str, e);

  // recover context
  emit_load(FP, 3, SP, str);
  emit_load(SELF, 2, SP, str);
  emit_load(RA, 1, SP, str);
  emit_addiu(SP, SP, 12 + formals->len() * WORD_SIZE, str);

  // return
  emit_return(str);
}

void attr_class::code_init(ostream& str, env& e) {
  if (!init->is_empty()) {
    init->code(str, e);
    int index = e.curr_node->find_attr_by_name(name);
    emit_store(ACC, DEFAULT_OBJFIELDS + index, SELF, str);
  }
}

void formal_class::code(ostream& str, env& e) {
  e.params_table.push_back(name);
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  if (cgen_debug) cout << "coding class name table" << endl;
  code_class_name_table();

  if (cgen_debug) cout << "coding class object table" << endl;
  code_class_object_table();

  if (cgen_debug) cout << "coding class dispatch and proto table" << endl;
  code_object_layout_table();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  if (cgen_debug) cout << "coding class object initializer" << endl;
  code_object_init();

  if (cgen_debug) cout << "coding class object method" << endl;
  code_object_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

///////////////////////////////////////////////////////////////////////
//
// Env help functions
//
///////////////////////////////////////////////////////////////////////

int find_param_in_env(Symbol p, env& e) {
  for (size_t i = 0; i < e.params_table.size(); ++i) {
    if (e.params_table[i] == p) {
      return e.params_table.size() - i - 1;
    }
  }
  return -1;
}

int find_attr_in_env(Symbol a, env& e) {
  return e.curr_node->find_attr_by_name(a);
}

int find_var_in_env(Symbol v, env& e) {
  if (e.vars_table.find(v) != e.vars_table.end()) {
    return e.stack_num - e.vars_table[v];
  }
  return -1;
}
//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, env& e) {
  // execute assign expression
  expr->code(s, e);
  
  // result is in ACC
  int idx;
  
  if ((idx = find_var_in_env(name, e)) != -1) {
    emit_store(ACC, idx, SP, s);
  } else if ((idx = find_param_in_env(name, e)) != -1) {
    emit_store(ACC, FRAME_PARAMS + idx, FP, s);
  } else if ((idx = find_attr_in_env(name, e)) != -1) {
    emit_store(ACC, DEFAULT_OBJFIELDS + idx, SELF, s);
  } else {
    cout << "Can't find assign variable!" << endl;
  }
  
}

void static_dispatch_class::code(ostream &s, env& e) {
  int true_label = label_num++;
  // push params
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, e);
    emit_push(ACC, s);
    ++e.stack_num;
  }

  expr->code(s, e);

  emit_bne(ACC, ZERO, true_label, s);
  emit_partial_load_address(ACC, s); stringtable.lookup_string(e.curr_node->get_filename()->get_string())->code_ref(s); s << endl;
  emit_load_imm(T1, this->get_line_number(), s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(true_label, s);

  emit_partial_load_address(T1, s); emit_disptable_ref(type_name, s); s << endl;
  int idx = e.ct->get_class_node(type_name)->find_method_by_name(name);
  if (idx != -1) {
    emit_load(T1, idx, T1, s);
    emit_jalr(T1, s);
  }
  e.stack_num -= actual->len();
}

void dispatch_class::code(ostream &s, env& e) {
  int true_label = label_num++;
  // push params
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, e);
    emit_push(ACC, s);
    ++e.stack_num;
  }

  expr->code(s, e);

  emit_bne(ACC, ZERO, true_label, s);
  emit_partial_load_address(ACC, s); stringtable.lookup_string(e.curr_node->get_filename()->get_string())->code_ref(s); s << endl;
  emit_load_imm(T1, this->get_line_number(), s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(true_label, s);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);

  CgenNodeP c_n = e.curr_node;
  if (expr->get_type() != SELF_TYPE) {
    c_n = e.ct->get_class_node(expr->get_type());
  }
  int idx = c_n->find_method_by_name(name);
  if (idx != -1) {
    emit_load(T1, idx, T1, s);
    emit_jalr(T1, s);
  }
  e.stack_num -= actual->len();
}

void cond_class::code(ostream &s, env& e) {
  int ret_label = label_num++;
  int false_branch_label = label_num++;
  pred->code(s, e);

  // true branch
  emit_fetch_bool(T1, ACC, s);
  emit_beqz(T1, false_branch_label, s);
  then_exp->code(s, e);
  emit_branch(ret_label, s);

  // false branch
  emit_label_def(false_branch_label, s);
  else_exp->code(s, e);

  // return
  emit_label_def(ret_label, s);
}

void loop_class::code(ostream &s, env& e) {
  // branch condition
  int condition_label = label_num++;
  int false_branch_label = label_num++;
  emit_label_def(condition_label, s);
  pred->code(s, e);

  // true branch
  emit_fetch_bool(T1, ACC, s);  
  emit_beqz(T1, false_branch_label, s);
  body->code(s, e);
  emit_branch(condition_label, s);
  
  // false branch
  emit_label_def(false_branch_label, s);
  emit_move(ACC, ZERO, s);
}

bool cmp(const pair<Case, int>& p1, const pair<Case, int>& p2) {
  return p1.second > p2.second;
}

void branch_class::code(ostream &s, env& e) {
  expr->code(s, e);
}

void typcase_class::code(ostream &s, env& e) {
  expr->code(s, e);
  int ret_label = label_num++;
  int no_viod_label = label_num++;

  emit_bne(ACC, ZERO, no_viod_label, s);
  emit_partial_load_address(ACC, s); stringtable.lookup_string(e.curr_node->get_filename()->get_string())->code_ref(s); s << endl;
  emit_load_imm(T1, this->get_line_number(), s);
  emit_jal("_case_abort2", s);

  emit_label_def(no_viod_label, s);
  emit_load(T3, 0, ACC, s);

  vector<pair<Case, int>> ordered_cases;
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case c = cases->nth(i);
    ordered_cases.push_back(make_pair(c, e.ct->find_classtag(c->get_type())));
  }
  sort(ordered_cases.begin(), ordered_cases.end(), cmp);

  for (vector<pair<Case, int>>::iterator it = ordered_cases.begin(); it != ordered_cases.end(); ++it) {
    int next_case_label = label_num++;
    int low_tag = it->second;
    int high_tag = e.ct->find_smallest_classtag(it->first->get_type());

    emit_blti(T3, low_tag, next_case_label, s);
    emit_bgti(T3, high_tag, next_case_label, s);
    emit_push(ACC, s);
    e.vars_table[it->first->get_name()] = e.stack_num++;

    it->first->code(s, e);

    emit_addiu(SP, SP, 4, s);
    e.vars_table.erase(it->first->get_name());
    --e.stack_num;

    emit_branch(ret_label, s);
    emit_label_def(next_case_label, s);
  }
  emit_jal("_case_abort", s);

  emit_label_def(ret_label, s);
}

void block_class::code(ostream &s, env& e) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(s, e);
  }
}

void let_class::code(ostream &s, env& e) {
  if (init->is_empty()) {
    if (type_decl == Int) {
      emit_load_int(ACC, inttable.lookup_string("0"), s);
    } else if (type_decl == Bool) {
      emit_load_bool(ACC, falsebool, s);
    } else if (type_decl == Str) {
      emit_load_string(ACC, stringtable.lookup_string(""), s);
    } else {
      emit_move(ACC, ZERO, s);
    }
  } else {
    init->code(s, e);
  }
  
  emit_push(ACC, s);
  e.vars_table[identifier] = e.stack_num++;

  body->code(s, e);

  emit_addiu(SP, SP, 4, s);
  // delete var
  e.vars_table.erase(identifier);
  --e.stack_num;
}

void plus_class::code(ostream &s, env& e) {
  // fetch expr1, push stack
  e1->code(s, e);
  emit_push(ACC, s);
  ++e.stack_num;
  // fetch expr2, copy, get int value
  e2->code(s, e);
  emit_jal(NEW_OBJECT, s);
  emit_fetch_int(T2, ACC, s);
  // pop expr1 from stack, get int value
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  --e.stack_num;
  emit_fetch_int(T1, T1, s);
  // add, store to acc
  emit_add(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void sub_class::code(ostream &s, env& e) {
  // fetch expr1, push stack
  e1->code(s, e);
  emit_push(ACC, s);
  ++e.stack_num;
  // fetch expr2, copy, get int value
  e2->code(s, e);
  emit_jal(NEW_OBJECT, s);
  emit_fetch_int(T2, ACC, s);
  // pop expr1 from stack, get int value
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  --e.stack_num;
  emit_fetch_int(T1, T1, s);
  // sub, store to acc
  emit_sub(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void mul_class::code(ostream &s, env& e) {
  // fetch expr1, push stack
  e1->code(s, e);
  emit_push(ACC, s);
  ++e.stack_num;
  // fetch expr2, copy, get int value
  e2->code(s, e);
  emit_jal(NEW_OBJECT, s);
  emit_fetch_int(T2, ACC, s);
  // pop expr1 from stack, get int value
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  --e.stack_num;
  emit_fetch_int(T1, T1, s);
  // mul, store to acc
  emit_mul(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void divide_class::code(ostream &s, env& e) {
  // fetch expr1, push stack
  e1->code(s, e);
  emit_push(ACC, s);
  ++e.stack_num;
  // fetch expr2, copy, get int value
  e2->code(s, e);
  emit_jal(NEW_OBJECT, s);
  emit_fetch_int(T2, ACC, s);
  // pop expr1 from stack, get int value
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  --e.stack_num;
  emit_fetch_int(T1, T1, s);
  // div, store to acc
  emit_div(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void neg_class::code(ostream &s, env& e) {
  e1->code(s, e);
  emit_jal(NEW_OBJECT, s);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);

  emit_store_int(T1, ACC, s);
}

void lt_class::code(ostream &s, env& e) {
  int true_label = label_num++;

  e1->code(s, e);
  emit_push(ACC, s);
  ++e.stack_num;

  e2->code(s, e);
  emit_fetch_int(T2, ACC, s);

  // pop expr1 from stack, get int value
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  --e.stack_num;
  emit_fetch_int(T1, T1, s);

  emit_load_bool(ACC, BoolConst(1), s);
  // if t1 is less than t2, goto label, a0 is true
  emit_blt(T1, T2, true_label, s);
  // t1 is greater than t2, a0 is false
  emit_load_bool(ACC, BoolConst(0), s);
  // label
  emit_label_def(true_label, s);
}

void eq_class::code(ostream &s, env& e) {
  int true_label = label_num++;

  e1->code(s, e);
  emit_push(ACC, s);
  ++e.stack_num;

  e2->code(s, e);
  emit_move(T2, ACC, s);

  // pop expr1 from stack, get int value
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  --e.stack_num;

  emit_load_bool(ACC, BoolConst(1), s);
  emit_beq(T1, T2, true_label, s);
  emit_load_bool(A1, BoolConst(0), s);
  emit_jal("equality_test", s);

  emit_label_def(true_label, s);
}

void leq_class::code(ostream &s, env& e) {
  int true_label = label_num++;

  e1->code(s, e);
  emit_push(ACC, s);
  ++e.stack_num;

  e2->code(s, e);
  emit_fetch_int(T2, ACC, s);

  // pop expr1 from stack, get int value
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  --e.stack_num;
  emit_fetch_int(T1, T1, s);

  emit_load_bool(ACC, BoolConst(1), s);
  // if t1 is less than t2, goto label, a0 is true
  emit_bleq(T1, T2, true_label, s);
  // t1 is greater than t2, a0 is false
  emit_load_bool(ACC, BoolConst(0), s);
  // label
  emit_label_def(true_label, s);
}

void comp_class::code(ostream &s, env& e) {
  int true_label = label_num++;
  // fetch operand
  e1->code(s, e);
  emit_fetch_bool(T1, ACC, s);

  emit_load_bool(ACC, BoolConst(1), s);
  // if operand is 0, goto label, a0 is true
  emit_beqz(T1, true_label, s);
  // operand is 1, a0 is false
  emit_load_bool(ACC, BoolConst(0), s);
  // label
  emit_label_def(true_label, s);
}

void int_const_class::code(ostream& s, env& e)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, env& e)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, env& e)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, env& e) {
  if (type_name == SELF_TYPE) {
    // todo
    emit_load_address(T1, CLASSOBJTAB, s);
    emit_load(T2, 0, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_addu(T1, T1, T2, s);

    // save T1
    emit_push(T1, s);
    ++e.stack_num;

    emit_load(ACC, 0, T1, s);
    emit_jal(NEW_OBJECT, s);

    // pop T1 from stack
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    --e.stack_num;

    emit_load(T1, 1, T1, s);
    emit_jalr(T1, s);
  } else {
    emit_partial_load_address(ACC, s); emit_protobj_ref(type_name, s); s << endl;
    emit_jal(NEW_OBJECT, s);
    s << JAL << ' '; emit_init_ref(type_name, s); s << endl;
  }
  
}

void isvoid_class::code(ostream &s, env& e) {
  int true_label = label_num++;

  e1->code(s, e);
  emit_move(T1, ACC, s);

  emit_load_bool(ACC, BoolConst(1), s);
  // if operand is void, goto label, a0 is true
  emit_beqz(T1, true_label, s);
  // operand is not void, a0 is false
  emit_load_bool(ACC, BoolConst(0), s);
  // label
  emit_label_def(true_label, s);
}

void no_expr_class::code(ostream &s, env& e) {
  emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s, env& e) {
  int idx;

  if ((idx = find_var_in_env(name, e)) != -1) {
    emit_load(ACC, idx, SP, s);
  } else if ((idx = find_param_in_env(name, e)) != -1) {
    emit_load(ACC, FRAME_PARAMS + idx, FP, s);
  } else if ((idx = find_attr_in_env(name, e)) != -1) {
    emit_load(ACC, DEFAULT_OBJFIELDS + idx, SELF, s);
  } else if (name == self) {
    emit_move(ACC, SELF, s);
  } else {
    cout << "Error: " << this->get_line_number() << " Can't find variable " << name << "!\n";
  }
}


