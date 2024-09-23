#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>


/* todo
- print:
  - program
  - structs
  - functions
- expressions:
  - if
  - loops
  - variables
  - assignment
  - block
  - operators
- types:
  - str
- check
- get_type
- literals (Type*, uintptr_t ?)
- varargs
*/
// typedefs

typedef  uint8_t  u8;
typedef   int8_t  i8;
typedef uint16_t u16;
typedef  int16_t i16;
typedef uint32_t u32;
typedef  int32_t i32;
typedef uint64_t u64;
typedef  int64_t i64;
typedef    float f32;
typedef   double f64;

typedef struct BaseType BaseType;
typedef struct StructField StructField;
typedef struct StructType StructType;
typedef struct Type Type;

typedef struct Variable Variable;
typedef struct Function Function;

typedef struct Expression         Expression;
typedef struct LiteralExpression  LiteralExpression;
typedef struct CallExpression     CallExpression;
typedef struct BlockExpression    BlockExpression;
typedef struct VariableExpression VariableExpression;
typedef struct IfExpression       IfExpression;
typedef struct ElseIfExpression   ElseIfExpression;
typedef struct ElseExpression     ElseExpression;

typedef struct Program Program;


// Type/Value

struct BaseType {
  size_t byte_size;
};

struct StructField {
  const char *name;
  Type *type;
};

struct StructType {
  StructField *fields;
  size_t fields_len;
};

enum TypeKind {
  T_BASE, T_STRUCT
};

struct Type {
  enum TypeKind kind;
  const char *name;

  union {
    BaseType b;
    StructType s;
  } t;
};


// Functions

#define PARAMETERS_MAX_LEN 16

struct Variable {
  const char *name;
  Type *type;
};

enum FunctionKind {
  F_BUILTIN, F_EXTERN, F_USER_DEFINED
};

struct Function {
  enum FunctionKind kind;
  const char *name;
  
  Type *return_type;
  Variable parameters[PARAMETERS_MAX_LEN];
  size_t parameters_len;
  
  Expression *body;
};


// Expressions

enum LiteralKind {
  L_I, L_U, L_F, L_S, L_B, L_C
};

struct LiteralExpression {
  enum LiteralKind kind;

  union {
    i64 i;
    u64 u;
    f64 f;
    const char *s;
    bool b;
    char c;
  } l;
};

struct CallExpression {
  Function *function;
  Expression *arguments;
  size_t arguments_len;
};
struct BlockExpression {
  Expression *expressions;
  size_t expressions_len;
};
struct VariableExpression {
  const char *name;
};
struct IfExpression {
  Expression *condition;
  Expression *body;
};
struct ElseIfExpression {
  Expression *condition;
  Expression *body;
};
struct ElseExpression {
  Expression *body;
};

enum ExpressionKind {
  E_LITERAL, E_CALL, E_BLOCK, E_VARIABLE, E_IF, E_ELSEIF, E_ELSE
};

struct Expression {
  enum ExpressionKind kind;

  union {
    LiteralExpression l;
    CallExpression c;
    BlockExpression b;
    VariableExpression v;
    IfExpression i;
    ElseIfExpression ei;
    ElseExpression e;
  } e;
};


// Program

struct Program {
  Type *types;
  size_t types_len;
  Function *functions;
  size_t functions_len;
};

Type *program_add_type(Program *p, Type t) {
  p->types[p->types_len] = t;
  p->types_len++;
  return &p->types[p->types_len-1];
}

Function *program_add_function(Program *p, Function f) {
  p->functions[p->functions_len] = f;
  p->functions_len++;
  return &p->functions[p->functions_len-1];
}

Type *program_find_type(Program *p, const char *name) {
  for (size_t i = 0; i < p->types_len; i++) {
    Type *t = &p->types[i];
    
    size_t j = 0;
    bool match_name = true;
    while (name[j] != 0 && t->name[j] != 0) {
      if (name[j] != t->name[j]) {
        match_name = false;
        break;
      }
      j++;
    }
    if (!match_name)
      continue;
    
    if (name[j] == 0 && t->name[j] == 0)
      return t;
  }
  return NULL;
}

Function *program_find_function(Program *p, const char *name, Type **parameter_types, size_t parameter_types_len) {
  for (size_t i = 0; i < p->functions_len; i++) {
    Function *f = &p->functions[i];

    if (f->parameters_len != parameter_types_len)
      continue;
    bool match_parameters = true;
    for (size_t j = 0; j < parameter_types_len; j++) {
      if (f->parameters[j].type != parameter_types[j]) {
        match_parameters = false;
        break;
      }
    }
    if (!match_parameters)
      continue;
    
    size_t j = 0;
    bool match_name = true;
    while (name[j] != 0 && f->name[j] != 0) {
      if (name[j] != f->name[j]) {
        match_name = false;
        break;
      }
      j++;
    }
    if (!match_name)
      continue;
    
    if (name[j] == 0 && f->name[j] == 0)
      return f;
  }
  return NULL;
}


// BASE functions/types

#define BASE_TYPES_LEN 14
static Type *base_types[BASE_TYPES_LEN];

void init_base_types(Program *p) {
  base_types[0]  = program_add_type(p, (Type){T_BASE, "u8", .t.b={1}});
  base_types[1]  = program_add_type(p, (Type){T_BASE, "i8", .t.b={1}});
  base_types[2]  = program_add_type(p, (Type){T_BASE, "u16", .t.b={2}});
  base_types[3]  = program_add_type(p, (Type){T_BASE, "i16", .t.b={2}});
  base_types[4]  = program_add_type(p, (Type){T_BASE, "u32", .t.b={4}});
  base_types[5]  = program_add_type(p, (Type){T_BASE, "i32", .t.b={4}});
  base_types[6]  = program_add_type(p, (Type){T_BASE, "u64", .t.b={8}});
  base_types[7]  = program_add_type(p, (Type){T_BASE, "i64", .t.b={8}});
  base_types[8]  = program_add_type(p, (Type){T_BASE, "f32", .t.b={4}});
  base_types[9]  = program_add_type(p, (Type){T_BASE, "f64", .t.b={8}});
  base_types[10] = program_add_type(p, (Type){T_BASE, "bool", .t.b={1}});
  base_types[11] = program_add_type(p, (Type){T_BASE, "void", .t.b={0}});
  base_types[12] = program_add_type(p, (Type){T_BASE, "str", .t.b={8}});
  base_types[13] = program_add_type(p, (Type){T_BASE, "...", .t.b={0}});

  Function new_f = {
    .kind           = F_BUILTIN,
    .parameters[0]  = (Variable){"a", NULL},
    .parameters[1]  = (Variable){"b", NULL},
    .parameters_len = 2,
  };
  
  // integer types
  for (size_t i = 0; i <= 7; i++) {
    Type *t = base_types[i];
    const char *ops[] = {"+","-","*","/","%","==","!=","<",">"};
    for (size_t j = 0; j < sizeof(ops)/sizeof(*ops); j++) {
      new_f.name = ops[j];
      new_f.return_type = t;
      new_f.parameters[0].type = t;
      new_f.parameters[1].type = t;
      program_add_function(p, new_f);
    }
  }

  // float types
  for (size_t i = 8; i <= 9; i++) {
    Type *t = base_types[i];
    const char *ops[] = {"+","-","*","/","==","!=","<",">"};
    for (size_t j = 0; j < sizeof(ops)/sizeof(*ops); j++) {
      new_f.name = ops[j];
      new_f.return_type = t;
      new_f.parameters[0].type = t;
      new_f.parameters[1].type = t;
      program_add_function(p, new_f);
    }
  }

  // bool
  {
    Type *t = base_types[10];
    const char *ops[] = {"&&","||","==","!="};
    for (size_t j = 0; j < sizeof(ops)/sizeof(*ops); j++) {
      new_f.name = ops[j];
      new_f.return_type = t;
      new_f.parameters[0].type = t;
      new_f.parameters[1].type = t;
      program_add_function(p, new_f);
    }
  }

  new_f.kind = F_EXTERN;
  new_f.name = "printf";
  new_f.return_type = base_types[11];
  new_f.parameters[0] = (Variable){"fmt",base_types[12]};
  new_f.parameters[1] = (Variable){"args",base_types[13]};
  program_add_function(p, new_f);

}


// get_type

Type *get_type(Program *p, Expression *expr) {
  switch (expr->kind) {
    case E_LITERAL: {
      switch (expr->e.l.kind) {
        case L_I: return program_find_type(p, "i64");
        case L_U: return program_find_type(p, "u64");
        case L_F: return program_find_type(p, "f64");
        case L_S: return program_find_type(p, "str");
        case L_B: return program_find_type(p, "bool");
        case L_C: return program_find_type(p, "i8");
        default: return NULL;
      }
    }
  
    case E_CALL: {
      return expr->e.c.function->return_type;
    }
  
    default: return NULL;
  }
}


// check

void logi(const char *, ...);
void logw(const char *, ...);
void loge(const char *, ...);

bool check(Program *p, Expression *expr) {
  switch (expr->kind) {
    case E_LITERAL: {
      return true;
    }
  
    case E_CALL: {
      CallExpression c = expr->e.c;
      if (c.function->kind == F_USER_DEFINED) {
        Type *body_type = get_type(p, c.function->body);
        if (body_type != c.function->return_type) {
          loge("Function body doesn't match return type (%s/%s)",
               body_type->name, c.function->return_type->name);
          return false;
        }
      }
      if (c.arguments_len != c.function->parameters_len) {
        loge("Number of arguments doesn't match function signature(%d/%d)",
             c.arguments_len, c.function->parameters_len);
        return false;
      }
      for (size_t i = 0; i < c.arguments_len; i++) {
        Type *arg_type = get_type(p, &c.arguments[i]);
        if (arg_type != c.function->parameters[i].type) {
          loge("Argument type doesn't match function signature(%s/%s)",
               arg_type->name, c.function->parameters[i].type->name);
          return false;
        }
      }
      return true;
    }

    default: return false;
  }
}


// c output

void echo(const char *, ...);

void to_c_program(Program *p);
void to_c_function(Program *p, Function *f);
void to_c_function_decl(Program *p, Function *f);
void to_c_call_builtin(Program *p, CallExpression *call);
void to_c_call(Program *p, CallExpression *call);
void to_c(Program *p, Expression *expr);


void to_c_program(Program *p) {
  for (size_t i = 0; i < p->functions_len; i++) {
    to_c_function_decl(p, &p->functions[i]);
  }
  for (size_t i = 0; i < p->functions_len; i++) {
    to_c_function(p, &p->functions[i]);
  }
}

void to_c_function(Program *p, Function *f) {
  if (f->kind == F_BUILTIN || f->kind == F_EXTERN)
    return;

  echo("%s %s(", f->return_type->name, f->name);
  for (size_t i = 0; i < f->parameters_len; i++) {
    if (i != 0) echo(", ");
    echo("%s %s", f->parameters[i].type->name, f->parameters[i].name);
  }
  echo(") {\n");
  to_c(p, f->body);
  echo("}\n");
}

void to_c_function_decl(Program *p, Function *f) {
  if (f->kind == F_BUILTIN)
    return;

  echo("%s %s(", f->return_type->name, f->name);
  for (size_t i = 0; i < f->parameters_len; i++) {
    if (i != 0) echo(", ");
    echo("%s %s", f->parameters[i].type->name, f->parameters[i].name);
  }
  echo(");\n");
}

void to_c_call_builtin(Program *p, CallExpression *call) {
  echo("(");
  to_c(p, &call->arguments[0]);
  echo(" %s ", call->function->name);
  to_c(p, &call->arguments[1]);
  echo(")");
  return;
}

void to_c_call(Program *p, CallExpression *call) {
  echo("%s(", call->function->name);
  for (size_t i = 0; i < call->arguments_len; i++) {
    if (i != 0)
      echo(", ");
    to_c(p, &call->arguments[i]);
  }
  echo(")");
}

void to_c(Program *p, Expression *expr) {
  switch (expr->kind) {
    case E_LITERAL: {
        switch (expr->e.l.kind) {
          case L_I: echo("%ld", expr->e.l.l.i); break;
          case L_U: echo("%lu", expr->e.l.l.u); break;
          case L_F: echo("%lf", expr->e.l.l.f); break;
          case L_S: echo("\"%s\"", expr->e.l.l.s); break;
          case L_B: echo("%b", expr->e.l.l.b); break;
          case L_C: echo("'%c'", expr->e.l.l.c); break;
        }
        break;
    }
    case E_CALL: {
      CallExpression *c = &expr->e.c;
      switch (c->function->kind) {
        case F_BUILTIN:
          to_c_call_builtin(p, c);
          break;
        case F_EXTERN:
        case F_USER_DEFINED:
          to_c_call(p, c);
          break;
        }
        break;
    }
    case E_BLOCK: {
        BlockExpression *b = &expr->e.b;
        echo("{\n");
        for (size_t i = 0; i < b->expressions_len; i++) {
          to_c(p, &b->expressions[i]);
          echo("\n");
        }
        echo("}\n");
        break;
    }
    case E_VARIABLE: {
        VariableExpression *v = &expr->e.v;
        echo("%s", v->name);
        break;
    }
    case E_IF: {
        IfExpression *i = &expr->e.i;
        echo("if (");
        to_c(p, i->condition);
        echo(")\n");
        to_c(p, i->body);
        break;
    }
    case E_ELSEIF: {
        ElseIfExpression *ei = &expr->e.ei;
        echo("else if (");
        to_c(p, ei->condition);
        echo(")\n");
        to_c(p, ei->body);
        break;
    }
    case E_ELSE: {
        ElseExpression *e = &expr->e.e;
        echo("else\n");
        to_c(p, e->body);
        break;
    }
  }
}


// main

#include <stdio.h>
#include <stdarg.h>

void logi(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  fprintf(stderr, "[INFO] ");
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  va_end(args);
}
void logw(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  fprintf(stderr, "[WARN] ");
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  va_end(args);
}
void loge(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  fprintf(stderr, "[ERR]  ");
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  va_end(args);
}
void echo(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vfprintf(stdout, fmt, args);
  va_end(args);
}


#define NUMEXPRS(...) (sizeof((Expression[]){__VA_ARGS__})/sizeof(Expression))
#define CALL(f, ...) (Expression){.kind = E_CALL,.e.c = (CallExpression){.function=f, .arguments=(Expression[]){__VA_ARGS__},.arguments_len=NUMEXPRS(__VA_ARGS__)}}
#define LIT_I(v) (Expression){.kind=E_LITERAL,.e.l=(LiteralExpression){.kind=L_I,.l.i=v}}
#define LIT_S(v) (Expression){.kind=E_LITERAL,.e.l=(LiteralExpression){.kind=L_S,.l.s=v}}
#define VAR(n) (Expression){.kind=E_VARIABLE,.e.v=(VariableExpression){n}}
#define BLOCK(e0, ...) (Expression){.kind=E_BLOCK, .e.b=(BlockExpression){(Expression[]){e0, __VA_ARGS__},NUMEXPRS(e0, __VA_ARGS__)}}
#define FUNC1(n, t, n0, t0, e) (Function){F_USER_DEFINED,n,t,.parameters[0]=(Variable){n0,t0},.parameters_len=1,e}
#define IF(c, b) (Expression){.kind=E_IF, .e.i=(IfExpression){c,b}}
#define ELSEIF(c, b) (Expression){.kind=E_ELSEIF, .e.ei=(ElseIfExpression){c,b}}
#define ELSE(b) (Expression){.kind=E_ELSE, .e.e=(ElseExpression){b}}

int main() {
  static Type types_buffer[1024] = {0};
  static Function functions_buffer[1024] = {0};
  Program p = {
    types_buffer, 0,
    functions_buffer, 0
  };
  init_base_types(&p);

  Type *t_i64 = program_find_type(&p, "i64");
  Type *t_str = program_find_type(&p, "str");
  Type *t_variadic = program_find_type(&p, "...");
  Type *t_bool = program_find_type(&p, "bool");
  Type *t_void = program_find_type(&p, "void");
  Function *f_i64_add = program_find_function(&p, "+", (Type*[]){t_i64, t_i64}, 2);
  Function *f_i64_eql = program_find_function(&p, "==", (Type*[]){t_i64, t_i64}, 2);
  Function *f_i64_mod = program_find_function(&p, "%", (Type*[]){t_i64, t_i64}, 2);
  Function *f_bool_and = program_find_function(&p, "&&", (Type*[]){t_bool, t_bool}, 2);
  Function *f_printf = program_find_function(&p, "printf", (Type*[]){t_str, t_variadic}, 2);
  
  Expression e =
    CALL(f_i64_add,
      LIT_I(1),
      CALL(f_i64_add,
        LIT_I(2),
        LIT_I(3)
      )
    );

  if (check(&p, &e)) {
    to_c(&p, &e);
  }  

  return 0;
}
