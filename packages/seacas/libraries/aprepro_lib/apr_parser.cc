// A Bison parser, made by GNU Bison 3.8.2.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2021 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.

// Take the name prefix into account.
#define yylex SEAMSlex

// First part of user prologue.
#line 6 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"

#include "apr_array.h"
#include "apr_util.h"
#include "aprepro.h"

#include <cerrno>
#include <cfenv>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fmt/format.h>
#include <fmt/printf.h>
#include <iostream>

namespace {
  void reset_error()
  {
#if !defined(WIN32) && !defined(__WIN32__) && !defined(_WIN32) && !defined(_MSC_VER) &&            \
    !defined(__MINGW32__) && !defined(_WIN64) && !defined(__MINGW64__)
#ifndef math_errhandling
#define math_errhandling MATH_ERRNO
#endif

    if (math_errhandling & MATH_ERREXCEPT) {
      std::feclearexcept(FE_ALL_EXCEPT);
    }
    if (math_errhandling & MATH_ERRNO) {
      errno = 0;
    }
#endif
  }
} // namespace

namespace SEAMS {
  extern bool echo;
}

#line 84 "apr_parser.cc"

#include "aprepro_parser.h"

// Second part of user prologue.
#line 112 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"

#include "apr_scanner.h"
#include "aprepro.h"

/* this "connects" the bison parser in aprepro to the flex scanner class
 * object. it defines the yylex() function call to pull the next token from the
 * current lexer object of the aprepro context. */
#undef yylex
#define yylex aprepro.lexer->lex

#line 103 "apr_parser.cc"

#ifndef YY_
#if defined YYENABLE_NLS && YYENABLE_NLS
#if ENABLE_NLS
#include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#define YY_(msgid) dgettext("bison-runtime", msgid)
#endif
#endif
#ifndef YY_
#define YY_(msgid) msgid
#endif
#endif

// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
#if defined __GNUC__ && !defined __EXCEPTIONS
#define YY_EXCEPTIONS 0
#else
#define YY_EXCEPTIONS 1
#endif
#endif

// Enable debugging if requested.
#if SEAMSDEBUG

// A pseudo ostream that takes yydebug_ into account.
#define YYCDEBUG                                                                                   \
  if (yydebug_)                                                                                    \
  (*yycdebug_)

#define YY_SYMBOL_PRINT(Title, Symbol)                                                             \
  do {                                                                                             \
    if (yydebug_) {                                                                                \
      *yycdebug_ << Title << ' ';                                                                  \
      yy_print_(*yycdebug_, Symbol);                                                               \
      *yycdebug_ << '\n';                                                                          \
    }                                                                                              \
  } while (false)

#define YY_REDUCE_PRINT(Rule)                                                                      \
  do {                                                                                             \
    if (yydebug_)                                                                                  \
      yy_reduce_print_(Rule);                                                                      \
  } while (false)

#define YY_STACK_PRINT()                                                                           \
  do {                                                                                             \
    if (yydebug_)                                                                                  \
      yy_stack_print_();                                                                           \
  } while (false)

#else // !SEAMSDEBUG

#define YYCDEBUG                                                                                   \
  if (false)                                                                                       \
  std::cerr
#define YY_SYMBOL_PRINT(Title, Symbol) YY_USE(Symbol)
#define YY_REDUCE_PRINT(Rule)          static_cast<void>(0)
#define YY_STACK_PRINT()               static_cast<void>(0)

#endif // !SEAMSDEBUG

#define yyerrok   (yyerrstatus_ = 0)
#define yyclearin (yyla.clear())

#define YYACCEPT       goto yyacceptlab
#define YYABORT        goto yyabortlab
#define YYERROR        goto yyerrorlab
#define YYRECOVERING() (!!yyerrstatus_)

namespace SEAMS {
#line 177 "apr_parser.cc"

  /// Build a parser object.
  Parser::Parser(class Aprepro &aprepro_yyarg)
#if SEAMSDEBUG
      : yydebug_(false), yycdebug_(&std::cerr),
#else
      :
#endif
        aprepro(aprepro_yyarg)
  {
  }

  Parser::~Parser() {}

  Parser::syntax_error::~syntax_error() YY_NOEXCEPT YY_NOTHROW {}

  /*---------.
  | symbol.  |
  `---------*/

  // basic_symbol.
  template <typename Base>
  Parser::basic_symbol<Base>::basic_symbol(const basic_symbol &that) : Base(that), value(that.value)
  {
  }

  /// Constructor for valueless symbols.
  template <typename Base>
  Parser::basic_symbol<Base>::basic_symbol(typename Base::kind_type t) : Base(t), value()
  {
  }

  template <typename Base>
  Parser::basic_symbol<Base>::basic_symbol(typename Base::kind_type t, YY_RVREF(value_type) v)
      : Base(t), value(YY_MOVE(v))
  {
  }

  template <typename Base>
  Parser::symbol_kind_type Parser::basic_symbol<Base>::type_get() const YY_NOEXCEPT
  {
    return this->kind();
  }

  template <typename Base> bool Parser::basic_symbol<Base>::empty() const YY_NOEXCEPT
  {
    return this->kind() == symbol_kind::S_YYEMPTY;
  }

  template <typename Base> void Parser::basic_symbol<Base>::move(basic_symbol &s)
  {
    super_type::move(s);
    value = YY_MOVE(s.value);
  }

  // by_kind.
  Parser::by_kind::by_kind() YY_NOEXCEPT : kind_(symbol_kind::S_YYEMPTY) {}

#if 201103L <= YY_CPLUSPLUS
  Parser::by_kind::by_kind(by_kind &&that) YY_NOEXCEPT : kind_(that.kind_) { that.clear(); }
#endif

  Parser::by_kind::by_kind(const by_kind &that) YY_NOEXCEPT : kind_(that.kind_) {}

  Parser::by_kind::by_kind(token_kind_type t) YY_NOEXCEPT : kind_(yytranslate_(t)) {}

  void Parser::by_kind::clear() YY_NOEXCEPT { kind_ = symbol_kind::S_YYEMPTY; }

  void Parser::by_kind::move(by_kind &that)
  {
    kind_ = that.kind_;
    that.clear();
  }

  Parser::symbol_kind_type Parser::by_kind::kind() const YY_NOEXCEPT { return kind_; }

  Parser::symbol_kind_type Parser::by_kind::type_get() const YY_NOEXCEPT { return this->kind(); }

  // by_state.
  Parser::by_state::by_state() YY_NOEXCEPT : state(empty_state) {}

  Parser::by_state::by_state(const by_state &that) YY_NOEXCEPT : state(that.state) {}

  void Parser::by_state::clear() YY_NOEXCEPT { state = empty_state; }

  void Parser::by_state::move(by_state &that)
  {
    state = that.state;
    that.clear();
  }

  Parser::by_state::by_state(state_type s) YY_NOEXCEPT : state(s) {}

  Parser::symbol_kind_type Parser::by_state::kind() const YY_NOEXCEPT
  {
    if (state == empty_state)
      return symbol_kind::S_YYEMPTY;
    else
      return YY_CAST(symbol_kind_type, yystos_[+state]);
  }

  Parser::stack_symbol_type::stack_symbol_type() {}

  Parser::stack_symbol_type::stack_symbol_type(YY_RVREF(stack_symbol_type) that)
      : super_type(YY_MOVE(that.state), YY_MOVE(that.value))
  {
#if 201103L <= YY_CPLUSPLUS
    // that is emptied.
    that.state = empty_state;
#endif
  }

  Parser::stack_symbol_type::stack_symbol_type(state_type s, YY_MOVE_REF(symbol_type) that)
      : super_type(s, YY_MOVE(that.value))
  {
    // that is emptied.
    that.kind_ = symbol_kind::S_YYEMPTY;
  }

#if YY_CPLUSPLUS < 201103L
  Parser::stack_symbol_type &Parser::stack_symbol_type::operator=(const stack_symbol_type &that)
  {
    state = that.state;
    value = that.value;
    return *this;
  }

  Parser::stack_symbol_type &Parser::stack_symbol_type::operator=(stack_symbol_type &that)
  {
    state = that.state;
    value = that.value;
    // that is emptied.
    that.state = empty_state;
    return *this;
  }
#endif

  template <typename Base>
  void Parser::yy_destroy_(const char *yymsg, basic_symbol<Base> &yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT(yymsg, yysym);

    // User destructor.
    YY_USE(yysym.kind());
  }

#if SEAMSDEBUG
  template <typename Base>
  void Parser::yy_print_(std::ostream &yyo, const basic_symbol<Base> &yysym) const
  {
    std::ostream &yyoutput = yyo;
    YY_USE(yyoutput);
    if (yysym.empty())
      yyo << "empty symbol";
    else {
      symbol_kind_type yykind = yysym.kind();
      yyo << (yykind < YYNTOKENS ? "token" : "nterm") << ' ' << yysym.name() << " (";
      YY_USE(yykind);
      yyo << ')';
    }
  }
#endif

  void Parser::yypush_(const char *m, YY_MOVE_REF(stack_symbol_type) sym)
  {
    if (m)
      YY_SYMBOL_PRINT(m, sym);
    yystack_.push(YY_MOVE(sym));
  }

  void Parser::yypush_(const char *m, state_type s, YY_MOVE_REF(symbol_type) sym)
  {
#if 201103L <= YY_CPLUSPLUS
    yypush_(m, stack_symbol_type(s, std::move(sym)));
#else
    stack_symbol_type ss(s, sym);
    yypush_(m, ss);
#endif
  }

  void Parser::yypop_(int n) YY_NOEXCEPT { yystack_.pop(n); }

#if SEAMSDEBUG
  std::ostream &Parser::debug_stream() const { return *yycdebug_; }

  void Parser::set_debug_stream(std::ostream &o) { yycdebug_ = &o; }

  Parser::debug_level_type Parser::debug_level() const { return yydebug_; }

  void Parser::set_debug_level(debug_level_type l) { yydebug_ = l; }
#endif // SEAMSDEBUG

  Parser::state_type Parser::yy_lr_goto_state_(state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - YYNTOKENS] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - YYNTOKENS];
  }

  bool Parser::yy_pact_value_is_default_(int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yypact_ninf_;
  }

  bool Parser::yy_table_value_is_error_(int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yytable_ninf_;
  }

  int Parser::operator()() { return parse(); }

  int Parser::parse()
  {
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_     = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The return value of parse ().
    int yyresult;

#if YY_EXCEPTIONS
    try
#endif // YY_EXCEPTIONS
    {
      YYCDEBUG << "Starting parse\n";

      /* Initialize the stack.  The initial state will be set in
         yynewstate, since the latter expects the semantical and the
         location values to have been already stored, initialize these
         stacks with a primary value.  */
      yystack_.clear();
      yypush_(YY_NULLPTR, 0, YY_MOVE(yyla));

    /*-----------------------------------------------.
    | yynewstate -- push a new symbol on the stack.  |
    `-----------------------------------------------*/
    yynewstate:
      YYCDEBUG << "Entering state " << int(yystack_[0].state) << '\n';
      YY_STACK_PRINT();

      // Accept?
      if (yystack_[0].state == yyfinal_)
        YYACCEPT;

      goto yybackup;

    /*-----------.
    | yybackup.  |
    `-----------*/
    yybackup:
      // Try to take a decision without lookahead.
      yyn = yypact_[+yystack_[0].state];
      if (yy_pact_value_is_default_(yyn))
        goto yydefault;

      // Read a lookahead token.
      if (yyla.empty()) {
        YYCDEBUG << "Reading a token\n";
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
        {
          yyla.kind_ = yytranslate_(yylex(&yyla.value));
        }
#if YY_EXCEPTIONS
        catch (const syntax_error &yyexc) {
          YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
          error(yyexc);
          goto yyerrlab1;
        }
#endif // YY_EXCEPTIONS
      }
      YY_SYMBOL_PRINT("Next token is", yyla);

      if (yyla.kind() == symbol_kind::S_YYerror) {
        // The scanner already issued an error message, process directly
        // to error recovery.  But do not keep the error token as
        // lookahead, it is too special and may lead us to an endless
        // loop in error recovery. */
        yyla.kind_ = symbol_kind::S_YYUNDEF;
        goto yyerrlab1;
      }

      /* If the proper action on seeing token YYLA.TYPE is to reduce or
         to detect an error, take that action.  */
      yyn += yyla.kind();
      if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.kind()) {
        goto yydefault;
      }

      // Reduce or error.
      yyn = yytable_[yyn];
      if (yyn <= 0) {
        if (yy_table_value_is_error_(yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

      // Count tokens shifted since error; after three, turn off error status.
      if (yyerrstatus_)
        --yyerrstatus_;

      // Shift the lookahead token.
      yypush_("Shifting", state_type(yyn), YY_MOVE(yyla));
      goto yynewstate;

    /*-----------------------------------------------------------.
    | yydefault -- do the default action for the current state.  |
    `-----------------------------------------------------------*/
    yydefault:
      yyn = yydefact_[+yystack_[0].state];
      if (yyn == 0)
        goto yyerrlab;
      goto yyreduce;

    /*-----------------------------.
    | yyreduce -- do a reduction.  |
    `-----------------------------*/
    yyreduce:
      yylen = yyr2_[yyn];
      {
        stack_symbol_type yylhs;
        yylhs.state = yy_lr_goto_state_(yystack_[yylen].state, yyr1_[yyn]);
        /* If YYLEN is nonzero, implement the default value of the
           action: '$$ = $1'.  Otherwise, use the top of the stack.

           Otherwise, the following line sets YYLHS.VALUE to garbage.
           This behavior is undocumented and Bison users should not rely
           upon it.  */
        if (yylen)
          yylhs.value = yystack_[yylen - 1].value;
        else
          yylhs.value = yystack_[0].value;

        // Perform the reduction.
        YY_REDUCE_PRINT(yyn);
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
        {
          switch (yyn) {
          case 4: // line: '\n'
#line 131 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (echo)
              aprepro.lexer->LexerOutput("\n", 1);
          }
#line 634 "apr_parser.cc"
          break;

          case 5: // line: LBRACE exp RBRACE
#line 132 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (echo) {
              SEAMS::symrec *format = aprepro.getsym("_FORMAT");
              if (format->value.svar.empty()) {
                auto tmpstr = fmt::format("{}", (yystack_[1].value.val));
                aprepro.lexer->LexerOutput(tmpstr.c_str(), tmpstr.size());
              }
              else {
                static char tmpstr[512];
                int         len =
                    snprintf(tmpstr, 512, format->value.svar.c_str(), (yystack_[1].value.val));
                aprepro.lexer->LexerOutput(tmpstr, len);
              }
            }
          }
#line 652 "apr_parser.cc"
          break;

          case 6: // line: LBRACE sexp RBRACE
#line 145 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (echo && (yystack_[1].value.string) != NULL) {
              aprepro.lexer->LexerOutput((yystack_[1].value.string),
                                         strlen((yystack_[1].value.string)));
            }
          }
#line 661 "apr_parser.cc"
          break;

          case 7: // line: LBRACE aexp RBRACE
#line 149 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
          }
#line 667 "apr_parser.cc"
          break;

          case 8: // line: LBRACE RBRACE
#line 150 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
          }
#line 673 "apr_parser.cc"
          break;

          case 9: // line: error RBRACE
#line 151 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            yyerrok;
          }
#line 679 "apr_parser.cc"
          break;

          case 10: // bool: exp LT exp
#line 154 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) < (yystack_[0].value.val);
          }
#line 685 "apr_parser.cc"
          break;

          case 11: // bool: exp GT exp
#line 155 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) > (yystack_[0].value.val);
          }
#line 691 "apr_parser.cc"
          break;

          case 12: // bool: NOT exp
#line 156 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = !((yystack_[0].value.val));
          }
#line 697 "apr_parser.cc"
          break;

          case 13: // bool: exp LE exp
#line 157 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) <= (yystack_[0].value.val);
          }
#line 703 "apr_parser.cc"
          break;

          case 14: // bool: exp GE exp
#line 158 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) >= (yystack_[0].value.val);
          }
#line 709 "apr_parser.cc"
          break;

          case 15: // bool: exp EQ exp
#line 159 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) == (yystack_[0].value.val);
          }
#line 715 "apr_parser.cc"
          break;

          case 16: // bool: exp NE exp
#line 160 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) != (yystack_[0].value.val);
          }
#line 721 "apr_parser.cc"
          break;

          case 17: // bool: exp LOR exp
#line 161 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) || (yystack_[0].value.val);
          }
#line 727 "apr_parser.cc"
          break;

          case 18: // bool: exp LAND exp
#line 162 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) && (yystack_[0].value.val);
          }
#line 733 "apr_parser.cc"
          break;

          case 19: // bool: bool LOR bool
#line 163 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) || (yystack_[0].value.val);
          }
#line 739 "apr_parser.cc"
          break;

          case 20: // bool: bool LAND bool
#line 164 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) && (yystack_[0].value.val);
          }
#line 745 "apr_parser.cc"
          break;

          case 21: // bool: bool LOR exp
#line 165 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) || (yystack_[0].value.val);
          }
#line 751 "apr_parser.cc"
          break;

          case 22: // bool: bool LAND exp
#line 166 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) && (yystack_[0].value.val);
          }
#line 757 "apr_parser.cc"
          break;

          case 23: // bool: exp LOR bool
#line 167 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) || (yystack_[0].value.val);
          }
#line 763 "apr_parser.cc"
          break;

          case 24: // bool: exp LAND bool
#line 168 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) && (yystack_[0].value.val);
          }
#line 769 "apr_parser.cc"
          break;

          case 25: // bool: LPAR bool RPAR
#line 169 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[1].value.val);
          }
#line 775 "apr_parser.cc"
          break;

          case 26: // bool: sexp LT sexp
#line 172 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) < 0 ? 1 : 0);
          }
#line 781 "apr_parser.cc"
          break;

          case 27: // bool: sexp GT sexp
#line 173 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) > 0 ? 1 : 0);
          }
#line 787 "apr_parser.cc"
          break;

          case 28: // bool: sexp LE sexp
#line 174 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) <= 0 ? 1 : 0);
          }
#line 793 "apr_parser.cc"
          break;

          case 29: // bool: sexp GE sexp
#line 175 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) >= 0 ? 1 : 0);
          }
#line 799 "apr_parser.cc"
          break;

          case 30: // bool: sexp EQ sexp
#line 176 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) == 0 ? 1 : 0);
          }
#line 805 "apr_parser.cc"
          break;

          case 31: // bool: sexp NE sexp
#line 177 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) != 0 ? 1 : 0);
          }
#line 811 "apr_parser.cc"
          break;

          case 32: // bool: UNDVAR LT sexp
#line 179 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp("", (yystack_[0].value.string)) < 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 817 "apr_parser.cc"
          break;

          case 33: // bool: UNDVAR GT sexp
#line 180 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp("", (yystack_[0].value.string)) > 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 823 "apr_parser.cc"
          break;

          case 34: // bool: UNDVAR LE sexp
#line 181 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp("", (yystack_[0].value.string)) <= 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 829 "apr_parser.cc"
          break;

          case 35: // bool: UNDVAR GE sexp
#line 182 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp("", (yystack_[0].value.string)) >= 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 835 "apr_parser.cc"
          break;

          case 36: // bool: UNDVAR EQ sexp
#line 183 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp("", (yystack_[0].value.string)) == 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 841 "apr_parser.cc"
          break;

          case 37: // bool: UNDVAR NE sexp
#line 184 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp("", (yystack_[0].value.string)) != 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 847 "apr_parser.cc"
          break;

          case 38: // bool: sexp LT UNDVAR
#line 186 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp((yystack_[2].value.string), "") < 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 853 "apr_parser.cc"
          break;

          case 39: // bool: sexp GT UNDVAR
#line 187 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp((yystack_[2].value.string), "") > 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 859 "apr_parser.cc"
          break;

          case 40: // bool: sexp LE UNDVAR
#line 188 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp((yystack_[2].value.string), "") <= 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 865 "apr_parser.cc"
          break;

          case 41: // bool: sexp GE UNDVAR
#line 189 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp((yystack_[2].value.string), "") >= 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 871 "apr_parser.cc"
          break;

          case 42: // bool: sexp EQ UNDVAR
#line 190 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp((yystack_[2].value.string), "") == 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 877 "apr_parser.cc"
          break;

          case 43: // bool: sexp NE UNDVAR
#line 191 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (strcmp((yystack_[2].value.string), "") != 0 ? 1 : 0);
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 883 "apr_parser.cc"
          break;

          case 44: // aexp: AVAR
#line 193 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) = aprepro.make_array(*((yystack_[0].value.tptr)->value.avar));
          }
#line 889 "apr_parser.cc"
          break;

          case 45: // aexp: AFNCT LPAR sexp RPAR
#line 194 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.arrfnct_c == NULL))
              (yylhs.value.arrval) =
                  (*((yystack_[3].value.tptr)->value.arrfnct_c))((yystack_[1].value.string));
            else
              yyerrok;
          }
#line 900 "apr_parser.cc"
          break;

          case 46: // aexp: AFNCT LPAR sexp COMMA exp RPAR
#line 200 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.arrfnct_cd == NULL))
              (yylhs.value.arrval) = (*((yystack_[5].value.tptr)->value.arrfnct_cd))(
                  (yystack_[3].value.string), (yystack_[1].value.val));
            else
              yyerrok;
          }
#line 911 "apr_parser.cc"
          break;

          case 47: // aexp: AFNCT LPAR sexp COMMA sexp RPAR
#line 206 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.arrfnct_cc == NULL))
              (yylhs.value.arrval) = (*((yystack_[5].value.tptr)->value.arrfnct_cc))(
                  (yystack_[3].value.string), (yystack_[1].value.string));
            else
              yyerrok;
          }
#line 922 "apr_parser.cc"
          break;

          case 48: // aexp: AFNCT LPAR exp COMMA exp COMMA exp RPAR
#line 212 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.arrfnct_ddd == NULL))
              (yylhs.value.arrval) = (*((yystack_[7].value.tptr)->value.arrfnct_ddd))(
                  (yystack_[5].value.val), (yystack_[3].value.val), (yystack_[1].value.val));
            else
              yyerrok;
          }
#line 933 "apr_parser.cc"
          break;

          case 49: // aexp: AFNCT LPAR exp COMMA exp RPAR
#line 218 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.arrfnct_dd == NULL))
              (yylhs.value.arrval) = (*((yystack_[5].value.tptr)->value.arrfnct_dd))(
                  (yystack_[3].value.val), (yystack_[1].value.val));
            else
              yyerrok;
          }
#line 944 "apr_parser.cc"
          break;

          case 50: // aexp: AFNCT LPAR exp RPAR
#line 224 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.arrfnct_d == NULL))
              (yylhs.value.arrval) =
                  (*((yystack_[3].value.tptr)->value.arrfnct_d))((yystack_[1].value.val));
            else
              yyerrok;
          }
#line 955 "apr_parser.cc"
          break;

          case 51: // aexp: AFNCT LPAR aexp RPAR
#line 230 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.arrfnct_a == NULL))
              (yylhs.value.arrval) =
                  (*((yystack_[3].value.tptr)->value.arrfnct_a))((yystack_[1].value.arrval));
            else
              yyerrok;
          }
#line 966 "apr_parser.cc"
          break;

          case 52: // aexp: SVAR EQUAL aexp
#line 236 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval)                 = (yystack_[0].value.arrval);
            (yystack_[2].value.tptr)->value.avar = (yystack_[0].value.arrval);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::AVAR);
          }
#line 975 "apr_parser.cc"
          break;

          case 53: // aexp: VAR EQUAL aexp
#line 240 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval)                 = (yystack_[0].value.arrval);
            (yystack_[2].value.tptr)->value.avar = (yystack_[0].value.arrval);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::AVAR);
          }
#line 984 "apr_parser.cc"
          break;

          case 54: // aexp: AVAR EQUAL aexp
#line 244 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) = (yystack_[0].value.arrval);
            aprepro.redefine_array((yystack_[2].value.tptr)->value.avar);
            (yystack_[2].value.tptr)->value.avar = (yystack_[0].value.arrval);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::AVAR);
          }
#line 992 "apr_parser.cc"
          break;

          case 55: // aexp: UNDVAR EQUAL aexp
#line 247 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval)                 = (yystack_[0].value.arrval);
            (yystack_[2].value.tptr)->value.avar = (yystack_[0].value.arrval);
            set_type(aprepro, (yystack_[2].value.tptr), token::AVAR);
          }
#line 999 "apr_parser.cc"
          break;

          case 56: // aexp: aexp PLU aexp
#line 249 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if ((yystack_[2].value.arrval)->cols == (yystack_[0].value.arrval)->cols &&
                (yystack_[2].value.arrval)->rows == (yystack_[0].value.arrval)->rows) {
              (yylhs.value.arrval) =
                  array_add((yystack_[2].value.arrval), (yystack_[0].value.arrval));
            }
            else {
              yyerror(aprepro, "Arrays do not have same row and column count");
              yyerrok;
            }
          }
#line 1012 "apr_parser.cc"
          break;

          case 57: // aexp: SUB aexp
#line 257 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) = array_scale((yystack_[0].value.arrval), -1.0);
          }
#line 1018 "apr_parser.cc"
          break;

          case 58: // aexp: aexp SUB aexp
#line 259 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if ((yystack_[2].value.arrval)->cols == (yystack_[0].value.arrval)->cols &&
                (yystack_[2].value.arrval)->rows == (yystack_[0].value.arrval)->rows) {
              (yylhs.value.arrval) =
                  array_sub((yystack_[2].value.arrval), (yystack_[0].value.arrval));
            }
            else {
              yyerror(aprepro, "Arrays do not have same row and column count");
              yyerrok;
            }
          }
#line 1031 "apr_parser.cc"
          break;

          case 59: // aexp: aexp TIM exp
#line 267 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) = array_scale((yystack_[2].value.arrval), (yystack_[0].value.val));
          }
#line 1037 "apr_parser.cc"
          break;

          case 60: // aexp: aexp DIV exp
#line 268 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) =
                array_scale((yystack_[2].value.arrval), 1.0 / (yystack_[0].value.val));
          }
#line 1043 "apr_parser.cc"
          break;

          case 61: // aexp: exp TIM aexp
#line 269 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) = array_scale((yystack_[0].value.arrval), (yystack_[2].value.val));
          }
#line 1049 "apr_parser.cc"
          break;

          case 62: // aexp: aexp TIM aexp
#line 270 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if ((yystack_[2].value.arrval)->cols == (yystack_[0].value.arrval)->rows) {
              (yylhs.value.arrval) =
                  array_mult((yystack_[2].value.arrval), (yystack_[0].value.arrval));
            }
            else {
              yyerror(aprepro,
                      "Column count of first array does not match row count of second array");
              yyerrok;
            }
          }
#line 1062 "apr_parser.cc"
          break;

          case 63: // sexp: QSTRING
#line 279 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = (yystack_[0].value.string);
          }
#line 1068 "apr_parser.cc"
          break;

          case 64: // sexp: SVAR
#line 280 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = (char *)(yystack_[0].value.tptr)->value.svar.c_str();
          }
#line 1074 "apr_parser.cc"
          break;

          case 65: // sexp: IMMSVAR
#line 281 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = (char *)(yystack_[0].value.tptr)->value.svar.c_str();
          }
#line 1080 "apr_parser.cc"
          break;

          case 66: // sexp: UNDVAR EQUAL sexp
#line 282 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string)                 = (yystack_[0].value.string);
            (yystack_[2].value.tptr)->value.svar = (yystack_[0].value.string);
            set_type(aprepro, (yystack_[2].value.tptr), Parser::token::SVAR);
          }
#line 1087 "apr_parser.cc"
          break;

          case 67: // sexp: SVAR EQUAL sexp
#line 284 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string)                 = (yystack_[0].value.string);
            (yystack_[2].value.tptr)->value.svar = (yystack_[0].value.string);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
          }
#line 1095 "apr_parser.cc"
          break;

          case 68: // sexp: VAR EQUAL sexp
#line 287 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string)                 = (yystack_[0].value.string);
            (yystack_[2].value.tptr)->value.svar = (yystack_[0].value.string);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::SVAR);
          }
#line 1104 "apr_parser.cc"
          break;

          case 69: // sexp: AVAR EQUAL sexp
#line 291 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = (yystack_[0].value.string);
            aprepro.redefine_array((yystack_[2].value.tptr)->value.avar);
            (yystack_[2].value.tptr)->value.svar = (yystack_[0].value.string);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::SVAR);
          }
#line 1114 "apr_parser.cc"
          break;

          case 70: // sexp: IMMSVAR EQUAL sexp
#line 296 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = (char *)(yystack_[2].value.tptr)->value.svar.c_str();
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1120 "apr_parser.cc"
          break;

          case 71: // sexp: IMMVAR EQUAL sexp
#line 297 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            immutable_modify(aprepro, (yystack_[2].value.tptr));
            YYERROR;
          }
#line 1126 "apr_parser.cc"
          break;

          case 72: // sexp: SFNCT LPAR sexp RPAR
#line 298 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.strfnct_c == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[3].value.tptr)->value.strfnct_c))(
                  (yystack_[1].value.string));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1137 "apr_parser.cc"
          break;

          case 73: // sexp: SFNCT LPAR RPAR
#line 304 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[2].value.tptr),
                          (yystack_[2].value.tptr)->value.strfnct == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[2].value.tptr)->value.strfnct))();
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1148 "apr_parser.cc"
          break;

          case 74: // sexp: SFNCT LPAR exp RPAR
#line 310 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.strfnct_d == NULL))
              (yylhs.value.string) =
                  (char *)(*((yystack_[3].value.tptr)->value.strfnct_d))((yystack_[1].value.val));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1159 "apr_parser.cc"
          break;

          case 75: // sexp: SFNCT LPAR aexp RPAR
#line 316 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.strfnct_a == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[3].value.tptr)->value.strfnct_a))(
                  (yystack_[1].value.arrval));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1170 "apr_parser.cc"
          break;

          case 76: // sexp: sexp CONCAT sexp
#line 322 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            concat_string((yystack_[2].value.string), (yystack_[0].value.string),
                          &(yylhs.value.string));
          }
#line 1176 "apr_parser.cc"
          break;

          case 77: // sexp: SFNCT LPAR exp COMMA exp RPAR
#line 323 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.strfnct_dd == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[5].value.tptr)->value.strfnct_dd))(
                  (yystack_[3].value.val), (yystack_[1].value.val));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1187 "apr_parser.cc"
          break;

          case 78: // sexp: SFNCT LPAR exp COMMA sexp COMMA sexp COMMA sexp COMMA sexp RPAR
#line 329 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[11].value.tptr),
                          (yystack_[11].value.tptr)->value.strfnct_dcccc == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[11].value.tptr)->value.strfnct_dcccc))(
                  (yystack_[9].value.val), (yystack_[7].value.string), (yystack_[5].value.string),
                  (yystack_[3].value.string), (yystack_[1].value.string));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1198 "apr_parser.cc"
          break;

          case 79: // sexp: SFNCT LPAR exp COMMA sexp COMMA sexp RPAR
#line 335 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.strfnct_dcc == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[7].value.tptr)->value.strfnct_dcc))(
                  (yystack_[5].value.val), (yystack_[3].value.string), (yystack_[1].value.string));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1209 "apr_parser.cc"
          break;

          case 80: // sexp: SFNCT LPAR sexp COMMA sexp COMMA sexp RPAR
#line 341 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.strfnct_ccc == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[7].value.tptr)->value.strfnct_ccc))(
                  (yystack_[5].value.string), (yystack_[3].value.string),
                  (yystack_[1].value.string));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1220 "apr_parser.cc"
          break;

          case 81: // sexp: SFNCT LPAR sexp COMMA sexp RPAR
#line 347 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.strfnct_cc == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[5].value.tptr)->value.strfnct_cc))(
                  (yystack_[3].value.string), (yystack_[1].value.string));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1231 "apr_parser.cc"
          break;

          case 82: // sexp: bool QUEST sexp COLON sexp
#line 353 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = ((yystack_[4].value.val)) ? ((yystack_[2].value.string))
                                                             : ((yystack_[0].value.string));
          }
#line 1237 "apr_parser.cc"
          break;

          case 83: // exp: NUM
#line 355 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val);
          }
#line 1243 "apr_parser.cc"
          break;

          case 84: // exp: INC NUM
#line 356 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val) + 1;
          }
#line 1249 "apr_parser.cc"
          break;

          case 85: // exp: DEC NUM
#line 357 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val) - 1;
          }
#line 1255 "apr_parser.cc"
          break;

          case 86: // exp: VAR
#line 358 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.tptr)->value.var;
          }
#line 1261 "apr_parser.cc"
          break;

          case 87: // exp: IMMVAR
#line 359 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.tptr)->value.var;
          }
#line 1267 "apr_parser.cc"
          break;

          case 88: // exp: INC VAR
#line 360 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ++((yystack_[0].value.tptr)->value.var);
          }
#line 1273 "apr_parser.cc"
          break;

          case 89: // exp: DEC VAR
#line 361 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = --((yystack_[0].value.tptr)->value.var);
          }
#line 1279 "apr_parser.cc"
          break;

          case 90: // exp: VAR INC
#line 362 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ((yystack_[1].value.tptr)->value.var)++;
          }
#line 1285 "apr_parser.cc"
          break;

          case 91: // exp: VAR DEC
#line 363 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ((yystack_[1].value.tptr)->value.var)--;
          }
#line 1291 "apr_parser.cc"
          break;

          case 92: // exp: VAR EQUAL exp
#line 364 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val)                   = (yystack_[0].value.val);
            (yystack_[2].value.tptr)->value.var = (yystack_[0].value.val);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
          }
#line 1298 "apr_parser.cc"
          break;

          case 93: // exp: SVAR EQUAL exp
#line 366 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val)                   = (yystack_[0].value.val);
            (yystack_[2].value.tptr)->value.var = (yystack_[0].value.val);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
          }
#line 1306 "apr_parser.cc"
          break;

          case 94: // exp: AVAR EQUAL exp
#line 369 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val);
            aprepro.redefine_array((yystack_[2].value.tptr)->value.avar);
            (yystack_[2].value.tptr)->value.var = (yystack_[0].value.val);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
          }
#line 1316 "apr_parser.cc"
          break;

          case 95: // exp: VAR EQ_PLUS exp
#line 374 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var += (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
          }
#line 1322 "apr_parser.cc"
          break;

          case 96: // exp: VAR EQ_MINUS exp
#line 375 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var -= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
          }
#line 1328 "apr_parser.cc"
          break;

          case 97: // exp: VAR EQ_TIME exp
#line 376 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var *= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
          }
#line 1334 "apr_parser.cc"
          break;

          case 98: // exp: VAR EQ_DIV exp
#line 377 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var /= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
          }
#line 1340 "apr_parser.cc"
          break;

          case 99: // exp: VAR EQ_POW exp
#line 378 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            reset_error();
            (yystack_[2].value.tptr)->value.var =
                std::pow((yystack_[2].value.tptr)->value.var, (yystack_[0].value.val));
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            SEAMS::math_error(aprepro, "Power");
          }
#line 1350 "apr_parser.cc"
          break;

          case 100: // exp: INC IMMVAR
#line 383 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[0].value.tptr));
          }
#line 1356 "apr_parser.cc"
          break;

          case 101: // exp: DEC IMMVAR
#line 384 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[0].value.tptr));
          }
#line 1362 "apr_parser.cc"
          break;

          case 102: // exp: IMMVAR INC
#line 385 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[1].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[1].value.tptr));
          }
#line 1368 "apr_parser.cc"
          break;

          case 103: // exp: IMMVAR DEC
#line 386 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[1].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[1].value.tptr));
          }
#line 1374 "apr_parser.cc"
          break;

          case 104: // exp: IMMVAR EQUAL exp
#line 387 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1380 "apr_parser.cc"
          break;

          case 105: // exp: IMMSVAR EQUAL exp
#line 388 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            immutable_modify(aprepro, (yystack_[2].value.tptr));
            YYERROR;
          }
#line 1386 "apr_parser.cc"
          break;

          case 106: // exp: IMMVAR EQ_PLUS exp
#line 389 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1392 "apr_parser.cc"
          break;

          case 107: // exp: IMMVAR EQ_MINUS exp
#line 390 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1398 "apr_parser.cc"
          break;

          case 108: // exp: IMMVAR EQ_TIME exp
#line 391 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1404 "apr_parser.cc"
          break;

          case 109: // exp: IMMVAR EQ_DIV exp
#line 392 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1410 "apr_parser.cc"
          break;

          case 110: // exp: IMMVAR EQ_POW exp
#line 393 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1416 "apr_parser.cc"
          break;

          case 111: // exp: UNDVAR
#line 395 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.tptr)->value.var;
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 1423 "apr_parser.cc"
          break;

          case 112: // exp: INC UNDVAR
#line 397 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ++((yystack_[0].value.tptr)->value.var);
            set_type(aprepro, (yystack_[0].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 1431 "apr_parser.cc"
          break;

          case 113: // exp: DEC UNDVAR
#line 400 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = --((yystack_[0].value.tptr)->value.var);
            set_type(aprepro, (yystack_[0].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 1439 "apr_parser.cc"
          break;

          case 114: // exp: UNDVAR INC
#line 403 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ((yystack_[1].value.tptr)->value.var)++;
            set_type(aprepro, (yystack_[1].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[1].value.tptr)->name);
          }
#line 1447 "apr_parser.cc"
          break;

          case 115: // exp: UNDVAR DEC
#line 406 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ((yystack_[1].value.tptr)->value.var)--;
            set_type(aprepro, (yystack_[1].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[1].value.tptr)->name);
          }
#line 1455 "apr_parser.cc"
          break;

          case 116: // exp: UNDVAR EQUAL exp
#line 409 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val)                   = (yystack_[0].value.val);
            (yystack_[2].value.tptr)->value.var = (yystack_[0].value.val);
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
          }
#line 1462 "apr_parser.cc"
          break;

          case 117: // exp: UNDVAR EQ_PLUS exp
#line 411 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var += (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 1470 "apr_parser.cc"
          break;

          case 118: // exp: UNDVAR EQ_MINUS exp
#line 414 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var -= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 1478 "apr_parser.cc"
          break;

          case 119: // exp: UNDVAR EQ_TIME exp
#line 417 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var *= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 1486 "apr_parser.cc"
          break;

          case 120: // exp: UNDVAR EQ_DIV exp
#line 420 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var /= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 1494 "apr_parser.cc"
          break;

          case 121: // exp: UNDVAR EQ_POW exp
#line 423 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            reset_error();
            (yystack_[2].value.tptr)->value.var =
                std::pow((yystack_[2].value.tptr)->value.var, (yystack_[0].value.val));
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
            SEAMS::math_error(aprepro, "Power");
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 1505 "apr_parser.cc"
          break;

          case 122: // exp: FNCT LPAR RPAR
#line 430 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[2].value.tptr),
                          (yystack_[2].value.tptr)->value.fnctptr == NULL))
              (yylhs.value.val) = (*((yystack_[2].value.tptr)->value.fnctptr))();
            else
              (yylhs.value.val) = 0.0;
          }
#line 1516 "apr_parser.cc"
          break;

          case 123: // exp: FNCT LPAR exp RPAR
#line 437 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.fnctptr_d == NULL))
              (yylhs.value.val) =
                  (*((yystack_[3].value.tptr)->value.fnctptr_d))((yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1527 "apr_parser.cc"
          break;

          case 124: // exp: FNCT LPAR sexp RPAR
#line 444 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.fnctptr_c == NULL))
              (yylhs.value.val) =
                  (*((yystack_[3].value.tptr)->value.fnctptr_c))((yystack_[1].value.string));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1538 "apr_parser.cc"
          break;

          case 125: // exp: FNCT LPAR aexp RPAR
#line 451 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.fnctptr_a == NULL))
              (yylhs.value.val) =
                  (*((yystack_[3].value.tptr)->value.fnctptr_a))((yystack_[1].value.arrval));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1549 "apr_parser.cc"
          break;

          case 126: // exp: FNCT LPAR sexp COMMA exp RPAR
#line 458 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.fnctptr_cd == NULL))
              (yylhs.value.val) = (*((yystack_[5].value.tptr)->value.fnctptr_cd))(
                  (yystack_[3].value.string), (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1560 "apr_parser.cc"
          break;

          case 127: // exp: FNCT LPAR exp COMMA sexp RPAR
#line 465 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.fnctptr_dc == NULL))
              (yylhs.value.val) = (*((yystack_[5].value.tptr)->value.fnctptr_dc))(
                  (yystack_[3].value.val), (yystack_[1].value.string));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1571 "apr_parser.cc"
          break;

          case 128: // exp: FNCT LPAR sexp COMMA sexp RPAR
#line 472 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.fnctptr_cc == NULL))
              (yylhs.value.val) = (*((yystack_[5].value.tptr)->value.fnctptr_cc))(
                  (yystack_[3].value.string), (yystack_[1].value.string));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1582 "apr_parser.cc"
          break;

          case 129: // exp: FNCT LPAR sexp COMMA sexp COMMA sexp RPAR
#line 479 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.fnctptr_ccc == NULL))
              (yylhs.value.val) = (*((yystack_[7].value.tptr)->value.fnctptr_ccc))(
                  (yystack_[5].value.string), (yystack_[3].value.string),
                  (yystack_[1].value.string));
            else
              yyerrok;
          }
#line 1593 "apr_parser.cc"
          break;

          case 130: // exp: FNCT LPAR exp COMMA exp RPAR
#line 486 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.fnctptr_dd == NULL))
              (yylhs.value.val) = (*((yystack_[5].value.tptr)->value.fnctptr_dd))(
                  (yystack_[3].value.val), (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1604 "apr_parser.cc"
          break;

          case 131: // exp: FNCT LPAR exp COMMA exp COMMA exp RPAR
#line 492 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.fnctptr_ddd == NULL))
              (yylhs.value.val) = (*((yystack_[7].value.tptr)->value.fnctptr_ddd))(
                  (yystack_[5].value.val), (yystack_[3].value.val), (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1615 "apr_parser.cc"
          break;

          case 132: // exp: FNCT LPAR sexp COMMA sexp COMMA exp RPAR
#line 498 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.fnctptr_ccd == NULL))
              (yylhs.value.val) = (*((yystack_[7].value.tptr)->value.fnctptr_ccd))(
                  (yystack_[5].value.string), (yystack_[3].value.string), (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1626 "apr_parser.cc"
          break;

          case 133: // exp: FNCT LPAR exp COMMA exp SEMI exp COMMA exp RPAR
#line 504 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[9].value.tptr),
                          (yystack_[9].value.tptr)->value.fnctptr_dddd == NULL))
              (yylhs.value.val) = (*((yystack_[9].value.tptr)->value.fnctptr_dddd))(
                  (yystack_[7].value.val), (yystack_[5].value.val), (yystack_[3].value.val),
                  (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1637 "apr_parser.cc"
          break;

          case 134: // exp: FNCT LPAR exp COMMA exp COMMA exp COMMA exp RPAR
#line 510 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[9].value.tptr),
                          (yystack_[9].value.tptr)->value.fnctptr_dddd == NULL))
              (yylhs.value.val) = (*((yystack_[9].value.tptr)->value.fnctptr_dddd))(
                  (yystack_[7].value.val), (yystack_[5].value.val), (yystack_[3].value.val),
                  (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1648 "apr_parser.cc"
          break;

          case 135: // exp: FNCT LPAR exp COMMA exp COMMA exp COMMA exp COMMA sexp RPAR
#line 516 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[11].value.tptr),
                          (yystack_[11].value.tptr)->value.fnctptr_ddddc == NULL))
              (yylhs.value.val) = (*((yystack_[11].value.tptr)->value.fnctptr_ddddc))(
                  (yystack_[9].value.val), (yystack_[7].value.val), (yystack_[5].value.val),
                  (yystack_[3].value.val), (yystack_[1].value.string));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1659 "apr_parser.cc"
          break;

          case 136: // exp: FNCT LPAR exp COMMA exp COMMA exp COMMA exp COMMA exp COMMA exp RPAR
#line 522 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[13].value.tptr),
                          (yystack_[13].value.tptr)->value.fnctptr_dddddd == NULL))
              (yylhs.value.val) = (*((yystack_[13].value.tptr)->value.fnctptr_dddddd))(
                  (yystack_[11].value.val), (yystack_[9].value.val), (yystack_[7].value.val),
                  (yystack_[5].value.val), (yystack_[3].value.val), (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1670 "apr_parser.cc"
          break;

          case 137: // exp: exp PLU exp
#line 528 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) + (yystack_[0].value.val);
          }
#line 1676 "apr_parser.cc"
          break;

          case 138: // exp: exp SUB exp
#line 529 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) - (yystack_[0].value.val);
          }
#line 1682 "apr_parser.cc"
          break;

          case 139: // exp: exp TIM exp
#line 530 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) * (yystack_[0].value.val);
          }
#line 1688 "apr_parser.cc"
          break;

          case 140: // exp: exp DIV exp
#line 531 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if ((yystack_[0].value.val) == 0.) {
              yyerror(aprepro, "Zero divisor");
              yyerrok;
            }
            else
              (yylhs.value.val) = (yystack_[2].value.val) / (yystack_[0].value.val);
          }
#line 1700 "apr_parser.cc"
          break;

          case 141: // exp: exp MOD exp
#line 538 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if ((yystack_[0].value.val) == 0.) {
              yyerror(aprepro, "Zero divisor");
              yyerrok;
            }
            else
              (yylhs.value.val) = (int)(yystack_[2].value.val) % (int)(yystack_[0].value.val);
          }
#line 1712 "apr_parser.cc"
          break;

          case 142: // exp: SUB exp
#line 545 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = -(yystack_[0].value.val);
          }
#line 1718 "apr_parser.cc"
          break;

          case 143: // exp: PLU exp
#line 546 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val);
          }
#line 1724 "apr_parser.cc"
          break;

          case 144: // exp: exp POW exp
#line 547 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            reset_error();
            (yylhs.value.val) = std::pow((yystack_[2].value.val), (yystack_[0].value.val));
            SEAMS::math_error(aprepro, "Power");
          }
#line 1732 "apr_parser.cc"
          break;

          case 145: // exp: LPAR exp RPAR
#line 550 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[1].value.val);
          }
#line 1738 "apr_parser.cc"
          break;

          case 146: // exp: LBRACK exp RBRACK
#line 551 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            reset_error();
            (yylhs.value.val) =
                (double)((yystack_[1].value.val) < 0 ? -floor(-((yystack_[1].value.val)))
                                                     : floor((yystack_[1].value.val)));
            SEAMS::math_error(aprepro, "floor (int)");
          }
#line 1746 "apr_parser.cc"
          break;

          case 147: // exp: bool
#line 554 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ((yystack_[0].value.val)) ? 1 : 0;
          }
#line 1752 "apr_parser.cc"
          break;

          case 148: // exp: bool QUEST exp COLON exp
#line 555 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                ((yystack_[4].value.val)) ? ((yystack_[2].value.val)) : ((yystack_[0].value.val));
          }
#line 1758 "apr_parser.cc"
          break;

          case 149: // exp: AVAR LBRACK exp RBRACK
#line 556 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                array_value((yystack_[3].value.tptr)->value.avar, (yystack_[1].value.val), 0);
          }
#line 1764 "apr_parser.cc"
          break;

          case 150: // exp: AVAR LBRACK exp COMMA exp RBRACK
#line 557 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = array_value((yystack_[5].value.tptr)->value.avar,
                                            (yystack_[3].value.val), (yystack_[1].value.val));
          }
#line 1770 "apr_parser.cc"
          break;

          case 151: // exp: AVAR LBRACK exp RBRACK EQUAL exp
#line 559 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val);
            array *arr        = (yystack_[5].value.tptr)->value.avar;
            int    cols       = arr->cols;
            if (cols > 1) {
              yyerror(aprepro, "Cannot use [index] array access with multi-column array");
              yyerrok;
            }
            int rows = arr->rows;
            int row  = (yystack_[3].value.val);
            if (aprepro.ap_options.one_based_index) {
              row--;
            }
            if (row < rows) {
              int offset                                         = row * cols;
              (yystack_[5].value.tptr)->value.avar->data[offset] = (yystack_[0].value.val);
            }
            else {
              yyerror(aprepro, "Row or Column index out of range");
              yyerrok;
            }
          }
#line 1796 "apr_parser.cc"
          break;

          case 152: // exp: AVAR LBRACK exp COMMA exp RBRACK EQUAL exp
#line 581 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val);
            array *arr        = (yystack_[7].value.tptr)->value.avar;
            int    cols       = arr->cols;
            int    rows       = arr->rows;
            int    row        = (yystack_[5].value.val);
            int    col        = (yystack_[3].value.val);
            if (aprepro.ap_options.one_based_index) {
              row--;
              col--;
            }
            if (row < rows && col < cols) {
              int offset                                         = row * cols + col;
              (yystack_[7].value.tptr)->value.avar->data[offset] = (yystack_[0].value.val);
            }
            else {
              yyerror(aprepro, "Row or Column index out of range");
              yyerrok;
            }
          }
#line 1820 "apr_parser.cc"
          break;

#line 1824 "apr_parser.cc"

          default: break;
          }
        }
#if YY_EXCEPTIONS
        catch (const syntax_error &yyexc) {
          YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
          error(yyexc);
          YYERROR;
        }
#endif // YY_EXCEPTIONS
        YY_SYMBOL_PRINT("-> $$ =", yylhs);
        yypop_(yylen);
        yylen = 0;

        // Shift the result of the reduction.
        yypush_(YY_NULLPTR, YY_MOVE(yylhs));
      }
      goto yynewstate;

    /*--------------------------------------.
    | yyerrlab -- here on detecting error.  |
    `--------------------------------------*/
    yyerrlab:
      // If not already recovering from an error, report this error.
      if (!yyerrstatus_) {
        ++yynerrs_;
        context     yyctx(*this, yyla);
        std::string msg = yysyntax_error_(yyctx);
        error(YY_MOVE(msg));
      }

      if (yyerrstatus_ == 3) {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.kind() == symbol_kind::S_YYEOF)
          YYABORT;
        else if (!yyla.empty()) {
          yy_destroy_("Error: discarding", yyla);
          yyla.clear();
        }
      }

      // Else will try to reuse lookahead token after shifting the error token.
      goto yyerrlab1;

    /*---------------------------------------------------.
    | yyerrorlab -- error raised explicitly by YYERROR.  |
    `---------------------------------------------------*/
    yyerrorlab:
      /* Pacify compilers when the user code never invokes YYERROR and
         the label yyerrorlab therefore never appears in user code.  */
      if (false)
        YYERROR;

      /* Do not reclaim the symbols of the rule whose action triggered
         this YYERROR.  */
      yypop_(yylen);
      yylen = 0;
      YY_STACK_PRINT();
      goto yyerrlab1;

    /*-------------------------------------------------------------.
    | yyerrlab1 -- common code for both syntax error and YYERROR.  |
    `-------------------------------------------------------------*/
    yyerrlab1:
      yyerrstatus_ = 3; // Each real token shifted decrements this.
      // Pop stack until we find a state that shifts the error token.
      for (;;) {
        yyn = yypact_[+yystack_[0].state];
        if (!yy_pact_value_is_default_(yyn)) {
          yyn += symbol_kind::S_YYerror;
          if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == symbol_kind::S_YYerror) {
            yyn = yytable_[yyn];
            if (0 < yyn)
              break;
          }
        }

        // Pop the current state because it cannot handle the error token.
        if (yystack_.size() == 1)
          YYABORT;

        yy_destroy_("Error: popping", yystack_[0]);
        yypop_();
        YY_STACK_PRINT();
      }
      {
        stack_symbol_type error_token;

        // Shift the error token.
        error_token.state = state_type(yyn);
        yypush_("Shifting", YY_MOVE(error_token));
      }
      goto yynewstate;

    /*-------------------------------------.
    | yyacceptlab -- YYACCEPT comes here.  |
    `-------------------------------------*/
    yyacceptlab:
      yyresult = 0;
      goto yyreturn;

    /*-----------------------------------.
    | yyabortlab -- YYABORT comes here.  |
    `-----------------------------------*/
    yyabortlab:
      yyresult = 1;
      goto yyreturn;

    /*-----------------------------------------------------.
    | yyreturn -- parsing is finished, return the result.  |
    `-----------------------------------------------------*/
    yyreturn:
      if (!yyla.empty())
        yy_destroy_("Cleanup: discarding lookahead", yyla);

      /* Do not reclaim the symbols of the rule whose action triggered
         this YYABORT or YYACCEPT.  */
      yypop_(yylen);
      YY_STACK_PRINT();
      while (1 < yystack_.size()) {
        yy_destroy_("Cleanup: popping", yystack_[0]);
        yypop_();
      }

      return yyresult;
    }
#if YY_EXCEPTIONS
    catch (...) {
      YYCDEBUG << "Exception caught: cleaning lookahead and stack\n";
      // Do not try to display the values of the reclaimed symbols,
      // as their printers might throw an exception.
      if (!yyla.empty())
        yy_destroy_(YY_NULLPTR, yyla);

      while (1 < yystack_.size()) {
        yy_destroy_(YY_NULLPTR, yystack_[0]);
        yypop_();
      }
      throw;
    }
#endif // YY_EXCEPTIONS
  }

  void Parser::error(const syntax_error &yyexc) { error(yyexc.what()); }

  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string Parser::yytnamerr_(const char *yystr)
  {
    if (*yystr == '"') {
      std::string yyr;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp) {
        case '\'':
        case ',': goto do_not_strip_quotes;

        case '\\':
          if (*++yyp != '\\')
            goto do_not_strip_quotes;
          else
            goto append;

        append:
        default: yyr += *yyp; break;

        case '"': return yyr;
        }
    do_not_strip_quotes:;
    }

    return yystr;
  }

  std::string Parser::symbol_name(symbol_kind_type yysymbol)
  {
    return yytnamerr_(yytname_[yysymbol]);
  }

  // Parser::context.
  Parser::context::context(const Parser &yyparser, const symbol_type &yyla)
      : yyparser_(yyparser), yyla_(yyla)
  {
  }

  int Parser::context::expected_tokens(symbol_kind_type yyarg[], int yyargn) const
  {
    // Actual number of expected tokens
    int yycount = 0;

    const int yyn = yypact_[+yyparser_.yystack_[0].state];
    if (!yy_pact_value_is_default_(yyn)) {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      const int yyxbegin = yyn < 0 ? -yyn : 0;
      // Stay within bounds of both yycheck and yytname.
      const int yychecklim = yylast_ - yyn + 1;
      const int yyxend     = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck_[yyx + yyn] == yyx && yyx != symbol_kind::S_YYerror &&
            !yy_table_value_is_error_(yytable_[yyx + yyn])) {
          if (!yyarg)
            ++yycount;
          else if (yycount == yyargn)
            return 0;
          else
            yyarg[yycount++] = YY_CAST(symbol_kind_type, yyx);
        }
    }

    if (yyarg && yycount == 0 && 0 < yyargn)
      yyarg[0] = symbol_kind::S_YYEMPTY;
    return yycount;
  }

  int Parser::yy_syntax_error_arguments_(const context &yyctx, symbol_kind_type yyarg[],
                                         int yyargn) const
  {
    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yyla) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yyla.  (However, yyla is currently not documented for users.)
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state merging
         (from LALR or IELR) and default reductions corrupt the expected
         token list.  However, the list is correct for canonical LR with
         one exception: it will still contain any token that will not be
         accepted due to an error action in a later state.
    */

    if (!yyctx.lookahead().empty()) {
      if (yyarg)
        yyarg[0] = yyctx.token();
      int yyn = yyctx.expected_tokens(yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      return yyn + 1;
    }
    return 0;
  }

  // Generate an error message.
  std::string Parser::yysyntax_error_(const context &yyctx) const
  {
    // Its maximum.
    enum { YYARGS_MAX = 5 };
    // Arguments of yyformat.
    symbol_kind_type yyarg[YYARGS_MAX];
    int              yycount = yy_syntax_error_arguments_(yyctx, yyarg, YYARGS_MAX);

    char const *yyformat = YY_NULLPTR;
    switch (yycount) {
#define YYCASE_(N, S)                                                                              \
  case N: yyformat = S; break
    default: // Avoid compiler warnings.
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

    std::string yyres;
    // Argument number.
    std::ptrdiff_t yyi = 0;
    for (char const *yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount) {
        yyres += symbol_name(yyarg[yyi++]);
        ++yyp;
      }
      else
        yyres += *yyp;
    return yyres;
  }

  const signed char Parser::yypact_ninf_ = -40;

  const signed char Parser::yytable_ninf_ = -1;

  const short Parser::yypact_[] = {
      -40,  7,    -40,  -16,  302,  -40,  -40,  -40,  -40,  -40,  1364, 128,  16,   154,  26,
      -12,  -6,   42,   49,   467,  467,  -40,  467,  422,  467,  121,  291,  69,   -17,  47,
      1340, 422,  467,  467,  467,  467,  467,  467,  467,  467,  467,  467,  467,  -40,  -40,
      422,  467,  467,  467,  467,  467,  -40,  -40,  422,  467,  467,  467,  467,  467,  467,
      -40,  -40,  467,  467,  422,  362,  407,  422,  1380, 307,  43,   100,  467,  9,    1386,
      1078, 1292, 21,   -40,  21,   21,   -40,  -40,  -40,  -40,  -40,  -40,  -40,  -40,  467,
      467,  467,  -40,  422,  422,  467,  422,  -40,  482,  527,  542,  587,  602,  647,  467,
      -40,  467,  467,  467,  467,  467,  467,  467,  467,  467,  467,  467,  422,  467,  467,
      35,   1386, 1432, 1448, 1448, 1448, 1448, 1448, 40,   1448, 40,   40,   40,   40,   40,
      35,   1386, 1432, 1448, 1448, 1448, 1448, 1448, 35,   1386, 1432, 1386, 1448, 1448, 1448,
      1448, 1448, 1448, 1386, 1448, 813,  35,   1386, 1432, -40,  -5,   173,  843,  -40,  31,
      202,  873,  80,   255,  903,  467,  467,  467,  467,  21,   -40,  -40,  467,  -40,  1396,
      1416, 70,   1479, -40,  1493, -39,  1432, -39,  1464, -40,  1464, 1380, 40,   1380, 40,
      1380, 40,   1380, 40,   1380, 40,   1380, 40,   -40,  70,   1479, -40,  1493, 239,  239,
      239,  239,  239,  239,  -28,  -28,  21,   -40,  21,   21,   21,   467,  106,  -40,  467,
      -40,  467,  -40,  -40,  467,  -40,  467,  -40,  -40,  467,  -40,  467,  -40,  1448, 1448,
      1448, 1448, 21,   467,  467,  1317, 467,  663,  1105, 212,  784,  673,  109,  1132, 704,
      1159, 933,  1386, 1448, 108,  1448, 467,  -40,  -40,  -40,  467,  -40,  467,  467,  -40,
      467,  -40,  -40,  -40,  467,  -40,  467,  726,  1186, 963,  1022, 732,  698,  1213, 1448,
      -40,  -40,  467,  -40,  467,  -40,  467,  -40,  -40,  993,  1240, 633,  467,  -40,  -40,
      467,  754,  1051, 760,  -40,  467,  -40,  1267, -40};

  const unsigned char Parser::yydefact_[] = {
      2,   0,   1,   0,   0,   4,   3,   9,   83,  63,  111, 86,  64,  87,  65,  44,  0,   0,   0,
      0,   0,   8,   0,   0,   0,   0,   0,   147, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   114, 115, 0,   0,   0,   0,   0,   0,   90,  91,  0,   0,   0,   0,
      0,   0,   0,   102, 103, 0,   0,   0,   0,   0,   0,   111, 86,  64,  0,   0,   147, 0,   0,
      0,   143, 57,  142, 12,  84,  112, 88,  100, 85,  113, 89,  101, 0,   0,   0,   7,   0,   0,
      0,   0,   6,   0,   0,   0,   0,   0,   0,   0,   5,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   55,  66,  116, 117, 118, 119, 120, 121, 32,  0,   33,  34,  35,
      36,  37,  53,  68,  92,  95,  96,  97,  98,  99,  52,  67,  93,  71,  104, 106, 107, 108, 109,
      110, 70,  105, 0,   54,  69,  94,  122, 0,   0,   0,   73,  0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   142, 25,  145, 0,   146, 0,   0,   19,  21,  20,  22,  56,  0,   58,  60,  62,
      59,  38,  26,  39,  27,  40,  28,  41,  29,  42,  30,  43,  31,  76,  23,  17,  24,  18,  10,
      11,  13,  14,  15,  16,  137, 138, 140, 61,  139, 141, 144, 0,   149, 125, 0,   124, 0,   123,
      75,  0,   72,  0,   74,  51,  0,   45,  0,   50,  116, 92,  93,  94,  139, 0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   82,  148, 150, 151, 0,   128, 126, 127, 0,
      130, 0,   0,   81,  0,   77,  47,  46,  0,   49,  0,   0,   0,   0,   0,   0,   0,   0,   152,
      129, 132, 0,   131, 0,   80,  0,   79,  48,  0,   0,   0,   0,   134, 133, 0,   0,   0,   0,
      135, 0,   78,  0,   136};

  const short Parser::yypgoto_[] = {-40, -40, -40, -13, 159, 94, -4};

  const unsigned char Parser::yydefgoto_[] = {0, 1, 6, 27, 28, 74, 129};

  const short Parser::yytable_[] = {
      30,  95,  96,  92,  7,   63,  73,  2,   3,   65,  64,  223, 116, 177, 118, 75,  76,  119,
      77,  79,  80,  93,  94,  95,  96,  175, 4,   122, 123, 124, 125, 126, 127, 93,  94,  95,
      96,  89,  53,  90,  91,  137, 138, 139, 140, 141, 142, 228, 62,  145, 147, 148, 149, 150,
      151, 152, 5,   66,  154, 155, 158, 162, 166, 169, 67,  172, 119, 97,  174, 93,  94,  95,
      96,  93,  94,  95,  96,  181, 183, 98,  99,  100, 101, 102, 103, 180, 182, 184, 104, 186,
      186, 188, 190, 204, 206, 104, 233, 89,  29,  90,  91,  91,  205, 207, 208, 209, 210, 211,
      212, 213, 214, 215, 216, 218, 219, 220, 0,   63,  93,  94,  95,  96,  173, 270, 81,  121,
      82,  83,  246, 84,  276, 128, 130, 131, 132, 133, 134, 0,   0,   136, 0,   98,  99,  100,
      101, 102, 103, 144, 146, 0,   45,  46,  47,  48,  49,  50,  153, 104, 157, 161, 165, 168,
      0,   0,   0,   0,   238, 239, 240, 241, 0,   0,   0,   242, 51,  52,  54,  55,  56,  57,
      58,  59,  78,  179, 0,   0,   0,   224, 0,   225, 120, 0,   192, 194, 196, 198, 200, 202,
      203, 0,   60,  61,  0,   0,   135, 98,  99,  100, 101, 102, 103, 0,   143, 0,   0,   0,
      229, 245, 230, 0,   248, 104, 250, 156, 160, 164, 167, 253, 264, 0,   255, 0,   256, 0,
      98,  99,  100, 101, 102, 103, 258, 0,   260, 0,   98,  99,  100, 101, 102, 103, 104, 0,
      185, 187, 0,   189, 0,   278, 0,   0,   104, 279, 0,   280, 121, 136, 144, 157, 0,   234,
      283, 235, 284, 0,   0,   0,   217, 114, 115, 116, 177, 118, 0,   294, 119, 295, 0,   98,
      99,  100, 101, 102, 103, 302, 85,  0,   86,  87,  0,   88,  0,   307, 0,   104, 0,   8,
      9,   10,  11,  12,  13,  14,  15,  16,  17,  18,  0,   19,  247, 20,  249, 0,   21,  251,
      0,   252, 0,   0,   254, 171, 46,  47,  48,  49,  50,  0,   0,   257, 0,   0,   22,  23,
      0,   0,   0,   0,   24,  0,   25,  26,  0,   0,   0,   51,  52,  277, 0,   0,   0,   0,
      0,   0,   281, 0,   282, 8,   9,   10,  11,  12,  13,  14,  15,  16,  17,  18,  0,   19,
      159, 20,  0,   0,   0,   0,   0,   296, 0,   0,   0,   0,   0,   301, 0,   0,   303, 0,
      0,   0,   0,   0,   22,  23,  0,   0,   0,   0,   24,  0,   25,  26,  8,   9,   10,  11,
      12,  13,  14,  15,  16,  17,  18,  0,   19,  163, 20,  8,   9,   10,  11,  12,  13,  14,
      15,  16,  17,  18,  0,   19,  0,   20,  0,   0,   0,   0,   0,   22,  23,  0,   0,   0,
      0,   24,  0,   25,  26,  0,   0,   0,   0,   0,   22,  23,  0,   0,   0,   0,   24,  0,
      25,  26,  8,   9,   68,  69,  70,  13,  14,  71,  16,  17,  0,   0,   19,  0,   20,  8,
      9,   191, 69,  70,  13,  14,  71,  16,  17,  0,   0,   19,  0,   20,  0,   0,   0,   0,
      0,   22,  72,  0,   0,   0,   0,   24,  0,   25,  26,  0,   0,   0,   0,   0,   22,  72,
      0,   0,   0,   0,   24,  0,   25,  26,  8,   9,   193, 69,  70,  13,  14,  71,  16,  17,
      0,   0,   19,  0,   20,  8,   9,   195, 69,  70,  13,  14,  71,  16,  17,  0,   0,   19,
      0,   20,  0,   0,   0,   0,   0,   22,  72,  0,   0,   0,   0,   24,  0,   25,  26,  0,
      0,   0,   0,   0,   22,  72,  0,   0,   0,   0,   24,  0,   25,  26,  8,   9,   197, 69,
      70,  13,  14,  71,  16,  17,  0,   0,   19,  0,   20,  8,   9,   199, 69,  70,  13,  14,
      71,  16,  17,  0,   0,   19,  0,   20,  0,   0,   0,   0,   0,   22,  72,  0,   0,   0,
      0,   24,  0,   25,  26,  0,   0,   0,   0,   0,   22,  72,  0,   0,   0,   0,   24,  300,
      25,  26,  8,   9,   201, 69,  70,  13,  14,  71,  16,  17,  0,   0,   19,  0,   20,  98,
      99,  100, 101, 102, 103, 0,   0,   0,   0,   0,   0,   261, 0,   262, 0,   104, 0,   0,
      0,   22,  72,  268, 0,   269, 0,   24,  0,   25,  26,  98,  99,  100, 101, 102, 103, 0,
      0,   0,   0,   98,  99,  100, 101, 102, 103, 104, 291, 0,   292, 0,   0,   0,   0,   0,
      272, 104, 0,   0,   0,   0,   0,   0,   0,   0,   98,  99,  100, 101, 102, 103, 98,  99,
      100, 101, 102, 103, 285, 0,   0,   0,   104, 0,   290, 0,   0,   0,   104, 0,   0,   0,
      0,   0,   98,  99,  100, 101, 102, 103, 98,  99,  100, 101, 102, 103, 304, 0,   0,   0,
      104, 0,   306, 0,   0,   0,   104, 0,   0,   0,   0,   0,   98,  99,  100, 101, 102, 103,
      98,  99,  100, 101, 102, 103, 265, 0,   266, 0,   104, 0,   0,   267, 0,   0,   104, 0,
      0,   0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 221,
      0,   119, 0,   222, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108,
      109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 0,   226, 119, 227, 0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113, 114,
      115, 116, 117, 118, 0,   231, 119, 232, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 0,   236,
      119, 237, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108,
      109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 0,   274, 119, 275, 0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113, 114,
      115, 116, 177, 118, 0,   287, 119, 288, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 0,   297,
      119, 298, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108,
      109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 289, 0,   119, 0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
      116, 177, 118, 305, 0,   119, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 176, 0,   119, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113,
      114, 115, 116, 177, 118, 263, 0,   119, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 271, 0,   119, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113,
      114, 115, 116, 177, 118, 273, 0,   119, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 286, 0,   119, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113,
      114, 115, 116, 177, 118, 293, 0,   119, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 299, 0,   119, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113,
      114, 115, 116, 177, 118, 308, 0,   119, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 178, 0,   119, 0,
      0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
      116, 177, 118, 259, 0,   119, 0,   0,   0,   0,   0,   0,   0,   0,   0,   106, 107, 108,
      109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 105, 0,   119, 0,   0,   0,   0,   0,
      0,   0,   106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 0,   0,   119,
      31,  32,  33,  34,  35,  36,  0,   0,   0,   0,   37,  38,  39,  40,  41,  42,  170, 32,
      33,  34,  35,  36,  0,   0,   43,  44,  37,  38,  39,  40,  41,  42,  98,  99,  100, 101,
      102, 103, 0,   243, 43,  44,  98,  99,  100, 101, 102, 103, 104, 0,   0,   0,   0,   0,
      0,   0,   0,   0,   104, 244, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 177,
      118, 0,   0,   119, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 0,
      0,   119, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 0,   0,   119,
      106, 107, 108, 109, 110, 111, 112, 113, 0,   0,   0,   0,   118, 0,   0,   119, 107, 108,
      109, 110, 111, 112, 113, 114, 115, 116, 177, 118, 0,   0,   119, 108, 109, 110, 111, 112,
      113, 114, 115, 116, 177, 118, 0,   0,   119};

  const short Parser::yycheck_[] = {
      4,   40,  41,  20,  20,  17,  19,  0,   1,   15,  22,  16,  40,  41,  42,  19,  20,  45,  22,
      23,  24,  38,  39,  40,  41,  16,  19,  31,  32,  33,  34,  35,  36,  38,  39,  40,  41,  28,
      22,  30,  31,  45,  46,  47,  48,  49,  50,  16,  22,  53,  54,  55,  56,  57,  58,  59,  49,
      15,  62,  63,  64,  65,  66,  67,  15,  22,  45,  20,  72,  38,  39,  40,  41,  38,  39,  40,
      41,  90,  91,  32,  33,  34,  35,  36,  37,  89,  90,  91,  48,  93,  94,  95,  96,  106, 107,
      48,  16,  28,  4,   30,  31,  31,  106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
      118, 119, -1,  17,  38,  39,  40,  41,  22,  14,  3,   31,  5,   6,   22,  8,   22,  37,  38,
      39,  40,  41,  42,  -1,  -1,  45,  -1,  32,  33,  34,  35,  36,  37,  53,  54,  -1,  22,  23,
      24,  25,  26,  27,  62,  48,  64,  65,  66,  67,  -1,  -1,  -1,  -1,  170, 171, 172, 173, -1,
      -1,  -1,  177, 46,  47,  22,  23,  24,  25,  26,  27,  23,  89,  -1,  -1,  -1,  14,  -1,  16,
      31,  -1,  98,  99,  100, 101, 102, 103, 104, -1,  46,  47,  -1,  -1,  45,  32,  33,  34,  35,
      36,  37,  -1,  53,  -1,  -1,  -1,  14,  221, 16,  -1,  224, 48,  226, 64,  65,  66,  67,  231,
      16,  -1,  234, -1,  236, -1,  32,  33,  34,  35,  36,  37,  244, -1,  246, -1,  32,  33,  34,
      35,  36,  37,  48,  -1,  93,  94,  -1,  96,  -1,  261, -1,  -1,  48,  265, -1,  267, 170, 171,
      172, 173, -1,  14,  274, 16,  276, -1,  -1,  -1,  117, 38,  39,  40,  41,  42,  -1,  287, 45,
      289, -1,  32,  33,  34,  35,  36,  37,  297, 3,   -1,  5,   6,   -1,  8,   -1,  305, -1,  48,
      -1,  3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  -1,  15,  224, 17,  226, -1,  20,
      229, -1,  231, -1,  -1,  234, 22,  23,  24,  25,  26,  27,  -1,  -1,  243, -1,  -1,  38,  39,
      -1,  -1,  -1,  -1,  44,  -1,  46,  47,  -1,  -1,  -1,  46,  47,  261, -1,  -1,  -1,  -1,  -1,
      -1,  268, -1,  270, 3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  -1,  15,  16,  17,
      -1,  -1,  -1,  -1,  -1,  291, -1,  -1,  -1,  -1,  -1,  297, -1,  -1,  300, -1,  -1,  -1,  -1,
      -1,  38,  39,  -1,  -1,  -1,  -1,  44,  -1,  46,  47,  3,   4,   5,   6,   7,   8,   9,   10,
      11,  12,  13,  -1,  15,  16,  17,  3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  -1,
      15,  -1,  17,  -1,  -1,  -1,  -1,  -1,  38,  39,  -1,  -1,  -1,  -1,  44,  -1,  46,  47,  -1,
      -1,  -1,  -1,  -1,  38,  39,  -1,  -1,  -1,  -1,  44,  -1,  46,  47,  3,   4,   5,   6,   7,
      8,   9,   10,  11,  12,  -1,  -1,  15,  -1,  17,  3,   4,   5,   6,   7,   8,   9,   10,  11,
      12,  -1,  -1,  15,  -1,  17,  -1,  -1,  -1,  -1,  -1,  38,  39,  -1,  -1,  -1,  -1,  44,  -1,
      46,  47,  -1,  -1,  -1,  -1,  -1,  38,  39,  -1,  -1,  -1,  -1,  44,  -1,  46,  47,  3,   4,
      5,   6,   7,   8,   9,   10,  11,  12,  -1,  -1,  15,  -1,  17,  3,   4,   5,   6,   7,   8,
      9,   10,  11,  12,  -1,  -1,  15,  -1,  17,  -1,  -1,  -1,  -1,  -1,  38,  39,  -1,  -1,  -1,
      -1,  44,  -1,  46,  47,  -1,  -1,  -1,  -1,  -1,  38,  39,  -1,  -1,  -1,  -1,  44,  -1,  46,
      47,  3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  -1,  -1,  15,  -1,  17,  3,   4,   5,
      6,   7,   8,   9,   10,  11,  12,  -1,  -1,  15,  -1,  17,  -1,  -1,  -1,  -1,  -1,  38,  39,
      -1,  -1,  -1,  -1,  44,  -1,  46,  47,  -1,  -1,  -1,  -1,  -1,  38,  39,  -1,  -1,  -1,  -1,
      44,  14,  46,  47,  3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  -1,  -1,  15,  -1,  17,
      32,  33,  34,  35,  36,  37,  -1,  -1,  -1,  -1,  -1,  -1,  14,  -1,  16,  -1,  48,  -1,  -1,
      -1,  38,  39,  14,  -1,  16,  -1,  44,  -1,  46,  47,  32,  33,  34,  35,  36,  37,  -1,  -1,
      -1,  -1,  32,  33,  34,  35,  36,  37,  48,  14,  -1,  16,  -1,  -1,  -1,  -1,  -1,  16,  48,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  32,  33,  34,  35,  36,  37,  32,  33,  34,  35,  36,
      37,  16,  -1,  -1,  -1,  48,  -1,  16,  -1,  -1,  -1,  48,  -1,  -1,  -1,  -1,  -1,  32,  33,
      34,  35,  36,  37,  32,  33,  34,  35,  36,  37,  16,  -1,  -1,  -1,  48,  -1,  16,  -1,  -1,
      -1,  48,  -1,  -1,  -1,  -1,  -1,  32,  33,  34,  35,  36,  37,  32,  33,  34,  35,  36,  37,
      14,  -1,  16,  -1,  48,  -1,  -1,  21,  -1,  -1,  48,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,
      33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  14,  -1,  45,  -1,  18,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,
      42,  -1,  14,  45,  16,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,
      31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  14,  45,  16,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,
      39,  40,  41,  42,  -1,  14,  45,  16,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  14,  45,  16,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,
      36,  37,  38,  39,  40,  41,  42,  -1,  14,  45,  16,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,
      14,  45,  16,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,
      33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  14,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,
      42,  14,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,
      32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  16,  -1,  45,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,
      16,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,
      35,  36,  37,  38,  39,  40,  41,  42,  16,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  16,  -1,  45,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,
      38,  39,  40,  41,  42,  16,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  16,  -1,  45,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
      41,  42,  16,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,
      33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  16,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  18,
      -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,
      38,  39,  40,  41,  42,  18,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,
      32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  20,  -1,  45,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  -1,  45,  22,
      23,  24,  25,  26,  27,  -1,  -1,  -1,  -1,  32,  33,  34,  35,  36,  37,  22,  23,  24,  25,
      26,  27,  -1,  -1,  46,  47,  32,  33,  34,  35,  36,  37,  32,  33,  34,  35,  36,  37,  -1,
      29,  46,  47,  32,  33,  34,  35,  36,  37,  48,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      48,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  -1,  45,  30,
      31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  -1,  45,  30,  31,  32,  33,
      34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  -1,  45,  30,  31,  32,  33,  34,  35,  36,
      37,  -1,  -1,  -1,  -1,  42,  -1,  -1,  45,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
      41,  42,  -1,  -1,  45,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  -1,  45};

  const signed char Parser::yystos_[] = {
      0,  51, 0,  1,  19, 49, 52, 20, 3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 15, 17, 20, 38,
      39, 44, 46, 47, 53, 54, 55, 56, 22, 23, 24, 25, 26, 27, 32, 33, 34, 35, 36, 37, 46, 47, 22,
      23, 24, 25, 26, 27, 46, 47, 22, 22, 23, 24, 25, 26, 27, 46, 47, 22, 17, 22, 15, 15, 15, 5,
      6,  7,  10, 39, 53, 55, 56, 56, 56, 54, 56, 56, 3,  5,  6,  8,  3,  5,  6,  8,  28, 30, 31,
      20, 38, 39, 40, 41, 20, 32, 33, 34, 35, 36, 37, 48, 20, 30, 31, 32, 33, 34, 35, 36, 37, 38,
      39, 40, 41, 42, 45, 54, 55, 56, 56, 56, 56, 56, 56, 55, 56, 55, 55, 55, 55, 55, 54, 55, 56,
      56, 56, 56, 56, 56, 54, 55, 56, 55, 56, 56, 56, 56, 56, 56, 55, 56, 56, 54, 55, 56, 16, 54,
      55, 56, 16, 54, 55, 56, 54, 55, 56, 22, 22, 22, 22, 56, 16, 16, 41, 18, 55, 56, 53, 56, 53,
      56, 54, 56, 54, 56, 54, 56, 5,  55, 5,  55, 5,  55, 5,  55, 5,  55, 5,  55, 55, 53, 56, 53,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 54, 56, 56, 56, 14, 18, 16, 14, 16, 14, 16, 16, 14,
      16, 14, 16, 16, 14, 16, 14, 16, 56, 56, 56, 56, 56, 29, 29, 56, 22, 55, 56, 55, 56, 55, 55,
      56, 55, 56, 56, 55, 56, 18, 56, 14, 16, 16, 16, 14, 16, 21, 14, 16, 14, 16, 16, 16, 14, 16,
      22, 55, 56, 56, 56, 55, 55, 56, 56, 16, 16, 14, 16, 14, 16, 14, 16, 16, 56, 56, 55, 14, 16,
      16, 14, 55, 56, 55, 16, 14, 16, 56, 16};

  const signed char Parser::yyr1_[] = {
      0,  50, 51, 51, 52, 52, 52, 52, 52, 52, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53,
      53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 55, 55, 55,
      55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56};

  const signed char Parser::yyr2_[] = {
      0,  2, 0, 2,  1,  3,  3,  3, 2, 2, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3,  3, 3, 3,  3,  3,  3,  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 4, 6, 6, 8, 6, 4, 4,
      3,  3, 3, 3,  3,  2,  3,  3, 3, 3, 3, 1, 1, 1, 3, 3, 3, 3, 3, 3, 4, 3, 4, 4, 3, 6,
      12, 8, 8, 6,  5,  1,  2,  2, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2,
      3,  3, 3, 3,  3,  3,  3,  1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 6, 6, 6, 8,
      6,  8, 8, 10, 10, 12, 14, 3, 3, 3, 3, 3, 2, 2, 3, 3, 3, 1, 5, 4, 6, 6, 8};

#if SEAMSDEBUG || 1
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a YYNTOKENS, nonterminals.
  const char *const Parser::yytname_[] = {"\"end of file\"",
                                          "error",
                                          "\"invalid token\"",
                                          "NUM",
                                          "QSTRING",
                                          "UNDVAR",
                                          "VAR",
                                          "SVAR",
                                          "IMMVAR",
                                          "IMMSVAR",
                                          "AVAR",
                                          "FNCT",
                                          "SFNCT",
                                          "AFNCT",
                                          "COMMA",
                                          "LPAR",
                                          "RPAR",
                                          "LBRACK",
                                          "RBRACK",
                                          "LBRACE",
                                          "RBRACE",
                                          "SEMI",
                                          "EQUAL",
                                          "EQ_PLUS",
                                          "EQ_MINUS",
                                          "EQ_TIME",
                                          "EQ_DIV",
                                          "EQ_POW",
                                          "QUEST",
                                          "COLON",
                                          "LOR",
                                          "LAND",
                                          "LT",
                                          "GT",
                                          "LE",
                                          "GE",
                                          "EQ",
                                          "NE",
                                          "PLU",
                                          "SUB",
                                          "DIV",
                                          "TIM",
                                          "MOD",
                                          "UNARY",
                                          "NOT",
                                          "POW",
                                          "INC",
                                          "DEC",
                                          "CONCAT",
                                          "'\\n'",
                                          "$accept",
                                          "input",
                                          "line",
                                          "bool",
                                          "aexp",
                                          "sexp",
                                          "exp",
                                          YY_NULLPTR};
#endif

#if SEAMSDEBUG
  const short Parser::yyrline_[] = {
      0,   127, 127, 128, 131, 132, 145, 149, 150, 151, 154, 155, 156, 157, 158, 159, 160,
      161, 162, 163, 164, 165, 166, 167, 168, 169, 172, 173, 174, 175, 176, 177, 179, 180,
      181, 182, 183, 184, 186, 187, 188, 189, 190, 191, 193, 194, 200, 206, 212, 218, 224,
      230, 236, 240, 244, 247, 249, 257, 259, 267, 268, 269, 270, 279, 280, 281, 282, 284,
      287, 291, 296, 297, 298, 304, 310, 316, 322, 323, 329, 335, 341, 347, 353, 355, 356,
      357, 358, 359, 360, 361, 362, 363, 364, 366, 369, 374, 375, 376, 377, 378, 383, 384,
      385, 386, 387, 388, 389, 390, 391, 392, 393, 395, 397, 400, 403, 406, 409, 411, 414,
      417, 420, 423, 430, 437, 444, 451, 458, 465, 472, 479, 486, 492, 498, 504, 510, 516,
      522, 528, 529, 530, 531, 538, 545, 546, 547, 550, 551, 554, 555, 556, 557, 558, 580};

  void Parser::yy_stack_print_() const
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator i = yystack_.begin(), i_end = yystack_.end(); i != i_end; ++i)
      *yycdebug_ << ' ' << int(i->state);
    *yycdebug_ << '\n';
  }

  void Parser::yy_reduce_print_(int yyrule) const
  {
    int yylno  = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1 << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT("   $" << yyi + 1 << " =", yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // SEAMSDEBUG

  Parser::symbol_kind_type Parser::yytranslate_(int t) YY_NOEXCEPT
  {
    // YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to
    // TOKEN-NUM as returned by yylex.
    static const signed char translate_table[] = {
        0,  2,  2,  2,  2,  2,  2,  2,  2,  2,  49, 2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  1,  2,  3,  4,  5,  6,  7,  8,
        9,  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
        31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48};
    // Last valid token kind.
    const int code_max = 303;

    if (t <= 0)
      return symbol_kind::S_YYEOF;
    else if (t <= code_max)
      return static_cast<symbol_kind_type>(translate_table[t]);
    else
      return symbol_kind::S_YYUNDEF;
  }

} // namespace SEAMS
#line 2777 "apr_parser.cc"

#line 603 "/Users/gdsjaar/src/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"

void SEAMS::Parser::error(const std::string &m) { aprepro.error(m); }
