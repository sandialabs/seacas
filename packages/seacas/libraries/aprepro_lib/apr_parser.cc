// A Bison parser, made by GNU Bison 3.4.2.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2019, 2022 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

// Undocumented macros, especially those whose name start with YY_,
// are private implementation details.  Do not rely on them.

// Take the name prefix into account.
#define yylex SEAMSlex

// First part of user prologue.
#line 6 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"

#include "apr_array.h"
#include "apr_util.h"
#include "aprepro.h"

#include <cerrno>
#include <cfenv>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <stdlib.h>

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

#line 81 "apr_parser.cc"

#include "aprepro_parser.h"

// Second part of user prologue.
#line 110 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"

#include "apr_scanner.h"
#include "aprepro.h"

/* this "connects" the bison parser in aprepro to the flex scanner class
 * object. it defines the yylex() function call to pull the next token from the
 * current lexer object of the aprepro context. */
#undef yylex
#define yylex aprepro.lexer->lex

#line 100 "apr_parser.cc"

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
      yystack_print_();                                                                            \
  } while (false)

#else // !SEAMSDEBUG

#define YYCDEBUG                                                                                   \
  if (false)                                                                                       \
  std::cerr
#define YY_SYMBOL_PRINT(Title, Symbol) YYUSE(Symbol)
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
#line 173 "apr_parser.cc"

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

  /// Build a parser object.
  Parser::Parser(class Aprepro &aprepro_yyarg)
      :
#if SEAMSDEBUG
        yydebug_(false), yycdebug_(&std::cerr),
#endif
        aprepro(aprepro_yyarg)
  {
  }

  Parser::~Parser() = default;

  Parser::syntax_error::~syntax_error() YY_NOEXCEPT YY_NOTHROW {}

  /*---------------.
  | Symbol types.  |
  `---------------*/

  // basic_symbol.
#if 201103L <= YY_CPLUSPLUS
  template <typename Base>
  Parser::basic_symbol<Base>::basic_symbol(basic_symbol &&that)
      : Base(std::move(that)), value(std::move(that.value))
  {
  }
#endif

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
  Parser::basic_symbol<Base>::basic_symbol(typename Base::kind_type t, YY_RVREF(semantic_type) v)
      : Base(t), value(YY_MOVE(v))
  {
  }

  template <typename Base> bool Parser::basic_symbol<Base>::empty() const YY_NOEXCEPT
  {
    return Base::type_get() == empty_symbol;
  }

  template <typename Base> void Parser::basic_symbol<Base>::move(basic_symbol &s)
  {
    super_type::move(s);
    value = YY_MOVE(s.value);
  }

  // by_type.
  Parser::by_type::by_type() : type(empty_symbol) {}

#if 201103L <= YY_CPLUSPLUS
  Parser::by_type::by_type(by_type &&that) : type(that.type) { that.clear(); }
#endif

  Parser::by_type::by_type(const by_type &that) : type(that.type) {}

  Parser::by_type::by_type(token_type t) : type(yytranslate_(t)) {}

  void Parser::by_type::clear() { type = empty_symbol; }

  void Parser::by_type::move(by_type &that)
  {
    type = that.type;
    that.clear();
  }

  int Parser::by_type::type_get() const YY_NOEXCEPT { return type; }

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

  Parser::symbol_number_type Parser::by_state::type_get() const YY_NOEXCEPT
  {
    if (state == empty_state)
      return empty_symbol;
    else
      return yystos_[state];
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
    that.type = empty_symbol;
  }

#if YY_CPLUSPLUS < 201103L
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
    YYUSE(yysym.type_get());
  }

#if SEAMSDEBUG
  template <typename Base>
  void Parser::yy_print_(std::ostream &yyo, const basic_symbol<Base> &yysym) const
  {
    std::ostream &yyoutput = yyo;
    YYUSE(yyoutput);
    symbol_number_type yytype = yysym.type_get();
#if defined __GNUC__ && !defined __clang__ && !defined __ICC &&                                    \
    __GNUC__ * 100 + __GNUC_MINOR__ <= 408
    // Avoid a (spurious) G++ 4.8 warning about "array subscript is
    // below array bounds".
    if (yysym.empty())
      std::abort();
#endif
    yyo << (yytype < yyntokens_ ? "token" : "nterm") << ' ' << yytname_[yytype] << " (";
    YYUSE(yytype);
    yyo << ')';
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

  void Parser::yypop_(int n) { yystack_.pop(n); }

#if SEAMSDEBUG
  std::ostream &Parser::debug_stream() const { return *yycdebug_; }

  void Parser::set_debug_stream(std::ostream &o) { yycdebug_ = &o; }

  Parser::debug_level_type Parser::debug_level() const { return yydebug_; }

  void Parser::set_debug_level(debug_level_type l) { yydebug_ = l; }
#endif // SEAMSDEBUG

  Parser::state_type Parser::yy_lr_goto_state_(state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  bool Parser::yy_pact_value_is_default_(int yyvalue) { return yyvalue == yypact_ninf_; }

  bool Parser::yy_table_value_is_error_(int yyvalue) { return yyvalue == yytable_ninf_; }

  int Parser::operator()() { return parse(); }

  int Parser::parse()
  {
    // State.
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
      YYCDEBUG << "Entering state " << yystack_[0].state << '\n';

      // Accept?
      if (yystack_[0].state == yyfinal_)
        YYACCEPT;

      goto yybackup;

    /*-----------.
    | yybackup.  |
    `-----------*/
    yybackup:
      // Try to take a decision without lookahead.
      yyn = yypact_[yystack_[0].state];
      if (yy_pact_value_is_default_(yyn))
        goto yydefault;

      // Read a lookahead token.
      if (yyla.empty()) {
        YYCDEBUG << "Reading a token: ";
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
        {
          yyla.type = yytranslate_(yylex(&yyla.value));
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

      /* If the proper action on seeing token YYLA.TYPE is to reduce or
         to detect an error, take that action.  */
      yyn += yyla.type_get();
      if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.type_get())
        goto yydefault;

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
      yypush_("Shifting", yyn, YY_MOVE(yyla));
      goto yynewstate;

    /*-----------------------------------------------------------.
    | yydefault -- do the default action for the current state.  |
    `-----------------------------------------------------------*/
    yydefault:
      yyn = yydefact_[yystack_[0].state];
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
          case 4:
#line 129 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (echo)
              aprepro.lexer->LexerOutput("\n", 1);
          }
#line 642 "apr_parser.cc"
          break;

          case 5:
#line 130 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (echo) {
              static char    tmpstr[512];
              SEAMS::symrec *format = aprepro.getsym("_FORMAT");
              int len = sprintf(tmpstr, format->value.svar.c_str(), (yystack_[1].value.val));
              aprepro.lexer->LexerOutput(tmpstr, len);
            }
          }
#line 654 "apr_parser.cc"
          break;

          case 6:
#line 137 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (echo && (yystack_[1].value.string) != NULL) {
              aprepro.lexer->LexerOutput((yystack_[1].value.string),
                                         strlen((yystack_[1].value.string)));
            }
          }
#line 663 "apr_parser.cc"
          break;

          case 7:
#line 141 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
          }
#line 669 "apr_parser.cc"
          break;

          case 8:
#line 142 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
          }
#line 675 "apr_parser.cc"
          break;

          case 9:
#line 143 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            yyerrok;
          }
#line 681 "apr_parser.cc"
          break;

          case 10:
#line 146 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) < (yystack_[0].value.val);
          }
#line 687 "apr_parser.cc"
          break;

          case 11:
#line 147 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) > (yystack_[0].value.val);
          }
#line 693 "apr_parser.cc"
          break;

          case 12:
#line 148 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = !((yystack_[0].value.val));
          }
#line 699 "apr_parser.cc"
          break;

          case 13:
#line 149 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) <= (yystack_[0].value.val);
          }
#line 705 "apr_parser.cc"
          break;

          case 14:
#line 150 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) >= (yystack_[0].value.val);
          }
#line 711 "apr_parser.cc"
          break;

          case 15:
#line 151 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) == (yystack_[0].value.val);
          }
#line 717 "apr_parser.cc"
          break;

          case 16:
#line 152 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) != (yystack_[0].value.val);
          }
#line 723 "apr_parser.cc"
          break;

          case 17:
#line 153 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) || (yystack_[0].value.val);
          }
#line 729 "apr_parser.cc"
          break;

          case 18:
#line 154 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) && (yystack_[0].value.val);
          }
#line 735 "apr_parser.cc"
          break;

          case 19:
#line 155 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) || (yystack_[0].value.val);
          }
#line 741 "apr_parser.cc"
          break;

          case 20:
#line 156 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) && (yystack_[0].value.val);
          }
#line 747 "apr_parser.cc"
          break;

          case 21:
#line 157 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) || (yystack_[0].value.val);
          }
#line 753 "apr_parser.cc"
          break;

          case 22:
#line 158 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) && (yystack_[0].value.val);
          }
#line 759 "apr_parser.cc"
          break;

          case 23:
#line 159 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) || (yystack_[0].value.val);
          }
#line 765 "apr_parser.cc"
          break;

          case 24:
#line 160 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) && (yystack_[0].value.val);
          }
#line 771 "apr_parser.cc"
          break;

          case 25:
#line 161 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[1].value.val);
          }
#line 777 "apr_parser.cc"
          break;

          case 26:
#line 164 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) < 0 ? 1 : 0);
          }
#line 783 "apr_parser.cc"
          break;

          case 27:
#line 165 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) > 0 ? 1 : 0);
          }
#line 789 "apr_parser.cc"
          break;

          case 28:
#line 166 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) <= 0 ? 1 : 0);
          }
#line 795 "apr_parser.cc"
          break;

          case 29:
#line 167 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) >= 0 ? 1 : 0);
          }
#line 801 "apr_parser.cc"
          break;

          case 30:
#line 168 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) == 0 ? 1 : 0);
          }
#line 807 "apr_parser.cc"
          break;

          case 31:
#line 169 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                (strcmp((yystack_[2].value.string), (yystack_[0].value.string)) != 0 ? 1 : 0);
          }
#line 813 "apr_parser.cc"
          break;

          case 32:
#line 171 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) = aprepro.make_array(*((yystack_[0].value.tptr)->value.avar));
          }
#line 819 "apr_parser.cc"
          break;

          case 33:
#line 172 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.arrfnct_c == NULL))
              (yylhs.value.arrval) =
                  (*((yystack_[3].value.tptr)->value.arrfnct_c))((yystack_[1].value.string));
            else
              yyerrok;
          }
#line 830 "apr_parser.cc"
          break;

          case 34:
#line 178 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.arrfnct_cd == NULL))
              (yylhs.value.arrval) = (*((yystack_[5].value.tptr)->value.arrfnct_cd))(
                  (yystack_[3].value.string), (yystack_[1].value.val));
            else
              yyerrok;
          }
#line 841 "apr_parser.cc"
          break;

          case 35:
#line 184 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.arrfnct_cc == NULL))
              (yylhs.value.arrval) = (*((yystack_[5].value.tptr)->value.arrfnct_cc))(
                  (yystack_[3].value.string), (yystack_[1].value.string));
            else
              yyerrok;
          }
#line 852 "apr_parser.cc"
          break;

          case 36:
#line 190 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.arrfnct_ddd == NULL))
              (yylhs.value.arrval) = (*((yystack_[7].value.tptr)->value.arrfnct_ddd))(
                  (yystack_[5].value.val), (yystack_[3].value.val), (yystack_[1].value.val));
            else
              yyerrok;
          }
#line 863 "apr_parser.cc"
          break;

          case 37:
#line 196 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.arrfnct_dd == NULL))
              (yylhs.value.arrval) = (*((yystack_[5].value.tptr)->value.arrfnct_dd))(
                  (yystack_[3].value.val), (yystack_[1].value.val));
            else
              yyerrok;
          }
#line 874 "apr_parser.cc"
          break;

          case 38:
#line 202 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.arrfnct_d == NULL))
              (yylhs.value.arrval) =
                  (*((yystack_[3].value.tptr)->value.arrfnct_d))((yystack_[1].value.val));
            else
              yyerrok;
          }
#line 885 "apr_parser.cc"
          break;

          case 39:
#line 208 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.arrfnct_a == NULL))
              (yylhs.value.arrval) =
                  (*((yystack_[3].value.tptr)->value.arrfnct_a))((yystack_[1].value.arrval));
            else
              yyerrok;
          }
#line 896 "apr_parser.cc"
          break;

          case 40:
#line 214 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval)                 = (yystack_[0].value.arrval);
            (yystack_[2].value.tptr)->value.avar = (yystack_[0].value.arrval);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::AVAR);
          }
#line 905 "apr_parser.cc"
          break;

          case 41:
#line 218 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval)                 = (yystack_[0].value.arrval);
            (yystack_[2].value.tptr)->value.avar = (yystack_[0].value.arrval);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::AVAR);
          }
#line 914 "apr_parser.cc"
          break;

          case 42:
#line 222 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) = (yystack_[0].value.arrval);
            aprepro.redefine_array((yystack_[2].value.tptr)->value.avar);
            (yystack_[2].value.tptr)->value.avar = (yystack_[0].value.arrval);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::AVAR);
          }
#line 922 "apr_parser.cc"
          break;

          case 43:
#line 225 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval)                 = (yystack_[0].value.arrval);
            (yystack_[2].value.tptr)->value.avar = (yystack_[0].value.arrval);
            set_type(aprepro, (yystack_[2].value.tptr), token::AVAR);
          }
#line 929 "apr_parser.cc"
          break;

          case 44:
#line 227 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
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
#line 942 "apr_parser.cc"
          break;

          case 45:
#line 235 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) = array_scale((yystack_[0].value.arrval), -1.0);
          }
#line 948 "apr_parser.cc"
          break;

          case 46:
#line 237 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
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
#line 961 "apr_parser.cc"
          break;

          case 47:
#line 245 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) = array_scale((yystack_[2].value.arrval), (yystack_[0].value.val));
          }
#line 967 "apr_parser.cc"
          break;

          case 48:
#line 246 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) =
                array_scale((yystack_[2].value.arrval), 1.0 / (yystack_[0].value.val));
          }
#line 973 "apr_parser.cc"
          break;

          case 49:
#line 247 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.arrval) = array_scale((yystack_[0].value.arrval), (yystack_[2].value.val));
          }
#line 979 "apr_parser.cc"
          break;

          case 50:
#line 248 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
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
#line 992 "apr_parser.cc"
          break;

          case 51:
#line 257 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = (yystack_[0].value.string);
          }
#line 998 "apr_parser.cc"
          break;

          case 52:
#line 258 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = (char *)(yystack_[0].value.tptr)->value.svar.c_str();
          }
#line 1004 "apr_parser.cc"
          break;

          case 53:
#line 259 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = (char *)(yystack_[0].value.tptr)->value.svar.c_str();
          }
#line 1010 "apr_parser.cc"
          break;

          case 54:
#line 260 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string)                 = (yystack_[0].value.string);
            (yystack_[2].value.tptr)->value.svar = (yystack_[0].value.string);
            set_type(aprepro, (yystack_[2].value.tptr), Parser::token::SVAR);
          }
#line 1017 "apr_parser.cc"
          break;

          case 55:
#line 262 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string)                 = (yystack_[0].value.string);
            (yystack_[2].value.tptr)->value.svar = (yystack_[0].value.string);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
          }
#line 1025 "apr_parser.cc"
          break;

          case 56:
#line 265 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string)                 = (yystack_[0].value.string);
            (yystack_[2].value.tptr)->value.svar = (yystack_[0].value.string);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::SVAR);
          }
#line 1034 "apr_parser.cc"
          break;

          case 57:
#line 269 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = (yystack_[0].value.string);
            aprepro.redefine_array((yystack_[2].value.tptr)->value.avar);
            (yystack_[2].value.tptr)->value.svar = (yystack_[0].value.string);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::SVAR);
          }
#line 1044 "apr_parser.cc"
          break;

          case 58:
#line 274 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = (char *)(yystack_[2].value.tptr)->value.svar.c_str();
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1050 "apr_parser.cc"
          break;

          case 59:
#line 275 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            immutable_modify(aprepro, (yystack_[2].value.tptr));
            YYERROR;
          }
#line 1056 "apr_parser.cc"
          break;

          case 60:
#line 276 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.strfnct_c == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[3].value.tptr)->value.strfnct_c))(
                  (yystack_[1].value.string));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1067 "apr_parser.cc"
          break;

          case 61:
#line 282 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[2].value.tptr),
                          (yystack_[2].value.tptr)->value.strfnct == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[2].value.tptr)->value.strfnct))();
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1078 "apr_parser.cc"
          break;

          case 62:
#line 288 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.strfnct_d == NULL))
              (yylhs.value.string) =
                  (char *)(*((yystack_[3].value.tptr)->value.strfnct_d))((yystack_[1].value.val));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1089 "apr_parser.cc"
          break;

          case 63:
#line 294 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.strfnct_a == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[3].value.tptr)->value.strfnct_a))(
                  (yystack_[1].value.arrval));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1100 "apr_parser.cc"
          break;

          case 64:
#line 300 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            concat_string((yystack_[2].value.string), (yystack_[0].value.string),
                          &(yylhs.value.string));
          }
#line 1106 "apr_parser.cc"
          break;

          case 65:
#line 301 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.strfnct_dd == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[5].value.tptr)->value.strfnct_dd))(
                  (yystack_[3].value.val), (yystack_[1].value.val));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1117 "apr_parser.cc"
          break;

          case 66:
#line 307 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[11].value.tptr),
                          (yystack_[11].value.tptr)->value.strfnct_dcccc == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[11].value.tptr)->value.strfnct_dcccc))(
                  (yystack_[9].value.val), (yystack_[7].value.string), (yystack_[5].value.string),
                  (yystack_[3].value.string), (yystack_[1].value.string));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1128 "apr_parser.cc"
          break;

          case 67:
#line 313 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.strfnct_dcc == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[7].value.tptr)->value.strfnct_dcc))(
                  (yystack_[5].value.val), (yystack_[3].value.string), (yystack_[1].value.string));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1139 "apr_parser.cc"
          break;

          case 68:
#line 319 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.strfnct_ccc == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[7].value.tptr)->value.strfnct_ccc))(
                  (yystack_[5].value.string), (yystack_[3].value.string),
                  (yystack_[1].value.string));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1150 "apr_parser.cc"
          break;

          case 69:
#line 325 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.strfnct_cc == NULL))
              (yylhs.value.string) = (char *)(*((yystack_[5].value.tptr)->value.strfnct_cc))(
                  (yystack_[3].value.string), (yystack_[1].value.string));
            else
              (yylhs.value.string) = (char *)"";
          }
#line 1161 "apr_parser.cc"
          break;

          case 70:
#line 331 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.string) = ((yystack_[4].value.val)) ? ((yystack_[2].value.string))
                                                             : ((yystack_[0].value.string));
          }
#line 1167 "apr_parser.cc"
          break;

          case 71:
#line 333 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val);
          }
#line 1173 "apr_parser.cc"
          break;

          case 72:
#line 334 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val) + 1;
          }
#line 1179 "apr_parser.cc"
          break;

          case 73:
#line 335 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val) - 1;
          }
#line 1185 "apr_parser.cc"
          break;

          case 74:
#line 336 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.tptr)->value.var;
          }
#line 1191 "apr_parser.cc"
          break;

          case 75:
#line 337 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.tptr)->value.var;
          }
#line 1197 "apr_parser.cc"
          break;

          case 76:
#line 338 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ++((yystack_[0].value.tptr)->value.var);
          }
#line 1203 "apr_parser.cc"
          break;

          case 77:
#line 339 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = --((yystack_[0].value.tptr)->value.var);
          }
#line 1209 "apr_parser.cc"
          break;

          case 78:
#line 340 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ((yystack_[1].value.tptr)->value.var)++;
          }
#line 1215 "apr_parser.cc"
          break;

          case 79:
#line 341 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ((yystack_[1].value.tptr)->value.var)--;
          }
#line 1221 "apr_parser.cc"
          break;

          case 80:
#line 342 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val)                   = (yystack_[0].value.val);
            (yystack_[2].value.tptr)->value.var = (yystack_[0].value.val);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
          }
#line 1228 "apr_parser.cc"
          break;

          case 81:
#line 344 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val)                   = (yystack_[0].value.val);
            (yystack_[2].value.tptr)->value.var = (yystack_[0].value.val);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
          }
#line 1236 "apr_parser.cc"
          break;

          case 82:
#line 347 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val);
            aprepro.redefine_array((yystack_[2].value.tptr)->value.avar);
            (yystack_[2].value.tptr)->value.var = (yystack_[0].value.val);
            redefined_warning(aprepro, (yystack_[2].value.tptr));
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
          }
#line 1246 "apr_parser.cc"
          break;

          case 83:
#line 352 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var += (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
          }
#line 1252 "apr_parser.cc"
          break;

          case 84:
#line 353 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var -= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
          }
#line 1258 "apr_parser.cc"
          break;

          case 85:
#line 354 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var *= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
          }
#line 1264 "apr_parser.cc"
          break;

          case 86:
#line 355 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var /= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
          }
#line 1270 "apr_parser.cc"
          break;

          case 87:
#line 356 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            reset_error();
            (yystack_[2].value.tptr)->value.var =
                std::pow((yystack_[2].value.tptr)->value.var, (yystack_[0].value.val));
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            SEAMS::math_error(aprepro, "Power");
          }
#line 1280 "apr_parser.cc"
          break;

          case 88:
#line 361 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[0].value.tptr));
          }
#line 1286 "apr_parser.cc"
          break;

          case 89:
#line 362 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[0].value.tptr));
          }
#line 1292 "apr_parser.cc"
          break;

          case 90:
#line 363 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[1].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[1].value.tptr));
          }
#line 1298 "apr_parser.cc"
          break;

          case 91:
#line 364 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[1].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[1].value.tptr));
          }
#line 1304 "apr_parser.cc"
          break;

          case 92:
#line 365 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1310 "apr_parser.cc"
          break;

          case 93:
#line 366 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            immutable_modify(aprepro, (yystack_[2].value.tptr));
            YYERROR;
          }
#line 1316 "apr_parser.cc"
          break;

          case 94:
#line 367 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1322 "apr_parser.cc"
          break;

          case 95:
#line 368 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1328 "apr_parser.cc"
          break;

          case 96:
#line 369 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1334 "apr_parser.cc"
          break;

          case 97:
#line 370 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1340 "apr_parser.cc"
          break;

          case 98:
#line 371 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            immutable_modify(aprepro, (yystack_[2].value.tptr));
          }
#line 1346 "apr_parser.cc"
          break;

          case 99:
#line 373 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.tptr)->value.var;
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 1353 "apr_parser.cc"
          break;

          case 100:
#line 375 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ++((yystack_[0].value.tptr)->value.var);
            set_type(aprepro, (yystack_[0].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 1361 "apr_parser.cc"
          break;

          case 101:
#line 378 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = --((yystack_[0].value.tptr)->value.var);
            set_type(aprepro, (yystack_[0].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[0].value.tptr)->name);
          }
#line 1369 "apr_parser.cc"
          break;

          case 102:
#line 381 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ((yystack_[1].value.tptr)->value.var)++;
            set_type(aprepro, (yystack_[1].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[1].value.tptr)->name);
          }
#line 1377 "apr_parser.cc"
          break;

          case 103:
#line 384 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ((yystack_[1].value.tptr)->value.var)--;
            set_type(aprepro, (yystack_[1].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[1].value.tptr)->name);
          }
#line 1385 "apr_parser.cc"
          break;

          case 104:
#line 387 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val)                   = (yystack_[0].value.val);
            (yystack_[2].value.tptr)->value.var = (yystack_[0].value.val);
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
          }
#line 1392 "apr_parser.cc"
          break;

          case 105:
#line 389 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var += (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 1400 "apr_parser.cc"
          break;

          case 106:
#line 392 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var -= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 1408 "apr_parser.cc"
          break;

          case 107:
#line 395 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var *= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 1416 "apr_parser.cc"
          break;

          case 108:
#line 398 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yystack_[2].value.tptr)->value.var /= (yystack_[0].value.val);
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 1424 "apr_parser.cc"
          break;

          case 109:
#line 401 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            reset_error();
            (yystack_[2].value.tptr)->value.var =
                std::pow((yystack_[2].value.tptr)->value.var, (yystack_[0].value.val));
            (yylhs.value.val) = (yystack_[2].value.tptr)->value.var;
            set_type(aprepro, (yystack_[2].value.tptr), token::VAR);
            SEAMS::math_error(aprepro, "Power");
            undefined_error(aprepro, (yystack_[2].value.tptr)->name);
          }
#line 1435 "apr_parser.cc"
          break;

          case 110:
#line 408 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[2].value.tptr),
                          (yystack_[2].value.tptr)->value.fnctptr == NULL))
              (yylhs.value.val) = (*((yystack_[2].value.tptr)->value.fnctptr))();
            else
              (yylhs.value.val) = 0.0;
          }
#line 1446 "apr_parser.cc"
          break;

          case 111:
#line 415 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.fnctptr_d == NULL))
              (yylhs.value.val) =
                  (*((yystack_[3].value.tptr)->value.fnctptr_d))((yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1457 "apr_parser.cc"
          break;

          case 112:
#line 422 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.fnctptr_c == NULL))
              (yylhs.value.val) =
                  (*((yystack_[3].value.tptr)->value.fnctptr_c))((yystack_[1].value.string));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1468 "apr_parser.cc"
          break;

          case 113:
#line 429 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[3].value.tptr),
                          (yystack_[3].value.tptr)->value.fnctptr_a == NULL))
              (yylhs.value.val) =
                  (*((yystack_[3].value.tptr)->value.fnctptr_a))((yystack_[1].value.arrval));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1479 "apr_parser.cc"
          break;

          case 114:
#line 436 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.fnctptr_cd == NULL))
              (yylhs.value.val) = (*((yystack_[5].value.tptr)->value.fnctptr_cd))(
                  (yystack_[3].value.string), (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1490 "apr_parser.cc"
          break;

          case 115:
#line 443 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.fnctptr_dc == NULL))
              (yylhs.value.val) = (*((yystack_[5].value.tptr)->value.fnctptr_dc))(
                  (yystack_[3].value.val), (yystack_[1].value.string));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1501 "apr_parser.cc"
          break;

          case 116:
#line 450 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.fnctptr_cc == NULL))
              (yylhs.value.val) = (*((yystack_[5].value.tptr)->value.fnctptr_cc))(
                  (yystack_[3].value.string), (yystack_[1].value.string));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1512 "apr_parser.cc"
          break;

          case 117:
#line 457 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.fnctptr_ccc == NULL))
              (yylhs.value.val) = (*((yystack_[7].value.tptr)->value.fnctptr_ccc))(
                  (yystack_[5].value.string), (yystack_[3].value.string),
                  (yystack_[1].value.string));
            else
              yyerrok;
          }
#line 1523 "apr_parser.cc"
          break;

          case 118:
#line 464 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[5].value.tptr),
                          (yystack_[5].value.tptr)->value.fnctptr_dd == NULL))
              (yylhs.value.val) = (*((yystack_[5].value.tptr)->value.fnctptr_dd))(
                  (yystack_[3].value.val), (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1534 "apr_parser.cc"
          break;

          case 119:
#line 470 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.fnctptr_ddd == NULL))
              (yylhs.value.val) = (*((yystack_[7].value.tptr)->value.fnctptr_ddd))(
                  (yystack_[5].value.val), (yystack_[3].value.val), (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1545 "apr_parser.cc"
          break;

          case 120:
#line 476 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[7].value.tptr),
                          (yystack_[7].value.tptr)->value.fnctptr_ccd == NULL))
              (yylhs.value.val) = (*((yystack_[7].value.tptr)->value.fnctptr_ccd))(
                  (yystack_[5].value.string), (yystack_[3].value.string), (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1556 "apr_parser.cc"
          break;

          case 121:
#line 482 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[9].value.tptr),
                          (yystack_[9].value.tptr)->value.fnctptr_dddd == NULL))
              (yylhs.value.val) = (*((yystack_[9].value.tptr)->value.fnctptr_dddd))(
                  (yystack_[7].value.val), (yystack_[5].value.val), (yystack_[3].value.val),
                  (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1567 "apr_parser.cc"
          break;

          case 122:
#line 488 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[9].value.tptr),
                          (yystack_[9].value.tptr)->value.fnctptr_dddd == NULL))
              (yylhs.value.val) = (*((yystack_[9].value.tptr)->value.fnctptr_dddd))(
                  (yystack_[7].value.val), (yystack_[5].value.val), (yystack_[3].value.val),
                  (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1578 "apr_parser.cc"
          break;

          case 123:
#line 494 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[11].value.tptr),
                          (yystack_[11].value.tptr)->value.fnctptr_ddddc == NULL))
              (yylhs.value.val) = (*((yystack_[11].value.tptr)->value.fnctptr_ddddc))(
                  (yystack_[9].value.val), (yystack_[7].value.val), (yystack_[5].value.val),
                  (yystack_[3].value.val), (yystack_[1].value.string));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1589 "apr_parser.cc"
          break;

          case 124:
#line 500 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if (arg_check((yystack_[13].value.tptr),
                          (yystack_[13].value.tptr)->value.fnctptr_dddddd == NULL))
              (yylhs.value.val) = (*((yystack_[13].value.tptr)->value.fnctptr_dddddd))(
                  (yystack_[11].value.val), (yystack_[9].value.val), (yystack_[7].value.val),
                  (yystack_[5].value.val), (yystack_[3].value.val), (yystack_[1].value.val));
            else
              (yylhs.value.val) = 0.0;
          }
#line 1600 "apr_parser.cc"
          break;

          case 125:
#line 506 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) + (yystack_[0].value.val);
          }
#line 1606 "apr_parser.cc"
          break;

          case 126:
#line 507 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) - (yystack_[0].value.val);
          }
#line 1612 "apr_parser.cc"
          break;

          case 127:
#line 508 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[2].value.val) * (yystack_[0].value.val);
          }
#line 1618 "apr_parser.cc"
          break;

          case 128:
#line 509 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if ((yystack_[0].value.val) == 0.) {
              yyerror(aprepro, "Zero divisor");
              yyerrok;
            }
            else
              (yylhs.value.val) = (yystack_[2].value.val) / (yystack_[0].value.val);
          }
#line 1630 "apr_parser.cc"
          break;

          case 129:
#line 516 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            if ((yystack_[0].value.val) == 0.) {
              yyerror(aprepro, "Zero divisor");
              yyerrok;
            }
            else
              (yylhs.value.val) = (int)(yystack_[2].value.val) % (int)(yystack_[0].value.val);
          }
#line 1642 "apr_parser.cc"
          break;

          case 130:
#line 523 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = -(yystack_[0].value.val);
          }
#line 1648 "apr_parser.cc"
          break;

          case 131:
#line 524 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[0].value.val);
          }
#line 1654 "apr_parser.cc"
          break;

          case 132:
#line 525 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            reset_error();
            (yylhs.value.val) = std::pow((yystack_[2].value.val), (yystack_[0].value.val));
            SEAMS::math_error(aprepro, "Power");
          }
#line 1662 "apr_parser.cc"
          break;

          case 133:
#line 528 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = (yystack_[1].value.val);
          }
#line 1668 "apr_parser.cc"
          break;

          case 134:
#line 529 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            reset_error();
            (yylhs.value.val) =
                (double)((yystack_[1].value.val) < 0 ? -floor(-((yystack_[1].value.val)))
                                                     : floor((yystack_[1].value.val)));
            SEAMS::math_error(aprepro, "floor (int)");
          }
#line 1676 "apr_parser.cc"
          break;

          case 135:
#line 532 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = ((yystack_[0].value.val)) ? 1 : 0;
          }
#line 1682 "apr_parser.cc"
          break;

          case 136:
#line 533 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                ((yystack_[4].value.val)) ? ((yystack_[2].value.val)) : ((yystack_[0].value.val));
          }
#line 1688 "apr_parser.cc"
          break;

          case 137:
#line 534 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) =
                array_value((yystack_[3].value.tptr)->value.avar, (yystack_[1].value.val), 0);
          }
#line 1694 "apr_parser.cc"
          break;

          case 138:
#line 535 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
          {
            (yylhs.value.val) = array_value((yystack_[5].value.tptr)->value.avar,
                                            (yystack_[3].value.val), (yystack_[1].value.val));
          }
#line 1700 "apr_parser.cc"
          break;

          case 139:
#line 537 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
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
#line 1726 "apr_parser.cc"
          break;

          case 140:
#line 559 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"
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
#line 1750 "apr_parser.cc"
          break;

#line 1754 "apr_parser.cc"

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
        YY_STACK_PRINT();

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
        error(yysyntax_error_(yystack_[0].state, yyla));
      }

      if (yyerrstatus_ == 3) {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.type_get() == yyeof_)
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
      goto yyerrlab1;

    /*-------------------------------------------------------------.
    | yyerrlab1 -- common code for both syntax error and YYERROR.  |
    `-------------------------------------------------------------*/
    yyerrlab1:
      yyerrstatus_ = 3; // Each real token shifted decrements this.
      {
        stack_symbol_type error_token;
        for (;;) {
          yyn = yypact_[yystack_[0].state];
          if (!yy_pact_value_is_default_(yyn)) {
            yyn += yyterror_;
            if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yyterror_) {
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

        // Shift the error token.
        error_token.state = yyn;
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

  // Generate an error message.
  std::string Parser::yysyntax_error_(state_type yystate, const symbol_type &yyla) const
  {
    // Number of reported tokens (one for the "unexpected", one per
    // "expected").
    size_t yycount = 0;
    // Its maximum.
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    // Arguments of yyformat.
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];

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
         scanner and before detecting a syntax error.  Thus, state
         merging (from LALR or IELR) and default reductions corrupt the
         expected token list.  However, the list is correct for
         canonical LR with one exception: it will still contain any
         token that will not be accepted due to an error action in a
         later state.
    */
    if (!yyla.empty()) {
      int yytoken      = yyla.type_get();
      yyarg[yycount++] = yytname_[yytoken];
      int yyn          = yypact_[yystate];
      if (!yy_pact_value_is_default_(yyn)) {
        /* Start YYX at -YYN if negative to avoid negative indexes in
           YYCHECK.  In other words, skip the first -YYN actions for
           this state because they are default actions.  */
        int yyxbegin = yyn < 0 ? -yyn : 0;
        // Stay within bounds of both yycheck and yytname.
        int yychecklim = yylast_ - yyn + 1;
        int yyxend     = yychecklim < yyntokens_ ? yychecklim : yyntokens_;
        for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
          if (yycheck_[yyx + yyn] == yyx && yyx != yyterror_ &&
              !yy_table_value_is_error_(yytable_[yyx + yyn])) {
            if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM) {
              yycount = 1;
              break;
            }
            else
              yyarg[yycount++] = yytname_[yyx];
          }
      }
    }

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
    size_t yyi = 0;
    for (char const *yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount) {
        yyres += yytnamerr_(yyarg[yyi++]);
        ++yyp;
      }
      else
        yyres += *yyp;
    return yyres;
  }

  const signed char Parser::yypact_ninf_ = -37;

  const signed char Parser::yytable_ninf_ = -1;

  const short Parser::yypact_[] = {
      -37,  2,    -37,  -7,   281,  -37,  -37,  -37,  -37,  -37,  233,  284,  0,    311,  11,
      -5,   19,   27,   35,   399,  399,  -37,  399,  384,  399,  116,  163,  175,  178,  41,
      1151, 384,  399,  399,  399,  399,  399,  -37,  -37,  384,  399,  399,  399,  399,  399,
      -37,  -37,  384,  399,  399,  399,  399,  399,  399,  -37,  -37,  399,  399,  384,  225,
      339,  384,  482,  625,  36,   73,   399,  101,  1199, 889,  1103, 14,   -37,  14,   14,
      -37,  -37,  -37,  -37,  -37,  -37,  -37,  -37,  399,  399,  399,  -37,  384,  384,  399,
      384,  -37,  399,  399,  399,  399,  399,  399,  399,  -37,  399,  399,  399,  399,  399,
      399,  399,  399,  399,  399,  399,  384,  399,  399,  29,   1199, 1218, 1234, 1234, 1234,
      1234, 1234, 29,   1199, 1218, 1234, 1234, 1234, 1234, 1234, 29,   1199, 1218, 1199, 1234,
      1234, 1234, 1234, 1234, 1234, 1199, 1234, 624,  29,   1199, 1218, -37,  -15,  78,   654,
      -37,  25,   418,  684,  100,  425,  714,  399,  399,  399,  399,  14,   -37,  -37,  399,
      -37,  1165, 1185, 51,   1265, -37,  1279, -36,  1218, -36,  1250, -37,  1250, 12,   1234,
      12,   12,   12,   12,   12,   -37,  51,   1265, -37,  1279, -31,  -31,  -31,  -31,  -31,
      -31,  155,  155,  14,   -37,  14,   14,   14,   399,  69,   -37,  399,  -37,  399,  -37,
      -37,  399,  -37,  399,  -37,  -37,  399,  -37,  399,  -37,  1234, 1234, 1234, 1234, 14,
      399,  399,  1128, 399,  449,  916,  141,  595,  455,  485,  943,  515,  970,  744,  1199,
      1234, 96,   1234, 399,  -37,  -37,  -37,  399,  -37,  399,  399,  -37,  399,  -37,  -37,
      -37,  399,  -37,  399,  537,  997,  774,  833,  543,  479,  1024, 1234, -37,  -37,  399,
      -37,  399,  -37,  399,  -37,  -37,  804,  1051, 509,  399,  -37,  -37,  399,  565,  862,
      571,  -37,  399,  -37,  1078, -37};

  const unsigned char Parser::yydefact_[] = {
      2,   0,   1,   0,   0,   4,   3,   9,   71,  51,  99,  74,  52,  75,  53,  32,  0,   0,   0,
      0,   0,   8,   0,   0,   0,   0,   0,   135, 0,   0,   0,   0,   0,   0,   0,   0,   0,   102,
      103, 0,   0,   0,   0,   0,   0,   78,  79,  0,   0,   0,   0,   0,   0,   0,   90,  91,  0,
      0,   0,   0,   0,   0,   99,  74,  52,  0,   0,   135, 0,   0,   0,   131, 45,  130, 12,  72,
      100, 76,  88,  73,  101, 77,  89,  0,   0,   0,   7,   0,   0,   0,   0,   6,   0,   0,   0,
      0,   0,   0,   0,   5,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      43,  54,  104, 105, 106, 107, 108, 109, 41,  56,  80,  83,  84,  85,  86,  87,  40,  55,  81,
      59,  92,  94,  95,  96,  97,  98,  58,  93,  0,   42,  57,  82,  110, 0,   0,   0,   61,  0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   130, 25,  133, 0,   134, 0,   0,   19,  21,  20,
      22,  44,  0,   46,  48,  50,  47,  26,  0,   27,  28,  29,  30,  31,  64,  23,  17,  24,  18,
      10,  11,  13,  14,  15,  16,  125, 126, 128, 49,  127, 129, 132, 0,   137, 113, 0,   112, 0,
      111, 63,  0,   60,  0,   62,  39,  0,   33,  0,   38,  104, 80,  81,  82,  127, 0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   70,  136, 138, 139, 0,   116, 114, 115,
      0,   118, 0,   0,   69,  0,   65,  35,  34,  0,   37,  0,   0,   0,   0,   0,   0,   0,   0,
      140, 117, 120, 0,   119, 0,   68,  0,   67,  36,  0,   0,   0,   0,   122, 121, 0,   0,   0,
      0,   123, 0,   66,  0,   124};

  const signed char Parser::yypgoto_[] = {-37, -37, -37, -13, 104, 89, -4};

  const short Parser::yydefgoto_[] = {-1, 1, 6, 27, 28, 68, 179};

  const unsigned short Parser::yytable_[] = {
      30,  205, 2,   3,   89,  90,  67,  108, 109, 110, 164, 112, 57,  7,   113, 69,  70,  58,  71,
      73,  74,  4,   47,  87,  88,  89,  90,  116, 117, 118, 119, 120, 121, 56,  59,  124, 125, 126,
      127, 128, 129, 210, 60,  132, 134, 135, 136, 137, 138, 139, 61,  5,   141, 142, 145, 149, 153,
      156, 159, 113, 98,  91,  161, 87,  88,  89,  90,  87,  88,  89,  90,  168, 170, 92,  93,  94,
      95,  96,  97,  167, 169, 171, 85,  173, 173, 175, 177, 186, 188, 98,  57,  228, 206, 29,  207,
      160, 187, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 200, 201, 202, 92,  93,  94,  95,
      96,  97,  215, 162, 258, 75,  115, 76,  77,  0,   78,  0,   98,  72,  123, 83,  0,   84,  85,
      0,   0,   114, 131, 133, 87,  88,  89,  90,  0,   122, 0,   140, 0,   144, 148, 152, 155, 130,
      0,   220, 221, 222, 223, 246, 0,   0,   224, 0,   143, 147, 151, 154, 79,  0,   80,  81,  0,
      82,  166, 92,  93,  94,  95,  96,  97,  0,   0,   178, 180, 181, 182, 183, 184, 185, 0,   98,
      0,   172, 174, 0,   176, 110, 164, 112, 86,  227, 113, 0,   230, 83,  232, 84,  85,  0,   0,
      235, 0,   0,   237, 0,   238, 199, 87,  88,  89,  90,  0,   0,   240, 0,   242, 0,   0,   0,
      8,   9,   10,  11,  12,  13,  14,  15,  16,  17,  18,  260, 19,  146, 20,  261, 0,   262, 115,
      123, 131, 144, 0,   0,   265, 0,   266, 31,  32,  33,  34,  35,  36,  0,   0,   22,  23,  276,
      0,   277, 0,   24,  0,   25,  26,  0,   0,   284, 0,   0,   0,   37,  38,  0,   0,   289, 8,
      9,   10,  11,  12,  13,  14,  15,  16,  17,  18,  229, 19,  231, 20,  0,   233, 21,  234, 0,
      0,   236, 39,  40,  41,  42,  43,  44,  0,   0,   239, 0,   0,   0,   0,   22,  23,  0,   0,
      0,   0,   24,  0,   25,  26,  0,   45,  46,  259, 48,  49,  50,  51,  52,  53,  263, 0,   264,
      8,   9,   10,  11,  12,  13,  14,  15,  16,  17,  18,  0,   19,  150, 20,  54,  55,  0,   0,
      0,   278, 0,   0,   0,   0,   0,   283, 0,   0,   285, 0,   0,   0,   0,   0,   22,  23,  0,
      0,   0,   0,   24,  0,   25,  26,  8,   9,   10,  11,  12,  13,  14,  15,  16,  17,  18,  0,
      19,  0,   20,  8,   9,   62,  63,  64,  13,  14,  65,  16,  17,  0,   0,   19,  0,   20,  0,
      0,   0,   0,   0,   22,  23,  0,   0,   0,   0,   24,  0,   25,  26,  211, 0,   212, 0,   0,
      22,  66,  216, 0,   217, 0,   24,  0,   25,  26,  0,   0,   0,   92,  93,  94,  95,  96,  97,
      0,   92,  93,  94,  95,  96,  97,  243, 0,   244, 98,  0,   0,   250, 0,   251, 0,   98,  0,
      0,   0,   0,   0,   0,   0,   92,  93,  94,  95,  96,  97,  92,  93,  94,  95,  96,  97,  273,
      0,   274, 0,   98,  0,   252, 0,   0,   0,   98,  157, 32,  33,  34,  35,  36,  0,   92,  93,
      94,  95,  96,  97,  92,  93,  94,  95,  96,  97,  282, 0,   0,   0,   98,  37,  38,  0,   254,
      0,   98,  0,   0,   0,   0,   0,   0,   0,   92,  93,  94,  95,  96,  97,  92,  93,  94,  95,
      96,  97,  267, 0,   0,   0,   98,  0,   272, 0,   0,   0,   98,  0,   0,   0,   0,   0,   92,
      93,  94,  95,  96,  97,  92,  93,  94,  95,  96,  97,  286, 0,   0,   0,   98,  0,   288, 0,
      0,   0,   98,  0,   0,   0,   0,   0,   92,  93,  94,  95,  96,  97,  92,  93,  94,  95,  96,
      97,  247, 0,   248, 0,   98,  0,   0,   249, 0,   0,   98,  0,   0,   0,   0,   0,   100, 101,
      102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112, 203, 0,   113, 0,   204, 0,   0,   0,
      0,   158, 40,  41,  42,  43,  44,  0,   100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
      164, 112, 0,   208, 113, 209, 45,  46,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 0,   213, 113, 214, 0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   100, 101, 102, 103, 104, 105, 106, 107,
      108, 109, 110, 111, 112, 0,   218, 113, 219, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 0,   256, 113,
      257, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   100, 101, 102, 103, 104,
      105, 106, 107, 108, 109, 110, 164, 112, 0,   269, 113, 270, 0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112,
      0,   279, 113, 280, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   100, 101,
      102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112, 271, 0,   113, 0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
      164, 112, 287, 0,   113, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   100,
      101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112, 163, 0,   113, 0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 164,
      112, 245, 0,   113, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   100, 101, 102, 103,
      104, 105, 106, 107, 108, 109, 110, 164, 112, 253, 0,   113, 0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112, 255, 0,
      113, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   100, 101, 102, 103, 104, 105, 106,
      107, 108, 109, 110, 164, 112, 268, 0,   113, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112, 275, 0,   113, 0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
      110, 164, 112, 281, 0,   113, 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   100, 101,
      102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112, 290, 0,   113, 0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112,
      165, 0,   113, 0,   0,   0,   0,   0,   0,   0,   0,   0,   100, 101, 102, 103, 104, 105, 106,
      107, 108, 109, 110, 164, 112, 241, 0,   113, 0,   0,   0,   0,   0,   0,   0,   0,   0,   100,
      101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112, 99,  0,   113, 0,   0,   0,   0,
      0,   0,   0,   100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 225, 0,   113,
      92,  93,  94,  95,  96,  97,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   98,  226, 100,
      101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112, 0,   0,   113, 92,  93,  94,  95,
      96,  97,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   98,  100, 101, 102, 103, 104, 105,
      106, 107, 108, 109, 110, 111, 112, 0,   0,   113, 100, 101, 102, 103, 104, 105, 106, 107, 108,
      109, 110, 164, 112, 0,   0,   113, 100, 101, 102, 103, 104, 105, 106, 107, 0,   0,   0,   0,
      112, 0,   0,   113, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112, 0,   0,   113,
      102, 103, 104, 105, 106, 107, 108, 109, 110, 164, 112, 0,   0,   113};

  const short Parser::yycheck_[] = {
      4,   16,  0,   1,   40,  41,  19,  38,  39,  40,  41,  42,  17,  20,  45,  19,  20,  22,  22,
      23,  24,  19,  22,  38,  39,  40,  41,  31,  32,  33,  34,  35,  36,  22,  15,  39,  40,  41,
      42,  43,  44,  16,  15,  47,  48,  49,  50,  51,  52,  53,  15,  49,  56,  57,  58,  59,  60,
      61,  22,  45,  48,  20,  66,  38,  39,  40,  41,  38,  39,  40,  41,  84,  85,  32,  33,  34,
      35,  36,  37,  83,  84,  85,  31,  87,  88,  89,  90,  100, 101, 48,  17,  22,  14,  4,   16,
      22,  100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 32,  33,  34,  35,
      36,  37,  16,  16,  22,  3,   31,  5,   6,   -1,  8,   -1,  48,  23,  39,  28,  -1,  30,  31,
      -1,  -1,  31,  47,  48,  38,  39,  40,  41,  -1,  39,  -1,  56,  -1,  58,  59,  60,  61,  47,
      -1,  157, 158, 159, 160, 16,  -1,  -1,  164, -1,  58,  59,  60,  61,  3,   -1,  5,   6,   -1,
      8,   83,  32,  33,  34,  35,  36,  37,  -1,  -1,  92,  93,  94,  95,  96,  97,  98,  -1,  48,
      -1,  87,  88,  -1,  90,  40,  41,  42,  20,  203, 45,  -1,  206, 28,  208, 30,  31,  -1,  -1,
      213, -1,  -1,  216, -1,  218, 111, 38,  39,  40,  41,  -1,  -1,  226, -1,  228, -1,  -1,  -1,
      3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  243, 15,  16,  17,  247, -1,  249, 157,
      158, 159, 160, -1,  -1,  256, -1,  258, 22,  23,  24,  25,  26,  27,  -1,  -1,  38,  39,  269,
      -1,  271, -1,  44,  -1,  46,  47,  -1,  -1,  279, -1,  -1,  -1,  46,  47,  -1,  -1,  287, 3,
      4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  206, 15,  208, 17,  -1,  211, 20,  213, -1,
      -1,  216, 22,  23,  24,  25,  26,  27,  -1,  -1,  225, -1,  -1,  -1,  -1,  38,  39,  -1,  -1,
      -1,  -1,  44,  -1,  46,  47,  -1,  46,  47,  243, 22,  23,  24,  25,  26,  27,  250, -1,  252,
      3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  -1,  15,  16,  17,  46,  47,  -1,  -1,
      -1,  273, -1,  -1,  -1,  -1,  -1,  279, -1,  -1,  282, -1,  -1,  -1,  -1,  -1,  38,  39,  -1,
      -1,  -1,  -1,  44,  -1,  46,  47,  3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  -1,
      15,  -1,  17,  3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  -1,  -1,  15,  -1,  17,  -1,
      -1,  -1,  -1,  -1,  38,  39,  -1,  -1,  -1,  -1,  44,  -1,  46,  47,  14,  -1,  16,  -1,  -1,
      38,  39,  14,  -1,  16,  -1,  44,  -1,  46,  47,  -1,  -1,  -1,  32,  33,  34,  35,  36,  37,
      -1,  32,  33,  34,  35,  36,  37,  14,  -1,  16,  48,  -1,  -1,  14,  -1,  16,  -1,  48,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  32,  33,  34,  35,  36,  37,  32,  33,  34,  35,  36,  37,  14,
      -1,  16,  -1,  48,  -1,  14,  -1,  -1,  -1,  48,  22,  23,  24,  25,  26,  27,  -1,  32,  33,
      34,  35,  36,  37,  32,  33,  34,  35,  36,  37,  14,  -1,  -1,  -1,  48,  46,  47,  -1,  16,
      -1,  48,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  32,  33,  34,  35,  36,  37,  32,  33,  34,  35,
      36,  37,  16,  -1,  -1,  -1,  48,  -1,  16,  -1,  -1,  -1,  48,  -1,  -1,  -1,  -1,  -1,  32,
      33,  34,  35,  36,  37,  32,  33,  34,  35,  36,  37,  16,  -1,  -1,  -1,  48,  -1,  16,  -1,
      -1,  -1,  48,  -1,  -1,  -1,  -1,  -1,  32,  33,  34,  35,  36,  37,  32,  33,  34,  35,  36,
      37,  14,  -1,  16,  -1,  48,  -1,  -1,  21,  -1,  -1,  48,  -1,  -1,  -1,  -1,  -1,  30,  31,
      32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  14,  -1,  45,  -1,  18,  -1,  -1,  -1,
      -1,  22,  23,  24,  25,  26,  27,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
      41,  42,  -1,  14,  45,  16,  46,  47,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  14,  45,  16,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,
      38,  39,  40,  41,  42,  -1,  14,  45,  16,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  14,  45,
      16,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,
      35,  36,  37,  38,  39,  40,  41,  42,  -1,  14,  45,  16,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,
      -1,  14,  45,  16,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,
      32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  14,  -1,  45,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
      41,  42,  14,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,
      31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  16,  -1,  45,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,
      42,  16,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,
      34,  35,  36,  37,  38,  39,  40,  41,  42,  16,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  16,  -1,
      45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,
      37,  38,  39,  40,  41,  42,  16,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  16,  -1,  45,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,
      40,  41,  42,  16,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,
      32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  16,  -1,  45,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,
      18,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,
      37,  38,  39,  40,  41,  42,  18,  -1,  45,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  30,
      31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  20,  -1,  45,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  29,  -1,  45,
      32,  33,  34,  35,  36,  37,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  48,  29,  30,
      31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  -1,  45,  32,  33,  34,  35,
      36,  37,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  48,  30,  31,  32,  33,  34,  35,
      36,  37,  38,  39,  40,  41,  42,  -1,  -1,  45,  30,  31,  32,  33,  34,  35,  36,  37,  38,
      39,  40,  41,  42,  -1,  -1,  45,  30,  31,  32,  33,  34,  35,  36,  37,  -1,  -1,  -1,  -1,
      42,  -1,  -1,  45,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  -1,  45,
      32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  -1,  -1,  45};

  const unsigned char Parser::yystos_[] = {
      0,  51, 0,  1,  19, 49, 52, 20, 3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 15, 17, 20, 38,
      39, 44, 46, 47, 53, 54, 55, 56, 22, 23, 24, 25, 26, 27, 46, 47, 22, 23, 24, 25, 26, 27, 46,
      47, 22, 22, 23, 24, 25, 26, 27, 46, 47, 22, 17, 22, 15, 15, 15, 5,  6,  7,  10, 39, 53, 55,
      56, 56, 56, 54, 56, 56, 3,  5,  6,  8,  3,  5,  6,  8,  28, 30, 31, 20, 38, 39, 40, 41, 20,
      32, 33, 34, 35, 36, 37, 48, 20, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 45, 54,
      55, 56, 56, 56, 56, 56, 56, 54, 55, 56, 56, 56, 56, 56, 56, 54, 55, 56, 55, 56, 56, 56, 56,
      56, 56, 55, 56, 56, 54, 55, 56, 16, 54, 55, 56, 16, 54, 55, 56, 54, 55, 56, 22, 22, 22, 22,
      56, 16, 16, 41, 18, 55, 56, 53, 56, 53, 56, 54, 56, 54, 56, 54, 56, 55, 56, 55, 55, 55, 55,
      55, 55, 53, 56, 53, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 54, 56, 56, 56, 14, 18, 16, 14,
      16, 14, 16, 16, 14, 16, 14, 16, 16, 14, 16, 14, 16, 56, 56, 56, 56, 56, 29, 29, 56, 22, 55,
      56, 55, 56, 55, 55, 56, 55, 56, 56, 55, 56, 18, 56, 14, 16, 16, 16, 14, 16, 21, 14, 16, 14,
      16, 16, 16, 14, 16, 22, 55, 56, 56, 56, 55, 55, 56, 56, 16, 16, 14, 16, 14, 16, 14, 16, 16,
      56, 56, 55, 14, 16, 16, 14, 55, 56, 55, 16, 14, 16, 56, 16};

  const unsigned char Parser::yyr1_[] = {
      0,  50, 51, 51, 52, 52, 52, 52, 52, 52, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53,
      53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55,
      55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56};

  const unsigned char Parser::yyr2_[] = {
      0, 2, 0, 2, 1, 3,  3,  3,  2,  2, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 1, 4, 6,  6,  8,  6,  4, 4, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 1, 1, 1, 3, 3, 3, 3,
      3, 3, 4, 3, 4, 4,  3,  6,  12, 8, 8, 6, 5, 1, 2, 2, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
      3, 2, 2, 2, 2, 3,  3,  3,  3,  3, 3, 3, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 6, 6,
      6, 8, 6, 8, 8, 10, 10, 12, 14, 3, 3, 3, 3, 3, 2, 2, 3, 3, 3, 1, 5, 4, 6, 6, 8};

  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a yyntokens_, nonterminals.
  const char *const Parser::yytname_[] = {"\"end of file\"",
                                          "error",
                                          "$undefined",
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

#if SEAMSDEBUG
  const unsigned short Parser::yyrline_[] = {
      0,   125, 125, 126, 129, 130, 137, 141, 142, 143, 146, 147, 148, 149, 150, 151, 152, 153,
      154, 155, 156, 157, 158, 159, 160, 161, 164, 165, 166, 167, 168, 169, 171, 172, 178, 184,
      190, 196, 202, 208, 214, 218, 222, 225, 227, 235, 237, 245, 246, 247, 248, 257, 258, 259,
      260, 262, 265, 269, 274, 275, 276, 282, 288, 294, 300, 301, 307, 313, 319, 325, 331, 333,
      334, 335, 336, 337, 338, 339, 340, 341, 342, 344, 347, 352, 353, 354, 355, 356, 361, 362,
      363, 364, 365, 366, 367, 368, 369, 370, 371, 373, 375, 378, 381, 384, 387, 389, 392, 395,
      398, 401, 408, 415, 422, 429, 436, 443, 450, 457, 464, 470, 476, 482, 488, 494, 500, 506,
      507, 508, 509, 516, 523, 524, 525, 528, 529, 532, 533, 534, 535, 536, 558};

  // Print the state stack on the debug stream.
  void Parser::yystack_print_()
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator i = yystack_.begin(), i_end = yystack_.end(); i != i_end; ++i)
      *yycdebug_ << ' ' << i->state;
    *yycdebug_ << '\n';
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void Parser::yy_reduce_print_(int yyrule)
  {
    unsigned yylno  = yyrline_[yyrule];
    int      yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1 << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT("   $" << yyi + 1 << " =", yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // SEAMSDEBUG

  Parser::token_number_type Parser::yytranslate_(int t)
  {
    // YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to
    // TOKEN-NUM as returned by yylex.
    static const token_number_type translate_table[] = {
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
    const unsigned          user_token_number_max_ = 303;
    const token_number_type undef_token_           = 2;

    if (static_cast<int>(t) <= yyeof_)
      return yyeof_;
    else if (static_cast<unsigned>(t) <= user_token_number_max_)
      return translate_table[t];
    else
      return undef_token_;
  }

} // namespace SEAMS
#line 2577 "apr_parser.cc"

#line 581 "/fgs/gdsjaar/seacas/packages/seacas/libraries/aprepro_lib/aprepro.yy"

void SEAMS::Parser::error(const std::string &m) { aprepro.error(m); }
