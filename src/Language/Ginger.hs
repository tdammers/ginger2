{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | [Jinja](https://jinja.palletsprojects.com/) is a dynamically-typed
-- template language designed for generating HTML and text output.
--
-- Ginger2 provides most of the Jinja template language, as much as that makes
-- sense in a Haskell host application.
--
-- We do, however, avoid some of the most blatant Pythonisms, especially where we
-- felt the features in question were more of an accidental result of binding
-- template constructs to Python constructs.
--
-- We also add some optional features that are absent from the Python
-- implementation, but that we felt useful and worth adding.
--
-- = Template Syntax
--
-- == __Minimal Example__
--
-- > <!DOCTYPE html>
-- > <html>
-- >     <head>
-- >         <title>{{ title }}</title>
-- >     </head>
-- >     {# This is a comment. Comments are removed from the output. #}
-- >     <body>
-- >         <menu id="nav-main">
-- >         {% for item in navigation %}
-- >             <li><a href="{{ item.url }}">{{ item.label }}</a></li>
-- >         {% endfor %}
-- >         </menu>
-- >         <div class="layout-content-main">
-- >             <h1>{{ title }}</h1>
-- >             {{ body }}
-- >         </div>
-- >     </body>
-- > </html>
--
-- == __Comments__
--
-- Comments are delimited by @{#@ ... @#}@ markers.
--
-- It is not currently possible to nest comments.
--
-- == __Interpolation__
--
-- The @{{@ and @}}@ markers delimit /interpolation/: anything between these
-- markers is interpreted as an /expression/, and the result of evaluating that
-- expression is written into the template output at this point.
--
-- The following expression constructs are available:
--
-- === Literals
--
-- ==== String Literals
-- String literals can be written with single quotes:
--
-- > 'Hello, world!'
--
-- ...or double quotes:
--
-- > "Hello, world!"
--
-- Quotes inside string literals can be escaped using backslashes:
--
-- > "Hello, \"world!\""
--
-- ==== Numeric Literals
-- Integers can be written in decimal:
--
-- > 23
--
-- Floats can be written as decimal fractions:
--
-- > 23.5
--
-- ...or in scientific notation:
--
-- > 0.25e10
--
-- ==== Booleans
-- Two boolean literals exist: @true@ and @false@. For backwards compatibility
-- with Jinja, @True@ and @False@ are accepted as alternative spellings.
--
-- ==== None
-- The literal @none@ designates the @NULL@ object (approximately equivalent
-- to '()' or 'Nothing' in Haskell). For backwards compatibility with Jinja,
-- @None@ is accepted as an alternative spelling.
--
-- === Variables
-- Variables (built-in, passed in by the host application, or defined within
-- the template itself) are written as barewords. Their names must follow the
-- rules for identifiers:
--
-- * They must start with an alphabetic ASCII character or an underscore.
-- * All subsequent characters must be alphanumeric ASCII characters or
--   underscores.
--
-- === Lists
-- List literals are written as comma-separated lists between square brackets,
-- like in most programming languages:
--
-- > ["foo", "bar", "baz"]
--
-- Because Ginger is dynamically typed, lists are heterogenous, so the
-- following is perfectly fine:
--
-- > ["foo", 23, none, true, 1.5]
--
-- Unlike Jinja, Ginger does not distinguish lists from tuples; there is only
-- one list type, and all lists in Ginger are immutable.
--
-- === Dictionaries
-- Dictionaries ("dicts") are key-value association containers. They are
-- written as comma-separated lists of dot-separated key/value pairs between
-- curly braces, like so:
--
-- > { "foo": 23, "bar": 42 }
--
-- Keys and values are both interpreted as expressions; this means that string
-- keys must be written as string literals, not barewords - barewords are
-- interpreted as variables, so for example @{ foo: "bar" }@ will not make a
-- dictionary key \"foo\", but rather try to look up a variable named @foo@ in
-- the current scope, and use its value as the key.
--
-- Values can be anything; keys, however, must be scalars, that is, strings,
-- numbers, booleans, or @none@.
--
-- === Dot Member Access
-- Members of dictionaries and (some) native objects can be obtained using dot
-- access syntax, like so:
--
-- > someDictionary.foo
--
-- Note that the member name is written as a bareword; these are *not*
-- interpreted as variables, and quoting member names in dot syntax like string
-- literals is a syntax error.
-- Dot syntax can also be used to access built-in methods of various types of
-- values; for example, the @string@ type provides a method @upper()@, which
-- returns the original string converted to uppercase, so you can write:
--
-- > "Hello, world!".upper()
--
-- ...which will yield the string @"HELLO, WORLD!"@.
--
-- Dot syntax will prioritize *attributes* (built-in properties) over *items*
-- (data items stored in a dictionary).
--
-- === Indexing
-- Members of dictionaries, as well as elements of lists and characters in a
-- string, can be accessed using an index between square brackets, as follows.
--
-- Get an item from a list:
--
-- > someList[3]
--
-- Get a member/item from a dictionary:
--
-- > someDict["foo"]
--
-- Get the n-th character from a string:
--
-- > someString[3]
--
-- Note that strings and lists will only accept integer indices, while
-- dictionaries and native objects may accept arbitrary scalars.
--
-- Note further that, unlike dot syntax, the thing between square brackets is
-- evaluated as an expression, so if you want to access a string key in a
-- dictionary, it must be written as a quoted string literal. The following is
-- an example of accessing a dictionary member through a variable key:
--
-- > someDict[foo]
--
-- This will *not* look up a key named "foo", but rather, try to find a
-- variable named "foo" in the current scope, get its value, and use that as a
-- key to look up in @someDict@.
--
-- Indices into strings and lists are zero-based, that is, @[0]@ yields the
-- first element of a list, or the first character of a string.
--
-- Square bracket syntax will prioritize *items* (data items stored in a
-- container) over *attributes* (built-in methods associated with a value).
--
-- === Slicing
-- Square bracket syntax can also be used to get sub-ranges from a list or
-- string, using the colon (@:@) to separate the start and end of the desired
-- range. Negative indices imply counting from the end of the range; absent
-- indices refer to the start or end of the original sequence respectively.
--
-- Examples:
--
-- > "Hello, world!"[1:2]
--
-- Get a substring of length 2, starting at the second character: "el".
--
-- > "Hello, world!"[:2]
--
-- Get a substring of length 2, starting at the beginning of the string: "He".
--
-- > "Hello, world!"[-2:]
--
-- Get a substring up to the end of the string, starting two characters before
-- the end: "d!".
--
-- > "Hello, world!"[2:-2]
--
-- Get a substring from the third character up to 2 characters before the end
-- of the string: "llo, worl".
--
-- === Unary Operators
-- Two unary operators are available, both written in prefix notation:
--
-- * @not@ provides boolean negation
-- * @-@ provides numeric negation
--
-- === Binary Operators
-- All binary operators are written in infix notation. In order of precedence,
-- the following classes of operators are available:
--
-- ==== Boolean
--
-- * @and@ - boolean AND
-- * @or@ - boolean OR
--
-- ==== Boolean Negation
--
-- Not a binary operator, but listed here to indicate precedence; see above.
--
-- ==== Comparative and Membership
--
-- * @==@ - equals
-- * @!=@ - not-equals
-- * @<@ - less-than; works on numbers as well as strings
-- * @>@ - greater-than; dito
-- * @<=@ - less-than-or-equal; dito
-- * @>=@ - greater-than-or-equal; dito
-- * @in@ - membership test
--
-- ==== Test
--
-- Not a real operator, but listed here to indicate precendence. See below for
-- details on tests.
--
-- ==== Concatenation
--
-- * @~@ - String concatenation. If non-string arguments are given, they are
--         converted to string. However, if either argument is encoded text,
--         then the other argument will also be encoded first.
--
-- ==== Additive
--
-- These operations are numeric. If both arguments are integers, integer
-- operations will be used, otherwise, both arguments will be converted to
-- float.
--
-- * @+@ - Numeric addition.
-- * @-@ - Numeric subtraction.
--
-- ==== Multiplicative
--
-- These operations are numeric. If both arguments are integers, integer
-- operations will be used, otherwise, both arguments will be converted to
-- float, unless specified otherwise.
--
-- * @*@ - Numeric multiplication.
-- * @/@ - Numeric division.
-- * @%@ - Integer modulus. Both arguments will be converted to int.
-- * @//@ - Integer division. Both arguments will be converted to int.
--
-- ==== Power
--
-- Numeric operation; if both arguments are integers, integer arithmetic will
-- be used, otherwise, both arguments will be cast to float.
--
-- * @**@ Exponentiation: @a ** b@ means \"a to the power of b\".
--
-- ==== Member access, filter, call
--
-- Not operators, but listed here to indicate precedence. See respective
-- sections for details.
--
-- === Ternary Operator
-- The ternary operator consists of the keywords @if@ and @else@, and works
-- much like in Python:
--
-- > "foo" if condition else "bar"
--
-- ...means \"evaluate to \'foo\' if @condition@ holds, otherwise, evaluate to
-- \'bar\'\".
--
-- === Procedure Calls
-- Procedure calls are written like in most imperative languages, appending
-- a comma-separated list of arguments between parentheses to the procedure to
-- be called.
--
-- To call a procedure @foo@ with two arguments, @1@ and @2@:
--
-- > foo(1, 2)
--
-- Procedures may have optional arguments (which can be left out, using a
-- default value instead), and arguments can also be given by name instead of
-- positionally, e.g.:
--
-- > foo(bar=23)
--
-- This calls procedure @foo@ with the @bar@ argument set to @23@, and any other
-- arguments left at their defaults.
--
-- Some objects can be called as procedures while also offering other APIs; an
-- exampe is the @loop@ object that is available within recursive @for@ loops
-- (see below), which exposes a number of fields providing information about
-- the state of the iteration, but can also be called as a procedure to recurse
-- into a deeper iteration level.
--
-- === Filters
-- Filter syntax is a convenient syntactic alternative to procedure call
-- syntax. It looks like this:
--
-- > foo|center(10)
--
-- This will call the @center@ filter, passing the value of @foo@ as the first
-- argument, and @10@ as a second argument. Thus, it is equivalent to:
--
-- > center(foo, 10)
--
-- If no additional arguments are passed, the parentheses can be omitted:
--
-- > foo|capitalize
--
-- This syntax is particularly useful when chaining multiple filters, e.g.:
--
-- > foo|default('n/a')|lower|trim
--
-- One filter, @default@, and its alias @d@, cannot be implemented as
-- procedures, because it must inspect its argument as an unevaluated
-- expression - evaluating it before passing it to the filter would cause a
-- \"Not In Scope\" error when the argument isn't defined, making the entire
-- filter moot. Hence, this filter is only available through filter syntax, not
-- as a procedure.
--
-- === Tests
--
-- Tests are written using the @is@ keyword, much like a binary operator;
-- however, they are special-cased in the language, for two reasons:
-- * They can inspect their argument unevaluated (e.g., the @defined@ test will
--   do this to determine whether the argument is in scope).
-- * Some tests have the same name as a procedure or filter, but their
--   functionality is different in a text context. E.g., @lower@, when used as
--   a filter of procedure will convert its argument to lowercase, but when
--   used as a test, it will instead check whether the argument is lowercase.
--
-- In Jinja, tests may only occur in contexts where a boolean condition is
-- expected (e.g., the ternary operator, or an @{% if ... %}@ statement);
-- Ginger allows tests in any expression context, and treats the result as a
-- boolean value. For example, the following would be perfectly fine in Ginger,
-- but (probably) not work in Jinja:
--
-- > {% set answers = { false: "odd", true: "even" } %}
-- > Foo is {{ answers[foo is even] }}.
--
-- == __Flow Control Statements__
--
-- All statements are delimited using statement brackets: @{%@ ... @%}@.
--
-- === @{% filter %}@
-- Apply a filter (see above, filter expressions) to a block of template code.
--
-- > {% filter 'capitalize' %}
-- > Hello, world!
-- > {% endfilter %}
--
-- ...will output:
--
-- > HELLO, WORLD!
--
-- The filter itself may be specified as an expression (e.g. @{% filter
-- capitalize %}@), or as a string that will be resolved as a variable (e.g.
-- @{%filter 'capitalize' %}@. Both are equivalent.
--
-- Additional arguments may be passed just like with filter expression syntax:
--
-- > {% filter center(100) %}
-- > Hello, world!
-- > {% endfilter %}
--
-- === @{% for %}@
--
-- Loops over a collection (list or dictionary).
--
-- In its simplest form, it looks like this:
--
-- > {% for user in users %}
-- > {{ user }}
-- > {% endfor %}
--
-- This will iterate over the elements of a list in the variable @users@,
-- binding the current element to the variable @user@ within the scope of the
-- iteration body.
--
-- Inside of a for-loop block, you can access some special variables:
--
-- * @loop.index@ - The current iteration of the loop. (1 indexed)
-- * @loop.index0@ - The current iteration of the loop. (0 indexed)
-- * @loop.revindex@ - The number of iterations from the end of the loop (1
--   indexed)
-- * @loop.revindex0@ - The number of iterations from the end of the loop (0
--   indexed)
-- * @loop.first@ - True if first iteration.
-- * @loop.last@ - True if last iteration.
-- * @loop.length@ - The number of items in the sequence.
-- * @loop.cycle@ - A helper function to cycle between a list of sequences. See
--   the explanation below.
-- * @loop.depth@ - Indicates how deep in a recursive loop the rendering
--   currently is. Starts at level 1
-- * @loop.depth0@ - Indicates how deep in a recursive loop the rendering
--   currently is. Starts at level 0
-- * @loop.previtem@ - The item from the previous iteration of the loop.
--   Undefined during the first iteration.
-- * @loop.nextitem@ - The item from the following iteration of the loop.
--   Undefined during the last iteration.
-- * @loop.changed(val)@ - True if previously called with a different value
--   (or not called at all).
--
-- While there are no @continue@ or @break@ statements to alter the flow of
-- a loop from within, it is possible to filter the iteree, by adding an @if@
-- construct to the loop header:
--
-- > {% for user in users if user.username is not none %}
-- > {{ user.username }}
-- > {% endfor %}
--
-- The same effect can be achieved by wrapping the loop body in an @if@
-- statement; however, filtering the loop iteree has the advantage that the
-- @loop@ variables will count correctly.
--
-- E.g., given a list of users like so:
--
-- > [ { "username": "tdammers" },
-- >   { "username": none },
-- >   { "username": "jdoe" }
-- > ]
--
-- This template will work correctly:
--
-- > {% for user in users if user.username is not none %}
-- > {{ loop.index }}. {{ user.username }}
-- > {% endfor %}
--
-- ...outputting:
--
-- > 1. tdammers
-- > 2. jdoe
--
-- Whereas this template:
--
-- > {% for user in users %}
-- > {% if user.username is not none %}
-- > {{ loop.index }}. {{ user.username }}
-- > {% endif %}
-- > {% endfor %}
--
-- ...would output:
--
-- > 1. tdammers
-- > 3. jdoe
--
-- An optional @else@ branch can be added to a loop, which will be used when
-- the iteration body has not been used at all (because the list was empty, or
-- because all items were filtered out):
--
-- > {% for user in users %}
-- > <p>{{ user.username }}</p>
-- > {% else %}
-- > <p class="no-results">No users found.</p>
-- > {% endfor %}
--
-- Loops can also be used recursively. For this, two things are required:
--
-- 1. The loop needs to be declared as being recursive: @{% for ... in ...
--    recursive %}@
-- 2. The @loop@ variable must be called with an appropriate iteree in order to
--    recurse into it.
--
-- Example:
--
-- > {% for branch in tree recursive %}
-- > <section>
-- > <h3>{{ branch.name }}</h3>
-- > <p>{{ branch.description }}</p>
-- > {% if "children" in branch %}
-- > {{ loop(branch.children) }}
-- > {% endif %}
-- > {% endfor %}
--
-- Please note that assignments in loops will be cleared at the end of the
-- iteration and cannot outlive the loop scope.
--
-- To work around this, consider using namespace objects, created using the
-- @namespace@ procedure. However, if all you need to do is check the previous
-- and/or next item in the iteration, you can simply use @loop.prev@ and
-- @loop.next@.
--
-- === @{% if %}@
--
-- Conditionals. These work much like if / then / else in a typical imperative
-- language. Three forms exist:
--
-- Simple @if@ with no @else@:
--
-- > {% if isGreeting %}
-- > Hi!
-- > {% endif %}
--
-- @if@ / @else@:
--
-- > {% if isArriving %}
-- > Hello!
-- > {% else %}
-- > Goodbye!
-- > {% endif %}
--
-- @if@ / @elif@ / @else@:
--
-- > {% if isMorning %}
-- > Good morning!
-- > {% elif isEvening %}
-- > Good night!
-- > {% else %}
-- > Good day!
-- > {% endif %}
--
-- === @{% set %}@
--
-- Assignment. There are two forms of the @set@ statement.
--
-- First form: assign an expression.
--
-- > {% set name = "tdammers" %}
--
-- Second form: assign a block of encoded output.
--
-- > {% set name %}
-- > tdammers
-- > {% endset %}
--
-- Variables set using either construct will be available within the current
-- scope; includes, imports, template inheritance, the @{% with %}@ statement,
-- and @{% for %}@ loops can affect scope.
--
-- === @{% with %}@
--
-- Creates a nested scope. Any variables set or overwritten within the nested
-- scope will only be reflected inside the nested scope; any variables from the
-- containing scope will be available until overridden, and will revert back to
-- their previous values when leaving the inner scope.
--
-- > {% set foo = "A" %}
-- > {{ foo }}
-- > {% with %}
-- > {{ foo }}
-- > {% set foo = "B" %}
-- > {{ foo }}
-- > {% endwith %}
-- > {{ foo }}
--
-- Will yield:
--
-- > A
-- > A
-- > B
-- > A
--
-- === @{% macro %}@
--
-- Defines a macro. Macros are reusable bits of Jinja code, akin to procedures.
-- In fact, macros and procedures are represented as the same thing internally
-- in Ginger, and you can call procedures as macros (using the @{% call %}@
-- statement), and macros as procedures or filters (using expression-level
-- procedure call or filter syntax).
--
-- A macro definition looks like this:
--
-- > {% macro userInfo(user, extraInfo=none) %}
-- > <section class="userinfo">
-- > <h3>{{ user.username }}</h3>
-- > <p>Role: {{ user.role }}</p>
-- > <p>Status: {% if user.active %}active{% else %}inactive{% endif %}
-- > {% if extraInfo is not none %}
-- > {{ extraInfo }}
-- > {% endif %}
-- > </section>
-- > {% endmacro %}
--
-- Macros can be declared to take any number of arguments, the values of which
-- will be bound to variables in the scope of the macro's body. Defaults can be
-- given for each argument, making it optional; it is considered good practice
-- to list required arguments (without defaults) before optional arguments, so
-- that there is no ambiguity when arguments are given positionally (i.e.,
-- without explicit argument names).
--
-- Inside a macro body, a special variable, @caller@, is made available if the
-- macro was invoked through a @{% call %}@ statement. In that case, @caller@
-- will be a procedure that outputs the body of the @{% call %}@ statement that
-- was used to invoke the macro.
--
-- === @{% call %}@
--
-- Calls a macro (see above) with an additional body, which is passed through
-- the magic @caller()@ procedure.
--
-- Given this macro definition:
--
-- > {% macro sectionize(title) %}
-- > <section>
-- > <h1>{{ title }}</h1>
-- > {{ caller() }}
-- > </section>
-- > {% endmacro %}
--
-- ...the following will call that macro with a @caller()@ body:
--
-- > {% call sectionize("Some Section") %}
-- > Lorem ipsum dolor sit amet.
-- > {% endcall %}
--
-- This will render as:
--
-- > <section>
-- > <h1>Some Section</h1>
-- > Lorem ipsum dolor sit amet.
-- > </section>
--
-- === @{% include %}@
--
-- > {% include "foo.html" %}
--
-- This will load the template "foo.html", and insert its output at this
-- position, as if the template source were pasted in.
--
-- By default, this implies that the included template will have access to the
-- scope of the location where it is included; you can change this by adding
-- @without context@ to the include statement:
--
-- > {% include "foo.html" without context %}
--
-- It is also valid to write @with context@, but since that is the default,
-- this will do nothing.
--
-- Missing templates are an error; if you want to be lenient, you can add
-- @ignore missing@ to the include, which ignore such errors. If both are given,
-- @ignore missing@ must come before @with@ / @without context@.
--
-- > {% include "foo.html" ignore missing without context %}
--
-- === @{% import %}@
--
-- Works much like @include@, however, there are some key differences:
--
-- * @import@ will not inject any output from the imported template. The
--   imported template will still be evaluated in full, and any macros and
--   top-level variables it defines (using @{% macro %}@ and @{% set %}@) will
--   be exported, but any output it generates will be discarded.
-- * @import@ defaults to @without context@, i.e., it does not have access to
--   the scope in which the import statement appears.
-- * Because the main purpose of @import@ is to pull top-level definitions into
--   the importing scope, @import@ supports syntax flavors that import specific
--   exports (macros / variables) selectively, as well as one that binds the
--   exports to a single variable acting as a quasi-namespace.
--
-- To import a template's exports wholesale:
--
-- > {% import "util.html" %}
--
-- To bind an entire template's export to a quasi-namespace dictionary:
--
-- > {% import "util.html" as util %}
--
-- To import specific exports selectively:
--
-- > {% from "util.html" import abbreviateUsername, decorateUser %}
--
-- To import specific exports, renaming them to aliases:
--
-- > {% from "util.html" import abbreviateUsername as abbrev, decorateUser as deco %}
--
-- === @{% extends %}@
--
-- Used for template inheritance:
--
-- > {% extends "parent.html" %}
--
-- Indicates that the current template \"extends\" the template
-- \"parent.html\". To render the current template, Ginger will take any blocks
-- (see below under \"@{% block %}@\") from the current template, and inject
-- them into the corresponding blocks of the parent template.
--
-- Child templates should not directly output anything themselves; they should
-- only override blocks (@{% block %}@) and top-level variables ({% set %}).
--
-- === @{% block %}@
--
-- Blocks are overridable subsections of a template. The @{% block %}@
-- statement is used both to define a block and to override it: the first
-- template in an inheritance chain to use a block name defines it, and
-- determines where it appears in the output; subsequent templates in the
-- chain can override its contents, but not where it appears in the output.
--
-- Maybe the most common use case for this is to have a \"skeleton\" template
-- that defines the overall layout of a web page, and a number of child
-- templates that override those parts that are specific to their respective
-- use cases.
--
-- Example:
--
-- (@skeleton.html@)
--
-- > <!DOCTYPE html>
-- > <html>
-- >   <head>
-- >     <meta charset="utf-8">
-- >     <title>{% block "title" %}Unnamed Page{% endblock %}</title>
-- >     <link rel="stylesheet" href="/static/style.css">
-- >   </head>
-- >   <body>
-- >     {% include "mainnav.html" %}
-- >     <div class="maincontent">
-- >     {% block content %}
-- >     Nothing to see here.
-- >     {% endblock }
-- >     </div>
-- >   </body>
-- > </html>
--
-- (@userlist.html@)
--
-- > {% extends "skeleton.html" %}
-- > {% block "title" %}Users{% endblock %}
-- > {% block "content" %}
-- > <h1>Users</h1>
-- > <ul>
-- > {% for user in users %}
-- >   <li>{{ user.username }}</li>
-- > {% endfor %}
-- > </ul>
-- > {% endblock %}


-- (Autogenerated documentation will be appended here.)

module Language.Ginger
( -- * Interpreting Templates
  ginger
, GingerT
, Eval (..)
, RuntimeError (..)
, Context (..)
, defContext
, Env (..)
, emptyEnv
, defEnv
, defVars
, defVarsCompat
  -- * AST
, Statement (..)
, Expr (..)
, Template (..)
, Block (..)
  -- * Representing Values
, Value (..)
, Scalar (..)
, Encoded (..)
, prettyRuntimeError
, Identifier (..)
  -- * Configuration
, Encoder
, htmlEncoder
, JinjaDialect (..)
  -- * Parser and Parser Options
, POptions (..)
, defPOptions
, BlockTrimming (..)
, BlockStripping (..)
  -- * Template Loaders
, TemplateLoader
, fileLoader
)
where

import Language.Ginger.AST
import Language.Ginger.Value
import Language.Ginger.Interpret
import Language.Ginger.Interpret.Builtins (builtinGlobals, builtinGlobalsNonJinja)
import Language.Ginger.RuntimeError (prettyRuntimeError)
import Language.Ginger.FileLoader
        ( fileLoader
        )
import Language.Ginger.Parse
        ( POptions (..)
        , defPOptions
        , BlockTrimming (..)
        , BlockStripping (..)
        )
import qualified Language.Ginger.Parse as P
import Language.Ginger.Interpret.DefEnv
        ( htmlEncoder
        , defVars
        , defVarsCompat
        , builtinTests
        , builtinFilters
        )

import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Identity (Identity)
import Control.Monad.Except (runExceptT, throwError)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Data.Maybe (fromMaybe)
import Language.Haskell.TH (getDoc, putDoc, DocLoc (..))

data JinjaDialect
  = DialectGinger2
  | DialectJinja2
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | One-stop function for parsing and interpreting a template.
ginger :: forall m. Monad m
       => TemplateLoader m
          -- ^ Template loader to use for loading the initial template and
          -- any included templates. For most use cases, 'fileLoader' should
          -- be appropriate.
       -> POptions
          -- ^ Parser options, determining parser behavior.
       -> JinjaDialect
          -- ^ Jinja dialect; currently determines which built-in globals to
          -- load into the initial namespace.
       -> Encoder m
          -- ^ Encoder to use for automatic encoding. Use 'htmlEncoder' for
          -- HTML templates.
       -> Text
          -- ^ Name of the initial template to load. For the 'fileLoader', this
          -- should be a filename, but for other loaders, it can be whatever
          -- the loader expects.
       -> Map Identifier (Value m)
          -- ^ Variables defined in the initial namespace.
       -> m (Either RuntimeError Encoded)
ginger loader parserOptions dialect encoder templateName vars = runExceptT $ do
  templateSrc <- maybe
                  (throwError $ TemplateFileNotFoundError templateName)
                  pure
                  =<< lift (loader templateName)
  let parseResult = P.parseGingerWith
                      parserOptions
                      P.template
                      (Text.unpack templateName)
                      templateSrc
  template <- either
                (throwError . TemplateParseError templateName . Text.pack)
                pure
                parseResult
  let ctx = defContext
              { contextEncode = encoder
              , contextLoadTemplateFile = loader
              }
      defVars' = case dialect of
                    DialectGinger2 -> defVars
                    DialectJinja2 -> defVarsCompat
      env = defEnv
              { envVars = defVars' <> vars
              }
  eitherExceptM $ runGingerT
    (evalT template >>= encode)
    ctx
    env

$(do
    doc <- fromMaybe "" <$> getDoc ModuleDoc

    let markdownToHaddock :: Text -> Text
        markdownToHaddock =
          Text.replace "'" "\\'" .
          Text.replace "\"" "\\\"" .
          Text.replace "\n" "\n\n" .
          Text.replace "`" "@"

    let goTy :: TypeDoc -> Text
        goTy TypeDocAny = "@any@"
        goTy TypeDocNone = "@none@"
        goTy (TypeDocSingle t) = "@" <> t <> "@"
        goTy (TypeDocAlternatives ts) =
          case Vector.unsnoc ts of
            Just (ts', t) ->
              "@" <> Text.intercalate "@, @" (Vector.toList $ ts') <> "@" <>
              ", or @" <> t <> "@"
            Nothing ->
              "@" <> Text.intercalate "@ or @" (Vector.toList $ ts) <> "@"

    let goItemHeading :: Maybe Text -> Text -> Text -> [Text]
        goItemHeading namespaceMay prefix name =
            [ ""
            , "#" <> prefix <> maybe "" (<> ".") namespaceMay <> name <> "#"
            , ""
            , "=== " <> maybe "" (<> ".") namespaceMay <> name
            ]


    let goDocumentedItem :: Maybe Text -> Text -> (Identifier, ProcedureDoc) -> String
        goDocumentedItem namespaceMay prefix (name, d) =
          let qualifiedName = maybe "" (<> ".") namespaceMay <> identifierName name
          in
            Text.unpack . Text.unlines $
              goItemHeading namespaceMay prefix (identifierName name) ++
              ( if qualifiedName /= procedureDocName d then
                  [ "Alias for [" <> procedureDocName d <> "](#" <> prefix <> procedureDocName d <> ")"
                  ]
                else
                  [ ""
                  , "Arguments:"
                  , ""
                  ] ++
                  [ "* @" <> argumentDocName arg
                  <> case argumentDocDefault arg of
                      Nothing -> ""
                      Just defval -> "=" <> defval
                  <> "@"
                  <> maybe "" ((" : " <>) . goTy) (argumentDocType arg)
                  <> case argumentDocDefault arg of
                      Nothing -> " __(required)__"
                      Just _ -> ""
                  | arg <- Vector.toList (procedureDocArgs d)
                  ] ++
                  [ ""
                  , "Return type: " <> maybe "n/a" goTy (procedureDocReturnType d)
                  , ""
                  , markdownToHaddock $ procedureDocDescription d
                  ]
              )

    let goItem :: Maybe Text -> Text -> (Identifier, Value Identity) -> String
        goItem namespaceMay prefix (name, DictV subitems) =
          let qualifiedName = maybe "" (<> ".") namespaceMay <> identifierName name
              heading = Text.unpack . Text.unlines $
                          [ ""
                          , "=== Module \\'" <> qualifiedName <> "\\'"
                          ]
          in
            heading ++
            unlines
              [ goItem (Just qualifiedName) prefix (Identifier k, v)
              | (StringScalar k, v) <- Map.toAscList subitems
              ]

        goItem namespaceMay prefix (name, ProcedureV (NativeProcedure _ (Just d) _)) =
          goDocumentedItem namespaceMay prefix (name, d)
        goItem namespaceMay prefix (name, FilterV (NativeFilter (Just d) _)) =
          goDocumentedItem namespaceMay prefix (name, d)
        goItem namespaceMay prefix (name, TestV (NativeTest (Just d) _)) =
          goDocumentedItem namespaceMay prefix (name, d)
        goItem namespaceMay prefix (name, _) =
          Text.unpack . Text.unlines $
            goItemHeading namespaceMay prefix (identifierName name)

    putDoc ModuleDoc $
      doc ++
      "\n\n== __List Of Builtin Globals__\n" ++
      "\nThese are available in Jinja, and work (mostly) the same in Ginger.\n" ++
      unlines (map (goItem Nothing "globals_jinja_") (Map.toAscList $ builtinGlobals evalE)) ++
      "\n\n== __List Of Extension Globals__\n" ++
      "\nThese are not available in Jinja\n" ++
      unlines (map (goItem Nothing "globals_ginger_") (Map.toAscList $ builtinGlobalsNonJinja evalE)) ++
      "\n\n== __List Of Builtin Filters__\n" ++
      "\nThese will only work in a filter context, not via procedure call syntax.\n" ++
      unlines (map (goItem Nothing "filters_") (Map.toAscList $ builtinFilters)) ++
      "\n\n== __List Of Builtin Tests__\n" ++
      "\nThese will only work in a test context (e.g., an @is@-expression).\n\n" ++
      "\nSome of these tests shadow globals of the same name but different functionality.\n\n" ++
      unlines (map (goItem Nothing "tests_") (Map.toAscList $ builtinTests))
    pure []
  )
