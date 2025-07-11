# Ginger 2

![](http://ginger.tobiasdammers.nl/static/img/ginger-leaf.svg)

A Haskell implementation of the [Jinja](https://jinja.palletsprojects.com/)
template language.

## Introduction

Ginger2 provides most of the Jinja template language, as much as that makes
sense in a Haskell host application.

We do, however, avoid some of the most blatant Pythonisms, especially where we
felt the features in question were more of an accidental result of binding
template constructs to Python constructs.

We also add some optional features that are absent from the Python
implementation, but that we felt useful and worth adding.

## Installation

Ginger is available from [Hackage](https://hackage.haskell.org/package/ginger2),
and can be installed using the usual methods.

### Installing with Cabal:

    cabal install ginger2

### Using as part of another project:

Add the following to your `.cabal`'s `build-depends`:

    ginger2 ^>=2.0.0

## Template Syntax

Template syntax largely follows
[Jinja2](https://jinja.palletsprojects.com/en/stable/templates/).

Important deviations are listed below.

### Minimal example template

    <!DOCTYPE html>
    <html>
        <head>
            <title>{{ title }}</title>
        </head>
        {# This is a comment. Comments are removed from the output. #}
        <body>
            <menu id="nav-main">
            {% for item in navigation %}
                <li><a href="{{ item.url }}">{{ item.label }}</a></li>
            {% endfor %}
            </menu>
            <div class="layout-content-main">
                <h1>{{ title }}</h1>
                {{ body }}
            </div>
        </body>
    </html>

For a full description of the syntax, refer to the Haddock documentation for the
[Language.Ginger module](https://hackage-content.haskell.org/package/ginger2/docs/Language-Ginger.html)

### Jinja syntax features absent from Ginger 2

- The `{% raw %}` statement does not currently exist in Ginger 2.
- Comments (`{#` `#}`) cannot currently be nested.
- Encoding is automatic and cannot be disabled; however, the Ginger API
  allows a programmer to change the encoding function, so it is possible to use
  Ginger 2 to produce output in any textual output format.
- Line statements (using a leading `#` to mark a line as a statement, instead
  of `{% ... %}` markers).
- Template objects (passing an expression to `extends` that evaluates to a
  template, instead of passing a template name as a string) are not supported.
- Ginger 2 makes no distinction between mutable "lists" and immutable "tuples";
  there are only immutable lists.

### Jinja functions, filters, and tests absent in Ginger 2

#### Functions/filters

- `forceescape`
- `format`
- `indent`
- `pprint`
- `slice`
- `striptags`
- `trim`
- `truncate`
- `unique`
- `urlencode`
- `urlize`
- `wordwrap`
- `xmlattr`
- `range`
- `lipsum`
- `dict`
- `joiner`
- `cycler`

#### Tests

- `escaped` - Due to the way Ginger 2 handles HTML-encoding, this filter makes
  no sense; Ginger 2 automatically tracks which values are "raw" data, and
  which are encoded HTML source, and treats them appropriately, so there should
  never be a need to test for this.
- `sameas` - In Ginger 2, all values (except "namespaces" and native objects)
  are immutable, and passed by-value; hence, there is no meaningful notion of
  "being the same object", and testing for it makes no sense.

## Haskell API

The Haskell API is documented fully through Haddock documentation, available
from [Hackage](https://hackage.haskell.org/package/ginger2). We'll just provide
a short overview here.

### Loading And Running A Template

The most straightforward way to run a template is to use the `ginger` function,
which is parametrized over an underlying "carrier" Monad, `m`, and takes the
following arguments:

- A `TemplateLoader`, a function that takes a template name and returns
  template source. The provided `fileLoader` will work for the normal situation
  of loading templates from source.
- Parser options (`POptions`). The default options (`defPOptions`) will work
  fine for most situations, but you can use this parameter to override parser
  settings.
- A `JinjaDialect`; this determines whether Ginger2 will be (mostly) faithful
  to the original Jinja (`DialectJinja2`), or whether it will add
  Ginger-specific functionality (`DialectGinger2`). The latter is recommended,
  unless compatibility with Jinja is a concern.
- An `Encoder`, which determines how raw data gets encoded to the output source
  format. For HTML output, use `htmlEncoder`; if you want Ginger to not encode
  anything and just output raw data, you can use `pure . Encoded`, which acts
  as a "do-nothing encoder".
- A template name. The exact meaning of the template name depends on the
  template loader; for the `fileLoader`, template names should be filenames
  relative to the file loader's initial path.
- An initial set of defined variables (on top of the built-in ones). This is
  what Jinja calls the "context"; the Ginger2 API however uses the term
  "environment".

### Working With Ginger Values

The main data structure to represent Ginger values is `Value`; this type
captures the "unitype" that Ginger uses internally.

The `ToValue` and `FromValue` typeclasses can be used to marshal Haskell values
to and from Ginger, or you can work with `Value`s directly.

Other types that appear in the surface API are `Identifier` (represents a
variable or attribute name) and `Scalar` (represents a "scalar" type, i.e.,
`none`, booleans, numbers, strings, byte arrays, and encoded strings).

### The `GingerT` Monad Transformer

The `GingerT` transformer captures an execution context/environment in which
Ginger templates (and template fragments) can be evaluated/run. It adds the
following concerns to the transformed base monad:

- Runtime errors (via `MonadExcept`)
- Execution state (variables/scope, and some other stateful concerns)
- Execution context (immutable configuration, including the encoder)

However, in some cases, it is necessary to implement functionality in terms of
the raw base monad, representing failures as `Either`, and passing in context
and, where necessary, execution state, as arguments. This is especially true
when adding user-defined Haskell functions or native Haskell objects into the
execution environment.
