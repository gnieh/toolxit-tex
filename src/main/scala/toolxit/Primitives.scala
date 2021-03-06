/*
* This file is part of the ToolXiT project.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit

object Primitives {
  /** Set of all primitive control sequence names that shall be expanded */
  val expandablePrimitives = Set(
    // all if primitive control sequences
    "ifnum",
    "ifdim",
    "ifodd",
    "ifvmode",
    "ifhmode",
    "ifmmode",
    "ifinner",
    "if",
    "ifcat",
    "ifx",
    "ifvoid",
    "ifeof",
    "iftrue",
    "iffalse",
    "ifcase",
    // a number
    "number",
    // a roman numeral
    "romannumeral",
    // a raw string
    "string",
    // the job name
    "jobname",
    // the external font name
    "fontname",
    // the meaning of a token
    "meaning",
    // start block of a control sequence name
    "csname",
    // expand after the next token
    "expandafter",
    // inhibits expansion of the next token
    "noexpand",
    // mark registers
    "topmark",
    "firstmark",
    "botmark",
    "splitfirstmark",
    "splitbotmark",
    // input file
    "input",
    // end of input file
    "endinput",
    // the magic and power `the` command
    "the")

  /** Indicates whether the given control sequence name is a primitive if control sequence */
  def isIf(name: String) =
    name.startsWith("if") && expandablePrimitives.contains(name)

  val integerParameter = Set(
    "pretolerance", // (badness tolerance before hyphenation)
    "tolerance", // (badness tolerance after hyphenation)
    "hbadness", // (badness above which bad hboxes will be shown)
    "vbadness", // (badness above which bad vboxes will be shown)
    "linepenalty", // (amount added to badness of every line in a paragraph)
    "hyphenpenalty", // (penalty for line break after discretionary hyphen)
    "exhyphenpenalty", // (penalty for line break after explicit hyphen)
    "binoppenalty", // (penalty for line break after binary operation)
    "relpenalty", // (penalty for line break after math relation)
    "clubpenalty", // (penalty for creating a club line at bottom of page)
    "widowpenalty", // (penalty for creating a widow line at top of page)
    "displaywidowpenalty", // (ditto, before a display)
    "brokenpenalty", // (penalty for page break after a hyphenated line)
    "predisplaypenalty", // (penalty for page break just before a display)
    "postdisplaypenalty", // (penalty for page break just after a display)
    "interlinepenalty", // (additional penalty for page break between lines)
    "floatingpenalty", // (penalty for insertions that are split)
    "outputpenalty", // (penalty at the current page break)
    "doublehyphendemerits", // (demerits for consecutive broken lines)
    "finalhyphendemerits", // (demerits for a penultimate broken line)
    "adjdemerits", // (demerits for adjacent incompatible lines)
    "looseness", // (change to the number of lines in a paragraph)
    "pausing", // (positive if pausing after each line is read from a file)
    "holdinginserts", // (positive if insertions remain dormant in output box)
    "tracingonline", // (positive if showing diagnostic info on the terminal)
    "tracingmacros", // (positive if showing macros as they are expanded)
    "tracingstats", // (positive if showing statistics about memory usage)
    "tracingparagraphs", // (positive if showing line-break calculations)
    "tracingpages", // (positive if showing page-break calculations)
    "tracingoutput", // (positive if showing boxes that are shipped out)
    "tracinglostchars", // (positive if showing characters not in the font)
    "tracingcommands", // (positive if showing commands before they are executed)
    "tracingrestores", // (positive if showing deassignments when groups end)
    "language", // (the current set of hyphenation rules)
    "uchyph", // (positive if hyphenating words beginning with capital letters)
    "lefthyphenmin", // (smallest fragment at beginning of hyphenated word)
    "righthyphenmin", // (smallest fragment at end of hyphenated word)
    "globaldefs", // (nonzero if overriding \global specifications)
    "defaulthyphenchar", // (\hyphenchar value when a font is loaded)
    "defaultskewchar", // (\skewchar value when a font is loaded)
    "escapechar", // (escape character in the output of control sequence tokens)
    "endlinechar", // (character placed at the right end of an input line)
    "newlinechar", // (character that starts a new output line)
    "maxdeadcycles", // (upper bound on \deadcycles)
    "hangafter", // (hanging indentation changes after this many lines)
    "fam", // (the current family number)
    "mag", // (magnification ratio, times 1000)
    "delimiterfactor", // (ratio for variable delimiters, times 1000)
    "time", // (current time of day in minutes since midnight)
    "day", // (current day of the month)
    "month", // (current month of the year)
    "year", // (current year of our Lord)
    "showboxbreadth", // (maximum items per level when boxes are shown)
    "showboxdepth", // (maximum level when boxes are shown)
    "errorcontextlines" // (maximum extra context shown when errors occur)
    )

  val dimenParameter = Set(
    "hfuzz", // (maximum overrun before overfull hbox messages occur)
    "vfuzz", // (maximum overrun before overfull vbox messages occur)
    "overfullrule", // (width of rules appended to overfull boxes)
    "emergencystretch", // (reduces badnesses on final pass of line-breaking)
    "hsize", // (line width in horizontal mode)
    "vsize", // (page height in vertical mode)
    "maxdepth", // (maximum depth of boxes on main pages)
    "splitmaxdepth", // (maximum depth of boxes on split pages)
    "boxmaxdepth", // (maximum depth of boxes on explicit pages)
    "lineskiplimit", // (threshold where \baselineskip changes to \lineskip)
    "delimitershortfall", // (maximum space not covered by a delimiter)
    "nulldelimiterspace", // (width of a null delimiter)
    "scriptspace", // (extra space after subscript or superscript)
    "mathsurround", // (kerning before and after math in text)
    "predisplaysize", // (length of text preceding a display)
    "displaywidth", // (length of line for displayed equation)
    "displayindent", // (indentation of line for displayed equation)
    "parindent", // (width of \indent)
    "hangindent", // (amount of hanging indentation)
    "hoffset", // (horizontal offset in \shipout)
    "voffset" // (vertical offset in \shipout)
    )

  val glueParameter = Set(
    "baselineskip", // (desired glue between baselines)
    "lineskip", // (interline glue if \baselineskip isn’t feasible)
    "parskip", // (extra glue just above paragraphs)
    "abovedisplayskip", // (extra glue just above displays)
    "abovedisplayshortskip", // (ditto, following short lines)
    "belowdisplayskip", // (extra glue just below displays)
    "belowdisplayshortskip", // (ditto, following short lines)
    "leftskip", // (glue at left of justified lines)
    "rightskip", // (glue at right of justified lines)
    "topskip", // (glue at top of main pages)
    "splittopskip", // (glue at top of split pages)
    "tabskip", // (glue between aligned entries)
    "spaceskip", // (glue between words, if nonzero)
    "xspaceskip", // (glue between sentences, if nonzero)
    "parfillskip" // (additional \rightskip at end of paragraphs)
    )

  val muglueParameter = Set(
    "thinmuskip", // (thin space in math formulas)
    "medmuskip", // (medium space in math formulas)
    "thickmuskip" // (thick space in math formulas)
    )

  val tokenParameter = Set(
    "output", // (the user’s output routine)
    "everypar", // (tokens to insert when a paragraph begins)
    "everymath", // (tokens to insert when math in text begins)
    "everydisplay", // (tokens to insert when display math begins)
    "everyhbox", // (tokens to insert when an hbox begins)
    "everyvbox", // (tokens to insert when a vbox begins)
    "everyjob", // (tokens to insert when the job begins)
    "everycr", // (tokens to insert after every \cr or nonredundant \crcr)
    "errhelp" // (tokens that supplement an \errmessage)
    )

  val all =
    expandablePrimitives ++
    integerParameter ++
    dimenParameter ++
    glueParameter ++
    muglueParameter ++
    tokenParameter

}
