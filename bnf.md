This tool defines the following grammar with respect to its file-based support.

    <chaos-specification> ::= <opt-freq-modifier> <cause-spec> <opt-body>
    <opt-freq-modifier> ::= "randomly" | "exponentially" | ""
    <cause-spec> ::= <spatial-spec> | <temporal-spec>
    <spatial-spec> ::= "TBD"
    <temporal-spec> ::= "between" <integer> <integer> <time-unit> 
                      | "every" <integer> <time-unit> 
                      | "in" <integer> <time-unit>
    <time-unit> ::= "seconds" | "minutes" | "hours" | "days"
    <opt-body> ::= <shell-command>
                 | <plain-text>
                 | "@" <url>
    <shell-command> ::= "{" <string> "}"
    <plain-text> ::= '"' <string> '"'
