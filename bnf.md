This tool defines the following grammar with respect to its file-based support.

    <chaos-specification> ::= <freq-modifier> <cause-spec> <opt-body> | <freq-modifier> <cause-spec> <action-ref> <operator>
    <freq-modifier> ::= "randomly" | "exponentially" | "every" | "in"
    <cause-spec> ::= <spatial-spec> | <temporal-spec>
    <spatial-spec> ::= "TBD"
    <temporal-spec> ::= <integer> <integer> <time-unit>
                      | <integer> <time-unit>
    <time-unit> ::= "seconds" | "minutes" | "hours" | "days"
    <opt-body> ::= <shell-command>
                 | ":" <plain-text>
                 | "@" <url>
    <shell-command> ::= "{" <string> "}"
    <plain-text> ::= '"' <string> '"'
