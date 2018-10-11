This tool defines the following grammar with respect to the support for plain-text specification of its rules.

    <rule> ::= <opt-freq-modifier> <cause-spec> <opt-body>
    <opt-freq-modifier> ::= "randomly" | "exponentially" | ""
    <cause-spec> ::= <spatial-spec> | <temporal-spec>
    <spatial-spec> ::= "TBD"
    <temporal-spec> ::= "between" <integer> <integer> <time-unit>
    <time-unit> ::= "SECONDS" | "MINUTES" | "HOURS" | "DAYS"
    <opt-body> ::= "{" <string> "}" | '"' <message> '"'
