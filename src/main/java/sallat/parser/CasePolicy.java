package sallat.parser;

/**
 *Constants that determine how {@link SimplePredicateParser} will prepare the input <code>String</code> before processing.
 *<br>
 *Your choice of the case policy should be consistent with the <code>Predicate</code> and <code>Operations</code> mappers you use in the following way:
 *<ul>
 *     <li>If you want the parser to be case-sensitive, <code>CASE_SENSITIVE</code> is the right choice.</li>
 *     <li>If all keys for you mappings are lower-case, <code>TO_LOWER_CASE</code> is the right choice.</li>
 *     <li>If all keys for you mappings are upper-case, <code>TO_UPPER_CASE</code> is the right choice.</li>
 *     <li>If keys for your mappings can have different case, <code>CASE_SENSITIVE</code> is the right choice.</li>
 *</ul>
 */
public enum CasePolicy {

    /**
     *The input string will be processed as-is.
     */
    CASE_SENSITIVE,
    /**
     *The input string will be converted to upper case before processing.
     */
    TO_UPPER_CASE,
    /**
     *The input string will be converted to lower case before processing.
     */
    TO_LOWER_CASE;
}
