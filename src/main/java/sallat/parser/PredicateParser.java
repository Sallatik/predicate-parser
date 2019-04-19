package sallat.parser;

import java.util.function.Predicate;

/**
 * A parser of complex {@link Predicate} from a boolean expression represented by a <code>String</code>.<br>
 * Can be used to compile composite filters for arguments of any type at runtime<br>
 * <h1>Syntax</h1>
 * The input <code>String</code> can contain one or more <i>words</i> and parentheses divided by whitespace characters.<br>
 * A word is simply any sequence of characters that does not itself contain whitespace or parentheses.<br>
 * Every word can either represent:
 * <ul>
 *     <li>One of the three boolean operators NOT, AND, and OR.</li>
 *     <li>Elementary boolean function of the argument of generic type.</li>
 * </ul>
 * Words will be mapped to {@link Operators} constants or {@link Predicate} instances respectively using implementation-dependent mappings,
 * and then compiled to a single composite <code>Predicate</code> obeying the rules of boolean algebra.<br>
 * Case-sensitivity depends on a particular implementation.<br>
 * Input string example: <code>"fluffy and not (black or white)"</code>
 * @param <T> Type of elements to be tested by the resulting <code>Predicate</code>
 */
public interface PredicateParser<T> {

    /**
     * Converts a boolean expression to the <code>Predicate</code>.
     * @param expression <code>String</code> representing the boolean expression.
     * @return composite <code>Predicate</code>
     * @throws RuntimeException if syntax is violated in any way
     */
    Predicate<T> parse(String expression);
}
