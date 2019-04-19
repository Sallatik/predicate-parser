package sallat.parser;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Simple minimal implementation of a <code>PredicateParser</code>.<br>
 * This implementation uses mappers, supplied by the user trough the builder.<br>
 * @param <T> type of arguments for <code>Predicate</code>
 */
public class SimplePredicateParser<T> implements PredicateParser<T> {

    private Function<String, Operators> stringToOperator;
    private Function<String, Predicate<T>> stringToPredicate;

    private CasePolicy casePolicy;

    /*
    Resolve the String token to either Operators constant or Predicate, returning
    the result as Object.
     */
    private Object resolve(String token) {

        try {

            Operators operator = stringToOperator.apply(token);

            if (operator != null)
                return operator;

            Predicate<T> predicate = stringToPredicate.apply(token);

            if (predicate != null)
                return predicate;

        } catch (Exception e) {
            throw new RuntimeException("Unknown token: " + token, e);
        }

        throw new RuntimeException("Unknown token: " + token);
    }

    @Override
    public Predicate<T> parse(String expression) {

        expression = prepare(expression);
        List<Object> tokens = getTokens(expression);
        return process(tokens);
    }

    /*
    Prepare input String before processing by trimming
    and fixing the case according to case policy.
    */
    private String prepare(String expression) {

        expression = expression.trim();

        switch (casePolicy) {

            case TO_LOWER_CASE:
                return expression.toLowerCase();
            case TO_UPPER_CASE:
                return expression.toUpperCase();
            default:
                return expression;
        }
    }

    private List<Object> getTokens(String expression) {

        List<Object> tokens = new ArrayList<>();

        int i = 0;
        while(i < expression.length()) {

            char ch = expression.charAt(i);

            if (Character.isWhitespace(ch))
                // just go to the next character
                i++;

            else if (ch == '(') {

                int matchingParenthesis = findMatchingParenthesis(expression, i);
                String insideParentheses = expression.substring(i + 1, matchingParenthesis);
                // recursively parse expression inside of parentheses to a Predicate token
                tokens.add(parse(insideParentheses));

                // jump to the first character after the matching parenthesis
                i = matchingParenthesis + 1;

            } else {

                int endOfWord = findEndOfWord(expression, i);
                String word = expression.substring(i, endOfWord);
                tokens.add(resolve(word));

                // jump to the first character after the word
                i = endOfWord;
            }
        }

        return tokens;
    }

    /*
    Find index of the first whitespace character
    or left parenthesis or the end of String
    after the specified begin index.
     */
    private int findEndOfWord(String str, int beginIndex) {

        for (int i = beginIndex; i < str.length(); i++) {

            char ch = str.charAt(i);

            if (ch == '(' || Character.isWhitespace(ch))
                return i;
        }

        return str.length();
    }

    /*
    Find the index of matching right parenthesis
    given the index of left parenthesis
     */
    private int findMatchingParenthesis(String str, int beginIndex) {

        int count = 0;

        for (int i = beginIndex; i < str.length(); i++) {

            if (str.charAt(i) == '(')
                count++;

            else if (str.charAt(i) == ')')
                count--;

            if (count == 0)
                return i;
        }

        throw new RuntimeException("Unclosed parenthesis on index " + beginIndex);
    }

    private Predicate<T> process(List<Object> tokens) {

        for (int i = 0; i < tokens.size(); i++)
            if (Operators.NOT.equals(tokens.get(i))) {

                if (i + 1 >= tokens.size())
                    throw new RuntimeException("Operand expected");

                Object operand = tokens.get(i + 1);
                if (! (operand instanceof Predicate))
                    throw new RuntimeException("Illegal operand for NOT: " + operand);

                Predicate<T> result = ((Predicate<T>) operand).negate();

                tokens.remove(i);
                tokens.remove(i);
                tokens.add(i, result);
            }

        for (int i = 0; i < tokens.size(); i++)
            if (Operators.AND.equals(tokens.get(i))) {
                compute(tokens, i, Predicate::and);
                i--;
            }

        for (int i = 0; i < tokens.size(); i++)
            if (Operators.OR.equals(tokens.get(i))) {
                compute(tokens, i, Predicate::or);
                i--;
            }

        if (tokens.size() == 1)
            return (Predicate<T>) tokens.get(0);
        else
            throw new RuntimeException("Unexpected tokens: " + tokens);
    }

    private void compute(List<Object> tokens, int index,
                         BiFunction<Predicate<T>, Predicate<T>, Predicate<T>> operator) {

        if (index - 1 < 0 || index + 1 >= tokens.size())
            throw new RuntimeException("Operand expected");

        Object operand1 = tokens.get(index - 1);
        Object operand2 = tokens.get(index + 1);

        if (!(operand1 instanceof Predicate && operand2 instanceof Predicate))
            throw new RuntimeException("Illegal operands: " + operand1 + ", " + operand2);

        Predicate<T> result = operator.apply((Predicate<T>) operand1, (Predicate<T>) operand2);

        tokens.remove(index - 1);
        tokens.remove(index - 1);
        tokens.remove(index - 1);

        tokens.add(index - 1, result);
    }

    SimplePredicateParser(Function<String, Operators> stringToOperator, Function<String, Predicate<T>> stringToPredicate, CasePolicy casePolicy) {

        this.stringToOperator = stringToOperator;
        this.stringToPredicate = stringToPredicate;
        this.casePolicy = casePolicy;
    }

    /**
     * Creates new builder.
     * @param <T> generic type argument for the instance to be constructed.
     * @return <code>SimplePredicateBuilder</code> instance.
     */
    public static <T> SimplePredicateParserBuilder<T> builder() {
        return new SimplePredicateParserBuilder<>();
    }
}
